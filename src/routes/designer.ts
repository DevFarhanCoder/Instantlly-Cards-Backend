import express, { Request, Response } from 'express';
import jwt from 'jsonwebtoken';
import multer from 'multer';
import Designer from '../models/Designer';
import DesignRequest from '../models/DesignRequest';
import DesignerUpload from '../models/DesignerUpload';
import { uploadToS3 } from '../services/s3Service';

const router = express.Router();

const JWT_SECRET = process.env.JWT_SECRET || 'instantlly-jwt-secret-key';

// Multer memory storage for file uploads
const upload = multer({
  storage: multer.memoryStorage(),
  limits: { fileSize: 50 * 1024 * 1024 }, // 50MB per file
  fileFilter: (_req, file, cb) => {
    const allowedTypes = [
      'image/jpeg', 'image/png', 'image/gif', 'image/webp',
      'video/mp4', 'video/quicktime', 'video/x-msvideo', 'video/webm',
      'application/pdf', 'application/zip',
      'image/svg+xml', 'application/postscript', // design files
    ];
    if (allowedTypes.includes(file.mimetype) || file.mimetype.startsWith('image/') || file.mimetype.startsWith('video/')) {
      cb(null, true);
    } else {
      cb(new Error(`File type ${file.mimetype} not allowed`));
    }
  }
});

// ==========================================
// Designer Auth Middleware
// ==========================================
interface DesignerRequest extends Request {
  designerId?: string;
  designerUsername?: string;
}

function designerAuth(req: DesignerRequest, res: Response, next: any) {
  try {
    const authHeader = req.headers.authorization;
    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      return res.status(401).json({ success: false, message: 'No token provided' });
    }

    const token = authHeader.split(' ')[1];
    const decoded = jwt.verify(token, JWT_SECRET) as any;

    if (decoded.role !== 'designer') {
      return res.status(403).json({ success: false, message: 'Invalid token type' });
    }

    req.designerId = decoded.designerId;
    req.designerUsername = decoded.username;
    next();
  } catch (error) {
    return res.status(401).json({ success: false, message: 'Invalid or expired token' });
  }
}

// ==========================================
// POST /api/designer/login
// ==========================================
router.post('/login', async (req: Request, res: Response) => {
  try {
    const { username, password } = req.body;

    if (!username || !password) {
      return res.status(400).json({ success: false, message: 'Username and password are required' });
    }

    const designer = await Designer.findOne({ username: username.trim() });
    if (!designer) {
      return res.status(401).json({ success: false, message: 'Invalid username or password' });
    }

    const isMatch = await designer.comparePassword(password);
    if (!isMatch) {
      return res.status(401).json({ success: false, message: 'Invalid username or password' });
    }

    const token = jwt.sign(
      { designerId: designer._id, username: designer.username, role: 'designer' },
      JWT_SECRET,
      { expiresIn: '7d' }
    );

    res.json({
      success: true,
      token,
      designer: {
        _id: designer._id,
        username: designer.username
      }
    });
  } catch (error) {
    console.error('Designer login error:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// ==========================================
// GET /api/designer/verify — verify token
// ==========================================
router.get('/verify', designerAuth, async (req: DesignerRequest, res: Response) => {
  try {
    const designer = await Designer.findById(req.designerId).select('-password');
    if (!designer) {
      return res.status(404).json({ success: false, message: 'Designer not found' });
    }
    res.json({ success: true, designer: { _id: designer._id, username: designer.username } });
  } catch (error) {
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// ==========================================
// GET /api/designer/requests — get assigned design requests
// ==========================================
router.get('/requests', designerAuth, async (req: DesignerRequest, res: Response) => {
  try {
    const designRequests = await DesignRequest.find({
      assignedDesignerId: req.designerId
    }).sort({ assignedAt: -1 });

    res.json({ success: true, designRequests });
  } catch (error) {
    console.error('Error fetching designer requests:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// ==========================================
// POST /api/designer/upload/:requestId — upload completed design
// ==========================================
router.post('/upload/:requestId', designerAuth, upload.array('files', 10), async (req: DesignerRequest, res: Response) => {
  try {
    const { requestId } = req.params;
    const { notes } = req.body;
    const files = req.files as Express.Multer.File[];

    if (!files || files.length === 0) {
      return res.status(400).json({ success: false, message: 'No files uploaded' });
    }

    // Verify this request is assigned to this designer
    const designRequest = await DesignRequest.findOne({
      _id: requestId,
      assignedDesignerId: req.designerId
    });

    if (!designRequest) {
      return res.status(404).json({ success: false, message: 'Design request not found or not assigned to you' });
    }

    // Upload files to S3
    const uploadedFiles: Array<{ url: string; key: string; filename: string; contentType: string }> = [];

    for (const file of files) {
      const timestamp = Date.now();
      const sanitizedFilename = file.originalname.replace(/[^a-zA-Z0-9._-]/g, '_');
      const key = `designer-uploads/${req.designerId}/${requestId}/${timestamp}_${sanitizedFilename}`;

      const result = await uploadToS3(file.buffer, key, file.mimetype);
      uploadedFiles.push({
        url: result.url,
        key: result.key,
        filename: file.originalname,
        contentType: file.mimetype
      });
    }

    // Create DesignerUpload record
    const designerUpload = new DesignerUpload({
      designRequestId: requestId,
      designerId: req.designerId,
      designerName: req.designerUsername,
      filesS3: uploadedFiles,
      notes: notes || '',
      status: 'uploaded'
    });

    await designerUpload.save();

    // Update design request status to completed
    designRequest.status = 'completed';
    await designRequest.save();

    res.json({
      success: true,
      message: 'Design uploaded successfully',
      upload: designerUpload
    });
  } catch (error) {
    console.error('Error uploading design:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// ==========================================
// GET /api/designer/completed — get designer's completed work
// ==========================================
router.get('/completed', designerAuth, async (req: DesignerRequest, res: Response) => {
  try {
    const uploads = await DesignerUpload.find({
      designerId: req.designerId
    })
      .populate('designRequestId', 'businessName adType uploaderName uploaderPhone status')
      .sort({ createdAt: -1 });

    res.json({ success: true, uploads });
  } catch (error) {
    console.error('Error fetching completed work:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

export default router;
