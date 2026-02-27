"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const jsonwebtoken_1 = __importDefault(require("jsonwebtoken"));
const multer_1 = __importDefault(require("multer"));
const https_1 = __importDefault(require("https"));
const http_1 = __importDefault(require("http"));
const Designer_1 = __importDefault(require("../models/Designer"));
const DesignRequest_1 = __importDefault(require("../models/DesignRequest"));
const DesignerUpload_1 = __importDefault(require("../models/DesignerUpload"));
const s3Service_1 = require("../services/s3Service");
const router = express_1.default.Router();
const JWT_SECRET = process.env.JWT_SECRET || 'instantlly-jwt-secret-key';
// Multer memory storage for file uploads
const upload = (0, multer_1.default)({
    storage: multer_1.default.memoryStorage(),
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
        }
        else {
            cb(new Error(`File type ${file.mimetype} not allowed`));
        }
    }
});
function designerAuth(req, res, next) {
    try {
        const authHeader = req.headers.authorization;
        if (!authHeader || !authHeader.startsWith('Bearer ')) {
            return res.status(401).json({ success: false, message: 'No token provided' });
        }
        const token = authHeader.split(' ')[1];
        const decoded = jsonwebtoken_1.default.verify(token, JWT_SECRET);
        if (decoded.role !== 'designer') {
            return res.status(403).json({ success: false, message: 'Invalid token type' });
        }
        req.designerId = decoded.designerId;
        req.designerUsername = decoded.username;
        next();
    }
    catch (error) {
        return res.status(401).json({ success: false, message: 'Invalid or expired token' });
    }
}
// ==========================================
// POST /api/designer/login
// ==========================================
router.post('/login', async (req, res) => {
    try {
        const { username, password } = req.body;
        if (!username || !password) {
            return res.status(400).json({ success: false, message: 'Username and password are required' });
        }
        const designer = await Designer_1.default.findOne({ username: username.trim() });
        if (!designer) {
            return res.status(401).json({ success: false, message: 'Invalid username or password' });
        }
        const isMatch = await designer.comparePassword(password);
        if (!isMatch) {
            return res.status(401).json({ success: false, message: 'Invalid username or password' });
        }
        const token = jsonwebtoken_1.default.sign({ designerId: designer._id, username: designer.username, role: 'designer' }, JWT_SECRET, { expiresIn: '7d' });
        res.json({
            success: true,
            token,
            designer: {
                _id: designer._id,
                username: designer.username
            }
        });
    }
    catch (error) {
        console.error('Designer login error:', error);
        res.status(500).json({ success: false, message: 'Server error' });
    }
});
// ==========================================
// GET /api/designer/verify — verify token
// ==========================================
router.get('/verify', designerAuth, async (req, res) => {
    try {
        const designer = await Designer_1.default.findById(req.designerId).select('-password');
        if (!designer) {
            return res.status(404).json({ success: false, message: 'Designer not found' });
        }
        res.json({ success: true, designer: { _id: designer._id, username: designer.username } });
    }
    catch (error) {
        res.status(500).json({ success: false, message: 'Server error' });
    }
});
// ==========================================
// GET /api/designer/requests — get assigned design requests
// ==========================================
router.get('/requests', designerAuth, async (req, res) => {
    try {
        const designRequests = await DesignRequest_1.default.find({
            assignedDesignerId: req.designerId
        }).sort({ assignedAt: -1 }).lean();
        // Attach the latest DesignerUpload info (userFeedback, approval status) to each request
        const requestIds = designRequests.map((dr) => dr._id);
        const uploads = await DesignerUpload_1.default.find({
            designRequestId: { $in: requestIds }
        }).sort({ createdAt: -1 }).lean();
        const enrichedRequests = designRequests.map((dr) => {
            const upload = uploads.find((u) => u.designRequestId.toString() === dr._id.toString());
            return {
                ...dr,
                userApprovalStatus: upload ? upload.status : null,
                userFeedback: upload ? upload.userFeedback || '' : '',
            };
        });
        res.json({ success: true, designRequests: enrichedRequests });
    }
    catch (error) {
        console.error('Error fetching designer requests:', error);
        res.status(500).json({ success: false, message: 'Server error' });
    }
});
// ==========================================
// POST /api/designer/upload/:requestId — upload completed design
// ==========================================
router.post('/upload/:requestId', designerAuth, upload.array('files', 10), async (req, res) => {
    try {
        const { requestId } = req.params;
        const { notes } = req.body;
        const files = req.files;
        if (!files || files.length === 0) {
            return res.status(400).json({ success: false, message: 'No files uploaded' });
        }
        // Verify this request is assigned to this designer
        const designRequest = await DesignRequest_1.default.findOne({
            _id: requestId,
            assignedDesignerId: req.designerId
        });
        if (!designRequest) {
            return res.status(404).json({ success: false, message: 'Design request not found or not assigned to you' });
        }
        // Upload files to S3
        const uploadedFiles = [];
        for (const file of files) {
            const timestamp = Date.now();
            const sanitizedFilename = file.originalname.replace(/[^a-zA-Z0-9._-]/g, '_');
            const key = `designer-uploads/${req.designerId}/${requestId}/${timestamp}_${sanitizedFilename}`;
            const result = await (0, s3Service_1.uploadToS3)(file.buffer, key, file.mimetype);
            uploadedFiles.push({
                url: result.url,
                key: result.key,
                filename: file.originalname,
                contentType: file.mimetype
            });
        }
        // Create DesignerUpload record
        const designerUpload = new DesignerUpload_1.default({
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
    }
    catch (error) {
        console.error('Error uploading design:', error);
        res.status(500).json({ success: false, message: 'Server error' });
    }
});
// ==========================================
// GET /api/designer/completed — get designer's completed work
// ==========================================
router.get('/completed', designerAuth, async (req, res) => {
    try {
        const uploads = await DesignerUpload_1.default.find({
            designerId: req.designerId
        })
            .populate('designRequestId', 'businessName adType uploaderName uploaderPhone status')
            .sort({ createdAt: -1 });
        res.json({ success: true, uploads });
    }
    catch (error) {
        console.error('Error fetching completed work:', error);
        res.status(500).json({ success: false, message: 'Server error' });
    }
});
// Proxy download for S3/CloudFront files (bypasses CORS)
router.get('/proxy-download', (req, res) => {
    const fileUrl = req.query.url;
    if (!fileUrl) {
        return res.status(400).json({ message: 'url parameter required' });
    }
    // Only allow downloads from our CloudFront/S3 domains
    const allowedHosts = ['d1rjsfuv5lw0hw.cloudfront.net', 'instantlly-media-prod.s3.amazonaws.com', 'instantlly-media-prod.s3.ap-south-1.amazonaws.com'];
    try {
        const parsedUrl = new URL(fileUrl);
        if (!allowedHosts.some(h => parsedUrl.hostname.includes(h))) {
            return res.status(403).json({ message: 'Domain not allowed' });
        }
        const client = parsedUrl.protocol === 'https:' ? https_1.default : http_1.default;
        client.get(fileUrl, (proxyRes) => {
            if (proxyRes.statusCode !== 200) {
                return res.status(proxyRes.statusCode || 500).json({ message: 'Failed to fetch file' });
            }
            res.setHeader('Content-Type', proxyRes.headers['content-type'] || 'application/octet-stream');
            res.setHeader('Content-Disposition', 'attachment');
            if (proxyRes.headers['content-length']) {
                res.setHeader('Content-Length', proxyRes.headers['content-length']);
            }
            proxyRes.pipe(res);
        }).on('error', (err) => {
            console.error('Proxy download error:', err);
            res.status(500).json({ message: 'Download failed' });
        });
    }
    catch (e) {
        res.status(400).json({ message: 'Invalid URL' });
    }
});
exports.default = router;
