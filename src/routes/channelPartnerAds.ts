import express, { Request, Response } from 'express';
import multer from 'multer';
import { GridFSBucket, ObjectId } from 'mongodb';
import mongoose from 'mongoose';
import Ad from '../models/Ad';
import AWS from "aws-sdk";
import User from '../models/User';
import Transaction from '../models/Transaction';
import DesignRequest from '../models/DesignRequest';
import DesignerUpload from '../models/DesignerUpload';

import { uploadPendingAdMedia, uploadDesignRequestMedia } from '../services/s3Service';

import { Readable } from 'stream';

const router = express.Router();

// Configure multer for memory storage - images only
export  const upload = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 16 * 1024 * 1024, // 16MB limit for images
  },
  fileFilter: (req, file, cb) => {
    // Accept images only
    // if (!file.mimetype.startsWith('image/')) {
    //   cb(new Error('Only image files are allowed'));
    //   return;
    // }
    cb(null, true);
  },
});

// Configure AWS S3
export  const s3 = new AWS.S3({
  region: process.env.AWS_REGION,
  credentials: {
    accessKeyId: process.env.AWS_ACCESS_KEY_ID!,
    secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY!,
  },
});

// Configure multer for video uploads
const uploadVideos = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 100 * 1024 * 1024, // 100MB limit for videos
    fieldSize: 100 * 1024 * 1024, // 100MB field size
  },
  fileFilter: (req, file, cb) => {
    // Accept videos only
    if (!file.mimetype.startsWith('video/')) {
      cb(new Error('Only video files are allowed'));
      return;
    }
    cb(null, true);
  },
});

// Configure multer for mixed uploads (images + videos) - supports both
const uploadMixed = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 50 * 1024 * 1024, // 50MB limit (covers both 16MB images and 50MB videos)
  },
  fileFilter: (req, file, cb) => {
    // Accept images and videos
    if (!file.mimetype.startsWith('image/') && !file.mimetype.startsWith('video/')) {
      cb(new Error('Only image and video files are allowed'));
      return;
    }
    cb(null, true);
  },
});

/**
 * POST /api/channel-partner/ads
 * Upload a new ad (status = 'pending')
 * NO AUTH REQUIRED - Anyone can create ads, admin will review
 */
router.post(
  '/',
  upload.any(), // Max 5 images
  async (req: Request, res: Response) => {
    try {
      console.log('?? Ad upload request (no auth):', {
        body: req.body,
        filesCount: (req.files as Express.Multer.File[])?.length || 0,
      });

      const { title, phoneNumber, startDate, endDate, uploaderName, priority = 1, bottomMediaType = "image", fullscreenMediaType = "image" } = req.body;
      const files = req.files as Express.Multer.File[];

      // Validation
      if (!title || !phoneNumber || !startDate || !endDate) {
        return res.status(400).json({
          message: 'Missing required fields',
          required: ['title', 'phoneNumber', 'startDate', 'endDate'],
        });
      }
      // Support BOTH 'bottomImage' AND 'images' fieldnames (mobile app sends 'images')
      const allImages = files?.filter(f => f.fieldname === "images" || f.fieldname === "bottomImage" || f.fieldname === "fullscreenImage") || [];
      const bottomImage = files?.find(f => f.fieldname === "bottomImage") || allImages[0];
      const fullscreenImage = files?.find(f => f.fieldname === "fullscreenImage") || allImages[1];
      const bottomVideo = files?.find(f => f.fieldname === "bottomVideo");
      const fullscreenVideo = files?.find(f => f.fieldname === "fullscreenVideo");

       if (bottomMediaType === "image" && !bottomImage) {
        return res.status(400).json({ message: "Bottom image required" });
      }

      if (bottomMediaType === "video" && !bottomVideo) {
        return res.status(400).json({ message: "Bottom video required" });
      }

      if (bottomVideo && !bottomVideo.mimetype.startsWith("video/")) {
        return res.status(400).json({ message: "Bottom video must be video format" });
      }
      if (fullscreenMediaType === "video" && fullscreenVideo && !fullscreenVideo.mimetype.startsWith("video/")) {
        return res.status(400).json({ message: "Fullscreen video format invalid" });
      }


      // if (!files || files.length === 0) {
      //   return res.status(400).json({ message: 'At least one image is required (bottom image)' });
      // }

      // if (files.length > 2) {
      //   return res.status(400).json({ message: 'Maximum 2 images allowed (bottom image and optional fullscreen)' });
      // }
      let bottomImageId = null
      let fullscreenImageId = null;
      let bottomVideoUrl = null;
      let fullscreenVideoUrl = null;
      let bottomImageS3Result: { url: string; key: string } | null = null;
      let fullscreenImageS3Result: { url: string; key: string } | null = null;
      let bottomVideoS3Result: { url: string; key: string } | null = null;
      let fullscreenVideoS3Result: { url: string; key: string } | null = null;
      // Validate dates
      const start = new Date(startDate);
      const end = new Date(endDate);

      if (isNaN(start.getTime()) || isNaN(end.getTime())) {
        return res.status(400).json({ message: 'Invalid date format' });
      }

      if (end <= start) {
        return res.status(400).json({ message: 'End date must be after start date' });
      }

      // CREDIT CHECK - 1020 credits required (+ 180 cash after admin approval)
      // Use main database User model for credits
      const uploaderPhone = req.body.uploaderPhone || phoneNumber;

      console.log('?? Looking for user with phone:', uploaderPhone);

      // Connect to Channel Partner database (separate database named 'channelpartner')
      // const channelPartnerDB = mongoose.connection.useDb('channelpartner');
      // const ChannelPartnerUser = channelPartnerDB.model('User', new mongoose.Schema({
      //   phone: String,
      //   credits: Number,
      //   creditsHistory: [{
      //     type: String,
      //     amount: Number,
      //     description: String,
      //     date: Date
      //   }]
      // }));

      // let user = await ChannelPartnerUser.findOne({ phone: uploaderPhone });

      // // If not found, try without country code prefix
      // if (!user && uploaderPhone.startsWith('+91')) {
      //   const phoneWithoutPrefix = uploaderPhone.substring(3);
      //   console.log('?? Trying without +91 prefix:', phoneWithoutPrefix);
      //   user = await ChannelPartnerUser.findOne({ phone: phoneWithoutPrefix });
      // }

      // // If still not found, try WITH country code prefix
      // if (!user && !uploaderPhone.startsWith('+')) {
      //   const phoneWithPrefix = '+91' + uploaderPhone;
      //   console.log('?? Trying with +91 prefix:', phoneWithPrefix);
      //   user = await ChannelPartnerUser.findOne({ phone: phoneWithPrefix });
      // }

      // console.log('?? Found user:', user ? `${user.phone} with ${user.credits} credits` : 'NOT FOUND');

      // if (!user) {
      //   // Try to find any user to see what's in the database
      //   const allUsers = await ChannelPartnerUser.find({}).limit(5);
      //   console.log('?? Sample users in database:', allUsers.map(u => ({ phone: u.phone, credits: u.credits })));
      //   return res.status(404).json({
      //     message: 'User not found. Please ensure you are logged in.',
      //     searchedPhone: uploaderPhone,
      //     hint: 'Check phone number format (+91XXXXXXXXXX or XXXXXXXXXX)'
      //   });
      // }

      // const currentCredits = user.credits || 0;

      // if (currentCredits < 1020) {
      //   return res.status(400).json({
      //     message: 'Insufficient credits. You need 1020 credits to create an ad.',
      //     currentCredits: currentCredits,
      //     required: 1020
      //   });
      // }

      // // Deduct 1020 credits
      // user.credits = currentCredits - 1020;
      // user.creditsHistory = user.creditsHistory || [];
      // (user.creditsHistory as any).push({
      //   type: 'deduction',
      //   amount: -1020,
      //   description: `Ad creation: ${title}`,
      //   date: new Date()
      // });
      // await user.save();

      // console.log(`? Deducted 1020 credits from ${uploaderPhone}. Remaining: ${user.credits}`);

      // ---- IMAGES â†’ S3 ----
      const tempAdId = Date.now().toString();

      if (bottomMediaType === "image" && bottomImage) {
        bottomImageS3Result = await uploadPendingAdMedia(
          bottomImage.buffer,
          bottomImage.originalname,
          uploaderPhone,
          tempAdId,
          bottomImage.mimetype
        );
        console.log('âœ… Bottom image uploaded to S3:', bottomImageS3Result.url);
      }

      if (fullscreenMediaType === "image" && fullscreenImage) {
        fullscreenImageS3Result = await uploadPendingAdMedia(
          fullscreenImage.buffer,
          fullscreenImage.originalname,
          uploaderPhone,
          tempAdId,
          fullscreenImage.mimetype
        );
        console.log('âœ… Fullscreen image uploaded to S3:', fullscreenImageS3Result.url);
      }

      // ---- VIDEO ? S3 ----
      if (bottomMediaType === "video" && bottomVideo) {
        const key = `ads/bottom/${Date.now()}-${bottomVideo.originalname}`;
        await s3.putObject({
          Bucket: process.env.S3_BUCKET!,
          Key: key,
          Body: bottomVideo.buffer,
          ContentType: bottomVideo.mimetype,
          // ACL: "public-read",
        }).promise();

        bottomVideoUrl = `${process.env.CLOUDFRONT_HOST}/${key}`;
        bottomVideoS3Result = { url: bottomVideoUrl, key };
      }
      console.log('Bottom video uploaded to S3:', bottomVideoUrl);

      if (fullscreenMediaType === "video" && fullscreenVideo) {
        const key = `ads/fullscreen/${Date.now()}-${fullscreenVideo.originalname}`;
        await s3.putObject({
          Bucket: process.env.S3_BUCKET!,
          Key: key,
          Body: fullscreenVideo.buffer,
          ContentType: fullscreenVideo.mimetype,
          // ACL: "public-read",
        }).promise();

        fullscreenVideoUrl = `${process.env.CLOUDFRONT_HOST}/${key}`;
        fullscreenVideoS3Result = { url: fullscreenVideoUrl, key };
      }
      console.log('Fullscreen video uploaded to S3:', fullscreenVideoUrl);
      // ---- SAVE AD ----

      // Create ad with pending status
      const ad = new Ad({
        title,
        phoneNumber,
        startDate: start,
        endDate: end,
        status: 'pending', // Requires admin approval
        uploadedBy: uploaderPhone, // Use actual uploader phone for filtering
        uploaderName: uploaderName || 'Channel Partner', // Use user's name, fallback to "Mobile User"
        priority: priority ? parseInt(priority) : 1, // Lower priority for channel partner ads
        clicks: 0,
        impressions: 0,
        bottomMediaType,
        fullscreenMediaType,
        bottomImageGridFS: bottomImageId,
        fullscreenImageGridFS: fullscreenImageId,
        bottomImageS3: bottomImageS3Result || undefined,
        fullscreenImageS3: fullscreenImageS3Result || undefined,
        bottomVideoS3: bottomVideoS3Result || undefined,
        fullscreenVideoS3: fullscreenVideoS3Result || undefined,
        bottomVideoUrl,
        fullscreenVideoUrl,
      });

      await ad.save();

      console.log(`? Ad created with pending status (1020 credits deducted):`, {
        id: ad._id,
        title: ad.title,
        uploadedBy: ad.uploadedBy,
        status: ad.status,
        creditsDeducted: 1020
      });

      res.status(201).json({
        message: 'Ad submitted successfully! 1020 credits deducted. Admin will review your ad. You will need to pay ?180 after approval.',
        creditsDeducted: 1020,
        // remainingCredits: user.credits,
        cashPaymentRequired: 180,
        totalCost: '1020 credits + ?180 cash',
        ad: {
          id: ad._id,
          title: ad.title,
          phoneNumber: ad.phoneNumber,
          startDate: ad.startDate,
          endDate: ad.endDate,
          status: ad.status,
          uploadedBy: ad.uploadedBy,
          uploaderName: ad.uploaderName,
          priority: ad.priority,
          hasBottomImage: !!bottomImageId || !!bottomImageS3Result,
          hasFullscreenImage: !!fullscreenImageId || !!fullscreenImageS3Result,
          bottomImageS3Url: bottomImageS3Result?.url || null,
          fullscreenImageS3Url: fullscreenImageS3Result?.url || null,
          bottomVideoS3Url: bottomVideoS3Result?.url || null,
          fullscreenVideoS3Url: fullscreenVideoS3Result?.url || null,
          createdAt: ad.createdAt,
        },
      });
    } catch (error) {
      console.error('? Channel partner ad upload error:', error);

      if (error instanceof Error) {
        return res.status(500).json({
          message: 'Failed to upload ad',
          error: error.message,
        });
      }

      res.status(500).json({ message: 'Failed to upload ad' });
    }
  }
);

/**
 * POST /api/channel-partner/ads/video
 * Upload a new VIDEO ad (status = 'pending')
 * NO AUTH REQUIRED - Anyone can create ads, admin will review
 */
router.post(
  '/video',
  uploadVideos.array('videos', 2), // Max 2 videos (bottom + fullscreen)
  async (req: Request, res: Response) => {
    try {
      console.log('?? Video Ad upload request (no auth):', {
        body: req.body,
        filesCount: (req.files as Express.Multer.File[])?.length || 0,
      });

      const { title, phoneNumber, startDate, endDate, uploaderName, priority } = req.body;
      const uploaderPhone = req.body.uploaderPhone || phoneNumber;
      const files = req.files as Express.Multer.File[];

      // Validation
      if (!title || !phoneNumber || !startDate || !endDate) {
        return res.status(400).json({
          message: 'Missing required fields',
          required: ['title', 'phoneNumber', 'startDate', 'endDate'],
        });
      }

      if (!files || files.length === 0) {
        return res.status(400).json({ message: 'At least one video is required (bottom video)' });
      }

      if (files.length > 2) {
        return res.status(400).json({ message: 'Maximum 2 videos allowed (bottom video and optional fullscreen)' });
      }

      // Validate dates
      const start = new Date(startDate);
      const end = new Date(endDate);

      if (isNaN(start.getTime()) || isNaN(end.getTime())) {
        return res.status(400).json({ message: 'Invalid date format' });
      }

      if (end <= start) {
        return res.status(400).json({ message: 'End date must be after start date' });
      }

      // CREDIT CHECK - 1020 credits required (+ 180 cash after admin approval)
      // uploaderPhone already declared above
      const userId = req.body.userId;
      
      console.log('?? Looking for user - phone:', uploaderPhone, 'userId:', userId);
      
      let user = null;
      
      // First try to find by userId if provided
      if (userId && userId.length > 0) {
        try {
          user = await User.findById(userId);
          console.log('?? Found by userId:', user ? user.name : 'NOT FOUND');
        } catch (e) {
          console.log('?? Invalid userId format');
        }
      }
      
      // If not found by ID, try phone number matching
      if (!user) {
        // Clean phone number - extract only digits
        const cleanPhone = uploaderPhone.replace(/[^0-9]/g, '');
        console.log('?? Cleaned phone (digits only):', cleanPhone);
      
        // Find user in main database - try multiple methods
        user = await User.findOne({ phone: uploaderPhone });
        
        // Try without country code prefix
        if (!user && uploaderPhone.startsWith('+91')) {
          const phoneWithoutPrefix = uploaderPhone.substring(3);
          user = await User.findOne({ phone: phoneWithoutPrefix });
        }
        
        // Try with +91 prefix
        if (!user && !uploaderPhone.startsWith('+')) {
          const phoneWithPrefix = '+91' + uploaderPhone;
          user = await User.findOne({ phone: phoneWithPrefix });
        }

        // Try with +880 prefix (Bangladesh)
        if (!user && !uploaderPhone.startsWith('+')) {
          const phoneWithPrefix = '+880' + uploaderPhone;
          user = await User.findOne({ phone: phoneWithPrefix });
        }

        // Try regex search - match phones ending with these digits
        if (!user && cleanPhone.length >= 10) {
          const last10Digits = cleanPhone.slice(-10);
          console.log('?? Trying regex search with last 10 digits:', last10Digits);
          user = await User.findOne({ phone: { $regex: last10Digits + '$' } });
        }

        // Last resort - search by contains
        if (!user && cleanPhone.length >= 8) {
          console.log('?? Trying contains search with digits:', cleanPhone);
          user = await User.findOne({ phone: { $regex: cleanPhone } });
        }
      }
      
      console.log('?? Found user:', user ? `${user.name} (${user.phone}) with ${(user as any).credits} credits` : 'NOT FOUND');
      
      if (!user) {
        return res.status(404).json({ 
          message: 'User not found. Please ensure you are logged in.',
          searchedPhone: uploaderPhone,
        });
      }

      const currentCredits = (user as any).credits || 0;
      
      if (currentCredits < 1020) {
        return res.status(400).json({ 
          message: 'Insufficient credits. You need 1020 credits to create an ad.',
          currentCredits: currentCredits,
          required: 1020
        });
      }

      // Deduct 1020 credits from main User model
      (user as any).credits = currentCredits - 1020;
      await user.save();

      // Create transaction record for the deduction
      await Transaction.create({
        type: 'ad_deduction',
        fromUser: user._id,
        toUser: null,
        amount: -1200,
        description: `Video Ad creation: ${title}`,
        balanceBefore: currentCredits,
        balanceAfter: currentCredits - 1200,
        status: 'completed'
      });

      console.log(`? Deducted 1200 credits from ${user.name} (${uploaderPhone}). Remaining: ${(user as any).credits}`);

      // Upload videos to GridFS
      const db = mongoose.connection.db;
      if (!db) {
        throw new Error('Database connection not established');
      }

      // Use the same bucket as images - 'adImages' for both
      const bucket = new GridFSBucket(db, { bucketName: 'adImages' });
      
      // Upload bottom video (required)
      const bottomVideoFile = files[0];
      const bottomVideoStream = bucket.openUploadStream(bottomVideoFile.originalname, {
        contentType: bottomVideoFile.mimetype,
        metadata: {
          type: 'bottom_video',
          uploadedBy: uploaderPhone,
        }
      });

      const bottomReadable = Readable.from(bottomVideoFile.buffer);
      const bottomVideoId = await new Promise<ObjectId>((resolve, reject) => {
        bottomReadable
          .pipe(bottomVideoStream)
          .on('finish', () => resolve(bottomVideoStream.id as ObjectId))
          .on('error', reject);
      });

      console.log(`? Uploaded bottom video to GridFS: ${bottomVideoId}`);

      // Upload fullscreen video (optional)
      let fullscreenVideoId: ObjectId | undefined;
      if (files.length > 1) {
        const fullscreenFile = files[1];
        const fullscreenStream = bucket.openUploadStream(fullscreenFile.originalname, {
          contentType: fullscreenFile.mimetype,
          metadata: {
            type: 'fullscreen_video',
            uploadedBy: uploaderPhone,
          }
        });

        const fullscreenReadable = Readable.from(fullscreenFile.buffer);
        fullscreenVideoId = await new Promise<ObjectId>((resolve, reject) => {
          fullscreenReadable
            .pipe(fullscreenStream)
            .on('finish', () => resolve(fullscreenStream.id as ObjectId))
            .on('error', reject);
        });

        console.log(`? Uploaded fullscreen video to GridFS: ${fullscreenVideoId}`);
      }

      // Create ad with pending status and video type
      const ad = new Ad({
        title,
        adType: 'video',
        bottomVideo: '',
        bottomVideoGridFS: bottomVideoId,
        fullscreenVideo: '',
        fullscreenVideoGridFS: fullscreenVideoId,
        phoneNumber,
        startDate: start,
        endDate: end,
        status: 'pending',
        uploadedBy: uploaderPhone,
        uploaderName: uploaderName || 'Channel Partner',
        priority: priority ? parseInt(priority) : 1,
        clicks: 0,
        impressions: 0,
      });

      await ad.save();

      console.log(`? Video Ad created with pending status:`, {
        id: ad._id,
        title: ad.title,
        adType: 'video',
        uploadedBy: ad.uploadedBy,
        status: ad.status,
        creditsDeducted: 1020
      });

      res.status(201).json({
        message: 'Video ad submitted successfully! 1200 credits deducted. Admin will review your ad.',
        creditsDeducted: 1200,
        remainingCredits: user.credits,
        ad: {
          id: ad._id,
          title: ad.title,
          adType: 'video',
          phoneNumber: ad.phoneNumber,
          startDate: ad.startDate,
          endDate: ad.endDate,
          status: ad.status,
          uploadedBy: ad.uploadedBy,
          uploaderName: ad.uploaderName,
          priority: ad.priority,
          hasBottomVideo: !!bottomVideoId,
          hasFullscreenVideo: !!fullscreenVideoId,
          createdAt: ad.createdAt,
        },
      });
    } catch (error) {
      console.error('? Video ad upload error:', error);

      if (error instanceof Error) {
        return res.status(500).json({
          message: 'Failed to upload video ad',
          error: error.message,
        });
      }

      res.status(500).json({ message: 'Failed to upload video ad' });
    }
  }
);

/**
 * GET /api/channel-partner/ads/video/:id
 * Stream a video from GridFS
 */
router.get('/video/:id', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    
    if (!ObjectId.isValid(id)) {
      return res.status(400).json({ message: 'Invalid video ID' });
    }

    const db = mongoose.connection.db;
    if (!db) {
      throw new Error('Database connection not established');
    }

    // Use the same bucket as images - gridfsService uses 'adImages' for both
    const bucket = new GridFSBucket(db, { bucketName: 'adImages' });
    
    // Check if file exists
    const files = await bucket.find({ _id: new ObjectId(id) }).toArray();
    if (files.length === 0) {
      return res.status(404).json({ message: 'Video not found' });
    }

    const file = files[0];
    
    // Set appropriate headers
    res.set('Content-Type', file.contentType || 'video/mp4');
    res.set('Content-Length', file.length.toString());
    res.set('Accept-Ranges', 'bytes');
    
    // Handle range requests for video streaming
    const range = req.headers.range;
    if (range) {
      const parts = range.replace(/bytes=/, '').split('-');
      const start = parseInt(parts[0], 10);
      const end = parts[1] ? parseInt(parts[1], 10) : file.length - 1;
      const chunkSize = end - start + 1;

      res.status(206);
      res.set('Content-Range', `bytes ${start}-${end}/${file.length}`);
      res.set('Content-Length', chunkSize.toString());

      bucket.openDownloadStream(new ObjectId(id), { start, end: end + 1 }).pipe(res);
    } else {
      bucket.openDownloadStream(new ObjectId(id)).pipe(res);
    }
  } catch (error) {
    console.error('? Video streaming error:', error);
    res.status(500).json({ message: 'Failed to stream video' });
  }
});


/**
 * PUT /api/channel-partner/ads/design-requests/:id/assign
 * Assign a designer to a design request (for admin)
 */
router.put('/design-requests/:id/assign', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const { designerId, designerName } = req.body;

    if (!mongoose.Types.ObjectId.isValid(id)) {
      return res.status(400).json({ message: 'Invalid design request ID' });
    }

    if (!designerId || !designerName) {
      return res.status(400).json({ message: 'designerId and designerName are required' });
    }

    const designRequest = await DesignRequest.findById(id);
    if (!designRequest) {
      return res.status(404).json({ message: 'Design request not found' });
    }

    designRequest.assignedDesignerId = designerId;
    designRequest.assignedDesignerName = designerName;
    designRequest.assignedAt = new Date();
    if (designRequest.status === 'pending') {
      designRequest.status = 'in-progress';
    }

    await designRequest.save();

    res.json({
      success: true,
      message: 'Designer assigned successfully',
      designRequest: {
        id: designRequest._id,
        assignedDesignerId: designRequest.assignedDesignerId,
        assignedDesignerName: designRequest.assignedDesignerName,
        assignedAt: designRequest.assignedAt,
        status: designRequest.status,
        updatedAt: designRequest.updatedAt,
      },
    });
  } catch (error) {
    console.error('Error assigning designer:', error);
    res.status(500).json({ message: 'Failed to assign designer' });
  }
});
/**
 * GET /api/channel-partner/ads/image/:id
 * Stream an image from GridFS by its ObjectId
 */
router.get('/image/:id', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    console.log('Image request for GridFS ID:', id);

    if (!ObjectId.isValid(id)) {
      return res.status(400).json({ message: 'Invalid image ID' });
    }

    const db = mongoose.connection.db;
    if (!db) throw new Error('Database connection not established');

    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Cache-Control', 'public, max-age=86400');

    const bucketNames = ['adImages', 'fs', 'images', 'uploads'];
    let foundBucket: GridFSBucket | null = null;

    for (const bucketName of bucketNames) {
      try {
        const bucket = bucketName === 'fs'
          ? new GridFSBucket(db)
          : new GridFSBucket(db, { bucketName });
        const files = await bucket.find({ _id: new ObjectId(id) }).toArray();
        if (files.length > 0) {
          foundBucket = bucket;
          break;
        }
      } catch (e) { /* continue */ }
    }

    if (!foundBucket) {
      return res.status(404).json({ message: 'Image not found in any bucket' });
    }

    res.setHeader('Content-Type', 'image/jpeg');
    const downloadStream = foundBucket.openDownloadStream(new ObjectId(id));
    downloadStream.pipe(res);
    downloadStream.on('error', (err) => {
      console.error('GridFS stream error:', err);
      if (!res.headersSent) res.status(500).json({ message: 'Failed to stream image' });
    });
  } catch (error) {
    console.error('Image serving error:', error);
    if (!res.headersSent) res.status(500).json({ message: 'Failed to serve image' });
  }
});
/**
 * GET /api/channel-partner/ads
 * Get all ads - NO AUTH REQUIRED
 * Query param: phone - to filter by uploader phone
 */
router.get('/', async (req: Request, res: Response) => {
  try {
    const { phone } = req.query;

    console.log('?? Fetching ads (no auth):', phone ? `for phone ${phone}` : 'all ads');

    const filter = phone ? { uploadedBy: phone } : {};
    const ads = await Ad.find(filter)
      .sort({ createdAt: -1 }) // Most recent first
      .select('-__v');

    console.log(`? Found ${ads.length} ads`);

    // Transform ads for response
    const adsWithDetails = ads.map((ad) => {
      // S3/CloudFront URLs (preferred over GridFS)
      const bottomS3Url = (ad as any).bottomImageS3?.url || null;
      const fullscreenS3Url = (ad as any).fullscreenImageS3?.url || null;
      const bottomVidS3Url = (ad as any).bottomVideoS3?.url || null;
      const fullscreenVidS3Url = (ad as any).fullscreenVideoS3?.url || null;

      return {
        id: ad._id,
        title: ad.title,
        adType: (ad as any).adType || 'image',
        phoneNumber: ad.phoneNumber,
        startDate: ad.startDate,
        endDate: ad.endDate,
        status: ad.status,
        uploadedBy: ad.uploadedBy,
        uploaderName: ad.uploaderName,
        approvedBy: ad.approvedBy,
        approvalDate: ad.approvalDate,
        rejectionReason: ad.rejectionReason,
        priority: ad.priority,
        bottomImageId: ad.bottomImageGridFS,
        fullscreenImageId: ad.fullscreenImageGridFS,
        bottomVideoId: (ad as any).bottomVideoGridFS,
        fullscreenVideoId: (ad as any).fullscreenVideoGridFS,
        // S3/CloudFront URLs
        bottomImageS3Url: bottomS3Url,
        fullscreenImageS3Url: fullscreenS3Url,
        bottomVideoS3Url: bottomVidS3Url,
        fullscreenVideoS3Url: fullscreenVidS3Url,
        hasBottomImage: !!(ad.bottomImageGridFS || bottomS3Url),
        hasFullscreenImage: !!(ad.fullscreenImageGridFS || fullscreenS3Url),
        hasBottomVideo: !!((ad as any).bottomVideoGridFS || bottomVidS3Url),
        hasFullscreenVideo: !!((ad as any).fullscreenVideoGridFS || fullscreenVidS3Url),
        impressions: ad.impressions,
        clicks: ad.clicks,
        createdAt: ad.createdAt,
        updatedAt: ad.updatedAt,
      };
    });

    res.json({
      count: adsWithDetails.length,
      ads: adsWithDetails,
    });
  } catch (error) {
    console.error('? Error fetching channel partner ads:', error);
    res.status(500).json({ message: 'Failed to fetch ads' });
  }
});

/**
 * PUT /api/channel-partner/ads/:id
 * Edit/update an existing ad - NO AUTH REQUIRED
 * Edited ads will go back to 'pending' status for re-approval
 */
router.put(
  '/:id',
  upload.array('images', 5),
  async (req: Request, res: Response) => {
    try {
      const { id } = req.params;

      if (!mongoose.Types.ObjectId.isValid(id)) {
        return res.status(400).json({ message: 'Invalid ad ID' });
      }

      const ad = await Ad.findById(id);

      if (!ad) {
        return res.status(404).json({ message: 'Ad not found' });
      }

      const { title, phoneNumber, startDate, endDate, uploaderName } = req.body;
      const files = req.files as Express.Multer.File[];

      // Update text fields if provided
      if (title) ad.title = title;
      if (phoneNumber) ad.phoneNumber = phoneNumber;
      if (uploaderName) ad.uploaderName = uploaderName;

      if (startDate) {
        const start = new Date(startDate);
        if (!isNaN(start.getTime())) {
          ad.startDate = start;
        }
      }

      if (endDate) {
        const end = new Date(endDate);
        if (!isNaN(end.getTime())) {
          ad.endDate = end;
        }
      }

      // Validate dates
      if (ad.endDate <= ad.startDate) {
        return res.status(400).json({ message: 'End date must be after start date' });
      }

      // Update images if provided
      if (files && files.length > 0) {
        const db = mongoose.connection.db;
        if (!db) {
          throw new Error('Database connection not established');
        }

        const bucket = new GridFSBucket(db, { bucketName: 'adImages' });

        // Delete old images from GridFS
        const oldImageIds: ObjectId[] = [];
        if (ad.bottomImageGridFS) oldImageIds.push(ad.bottomImageGridFS as ObjectId);
        if (ad.fullscreenImageGridFS) oldImageIds.push(ad.fullscreenImageGridFS as ObjectId);

        for (const imageId of oldImageIds) {
          try {
            await bucket.delete(imageId);
          } catch (error) {
            
            console.warn(`?? Failed to delete old image ${imageId}:`, error);
          }
        }

        // Upload new bottom image
        const bottomImageFile = files[0];
        const bottomImageStream = bucket.openUploadStream(bottomImageFile.originalname, {
          contentType: bottomImageFile.mimetype,
        });

        const bottomReadable = Readable.from(bottomImageFile.buffer);
        const bottomImageId = await new Promise<ObjectId>((resolve, reject) => {
          bottomReadable
            .pipe(bottomImageStream)
            .on('finish', () => resolve(bottomImageStream.id as ObjectId))
            .on('error', reject);
        });

        ad.bottomImageGridFS = bottomImageId;

        // Upload new fullscreen image if provided
        if (files.length > 1) {
          const fullscreenFile = files[1];
          const fullscreenStream = bucket.openUploadStream(fullscreenFile.originalname, {
            contentType: fullscreenFile.mimetype,
          });

          const fullscreenReadable = Readable.from(fullscreenFile.buffer);
          const fullscreenImageId = await new Promise<ObjectId>((resolve, reject) => {
            fullscreenReadable
              .pipe(fullscreenStream)
              .on('finish', () => resolve(fullscreenStream.id as ObjectId))
              .on('error', reject);
          });

          ad.fullscreenImageGridFS = fullscreenImageId;
        } else {
          ad.fullscreenImageGridFS = undefined;
        }
      }

      // Reset to pending status for re-approval
      ad.status = 'pending';
      ad.approvedBy = '';
      ad.approvalDate = null as any;
      ad.rejectionReason = '';

      await ad.save();

      console.log(`? Ad updated and reset to pending: ${id}`);

      res.json({
        message: 'Ad updated successfully. Awaiting admin re-approval.',
        ad: {
          id: ad._id,
          title: ad.title,
          phoneNumber: ad.phoneNumber,
          startDate: ad.startDate,
          endDate: ad.endDate,
          status: ad.status,
          updatedAt: ad.updatedAt,
        },
      });
    } catch (error) {
      console.error('? Error updating ad:', error);
      res.status(500).json({ message: 'Failed to update ad' });
    }
  }
);

/**
 * POST /api/channel-partner/ads/design-request
 * Submit a design request when user doesn't have ad design
 * Fields: webLinks (array), phoneNumber, adText, businessAddress, referenceImages, referenceVideos
 * NO AUTH REQUIRED - Anyone can submit design requests
 */
router.post(
  '/design-request',
  uploadMixed.fields([
    { name: 'referenceImages', maxCount: 10 },
    { name: 'referenceVideos', maxCount: 5 },
  ]),
  async (req: Request, res: Response) => {
    try {
      console.log('?? Design Request submission:', {
        body: req.body,
        files: req.files,
      });

      const { businessName, email, webLinks, phoneNumber, adText, businessAddress, uploaderPhone, uploaderName, userId, adType, channelType } = req.body;
      const files = req.files as { [fieldname: string]: Express.Multer.File[] };

      // Parse webLinks if it's a JSON string
      let parsedWebLinks: string[] = [];
      try {
        if (webLinks) {
          parsedWebLinks = typeof webLinks === 'string' ? JSON.parse(webLinks) : webLinks;
          parsedWebLinks = parsedWebLinks.filter(link => link && link.trim().length > 0);
        }
      } catch (e) {
        parsedWebLinks = [];
      }

      // Validation - at least one content should be provided
      const hasWebLinks = parsedWebLinks.length > 0;
      const hasPhoneNumber = phoneNumber && phoneNumber.trim().length > 0;
      const hasAdText = adText && adText.trim().length > 0;
      const hasBusinessAddress = businessAddress && businessAddress.trim().length > 0;
      const hasImages = files?.referenceImages && files.referenceImages.length > 0;
      const hasVideos = files?.referenceVideos && files.referenceVideos.length > 0;

      if (!hasWebLinks && !hasPhoneNumber && !hasAdText && !hasBusinessAddress && !hasImages && !hasVideos) {
        return res.status(400).json({
          message: 'Please provide at least one of: Web Links, Phone Number, Ad Text, Business Address, or Images/Videos',
        });
      }

      if (!uploaderPhone) {
        return res.status(400).json({
          message: 'Uploader phone number is required',
        });
      }

      const db = mongoose.connection.db;
      if (!db) {
        throw new Error('Database connection not established');
      }

      // Arrays to store S3 media
      const referenceImagesS3: { url: string; key: string }[] = [];
      const referenceVideosS3: { url: string; key: string }[] = [];

      // Generate a temporary request ID for S3 key organization
      const tempRequestId = new ObjectId().toString();

      // Upload multiple reference images to S3 if provided
      if (hasImages) {
        for (let i = 0; i < files.referenceImages.length; i++) {
          const imageFile = files.referenceImages[i];
          const filename = `reference_img_${Date.now()}_${i}${imageFile.originalname.substring(imageFile.originalname.lastIndexOf('.'))}`;

          const s3Result = await uploadDesignRequestMedia(
            imageFile.buffer,
            filename,
            userId || 'unknown',
            uploaderPhone,
            tempRequestId,
            imageFile.mimetype
          );

          referenceImagesS3.push(s3Result);
          console.log(`? Uploaded reference image ${i + 1} to S3:`, s3Result.key);
        }
      }

      // Upload multiple reference videos to S3 if provided
      if (hasVideos) {
        for (let i = 0; i < files.referenceVideos.length; i++) {
          const videoFile = files.referenceVideos[i];
          const filename = `reference_vid_${Date.now()}_${i}${videoFile.originalname.substring(videoFile.originalname.lastIndexOf('.'))}`;

          const s3Result = await uploadDesignRequestMedia(
            videoFile.buffer,
            filename,
            userId || 'unknown',
            uploaderPhone,
            tempRequestId,
            videoFile.mimetype
          );

          referenceVideosS3.push(s3Result);
          console.log(`? Uploaded reference video ${i + 1} to S3:`, s3Result.key);
        }
      }

      // Create design request
      const designRequest = new DesignRequest({
        businessName: businessName?.trim() || '',
        email: email?.trim() || '',
        webLinks: parsedWebLinks,
        phoneNumber: phoneNumber?.trim() || '',
        adText: adText?.trim() || '',
        businessAddress: businessAddress?.trim() || '',
        adType: adType || 'image',
        channelType: channelType || 'withoutChannel',
        referenceImagesS3: referenceImagesS3,
        referenceVideosS3: referenceVideosS3,
        uploaderPhone,
        uploaderName: uploaderName || 'Mobile User',
        userId: userId || '',
        status: 'pending',
      });

      await designRequest.save();

      console.log(`? Design Request created:`, {
        id: designRequest._id,
        uploaderPhone: designRequest.uploaderPhone,
        uploaderName: designRequest.uploaderName,
        webLinksCount: parsedWebLinks.length,
        hasPhoneNumber,
        hasAdText,
        hasBusinessAddress,
        imagesCount: referenceImagesS3.length,
        videosCount: referenceVideosS3.length,
      });

      res.status(201).json({
        success: true,
        message: 'Design request submitted successfully! Our team will create the ad design for you.',
        designRequest: {
          id: designRequest._id,
          businessName: designRequest.businessName,
          email: designRequest.email,
          webLinks: designRequest.webLinks,
          phoneNumber: designRequest.phoneNumber,
          adText: designRequest.adText,
          businessAddress: designRequest.businessAddress,
          adType: designRequest.adType,
          channelType: designRequest.channelType,
          referenceImagesCount: referenceImagesS3.length,
          referenceVideosCount: referenceVideosS3.length,
          status: designRequest.status,
          createdAt: designRequest.createdAt,
        },
      });
    } catch (error) {
      console.error('? Design request submission error:', error);

      if (error instanceof Error) {
        return res.status(500).json({
          message: 'Failed to submit design request',
          error: error.message,
        });
      }

      res.status(500).json({ message: 'Failed to submit design request' });
    }
  }
);

/**
 * GET /api/channel-partner/ads/design-requests/all
 * Get all design requests (admin)
 */
router.get('/design-requests/all', async (req: Request, res: Response) => {
  try {
    console.log('Fetching all design requests...');
    const designRequests = await DesignRequest.find({}).sort({ createdAt: -1 }).limit(500);
    console.log('Found ' + designRequests.length + ' design requests');
    res.json({
      success: true,
      count: designRequests.length,
      designRequests: designRequests.map(dr => ({
        _id: dr._id,
        businessName: dr.businessName || '',
        email: dr.email || '',
        webLinks: dr.webLinks || [],
        phoneNumber: dr.phoneNumber || '',
        adText: dr.adText || '',
        businessAddress: dr.businessAddress || '',
        adType: dr.adType,
        channelType: dr.channelType,
        referenceImagesGridFS: dr.referenceImagesGridFS || [],
        referenceVideosGridFS: dr.referenceVideosGridFS || [],
        referenceImagesS3: dr.referenceImagesS3 || [],
        referenceVideosS3: dr.referenceVideosS3 || [],
        uploaderPhone: dr.uploaderPhone,
        uploaderName: dr.uploaderName,
        userId: dr.userId || '',
        status: dr.status,
        adminNotes: dr.adminNotes || '',
        completedAdId: dr.completedAdId || null,
        createdAt: dr.createdAt,
        updatedAt: dr.updatedAt,
      })),
    });
  } catch (error) {
    console.error('Error fetching all design requests:', error);
    res.status(500).json({ message: 'Failed to fetch design requests' });
  }
});

/**
 * GET /api/channel-partner/ads/design-requests
 * Get design requests for a user by phone
 */
router.get('/design-requests', async (req: Request, res: Response) => {
  try {
    const { phone } = req.query;

    if (!phone) {
      return res.status(400).json({ message: 'Phone number is required' });
    }

    const designRequests = await DesignRequest.find({ uploaderPhone: phone as string })
      .sort({ createdAt: -1 })
      .limit(50);

    res.json({
      success: true,
      count: designRequests.length,
      designRequests: designRequests.map(dr => ({
        id: dr._id,
        webLinks: dr.webLinks || [],
        phoneNumber: dr.phoneNumber || '',
        adText: dr.adText,
        businessAddress: dr.businessAddress || '',
        adType: dr.adType,
        channelType: dr.channelType,
        referenceImagesCount: (dr.referenceImagesGridFS?.length || 0) + (dr.referenceImagesS3?.length || 0),
        referenceVideosCount: (dr.referenceVideosGridFS?.length || 0) + (dr.referenceVideosS3?.length || 0),
        status: dr.status,
        adminNotes: dr.adminNotes,
        createdAt: dr.createdAt,
        updatedAt: dr.updatedAt,
      })),
    });
  } catch (error) {
    console.error('? Error fetching design requests:', error);
    res.status(500).json({ message: 'Failed to fetch design requests' });
  }
});

/**
 * DELETE /api/channel-partner/ads/:id
 * Delete ad (only if status is 'pending') - NO AUTH REQUIRED
 */
router.delete('/:id', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;

    if (!mongoose.Types.ObjectId.isValid(id)) {
      return res.status(400).json({ message: 'Invalid ad ID' });
    }

    const ad = await Ad.findById(id);

    if (!ad) {
      return res.status(404).json({ message: 'Ad not found' });
    }

    // Only allow deletion if pending
    if (ad.status !== 'pending') {
      return res.status(400).json({
        message: 'Only pending ads can be deleted. Contact admin for approved/rejected ads.',
      });
    }

    // Delete images from GridFS
    const db = mongoose.connection.db;
    if (db) {
      const bucket = new GridFSBucket(db, { bucketName: 'adImages' });
      const imageIdsToDelete: ObjectId[] = [];

      if (ad.bottomImageGridFS) {
        imageIdsToDelete.push(ad.bottomImageGridFS as ObjectId);
      }
      if (ad.fullscreenImageGridFS) {
        imageIdsToDelete.push(ad.fullscreenImageGridFS as ObjectId);
      }

      for (const imageId of imageIdsToDelete) {
        try {
          await bucket.delete(imageId);
        } catch (error) {
          console.warn(`?? Failed to delete image ${imageId}:`, error);
        }
      }
    }

    await ad.deleteOne();

    console.log(`? Deleted pending ad: ${id}`);

    res.json({ message: 'Ad deleted successfully' });
  } catch (error) {
    console.error('? Error deleting ad:', error);
    res.status(500).json({ message: 'Failed to delete ad' });
  }
});


// ==========================================
// GET /designs-for-approval?phone=
// Returns completed designs sent to user for approval (mobile app)
// ==========================================
router.get('/designs-for-approval', async (req: Request, res: Response) => {
  try {
    const phone = req.query.phone as string;
    if (!phone) {
      return res.status(400).json({ message: 'Phone number is required' });
    }

    const designRequests = await DesignRequest.find({ uploaderPhone: phone });
    if (!designRequests.length) {
      return res.json({ designs: [] });
    }

    const designRequestIds = designRequests.map(dr => dr._id);

    const uploads = await DesignerUpload.find({
      designRequestId: { $in: designRequestIds },
      status: { $in: ['sent-to-user', 'approved', 'rejected'] }
    }).sort({ createdAt: -1 });

    const designs = uploads.map(upload => {
      const dr = designRequests.find((r: any) => r._id.toString() === upload.designRequestId.toString());
      return {
        id: upload._id,
        designRequestId: upload.designRequestId,
        designerName: upload.designerName,
        businessName: dr?.businessName || '',
        adType: dr?.adType || 'image',
        status: upload.status === 'sent-to-user' ? 'pending-approval'
              : upload.status === 'approved' ? 'approved'
              : 'changes-requested',
        designFiles: (upload.filesS3 || []).map((f: any) => ({
          url: f.url,
          type: (f.contentType && f.contentType.startsWith('video')) ? 'video' : 'image',
          name: f.filename || 'design-file'
        })),
        adminMessage: upload.adminNotes || '',
        userFeedback: upload.userFeedback || '',
        sentAt: upload.createdAt,
        respondedAt: upload.updatedAt !== upload.createdAt ? upload.updatedAt : undefined
      };
    });

    res.json({ designs });
  } catch (error) {
    console.error('Error fetching designs for approval:', error);
    res.status(500).json({ message: 'Failed to fetch designs' });
  }
});

// ==========================================
// POST /designs/:id/approve
// User approves a completed design
// ==========================================
router.post('/designs/:id/approve', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;

    if (!mongoose.Types.ObjectId.isValid(id)) {
      return res.status(400).json({ message: 'Invalid ID' });
    }

    const upload = await DesignerUpload.findById(id);
    if (!upload) {
      return res.status(404).json({ message: 'Design not found' });
    }

    upload.status = 'approved';
    await upload.save();

    const designRequest = await DesignRequest.findById(upload.designRequestId);
    if (designRequest) {
      designRequest.status = 'completed';
      await designRequest.save();
    }

    res.json({ success: true, message: 'Design approved successfully' });
  } catch (error) {
    console.error('Error approving design:', error);
    res.status(500).json({ message: 'Failed to approve design' });
  }
});

// ==========================================
// POST /designs/:id/request-changes
// User requests changes to a design
// ==========================================
router.post('/designs/:id/request-changes', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const { feedback } = req.body;

    if (!mongoose.Types.ObjectId.isValid(id)) {
      return res.status(400).json({ message: 'Invalid ID' });
    }

    const upload = await DesignerUpload.findById(id);
    if (!upload) {
      return res.status(404).json({ message: 'Design not found' });
    }

    upload.status = 'rejected';
    upload.userFeedback = feedback || '';
    await upload.save();

    const designRequest = await DesignRequest.findById(upload.designRequestId);
    if (designRequest) {
      designRequest.status = 'in-progress';
      await designRequest.save();
    }

    res.json({ success: true, message: 'Feedback sent successfully' });
  } catch (error) {
    console.error('Error requesting changes:', error);
    res.status(500).json({ message: 'Failed to send feedback' });
  }
});

export default router;



