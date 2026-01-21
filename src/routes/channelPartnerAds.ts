import express, { Request, Response } from 'express';
import multer from 'multer';
import { GridFSBucket, ObjectId } from 'mongodb';
import mongoose from 'mongoose';
import Ad from '../models/Ad';
import AWS from "aws-sdk";
import User from '../models/User';
import Transaction from '../models/Transaction';
import DesignRequest from '../models/DesignRequest';

import { Readable } from 'stream';

const router = express.Router();

// Configure multer for memory storage - images only
const upload = multer({
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
const s3 = new AWS.S3({
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
      console.log('📤 Ad upload request (no auth):', {
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
      const bottomImage = files.find(f => f.fieldname === "bottomImage");
      const bottomVideo = files.find(f => f.fieldname === "bottomVideo");
      const fullscreenImage = files.find(f => f.fieldname === "fullscreenImage");
      const fullscreenVideo = files.find(f => f.fieldname === "fullscreenVideo");

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

      console.log('🔍 Looking for user with phone:', uploaderPhone);

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
      //   console.log('🔄 Trying without +91 prefix:', phoneWithoutPrefix);
      //   user = await ChannelPartnerUser.findOne({ phone: phoneWithoutPrefix });
      // }

      // // If still not found, try WITH country code prefix
      // if (!user && !uploaderPhone.startsWith('+')) {
      //   const phoneWithPrefix = '+91' + uploaderPhone;
      //   console.log('🔄 Trying with +91 prefix:', phoneWithPrefix);
      //   user = await ChannelPartnerUser.findOne({ phone: phoneWithPrefix });
      // }

      // console.log('👤 Found user:', user ? `${user.phone} with ${user.credits} credits` : 'NOT FOUND');

      // if (!user) {
      //   // Try to find any user to see what's in the database
      //   const allUsers = await ChannelPartnerUser.find({}).limit(5);
      //   console.log('📋 Sample users in database:', allUsers.map(u => ({ phone: u.phone, credits: u.credits })));
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

      // console.log(`✅ Deducted 1020 credits from ${uploaderPhone}. Remaining: ${user.credits}`);

      // Upload images to GridFS
      const db = mongoose.connection.db;
      if (!db) {
        throw new Error('Database connection not established');
      }

      const bucket = new GridFSBucket(db, { bucketName: 'uploads' });

      // Upload bottom image (required)
      // const bottomImageFile = files[0];
      // const bottomImageStream = bucket.openUploadStream(bottomImageFile.originalname, {
      //   contentType: bottomImageFile.mimetype,
      // });

      // const bottomReadable = Readable.from(bottomImageFile.buffer);
      // await new Promise<ObjectId>((resolve, reject) => {
      //   bottomReadable
      //     .pipe(bottomImageStream)
      //     .on('finish', () => resolve(bottomImageStream.id as ObjectId))
      //     .on('error', reject);
      // });

      // console.log(`✅ Uploaded bottom image to GridFS: ${bottomImageId}`);

      // Upload fullscreen image (optional)
      // ObjectId | undefined;
      // if (files.length > 1) {
      //   const fullscreenFile = files[1];
      //   const fullscreenStream = bucket.openUploadStream(fullscreenFile.originalname, {
      //     contentType: fullscreenFile.mimetype,
      //   });

      // const fullscreenReadable = Readable.from(fullscreenFile.buffer);
      // fullscreenImageId = await new Promise<ObjectId>((resolve, reject) => {
      //   fullscreenReadable
      //     .pipe(fullscreenStream)
      //     .on('finish', () => resolve(fullscreenStream.id as ObjectId))
      //     .on('error', reject);
      // });
      if (bottomMediaType === "image" && bottomImage) {
        bottomImageId = await new Promise<ObjectId>((resolve, reject) => {
          const stream = bucket.openUploadStream(bottomImage.originalname);
          Readable.from(bottomImage.buffer)
            .pipe(stream)
            .on("finish", () => resolve(stream.id))
            .on("error", reject);
        });
      }
       console.log(`✅ Uploaded bottom image to GridFS: ${bottomImageId}`);


      if (fullscreenMediaType === "image" && fullscreenImage) {
        fullscreenImageId = await new Promise<ObjectId>((resolve, reject) => {
          const stream = bucket.openUploadStream(fullscreenImage.originalname);
          Readable.from(fullscreenImage.buffer)
            .pipe(stream)
            .on("finish", () => resolve(stream.id))
            .on("error", reject);
        });
      }
      // }
      // ---- VIDEO → S3 ----
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
        bottomVideoUrl,
        fullscreenVideoUrl,
      });

      await ad.save();

      console.log(`✅ Ad created with pending status (1020 credits deducted):`, {
        id: ad._id,
        title: ad.title,
        uploadedBy: ad.uploadedBy,
        status: ad.status,
        creditsDeducted: 1020
      });

      res.status(201).json({
        message: 'Ad submitted successfully! 1020 credits deducted. Admin will review your ad. You will need to pay ₹180 after approval.',
        creditsDeducted: 1020,
        // remainingCredits: user.credits,
        cashPaymentRequired: 180,
        totalCost: '1020 credits + ₹180 cash',
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
          hasBottomImage: !!bottomImageId,
          hasFullscreenImage: !!fullscreenImageId,
          createdAt: ad.createdAt,
        },
      });
    } catch (error) {
      console.error('❌ Channel partner ad upload error:', error);

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
      console.log('📤 Video Ad upload request (no auth):', {
        body: req.body,
        filesCount: (req.files as Express.Multer.File[])?.length || 0,
      });

      const { title, phoneNumber, startDate, endDate, uploaderName, priority } = req.body;
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
      const uploaderPhone = req.body.uploaderPhone || phoneNumber;
      const userId = req.body.userId;
      
      console.log('🔍 Looking for user - phone:', uploaderPhone, 'userId:', userId);
      
      let user = null;
      
      // First try to find by userId if provided
      if (userId && userId.length > 0) {
        try {
          user = await User.findById(userId);
          console.log('👤 Found by userId:', user ? user.name : 'NOT FOUND');
        } catch (e) {
          console.log('⚠️ Invalid userId format');
        }
      }
      
      // If not found by ID, try phone number matching
      if (!user) {
        // Clean phone number - extract only digits
        const cleanPhone = uploaderPhone.replace(/[^0-9]/g, '');
        console.log('🔍 Cleaned phone (digits only):', cleanPhone);
      
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
          console.log('🔄 Trying regex search with last 10 digits:', last10Digits);
          user = await User.findOne({ phone: { $regex: last10Digits + '$' } });
        }

        // Last resort - search by contains
        if (!user && cleanPhone.length >= 8) {
          console.log('🔄 Trying contains search with digits:', cleanPhone);
          user = await User.findOne({ phone: { $regex: cleanPhone } });
        }
      }
      
      console.log('👤 Found user:', user ? `${user.name} (${user.phone}) with ${(user as any).credits} credits` : 'NOT FOUND');
      
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

      console.log(`✅ Deducted 1200 credits from ${user.name} (${uploaderPhone}). Remaining: ${(user as any).credits}`);

      // Upload videos to GridFS
      const db = mongoose.connection.db;
      if (!db) {
        throw new Error('Database connection not established');
      }

      const bucket = new GridFSBucket(db, { bucketName: 'adVideos' });
      
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

      console.log(`✅ Uploaded bottom video to GridFS: ${bottomVideoId}`);

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

        console.log(`✅ Uploaded fullscreen video to GridFS: ${fullscreenVideoId}`);
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

      console.log(`✅ Video Ad created with pending status:`, {
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
      console.error('❌ Video ad upload error:', error);

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

    const bucket = new GridFSBucket(db, { bucketName: 'adVideos' });
    
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
    console.error('❌ Video streaming error:', error);
    res.status(500).json({ message: 'Failed to stream video' });
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

    console.log('📋 Fetching ads (no auth):', phone ? `for phone ${phone}` : 'all ads');

    const filter = phone ? { uploadedBy: phone } : {};
    const ads = await Ad.find(filter)
      .sort({ createdAt: -1 }) // Most recent first
      .select('-__v');

    console.log(`✅ Found ${ads.length} ads`);

    // Transform ads for response
    const adsWithDetails = ads.map((ad) => ({
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
      hasBottomImage: !!ad.bottomImageGridFS,
      hasFullscreenImage: !!ad.fullscreenImageGridFS,
      hasBottomVideo: !!(ad as any).bottomVideoGridFS,
      hasFullscreenVideo: !!(ad as any).fullscreenVideoGridFS,
      impressions: ad.impressions,
      clicks: ad.clicks,
      createdAt: ad.createdAt,
      updatedAt: ad.updatedAt,
    }));

    res.json({
      count: adsWithDetails.length,
      ads: adsWithDetails,
    });
  } catch (error) {
    console.error('❌ Error fetching channel partner ads:', error);
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
            
            console.warn(`⚠️ Failed to delete old image ${imageId}:`, error);
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

      console.log(`✅ Ad updated and reset to pending: ${id}`);

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
      console.error('❌ Error updating ad:', error);
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
      console.log('📤 Design Request submission:', {
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

      const bucket = new GridFSBucket(db, { bucketName: 'adImages' });
      
      const referenceImageIds: ObjectId[] = [];
      const referenceVideoIds: ObjectId[] = [];

      // Upload multiple reference images if provided
      if (hasImages) {
        for (let i = 0; i < files.referenceImages.length; i++) {
          const imageFile = files.referenceImages[i];
          const imageReadable = new Readable();
          imageReadable.push(imageFile.buffer);
          imageReadable.push(null);

          const imageStream = bucket.openUploadStream(`design_ref_img_${Date.now()}_${i}.jpg`, {
            contentType: imageFile.mimetype,
            metadata: {
              type: 'design-reference-image',
              originalName: imageFile.originalname,
            },
          });

          const imageId = await new Promise<ObjectId>((resolve, reject) => {
            imageReadable
              .pipe(imageStream)
              .on('finish', () => resolve(imageStream.id as ObjectId))
              .on('error', reject);
          });

          referenceImageIds.push(imageId);
          console.log(`✅ Uploaded reference image ${i + 1} to GridFS: ${imageId}`);
        }
      }

      // Upload multiple reference videos if provided
      if (hasVideos) {
        for (let i = 0; i < files.referenceVideos.length; i++) {
          const videoFile = files.referenceVideos[i];
          const videoReadable = new Readable();
          videoReadable.push(videoFile.buffer);
          videoReadable.push(null);

          const videoStream = bucket.openUploadStream(`design_ref_vid_${Date.now()}_${i}.mp4`, {
            contentType: videoFile.mimetype,
            metadata: {
              type: 'design-reference-video',
              originalName: videoFile.originalname,
            },
          });

          const videoId = await new Promise<ObjectId>((resolve, reject) => {
            videoReadable
              .pipe(videoStream)
              .on('finish', () => resolve(videoStream.id as ObjectId))
              .on('error', reject);
          });

          referenceVideoIds.push(videoId);
          console.log(`✅ Uploaded reference video ${i + 1} to GridFS: ${videoId}`);
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
        referenceImagesGridFS: referenceImageIds,
        referenceVideosGridFS: referenceVideoIds,
        uploaderPhone,
        uploaderName: uploaderName || 'Mobile User',
        userId: userId || '',
        status: 'pending',
      });

      await designRequest.save();

      console.log(`✅ Design Request created:`, {
        id: designRequest._id,
        uploaderPhone: designRequest.uploaderPhone,
        uploaderName: designRequest.uploaderName,
        webLinksCount: parsedWebLinks.length,
        hasPhoneNumber,
        hasAdText,
        hasBusinessAddress,
        imagesCount: referenceImageIds.length,
        videosCount: referenceVideoIds.length,
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
          referenceImagesCount: referenceImageIds.length,
          referenceVideosCount: referenceVideoIds.length,
          status: designRequest.status,
          createdAt: designRequest.createdAt,
        },
      });
    } catch (error) {
      console.error('❌ Design request submission error:', error);

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
        referenceImagesCount: dr.referenceImagesGridFS?.length || 0,
        referenceVideosCount: dr.referenceVideosGridFS?.length || 0,
        status: dr.status,
        adminNotes: dr.adminNotes,
        createdAt: dr.createdAt,
        updatedAt: dr.updatedAt,
      })),
    });
  } catch (error) {
    console.error('❌ Error fetching design requests:', error);
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
          console.warn(`⚠️ Failed to delete image ${imageId}:`, error);
        }
      }
    }

    await ad.deleteOne();

    console.log(`✅ Deleted pending ad: ${id}`);

    res.json({ message: 'Ad deleted successfully' });
  } catch (error) {
    console.error('❌ Error deleting ad:', error);
    res.status(500).json({ message: 'Failed to delete ad' });
  }
});

export default router;
