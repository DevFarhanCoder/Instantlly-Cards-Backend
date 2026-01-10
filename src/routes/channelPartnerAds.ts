import express, { Request, Response } from 'express';
import multer from 'multer';
import { GridFSBucket, ObjectId } from 'mongodb';
import mongoose from 'mongoose';
import Ad from '../models/Ad';
import User from '../models/User';
import Transaction from '../models/Transaction';
import { Readable } from 'stream';

const router = express.Router();

// Configure multer for memory storage - images only
const upload = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 10 * 1024 * 1024, // 10MB limit
  },
  fileFilter: (req, file, cb) => {
    // Accept images only
    if (!file.mimetype.startsWith('image/')) {
      cb(new Error('Only image files are allowed'));
      return;
    }
    cb(null, true);
  },
});

// Configure multer for video uploads
const uploadVideos = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 50 * 1024 * 1024, // 50MB limit for videos
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

// Configure multer for mixed uploads (images + videos)
const uploadMixed = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 50 * 1024 * 1024, // 50MB limit
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
  upload.array('images', 5), // Max 5 images
  async (req: Request, res: Response) => {
    try {
      console.log('📤 Ad upload request (no auth):', {
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
        return res.status(400).json({ message: 'At least one image is required (bottom image)' });
      }

      if (files.length > 2) {
        return res.status(400).json({ message: 'Maximum 2 images allowed (bottom image and optional fullscreen)' });
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
      // Use main database User model for credits
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
        
        // If not found, try without country code prefix (+91)
        if (!user && uploaderPhone.startsWith('+91')) {
          const phoneWithoutPrefix = uploaderPhone.substring(3);
          console.log('🔄 Trying without +91 prefix:', phoneWithoutPrefix);
          user = await User.findOne({ phone: phoneWithoutPrefix });
        }
        
        // Try with +91 prefix
        if (!user && !uploaderPhone.startsWith('+')) {
          const phoneWithPrefix = '+91' + uploaderPhone;
          console.log('🔄 Trying with +91 prefix:', phoneWithPrefix);
          user = await User.findOne({ phone: phoneWithPrefix });
        }

        // Try with +880 prefix (Bangladesh)
        if (!user && !uploaderPhone.startsWith('+')) {
          const phoneWithPrefix = '+880' + uploaderPhone;
          console.log('🔄 Trying with +880 prefix:', phoneWithPrefix);
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
        // Log some sample users for debugging
        const sampleUsers = await User.find({}).select('phone name').limit(5).lean();
        console.log('📋 Sample users in DB:', sampleUsers.map(u => ({ name: u.name, phone: u.phone })));
        
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
        amount: -1020,
        description: `Ad creation: ${title}`,
        balanceBefore: currentCredits,
        balanceAfter: currentCredits - 1020,
        status: 'completed'
      });

      console.log(`✅ Deducted 1020 credits from ${user.name} (${uploaderPhone}). Remaining: ${(user as any).credits}`);

      // Upload images to GridFS
      const db = mongoose.connection.db;
      if (!db) {
        throw new Error('Database connection not established');
      }

      const bucket = new GridFSBucket(db, { bucketName: 'adImages' });
      
      // Upload bottom image (required)
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

      console.log(`✅ Uploaded bottom image to GridFS: ${bottomImageId}`);

      // Upload fullscreen image (optional)
      let fullscreenImageId: ObjectId | undefined;
      if (files.length > 1) {
        const fullscreenFile = files[1];
        const fullscreenStream = bucket.openUploadStream(fullscreenFile.originalname, {
          contentType: fullscreenFile.mimetype,
        });

        const fullscreenReadable = Readable.from(fullscreenFile.buffer);
        fullscreenImageId = await new Promise<ObjectId>((resolve, reject) => {
          fullscreenReadable
            .pipe(fullscreenStream)
            .on('finish', () => resolve(fullscreenStream.id as ObjectId))
            .on('error', reject);
        });

        console.log(`✅ Uploaded fullscreen image to GridFS: ${fullscreenImageId}`);
      }

      // Create ad with pending status
      const ad = new Ad({
        title,
        bottomImage: '', // Empty when using GridFS
        bottomImageGridFS: bottomImageId,
        fullscreenImage: '',
        fullscreenImageGridFS: fullscreenImageId,
        phoneNumber,
        startDate: start,
        endDate: end,
        status: 'pending', // Requires admin approval
        uploadedBy: uploaderPhone, // Use actual uploader phone for filtering
        uploaderName: uploaderName || 'Channel Partner',
        priority: priority ? parseInt(priority) : 1, // Lower priority for channel partner ads
        clicks: 0,
        impressions: 0,
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
        remainingCredits: user.credits,
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
        amount: -1020,
        description: `Video Ad creation: ${title}`,
        balanceBefore: currentCredits,
        balanceAfter: currentCredits - 1020,
        status: 'completed'
      });

      console.log(`✅ Deducted 1020 credits from ${user.name} (${uploaderPhone}). Remaining: ${(user as any).credits}`);

      // Upload videos to GridFS
      const db = mongoose.connection.db;
      if (!db) {
        throw new Error('Database connection not established');
      }

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
        message: 'Video ad submitted successfully! 1020 credits deducted. Admin will review your ad.',
        creditsDeducted: 1020,
        remainingCredits: user.credits,
        cashPaymentRequired: 180,
        totalCost: '1020 credits + ₹180 cash',
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
