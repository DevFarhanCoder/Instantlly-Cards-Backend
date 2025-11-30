import express, { Request, Response } from 'express';
import multer from 'multer';
import { GridFSBucket, ObjectId } from 'mongodb';
import mongoose from 'mongoose';
import Ad from '../models/Ad';
import { authenticateChannelPartner } from '../middleware/channelPartnerAuth';
import { Readable } from 'stream';

const router = express.Router();

// Configure multer for memory storage
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

/**
 * POST /api/channel-partner/ads
 * Upload a new ad (status = 'pending')
 * Requires: Bearer token (channel partner JWT)
 */
router.post(
  '/',
  authenticateChannelPartner,
  upload.array('images', 5), // Max 5 images
  async (req: Request, res: Response) => {
    try {
      console.log('📤 Channel partner ad upload request:', {
        channelPartnerPhone: req.channelPartnerPhone,
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

      // Check user credits and deduct 1020 credits
      const User = mongoose.model('User');
      const user = await User.findOne({ phone: req.channelPartnerPhone });
      
      if (!user) {
        return res.status(404).json({ message: 'User not found' });
      }

      const userCredits = (user as any).credits || 0;
      const AD_COST = 1020;

      if (userCredits < AD_COST) {
        return res.status(400).json({ 
          message: `Insufficient credits. You need ${AD_COST} credits to create an ad. Current balance: ${userCredits}` 
        });
      }

      // Upload images to GridFS
      const db = mongoose.connection.db;
      if (!db) {
        throw new Error('Database connection not established');
      }

      const bucket = new GridFSBucket(db, { bucketName: 'uploads' });
      
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
        uploadedBy: req.channelPartnerPhone || 'unknown',
        uploaderName: uploaderName || 'Channel Partner',
        priority: priority ? parseInt(priority) : 1, // Lower priority for channel partner ads
        clicks: 0,
        impressions: 0,
      });

      await ad.save();

      // Deduct credits from user
      (user as any).credits = userCredits - AD_COST;
      await user.save();

      // Create transaction record
      const Transaction = mongoose.model('Transaction');
      await Transaction.create({
        fromUser: user._id,
        toUser: null,
        amount: AD_COST,
        type: 'ad_purchase',
        description: `Advertisement: ${title}`,
        status: 'completed'
      });

      console.log(`✅ Ad created with pending status:`, {
        id: ad._id,
        title: ad.title,
        uploadedBy: ad.uploadedBy,
        status: ad.status,
        creditsDeducted: AD_COST,
        remainingCredits: userCredits - AD_COST
      });

      res.status(201).json({
        message: 'Ad uploaded successfully. Awaiting admin approval.',
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
 * GET /api/channel-partner/ads
 * Get all ads uploaded by the authenticated channel partner
 */
router.get('/', authenticateChannelPartner, async (req: Request, res: Response) => {
  try {
    console.log('📋 Fetching ads for channel partner:', req.channelPartnerPhone);

    const ads = await Ad.find({
      uploadedBy: req.channelPartnerPhone,
    })
      .sort({ createdAt: -1 }) // Most recent first
      .select('-__v');

    console.log(`✅ Found ${ads.length} ads for channel partner ${req.channelPartnerPhone}`);

    // Transform ads for response
    const adsWithDetails = ads.map((ad) => ({
      id: ad._id,
      title: ad.title,
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
 * DELETE /api/channel-partner/ads/:id
 * Delete own ad (only if status is 'pending')
 */
router.delete('/:id', authenticateChannelPartner, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;

    if (!mongoose.Types.ObjectId.isValid(id)) {
      return res.status(400).json({ message: 'Invalid ad ID' });
    }

    const ad = await Ad.findById(id);

    if (!ad) {
      return res.status(404).json({ message: 'Ad not found' });
    }

    // Verify ownership
    if (ad.uploadedBy !== req.channelPartnerPhone) {
      return res.status(403).json({ message: 'You can only delete your own ads' });
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
      const bucket = new GridFSBucket(db, { bucketName: 'uploads' });
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

    console.log(`✅ Deleted pending ad: ${id} by ${req.channelPartnerPhone}`);

    res.json({ message: 'Ad deleted successfully' });
  } catch (error) {
    console.error('❌ Error deleting ad:', error);
    res.status(500).json({ message: 'Failed to delete ad' });
  }
});

export default router;
