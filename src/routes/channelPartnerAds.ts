import express, { Request, Response } from 'express';
import multer from 'multer';
import { GridFSBucket, ObjectId } from 'mongodb';
import mongoose from 'mongoose';
import Ad from '../models/Ad';
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

      // NO CREDIT CHECK - Free ad creation for now
      // Admin will approve/reject

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

      console.log(`✅ Ad created with pending status (no credit charge):`, {
        id: ad._id,
        title: ad.title,
        uploadedBy: ad.uploadedBy,
        status: ad.status
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

        const bucket = new GridFSBucket(db, { bucketName: 'uploads' });

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
