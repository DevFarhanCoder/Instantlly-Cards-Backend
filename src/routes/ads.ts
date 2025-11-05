import { Router, Request, Response } from "express";
import Ad from "../models/Ad";
import { requireAuth, AuthReq } from "../middleware/auth";
import { gridfsService } from "../services/gridfsService";

const router = Router();

// GET /api/ads/active - Get all active ads (NO AUTH - for mobile app)
// ⚡ OPTIMIZED: Returns metadata only, images fetched separately via GridFS
router.get("/active", async (req: Request, res: Response) => {
  try {
    const now = new Date();

    // Set cache headers for 5 minutes
    res.setHeader('Cache-Control', 'public, max-age=300');

    // Get ads that are currently active (within date range)
    // Only fetch metadata, NOT images (GridFS handles images separately)
    const ads = await Ad.find({
      startDate: { $lte: now },
      endDate: { $gte: now }
    })
      .select('title phoneNumber priority impressions clicks startDate endDate bottomImageGridFS fullscreenImageGridFS')
      .sort({ priority: -1, createdAt: -1 })
      .limit(50) // Can handle more ads now (no heavy base64 payload)
      .lean()
      .exec();

    // Transform ads to include image URLs instead of base64
    const adsWithUrls = ads.map(ad => ({
      _id: ad._id,
      title: ad.title,
      phoneNumber: ad.phoneNumber,
      priority: ad.priority,
      impressions: ad.impressions,
      clicks: ad.clicks,
      startDate: ad.startDate,
      endDate: ad.endDate,
      // Provide image URLs - client will fetch these separately
      bottomImageUrl: ad.bottomImageGridFS 
        ? `/api/ads/image/${ad._id}/bottom`
        : null,
      fullscreenImageUrl: ad.fullscreenImageGridFS 
        ? `/api/ads/image/${ad._id}/fullscreen`
        : null,
      hasBottomImage: !!ad.bottomImageGridFS,
      hasFullscreenImage: !!ad.fullscreenImageGridFS
    }));

    res.json({
      success: true,
      data: adsWithUrls,
      count: adsWithUrls.length,
      timestamp: now.toISOString(),
      imageBaseUrl: process.env.API_BASE_URL || "https://instantlly-cards-backend-6ki0.onrender.com"
    });
  } catch (error) {
    console.error("GET ACTIVE ADS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ads"
    });
  }
});

// GET /api/ads/image/:id/:type - Get single ad's image from GridFS (NO AUTH)
// Streams image efficiently from GridFS storage
// :type can be 'bottom' or 'fullscreen'
router.get("/image/:id/:type", async (req: Request, res: Response) => {
  try {
    const { id, type } = req.params;

    if (type !== "bottom" && type !== "fullscreen") {
      return res.status(400).json({
        success: false,
        message: "Invalid image type. Must be 'bottom' or 'fullscreen'"
      });
    }

    const ad = await Ad.findById(id)
      .select('bottomImageGridFS fullscreenImageGridFS bottomImage fullscreenImage')
      .lean();

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    // Get GridFS file ID based on type
    const gridfsId = type === "bottom" ? ad.bottomImageGridFS : ad.fullscreenImageGridFS;

    if (!gridfsId) {
      // Fallback to base64 if GridFS migration not complete
      const base64Data = type === "bottom" ? ad.bottomImage : ad.fullscreenImage;
      
      if (!base64Data || base64Data.length === 0) {
        return res.status(404).json({
          success: false,
          message: `${type} image not found`
        });
      }

      // Return base64 as fallback (legacy support during migration)
      console.warn(`⚠️ Serving base64 fallback for ad ${id} ${type} image`);
      return res.json({
        success: true,
        data: base64Data,
        source: "base64-legacy"
      });
    }

    // Set aggressive caching for images (24 hours)
    res.setHeader('Cache-Control', 'public, max-age=86400');
    res.setHeader('Content-Type', 'image/jpeg');

    // Stream image from GridFS (most efficient - no buffering needed)
    const downloadStream = gridfsService.getDownloadStream(gridfsId);
    
    downloadStream.on('error', (error) => {
      console.error(`GridFS download error for ${id} ${type}:`, error);
      res.status(500).json({
        success: false,
        message: "Failed to fetch image from storage"
      });
    });

    // Pipe GridFS stream directly to response (efficient streaming)
    downloadStream.pipe(res);

  } catch (error) {
    console.error("GET AD IMAGE ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ad image"
    });
  }
});

// POST /api/ads/track-impression - Track ad impression (NO AUTH)
router.post("/track-impression/:id", async (req: Request, res: Response) => {
  try {
    const { id } = req.params;

    await Ad.findByIdAndUpdate(
      id,
      { $inc: { impressions: 1 } },
      { new: true }
    );

    res.json({ success: true });
  } catch (error) {
    console.error("TRACK IMPRESSION ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to track impression"
    });
  }
});

// POST /api/ads/track-click - Track ad click (NO AUTH)
router.post("/track-click/:id", async (req: Request, res: Response) => {
  try {
    const { id } = req.params;

    await Ad.findByIdAndUpdate(
      id,
      { $inc: { clicks: 1 } },
      { new: true }
    );

    res.json({ success: true });
  } catch (error) {
    console.error("TRACK CLICK ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to track click"
    });
  }
});

// ========== ADMIN ROUTES (REQUIRE AUTH) ==========

router.use(requireAuth);

// GET /api/ads/analytics/summary - Get analytics summary (admin)
// MUST be before /:id route to avoid matching "analytics" as an id
router.get("/analytics/summary", async (req: AuthReq, res: Response) => {
  try {
    const totalAds = await Ad.countDocuments();
    const now = new Date();
    const activeAds = await Ad.countDocuments({
      startDate: { $lte: now },
      endDate: { $gte: now }
    });
    
    const analytics = await Ad.aggregate([
      {
        $group: {
          _id: null,
          totalImpressions: { $sum: "$impressions" },
          totalClicks: { $sum: "$clicks" }
        }
      }
    ]);

    const summary = {
      totalAds,
      activeAds,
      expiredAds: totalAds - activeAds,
      totalImpressions: analytics[0]?.totalImpressions || 0,
      totalClicks: analytics[0]?.totalClicks || 0,
      clickThroughRate: analytics[0]?.totalImpressions > 0
        ? ((analytics[0]?.totalClicks / analytics[0]?.totalImpressions) * 100).toFixed(2)
        : 0
    };

    res.json({
      success: true,
      data: summary
    });
  } catch (error) {
    console.error("GET ANALYTICS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch analytics"
    });
  }
});

// GET /api/ads - Get all ads (admin)
router.get("/", async (req: AuthReq, res: Response) => {
  try {
    const ads = await Ad.find({})
      .sort({ createdAt: -1 })
      .limit(1000) // Limit to prevent memory issues
      .lean()
      .exec();

    res.json({
      success: true,
      data: ads
    });
  } catch (error) {
    console.error("GET ALL ADS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ads"
    });
  }
});

// GET /api/ads/:id - Get single ad (admin)
router.get("/:id", async (req: AuthReq, res: Response) => {
  try {
    const ad = await Ad.findById(req.params.id).lean();

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    res.json({
      success: true,
      data: ad
    });
  } catch (error) {
    console.error("GET AD ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ad"
    });
  }
});

// POST /api/ads - Create new ad (admin)
router.post("/", async (req: AuthReq, res: Response) => {
  try {
    const { title, bottomImage, fullscreenImage, phoneNumber, startDate, endDate, priority } = req.body;

    // Validation
    if (!title || !bottomImage || !phoneNumber || !startDate || !endDate) {
      return res.status(400).json({
        success: false,
        message: "Missing required fields (title, bottomImage, phoneNumber, startDate, endDate)"
      });
    }

    // Upload images to GridFS
    console.log(`📤 Uploading images to GridFS for new ad: ${title}`);
    
    const bottomImageId = await gridfsService.uploadBase64(
      bottomImage,
      `${Date.now()}_bottom.jpg`,
      {
        title,
        type: "bottom"
      }
    );

    let fullscreenImageId = null;
    if (fullscreenImage && fullscreenImage.length > 0) {
      fullscreenImageId = await gridfsService.uploadBase64(
        fullscreenImage,
        `${Date.now()}_fullscreen.jpg`,
        {
          title,
          type: "fullscreen"
        }
      );
    }

    // Create ad with GridFS references
    const ad = await Ad.create({
      title,
      bottomImage: "", // Empty - using GridFS
      bottomImageGridFS: bottomImageId,
      fullscreenImage: "", // Empty - using GridFS
      fullscreenImageGridFS: fullscreenImageId,
      phoneNumber,
      startDate: new Date(startDate),
      endDate: new Date(endDate),
      priority: priority || 5
    });

    console.log(`✅ Ad created with GridFS images: ${ad._id}`);

    res.status(201).json({
      success: true,
      data: ad
    });
  } catch (error) {
    console.error("CREATE AD ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to create ad"
    });
  }
});

// PUT /api/ads/:id - Update ad (admin)
router.put("/:id", async (req: AuthReq, res: Response) => {
  try {
    const { title, bottomImage, fullscreenImage, phoneNumber, startDate, endDate, priority } = req.body;

    const updateData: any = {};

    if (title) updateData.title = title;
    if (bottomImage) updateData.bottomImage = bottomImage;
    if (fullscreenImage !== undefined) updateData.fullscreenImage = fullscreenImage;
    if (phoneNumber) updateData.phoneNumber = phoneNumber;
    if (startDate) updateData.startDate = new Date(startDate);
    if (endDate) updateData.endDate = new Date(endDate);
    if (priority !== undefined) updateData.priority = priority;

    const ad = await Ad.findByIdAndUpdate(
      req.params.id,
      updateData,
      { new: true, runValidators: true }
    );

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    res.json({
      success: true,
      data: ad
    });
  } catch (error) {
    console.error("UPDATE AD ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to update ad"
    });
  }
});

// DELETE /api/ads/:id - Delete ad (admin)
router.delete("/:id", async (req: AuthReq, res: Response) => {
  try {
    const ad = await Ad.findById(req.params.id);

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    // Delete GridFS images if they exist
    if (ad.bottomImageGridFS) {
      try {
        await gridfsService.deleteFile(ad.bottomImageGridFS);
        console.log(`🗑️ Deleted bottom image from GridFS: ${ad.bottomImageGridFS}`);
      } catch (error) {
        console.error("Failed to delete bottom image from GridFS:", error);
      }
    }

    if (ad.fullscreenImageGridFS) {
      try {
        await gridfsService.deleteFile(ad.fullscreenImageGridFS);
        console.log(`🗑️ Deleted fullscreen image from GridFS: ${ad.fullscreenImageGridFS}`);
      } catch (error) {
        console.error("Failed to delete fullscreen image from GridFS:", error);
      }
    }

    // Delete ad document
    await Ad.findByIdAndDelete(req.params.id);

    res.json({
      success: true,
      message: "Ad and associated images deleted successfully"
    });
  } catch (error) {
    console.error("DELETE AD ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to delete ad"
    });
  }
});

export default router;
