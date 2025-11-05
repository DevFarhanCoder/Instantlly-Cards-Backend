import { Router, Request, Response } from "express";
import Ad from "../models/Ad";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

// GET /api/ads/active - Get all active ads (NO AUTH - for mobile app)
// Optimized for speed with caching and reduced payload
router.get("/active", async (req: Request, res: Response) => {
  try {
    const now = new Date();

    // Set cache headers for 30 minutes (extended to reduce load)
    res.setHeader('Cache-Control', 'public, max-age=1800'); // 30 minutes
    res.setHeader('ETag', `ads-${now.getTime()}`);

    // Get ads that are currently active (within date range)
    // Limit results and use lean() for better performance
    const ads = await Ad.find({
      startDate: { $lte: now },
      endDate: { $gte: now }
    })
      .select('title bottomImage fullscreenImage phoneNumber priority impressions clicks startDate endDate') // Only select needed fields
      .sort({ priority: -1, createdAt: -1 })
      .limit(20) // CRITICAL: Reduced from 50 to 20 to prevent 502 errors
      .lean() // Convert to plain objects for faster serialization
      .exec();

    // CRITICAL FIX: Truncate base64 images if response is too large
    // This prevents 502 gateway timeouts on Render
    const maxResponseSize = 10 * 1024 * 1024; // 10MB limit
    let estimatedSize = JSON.stringify(ads).length;

    if (estimatedSize > maxResponseSize) {
      console.warn(`⚠️ LARGE PAYLOAD WARNING: ${(estimatedSize / 1024 / 1024).toFixed(2)}MB - Applying image truncation`);
      
      // Return metadata only, images should be fetched separately
      const lightweightAds = ads.map(ad => ({
        _id: ad._id,
        title: ad.title,
        phoneNumber: ad.phoneNumber,
        priority: ad.priority,
        impressions: ad.impressions,
        clicks: ad.clicks,
        startDate: ad.startDate,
        endDate: ad.endDate,
        hasBottomImage: !!ad.bottomImage,
        hasFullscreenImage: !!ad.fullscreenImage,
        // Only include small preview or URL reference
        bottomImageSize: ad.bottomImage?.length || 0,
        fullscreenImageSize: ad.fullscreenImage?.length || 0
      }));

      return res.json({
        success: true,
        data: lightweightAds,
        count: lightweightAds.length,
        timestamp: now.toISOString(),
        warning: "Images too large - use /api/ads/image/:id endpoint to fetch images separately"
      });
    }

    res.json({
      success: true,
      data: ads,
      count: ads.length,
      timestamp: now.toISOString()
    });
  } catch (error) {
    console.error("GET ACTIVE ADS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ads"
    });
  }
});

// GET /api/ads/image/:id - Get single ad's images (NO AUTH)
// Separate endpoint to fetch images on-demand
router.get("/image/:id", async (req: Request, res: Response) => {
  try {
    const ad = await Ad.findById(req.params.id)
      .select('bottomImage fullscreenImage')
      .lean();

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    // Set aggressive caching for images (24 hours)
    res.setHeader('Cache-Control', 'public, max-age=86400');
    
    res.json({
      success: true,
      data: {
        bottomImage: ad.bottomImage,
        fullscreenImage: ad.fullscreenImage
      }
    });
  } catch (error) {
    console.error("GET AD IMAGE ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ad images"
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

    // Create ad
    const ad = await Ad.create({
      title,
      bottomImage,
      fullscreenImage: fullscreenImage || "",
      phoneNumber,
      startDate: new Date(startDate),
      endDate: new Date(endDate),
      priority: priority || 5
    });

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
    const ad = await Ad.findByIdAndDelete(req.params.id);

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    res.json({
      success: true,
      message: "Ad deleted successfully"
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
