import { Router, Request, Response } from "express";
import Ad from "../models/Ad";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

// GET /api/ads/active - Get all active ads (NO AUTH - for mobile app)
router.get("/active", async (req: Request, res: Response) => {
  try {
    const now = new Date();

    // Get ads that are currently active (within date range)
    // Limit results and use indexes to avoid memory issues
    const ads = await Ad.find({
      startDate: { $lte: now },
      endDate: { $gte: now }
    })
      .sort({ priority: -1, createdAt: -1 })
      .limit(100) // Limit to prevent memory issues
      .lean()
      .exec();

    res.json({
      success: true,
      data: ads
    });
  } catch (error) {
    console.error("GET ACTIVE ADS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ads"
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
