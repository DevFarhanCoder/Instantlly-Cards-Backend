import { Router, Request, Response } from "express";
import Ad from "../models/Ad";
import { requireAuth, AuthReq } from "../middleware/auth";
import { requireAdminAuth, AdminAuthReq } from "../middleware/adminAuth";
import { gridfsService } from "../services/gridfsService";

const router = Router();

// GET /api/ads/active - Get all active ads (NO AUTH - for mobile app)
// ⚡ OPTIMIZED: Returns metadata only, images fetched separately via GridFS
router.get("/active", async (req: Request, res: Response) => {
  try {
    const now = new Date();

    // 🔍 LOG: Request received
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📱 [STEP 1] GET /api/ads/active - Request Received');
    console.log('🕐 Timestamp:', now.toISOString());
    console.log('🌐 User-Agent:', req.headers['user-agent']);
    console.log('🔗 Origin:', req.headers.origin || 'No origin');
    console.log('🔗 Referer:', req.headers.referer || 'No referer');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

    // Set cache headers for 5 minutes
    res.setHeader('Cache-Control', 'public, max-age=300');

    // 🔍 LOG: Database query start
    console.log('📊 [STEP 2] Querying Database for Active Ads');
    console.log('🔎 Query criteria:', {
      startDate: { $lte: now },
      endDate: { $gte: now }
    });

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

    // 🔍 LOG: Database query result
    console.log('✅ [STEP 3] Database Query Complete');
    console.log('📈 Total ads found:', ads.length);
    if (ads.length > 0) {
      console.log('📋 First ad sample:', {
        _id: ads[0]._id,
        title: ads[0].title,
        hasBottomImageGridFS: !!ads[0].bottomImageGridFS,
        hasFullscreenImageGridFS: !!ads[0].fullscreenImageGridFS,
        bottomImageGridFS: ads[0].bottomImageGridFS?.toString() || 'NULL',
        fullscreenImageGridFS: ads[0].fullscreenImageGridFS?.toString() || 'NULL'
      });
    }

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

    const imageBaseUrl = process.env.API_BASE_URL || "https://instantlly-cards-backend-6ki0.onrender.com";

    // 🔍 LOG: Response preparation
    console.log('🔧 [STEP 4] Preparing Response');
    console.log('🌐 Image Base URL:', imageBaseUrl);
    if (adsWithUrls.length > 0) {
      console.log('📸 First ad URLs:', {
        bottomImageUrl: adsWithUrls[0].bottomImageUrl,
        fullscreenImageUrl: adsWithUrls[0].fullscreenImageUrl,
        fullBottomUrl: adsWithUrls[0].bottomImageUrl 
          ? `${imageBaseUrl}${adsWithUrls[0].bottomImageUrl}` 
          : 'NULL',
        fullFullscreenUrl: adsWithUrls[0].fullscreenImageUrl 
          ? `${imageBaseUrl}${adsWithUrls[0].fullscreenImageUrl}` 
          : 'NULL'
      });
    }

    const responseData = {
      success: true,
      data: adsWithUrls,
      count: adsWithUrls.length,
      timestamp: now.toISOString(),
      imageBaseUrl: imageBaseUrl
    };

    // 🔍 LOG: Sending response
    console.log('📤 [STEP 5] Sending Response to Client');
    console.log('📊 Response summary:', {
      success: true,
      count: adsWithUrls.length,
      imageBaseUrl: imageBaseUrl,
      adsWithImages: adsWithUrls.filter(ad => ad.hasBottomImage).length,
      adsWithFullscreen: adsWithUrls.filter(ad => ad.hasFullscreenImage).length
    });
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    res.json(responseData);
  } catch (error) {
    console.error("❌ GET ACTIVE ADS ERROR:", error);
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

    // 🔍 LOG: Image request received
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('🖼️  [IMG STEP 1] GET /api/ads/image/:id/:type - Request Received');
    console.log('🆔 Ad ID:', id);
    console.log('📷 Image Type:', type);
    console.log('🕐 Timestamp:', new Date().toISOString());
    console.log('🌐 User-Agent:', req.headers['user-agent']);
    console.log('🔗 Referer:', req.headers.referer || 'No referer');

    if (type !== "bottom" && type !== "fullscreen") {
      console.error('❌ [IMG ERROR] Invalid image type:', type);
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      return res.status(400).json({
        success: false,
        message: "Invalid image type. Must be 'bottom' or 'fullscreen'"
      });
    }

    // 🔍 LOG: Fetching ad from database
    console.log('📊 [IMG STEP 2] Fetching Ad from Database');
    const ad = await Ad.findById(id)
      .select('bottomImageGridFS fullscreenImageGridFS bottomImage fullscreenImage')
      .lean();

    if (!ad) {
      console.error('❌ [IMG ERROR] Ad not found in database:', id);
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    console.log('✅ [IMG STEP 3] Ad Found in Database');
    console.log('📋 Ad data:', {
      _id: ad._id,
      bottomImageGridFS: ad.bottomImageGridFS?.toString() || 'NULL',
      fullscreenImageGridFS: ad.fullscreenImageGridFS?.toString() || 'NULL',
      hasLegacyBottomImage: !!(ad.bottomImage && ad.bottomImage.length > 0),
      hasLegacyFullscreenImage: !!(ad.fullscreenImage && ad.fullscreenImage.length > 0)
    });

    // Get GridFS file ID based on type
    const gridfsId = type === "bottom" ? ad.bottomImageGridFS : ad.fullscreenImageGridFS;

    if (!gridfsId) {
      console.warn('⚠️  [IMG STEP 4] No GridFS ID found - Checking for legacy base64');
      
      // Fallback to base64 if GridFS migration not complete
      const base64Data = type === "bottom" ? ad.bottomImage : ad.fullscreenImage;
      
      if (!base64Data || base64Data.length === 0) {
        console.error('❌ [IMG ERROR] No image found (neither GridFS nor base64)');
        console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
        return res.status(404).json({
          success: false,
          message: `${type} image not found`
        });
      }

      // Return base64 as fallback (legacy support during migration)
      console.warn(`⚠️  [IMG STEP 5] Serving base64 fallback for ad ${id} ${type} image`);
      console.log('📏 Base64 length:', base64Data.length, 'characters');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      return res.json({
        success: true,
        data: base64Data,
        source: "base64-legacy"
      });
    }

    // 🔍 LOG: Preparing to stream from GridFS
    console.log('✅ [IMG STEP 4] GridFS ID Found:', gridfsId.toString());
    console.log('🔄 [IMG STEP 5] Initiating GridFS Stream');

    // Set aggressive caching for images (24 hours)
    res.setHeader('Cache-Control', 'public, max-age=86400');
    res.setHeader('Content-Type', 'image/jpeg');

    // Stream image from GridFS (most efficient - no buffering needed)
    const downloadStream = gridfsService.getDownloadStream(gridfsId);
    
    let streamStarted = false;
    
    downloadStream.on('data', (chunk) => {
      if (!streamStarted) {
        console.log('📦 [IMG STEP 6] GridFS Stream Started - Sending data to client');
        console.log('📏 First chunk size:', chunk.length, 'bytes');
        streamStarted = true;
      }
    });

    downloadStream.on('end', () => {
      console.log('✅ [IMG STEP 7] GridFS Stream Complete - Image fully sent');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    });
    
    downloadStream.on('error', (error) => {
      console.error('❌ [IMG ERROR] GridFS download error:', error);
      console.error('🆔 Failed GridFS ID:', gridfsId.toString());
      console.error('📷 Failed Type:', type);
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      
      if (!res.headersSent) {
        res.status(500).json({
          success: false,
          message: "Failed to fetch image from storage"
        });
      }
    });

    // Pipe GridFS stream directly to response (efficient streaming)
    downloadStream.pipe(res);

  } catch (error) {
    console.error("❌ GET AD IMAGE ERROR:", error);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
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

// ========== ADMIN ROUTES (REQUIRE ADMIN AUTH) ==========

router.use(requireAdminAuth);

// GET /api/ads/analytics/summary - Get analytics summary (admin)
// MUST be before /:id route to avoid matching "analytics" as an id
router.get("/analytics/summary", async (req: AdminAuthReq, res: Response) => {
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
router.get("/", async (req: AdminAuthReq, res: Response) => {
  try {
    const ads = await Ad.find({})
      .sort({ createdAt: -1 })
      .limit(1000) // Limit to prevent memory issues
      .lean()
      .exec();

    // Transform ads to include proper image URLs for admin dashboard
    const imageBaseUrl = process.env.API_BASE_URL || "https://instantlly-cards-backend-6ki0.onrender.com";
    const adsWithImageUrls = ads.map(ad => {
      // If using GridFS (new ads), provide URL endpoints
      if (ad.bottomImageGridFS) {
        return {
          ...ad,
          bottomImage: `${imageBaseUrl}/api/ads/image/${ad._id}/bottom`,
          fullscreenImage: ad.fullscreenImageGridFS 
            ? `${imageBaseUrl}/api/ads/image/${ad._id}/fullscreen`
            : ""
        };
      }
      // Legacy ads with base64 - return as is
      return ad;
    });

    res.json({
      success: true,
      data: adsWithImageUrls
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
router.get("/:id", async (req: AdminAuthReq, res: Response) => {
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
router.post("/", async (req: AdminAuthReq, res: Response) => {
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
router.put("/:id", async (req: AdminAuthReq, res: Response) => {
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
router.delete("/:id", async (req: AdminAuthReq, res: Response) => {
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
