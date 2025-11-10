import { Router, Request, Response } from "express";
import mongoose from "mongoose";
import Ad from "../models/Ad";
import { requireAuth, AuthReq } from "../middleware/auth";
import { gridfsService } from "../services/gridfsService";
import { optimizedImageService } from "../services/optimizedImageService";
import { imageCache } from "../utils/imageCache";
import { isDBConnected } from "../db";

const router = Router();

// GET /api/ads/health - Health check endpoint
router.get("/health", async (req: Request, res: Response) => {
  const health = {
    status: "ok",
    timestamp: new Date().toISOString(),
    database: {
      connected: isDBConnected(),
      state: ["disconnected", "connected", "connecting", "disconnecting"][
        mongoose.connection.readyState
      ] || "unknown"
    },
    cache: imageCache.getStats(),
    uptime: process.uptime()
  };

  const statusCode = health.database.connected ? 200 : 503;
  res.status(statusCode).json(health);
});

// GET /api/ads/active - Get all active ads (NO AUTH - for mobile app)
// ⚡ OPTIMIZED: Returns metadata only, images fetched separately via GridFS
router.get("/active", async (req: Request, res: Response) => {
  try {
    const now = new Date();

    // � Check database connection first
    if (!isDBConnected()) {
      console.error('❌ Database not connected - cannot fetch active ads');
      return res.status(503).json({
        success: false,
        message: "Database temporarily unavailable",
        data: [],
        count: 0
      });
    }

    // Set cache headers for 5 minutes
    res.setHeader('Cache-Control', 'public, max-age=300');

    // Get ads that are currently active (within date range)
    const ads = await Ad.find({
      startDate: { $lte: now },
      endDate: { $gte: now }
    })
      .select('title phoneNumber priority impressions clicks startDate endDate bottomImageGridFS fullscreenImageGridFS')
      .sort({ priority: -1, createdAt: -1 })
      .limit(50)
      .lean()
      .maxTimeMS(10000) // 10 second timeout
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

    const imageBaseUrl = process.env.API_BASE_URL || "https://instantlly-cards-backend-6ki0.onrender.com";

    const responseData = {
      success: true,
      data: adsWithUrls,
      count: adsWithUrls.length,
      timestamp: now.toISOString(),
      imageBaseUrl: imageBaseUrl
    };

    res.json(responseData);
  } catch (error: any) {
    // Check for authentication errors
    if (error.message?.includes('Authentication failed') || 
        error.message?.includes('auth failed') ||
        error.message?.includes('bad auth')) {
      console.error('🔐 MongoDB authentication error in active ads fetch');
      return res.status(503).json({
        success: false,
        message: "Database authentication error. Please contact administrator.",
        error: "DB_AUTH_FAILED"
      });
    }

    // Check for timeout errors
    if (error.message?.includes('timeout') || error.name === 'MongoNetworkTimeoutError') {
      console.error('⏱️ Database timeout in active ads fetch');
      return res.status(504).json({
        success: false,
        message: "Database query timeout. Please try again.",
        error: "DB_TIMEOUT"
      });
    }

    console.error("❌ GET ACTIVE ADS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ads",
      error: "INTERNAL_ERROR"
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

    // 🔒 CRITICAL: Check database connection first
    if (!isDBConnected()) {
      console.error('❌ Database not connected - cannot fetch ad image');
      return res.status(503).json({
        success: false,
        message: "Database temporarily unavailable. Please check connection settings.",
        error: "DB_NOT_CONNECTED"
      });
    }

    // Check cache first to avoid database query
    const cached = imageCache.get(id, type as 'bottom' | 'fullscreen');
    if (cached) {
      if (cached.gridfsId) {
        res.setHeader('Cache-Control', 'public, max-age=86400');
        res.setHeader('Content-Type', 'image/jpeg');
        
        // Use optimized chunked streaming
        try {
          const downloadStream = optimizedImageService.getChunkedDownloadStream(cached.gridfsId);
          
          downloadStream.on('error', (error) => {
            console.error('❌ Chunked stream error:', error.message);
            if (!res.headersSent) {
              res.status(500).json({ 
                success: false, 
                message: "Image stream error",
                error: "STREAM_ERROR"
              });
            }
          });
          
          return downloadStream.pipe(res);
        } catch (error: any) {
          console.error('❌ Failed to create stream:', error.message);
          // Fall through to database query
        }
      } else if (cached.base64Data) {
        return res.json({
          success: true,
          data: cached.base64Data,
          source: "base64-cached"
        });
      }
    }

    // Cache miss - fetch from database with proper error handling
    let ad;
    try {
      ad = await Ad.findById(id)
        .select('bottomImageGridFS fullscreenImageGridFS bottomImage fullscreenImage')
        .lean()
        .maxTimeMS(10000) // Increased to 10s
        .exec();
    } catch (dbError: any) {
      console.error('❌ Database query failed:', dbError.message);
      
      // Check for authentication errors
      if (dbError.message.includes('Authentication failed') ||
          dbError.message.includes('auth failed') ||
          dbError.message.includes('bad auth')) {
        console.error('🔐 MongoDB authentication error - wrong password!');
        return res.status(503).json({
          success: false,
          message: "Database authentication error. Please contact administrator.",
          error: "DB_AUTH_FAILED"
        });
      }
      
      // Timeout or network error
      return res.status(504).json({
        success: false,
        message: "Database query timeout",
        error: "DB_TIMEOUT"
      });
    }

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    // Get GridFS file ID based on type
    const gridfsId = type === "bottom" ? ad.bottomImageGridFS : ad.fullscreenImageGridFS;
    const base64Data = type === "bottom" ? ad.bottomImage : ad.fullscreenImage;

    // Cache the result for future requests
    imageCache.set(
      id, 
      type as 'bottom' | 'fullscreen', 
      gridfsId?.toString() || null, 
      base64Data || null
    );

    if (!gridfsId) {
      if (!base64Data || base64Data.length === 0) {
        return res.status(404).json({
          success: false,
          message: `${type} image not found`
        });
      }

      return res.json({
        success: true,
        data: base64Data,
        source: "base64-legacy"
      });
    }

    // Set aggressive caching for images (24 hours)
    res.setHeader('Cache-Control', 'public, max-age=86400');
    res.setHeader('Content-Type', 'image/jpeg');

    // Use optimized chunked streaming with automatic timeout protection
    try {
      const downloadStream = optimizedImageService.getChunkedDownloadStream(gridfsId);
      
      // Add response timeout handler
      const streamTimeout = setTimeout(() => {
        console.error('⏱️ Response timeout for ad:', id);
        downloadStream.destroy();
        if (!res.headersSent) {
          res.status(504).json({
            success: false,
            message: "Image download timeout - please try again",
            error: "RESPONSE_TIMEOUT"
          });
        }
      }, 20000); // 20 second max for full response
      
      downloadStream.on('error', (error) => {
        clearTimeout(streamTimeout);
        console.error('❌ GridFS error for ad:', id, error.message);
        if (!res.headersSent) {
          res.status(500).json({
            success: false,
            message: "Failed to fetch image from storage",
            error: "GRIDFS_ERROR"
          });
        }
      });
      
      downloadStream.on('end', () => {
        clearTimeout(streamTimeout);
      });

      downloadStream.pipe(res);
    } catch (error: any) {
      console.error('❌ Failed to stream image:', error.message);
      if (!res.headersSent) {
        res.status(500).json({
          success: false,
          message: "Failed to initialize image stream",
          error: "STREAM_INIT_ERROR"
        });
      }
    }

  } catch (error: any) {
    console.error("❌ GET AD IMAGE ERROR:", error.message || error);
    
    if (!res.headersSent) {
      res.status(500).json({
        success: false,
        message: "Internal server error"
      });
    }
  }
});

// GET /api/ads/image/:id/:type/metadata - Get image metadata without downloading
// Useful for clients to check file size before downloading
router.get("/image/:id/:type/metadata", async (req: Request, res: Response) => {
  try {
    const { id, type } = req.params;

    if (type !== "bottom" && type !== "fullscreen") {
      return res.status(400).json({
        success: false,
        message: "Invalid image type"
      });
    }

    // Check database connection
    if (!isDBConnected()) {
      return res.status(503).json({
        success: false,
        message: "Database temporarily unavailable",
        error: "DB_NOT_CONNECTED"
      });
    }

    // Get ad with timeout protection
    const ad = await Ad.findById(id)
      .select('bottomImageGridFS fullscreenImageGridFS title')
      .lean()
      .maxTimeMS(5000)
      .exec();

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: "Ad not found"
      });
    }

    const gridfsId = type === "bottom" ? ad.bottomImageGridFS : ad.fullscreenImageGridFS;

    if (!gridfsId) {
      return res.status(404).json({
        success: false,
        message: `${type} image not found`
      });
    }

    // Get file metadata
    const metadata = await optimizedImageService.getFileMetadata(gridfsId);

    if (!metadata) {
      return res.status(404).json({
        success: false,
        message: "Image file not found in storage"
      });
    }

    res.json({
      success: true,
      data: {
        adId: id,
        type: type,
        filename: metadata.filename,
        sizeKB: metadata.sizeKB,
        sizeBytes: metadata.length,
        chunks: metadata.chunks,
        chunkSize: metadata.chunkSize,
        uploadDate: metadata.uploadDate,
        // Suggest batched loading for large files
        suggestBatchedLoad: metadata.length > 1024 * 1024, // > 1MB
        estimatedLoadTime: `${Math.ceil(metadata.length / (256 * 1024))} seconds`
      }
    });

  } catch (error: any) {
    console.error("❌ GET IMAGE METADATA ERROR:", error.message);
    res.status(500).json({
      success: false,
      message: "Failed to get image metadata"
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
  const requestStartTime = Date.now();
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('📋 [GET /api/ads] Request received from:', req.ip);
  console.log('🕐 Request time:', new Date().toISOString());
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  try {
    console.log('🔍 Step 1: Checking MongoDB connection status...');
    console.log('   Mongoose state:', mongoose.connection.readyState);
    console.log('   States: 0=disconnected, 1=connected, 2=connecting, 3=disconnecting');
    
    if (mongoose.connection.readyState !== 1) {
      console.error('❌ MongoDB not connected! Current state:', mongoose.connection.readyState);
      return res.status(503).json({
        success: false,
        message: "Database not connected",
        debug: {
          mongooseState: mongoose.connection.readyState,
          timestamp: new Date().toISOString()
        }
      });
    }
    
    console.log('✅ Step 1 complete: MongoDB is connected');
    console.log('');
    console.log('🔍 Step 2: Starting database query...');
    console.log('   Query: Ad.find({}).sort({ createdAt: -1 }).limit(1000)');
    
    const queryStartTime = Date.now();
    
    const ads = await Ad.find({})
      .sort({ createdAt: -1 })
      .limit(1000)
      .lean()
      .exec();

    const queryTime = Date.now() - queryStartTime;
    console.log(`✅ Step 2 complete: Query finished in ${queryTime}ms`);
    console.log(`   Found ${ads.length} ads`);
    console.log('');
    console.log('🔍 Step 3: Preparing response...');
    
    const totalTime = Date.now() - requestStartTime;
    const response = {
      success: true,
      data: ads,
      debug: {
        totalAds: ads.length,
        queryTimeMs: queryTime,
        totalTimeMs: totalTime,
        timestamp: new Date().toISOString()
      }
    };
    
    console.log(`✅ Step 3 complete: Sending response (${JSON.stringify(response).length} bytes)`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log(`✅ [GET /api/ads] SUCCESS - Total time: ${totalTime}ms`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

    res.json(response);
  } catch (error: any) {
    const totalTime = Date.now() - requestStartTime;
    console.log('');
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.error('❌ [GET /api/ads] ERROR after', totalTime, 'ms');
    console.error('Error name:', error.name);
    console.error('Error message:', error.message);
    console.error('Error stack:', error.stack);
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    res.status(500).json({
      success: false,
      message: "Failed to fetch ads",
      error: error.message,
      debug: {
        errorName: error.name,
        errorMessage: error.message,
        totalTimeMs: totalTime,
        timestamp: new Date().toISOString()
      }
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

    // Clear cache for this ad
    imageCache.clear(req.params.id);

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
