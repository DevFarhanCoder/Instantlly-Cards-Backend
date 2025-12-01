import { Router, Request, Response } from "express";
import Ad from "../models/Ad";
import { requireAuth, AuthReq } from "../middleware/auth";
import { requireAdminAuth, AdminAuthReq } from "../middleware/adminAuth";
import { gridfsService } from "../services/gridfsService";
import { imageCache } from "../services/imageCache";

const router = Router();

// Simple rate limiting to prevent accidental spam
const uploadAttempts = new Map<string, { count: number; resetTime: number }>();

// Cleanup old entries every 5 minutes
setInterval(() => {
  const now = Date.now();
  for (const [key, value] of uploadAttempts.entries()) {
    if (value.resetTime < now) {
      uploadAttempts.delete(key);
    }
  }
}, 5 * 60 * 1000);

function checkRateLimit(adminId: string, maxPerMinute: number = 60): boolean {
  const now = Date.now();
  const key = adminId;
  
  if (!uploadAttempts.has(key) || uploadAttempts.get(key)!.resetTime < now) {
    uploadAttempts.set(key, { count: 1, resetTime: now + 60000 });
    return true;
  }
  
  const attempt = uploadAttempts.get(key)!;
  if (attempt.count >= maxPerMinute) {
    return false; // Rate limit exceeded
  }
  
  attempt.count++;
  return true;
}

// GET /api/ads/my-ads - Get user's own ads by phone number (all statuses)
router.get("/my-ads", async (req: Request, res: Response) => {
  try {
    const { phone } = req.query;
    
    if (!phone) {
      return res.status(400).json({
        success: false,
        message: 'Phone number is required'
      });
    }

    console.log(`📱 Fetching ads for user: ${phone}`);

    // Find all ads uploaded by this phone number (all statuses: pending, approved, rejected)
    const ads = await Ad.find({ uploadedBy: phone })
      .select('-__v')
      .sort({ createdAt: -1 })
      .lean()
      .exec();

    console.log(`✅ Found ${ads.length} ads for user ${phone}`);

    // Transform ads to include image URLs (AWS Cloud primary)
    const imageBaseUrl = process.env.API_BASE_URL || "https://api.instantllycards.com";
    const adsWithUrls = ads.map((ad: any) => ({
      ...ad,
      _id: ad._id.toString(),
      bottomImage: ad.bottomImageGridFS 
        ? `${imageBaseUrl}/api/ads/image/${ad._id}/bottom`
        : "",
      fullscreenImage: ad.fullscreenImageGridFS 
        ? `${imageBaseUrl}/api/ads/image/${ad._id}/fullscreen`
        : "",
    }));

    res.json({
      success: true,
      data: adsWithUrls,
      count: adsWithUrls.length
    });
  } catch (error) {
    console.error('❌ Error fetching user ads:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to fetch ads'
    });
  }
});

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

    // Get ads that are currently active (within date range) AND approved
    // Only fetch metadata, NOT images (GridFS handles images separately)
    const ads = await Ad.find({
      startDate: { $lte: now },
      endDate: { $gte: now },
      status: 'approved' // ONLY show approved ads to mobile users
    })
      .select('title phoneNumber priority impressions clicks startDate endDate bottomImageGridFS fullscreenImageGridFS')
      .sort({ priority: -1, createdAt: -1 })
      .limit(50) // Can handle more ads now (no heavy base64 payload)
      .lean()
      .exec();

    // 🔍 LOG: Database query result
    console.log('✅ [STEP 3] Database Query Complete');
    console.log('📈 Total approved ads found:', ads.length);
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

    // AWS Cloud (Primary) - Render backup handled by client
    const imageBaseUrl = process.env.API_BASE_URL || "https://api.instantllycards.com";

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
// Streams image efficiently from GridFS storage with timeout protection
// :type can be 'bottom' or 'fullscreen'
router.get("/image/:id/:type", async (req: Request, res: Response) => {
  // Set response timeout to 40 seconds (less than socket timeout)
  req.setTimeout(40000);
  
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

    // Check cache first (avoids GridFS timeout)
    const cacheKey = `${id}-${type}`;
    const cachedImage = imageCache.get(cacheKey);
    
    if (cachedImage) {
      // Set caching headers
      res.setHeader('Cache-Control', 'public, max-age=86400');
      res.setHeader('Content-Type', 'image/jpeg');
      res.setHeader('X-Cache', 'HIT');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      return res.send(cachedImage);
    }

    console.log('💨 [CACHE MISS] Image not in cache, fetching from GridFS');

    // 🔍 LOG: Fetching ad from database
    console.log('📊 [IMG STEP 2] Fetching Ad from Database');
    
    // Retry logic for database query (up to 3 attempts)
    let ad = null;
    let retries = 0;
    const maxRetries = 3;
    
    while (retries < maxRetries && !ad) {
      try {
        ad = await Ad.findById(id)
          .select('bottomImageGridFS fullscreenImageGridFS bottomImage fullscreenImage')
          .maxTimeMS(10000) // 10 second timeout for this query
          .lean();
        
        if (!ad && retries < maxRetries - 1) {
          retries++;
          console.warn(`⚠️  [IMG RETRY] Database query failed, retry ${retries}/${maxRetries}`);
          await new Promise(resolve => setTimeout(resolve, 500 * retries)); // Exponential backoff
        }
      } catch (dbError: any) {
        retries++;
        console.error(`❌ [IMG DB ERROR] Attempt ${retries}/${maxRetries}:`, dbError.message);
        
        if (retries >= maxRetries) {
          throw dbError; // Throw after all retries exhausted
        }
        
        // Exponential backoff: 500ms, 1000ms, 1500ms
        await new Promise(resolve => setTimeout(resolve, 500 * retries));
      }
    }

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
    console.log('🔄 [IMG STEP 5] Buffering image from GridFS for caching');

    // Buffer the entire image first (enables caching and retry)
    const chunks: Buffer[] = [];
    const downloadStream = gridfsService.getDownloadStream(gridfsId);
    
    let streamTimeout: NodeJS.Timeout | null = null;
    let streamStarted = false;
    
    // Set 60 second timeout for GridFS retrieval (increased for buffering)
    streamTimeout = setTimeout(() => {
      console.error('❌ [IMG TIMEOUT] GridFS buffering timed out after 60s');
      downloadStream.destroy();
    }, 60000);
    
    downloadStream.on('data', (chunk: Buffer) => {
      if (!streamStarted) {
        console.log('📦 [IMG STEP 6] GridFS Stream Started - Buffering');
        streamStarted = true;
      }
      chunks.push(chunk);
    });

    downloadStream.on('end', () => {
      if (streamTimeout) clearTimeout(streamTimeout);
      
      // Combine all chunks into single buffer
      const imageBuffer = Buffer.concat(chunks);
      console.log('✅ [IMG STEP 7] Image buffered successfully');
      console.log(`📏 Total size: ${(imageBuffer.length / 1024).toFixed(2)} KB`);
      
      // Store in cache for future requests
      imageCache.set(cacheKey, imageBuffer);
      
      // Send to client
      res.setHeader('Cache-Control', 'public, max-age=86400');
      res.setHeader('Content-Type', 'image/jpeg');
      res.setHeader('X-Cache', 'MISS');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      res.send(imageBuffer);
    });
    
    downloadStream.on('error', (error) => {
      if (streamTimeout) clearTimeout(streamTimeout);
      
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

  } catch (error: any) {
    console.error("❌ GET AD IMAGE ERROR:", error);
    console.error("❌ Error type:", error.name);
    console.error("❌ Error message:", error.message);
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

// POST /api/ads - Create new ad (admin or mobile user with approval workflow)
// This route is BEFORE requireAdminAuth to allow mobile users to upload with pending status
router.post("/", async (req: Request, res: Response) => {
  try {
    // Check if request has admin authentication
    const token = req.headers.authorization?.replace('Bearer ', '');
    let isAdmin = false;
    let adminId = null;
    
    // Try to verify admin token
    if (token) {
      try {
        const jwt = require('jsonwebtoken');
        const decoded = jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key-change-in-production');
        if (decoded.role === 'admin' || decoded.role === 'super_admin') {
          isAdmin = true;
          adminId = decoded.id || decoded.username;
        }
      } catch (err) {
        // Not an admin token, treat as mobile user
        console.log('📱 Non-admin token detected - will create ad with pending status');
      }
    }
    
    // Rate limiting: Max 60 ads per minute for admins, 10 per minute for mobile users
    const rateLimitId = adminId || req.ip || 'anonymous';
    const maxRate = isAdmin ? 60 : 10;
    
    if (!checkRateLimit(rateLimitId, maxRate)) {
      console.warn(`⚠️  Rate limit exceeded for ${isAdmin ? 'admin' : 'mobile user'} ${rateLimitId}`);
      return res.status(429).json({
        success: false,
        message: `Too many uploads. Maximum ${maxRate} ads per minute. Please wait a moment.`
      });
    }
    
    console.log('📝 POST /api/ads - Creating new ad');
    console.log('👤 User type:', isAdmin ? 'Admin' : 'Mobile User');
    console.log('👤 User ID:', rateLimitId);
    console.log('📊 Request body keys:', Object.keys(req.body));
    
    const { title, bottomImage, fullscreenImage, phoneNumber, startDate, endDate, priority, uploaderName } = req.body;

    // Validation
    console.log('🔍 Validating fields:', {
      hasTitle: !!title,
      hasBottomImage: !!bottomImage,
      hasPhoneNumber: !!phoneNumber,
      hasStartDate: !!startDate,
      hasEndDate: !!endDate,
      bottomImageLength: bottomImage?.length,
      fullscreenImageLength: fullscreenImage?.length
    });
    
    if (!title || !bottomImage || !phoneNumber || !startDate || !endDate) {
      console.error('❌ Validation failed - missing required fields');
      return res.status(400).json({
        success: false,
        message: "Missing required fields (title, bottomImage, phoneNumber, startDate, endDate)"
      });
    }

    // Upload images to GridFS
    console.log(`📤 Uploading images to GridFS for new ad: ${title}`);
    console.log(`📏 Bottom image size: ${(bottomImage.length / 1024).toFixed(2)} KB`);
    if (fullscreenImage) {
      console.log(`📏 Fullscreen image size: ${(fullscreenImage.length / 1024).toFixed(2)} KB`);
    }
    
    let bottomImageId;
    let fullscreenImageId = null;
    
    try {
      bottomImageId = await gridfsService.uploadBase64(
        bottomImage,
        `${Date.now()}_bottom.jpg`,
        {
          title,
          type: "bottom"
        }
      );
      console.log(`✅ Bottom image uploaded to GridFS: ${bottomImageId}`);
    } catch (uploadError) {
      console.error(`❌ Failed to upload bottom image to GridFS:`, uploadError);
      throw new Error(`Image upload failed: ${uploadError instanceof Error ? uploadError.message : 'Unknown error'}`);
    }

    if (fullscreenImage && fullscreenImage.length > 0) {
      try {
        fullscreenImageId = await gridfsService.uploadBase64(
          fullscreenImage,
          `${Date.now()}_fullscreen.jpg`,
          {
            title,
            type: "fullscreen"
          }
        );
        console.log(`✅ Fullscreen image uploaded to GridFS: ${fullscreenImageId}`);
      } catch (uploadError) {
        console.error(`❌ Failed to upload fullscreen image to GridFS:`, uploadError);
        throw new Error(`Fullscreen image upload failed: ${uploadError instanceof Error ? uploadError.message : 'Unknown error'}`);
      }
    }

    // Create ad with GridFS references and approval workflow
    const adData: any = {
      title,
      bottomImage: "", // Empty - using GridFS
      bottomImageGridFS: bottomImageId,
      fullscreenImage: "", // Empty - using GridFS
      fullscreenImageGridFS: fullscreenImageId,
      phoneNumber,
      startDate: new Date(startDate),
      endDate: new Date(endDate),
      priority: priority || 5
    };
    
    // Set approval status based on user type
    if (isAdmin) {
      // Admin uploads are auto-approved
      adData.status = 'approved';
      adData.uploadedBy = 'admin';
      adData.uploaderName = 'Admin';
      adData.approvedBy = adminId;
      adData.approvalDate = new Date();
      console.log('✅ Admin upload - auto-approved');
    } else {
      // Mobile user uploads require approval
      adData.status = 'pending';
      adData.uploadedBy = phoneNumber; // Use phone as identifier
      adData.uploaderName = uploaderName || 'Mobile User';
      adData.priority = 1; // Lower priority for pending ads
      console.log('📱 Mobile upload - pending approval');
    }
    
    const ad = await Ad.create(adData);

    console.log(`✅ Ad created with GridFS images: ${ad._id}, status: ${ad.status}`);

    // Different response messages for admin vs mobile user
    const responseMessage = isAdmin 
      ? 'Advertisement created and published successfully'
      : 'Advertisement uploaded successfully. Awaiting admin approval.';

    res.status(201).json({
      success: true,
      message: responseMessage,
      data: ad,
      requiresApproval: !isAdmin
    });
  } catch (error) {
    console.error("❌ CREATE AD ERROR:", error);
    console.error("❌ Error stack:", error instanceof Error ? error.stack : 'No stack trace');
    console.error("❌ Error details:", {
      name: error instanceof Error ? error.name : 'Unknown',
      message: error instanceof Error ? error.message : String(error)
    });
    
    res.status(500).json({
      success: false,
      message: "Failed to create ad",
      error: error instanceof Error ? error.message : 'Unknown error',
      details: process.env.NODE_ENV === 'development' ? error : undefined
    });
  }
});

// GET /api/ads/my-ads - Get user's own ads by phone number (public, no auth required)
router.get("/my-ads", async (req: Request, res: Response) => {
  try {
    const phoneNumber = req.query.phoneNumber as string;
    
    if (!phoneNumber) {
      return res.status(400).json({
        success: false,
        message: "Phone number is required"
      });
    }

    console.log(`📱 Fetching ads for phone number: ${phoneNumber}`);

    // Find all ads for this user (including pending, approved, rejected)
    const ads = await Ad.find({ phoneNumber })
      .select('-bottomImage -fullscreenImage') // Exclude large base64 fields
      .sort({ createdAt: -1 })
      .lean()
      .exec();

    console.log(`✅ Found ${ads.length} ads for user ${phoneNumber}`);

    // Transform ads to include proper image URLs (AWS Cloud primary)
    const imageBaseUrl = process.env.API_BASE_URL || "https://api.instantllycards.com";
    const adsWithImageUrls = ads.map((ad: any) => {
      const adId = ad._id.toString();
      return {
        ...ad,
        _id: adId,
        bottomImage: ad.bottomImageGridFS 
          ? `${imageBaseUrl}/api/ads/image/${adId}/bottom`
          : "",
        fullscreenImage: ad.fullscreenImageGridFS 
          ? `${imageBaseUrl}/api/ads/image/${adId}/fullscreen`
          : "",
        bottomImageGridFS: ad.bottomImageGridFS?.toString(),
        fullscreenImageGridFS: ad.fullscreenImageGridFS?.toString()
      };
    });

    res.json({
      success: true,
      data: adsWithImageUrls,
      count: adsWithImageUrls.length
    });
  } catch (error) {
    console.error("❌ GET MY ADS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch your ads"
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

// GET /api/ads - Get all ads with pagination and filtering (NO AUTH REQUIRED for Channel Partner Admin)
router.get("/", async (req: Request, res: Response) => {
  try {
    console.log('📊 GET /api/ads - Request received (No auth required)');
    
    // SCALABILITY: Pagination parameters
    const page = parseInt(req.query.page as string) || 1;
    const limit = Math.min(parseInt(req.query.limit as string) || 50, 100); // Max 100 per page
    const skip = (page - 1) * limit;
    
    // SCALABILITY: Filtering options
    const filter: any = {};
    
    // IMPORTANT: Only show approved ads by default (for web dashboard)
    // Admin dashboard can override this by passing approvalStatus=all
    if (req.query.approvalStatus === 'all') {
      // Show all ads regardless of approval status (for admin review)
      // Don't add status filter
    } else if (req.query.approvalStatus === 'pending') {
      filter.status = 'pending';
    } else if (req.query.approvalStatus === 'rejected') {
      filter.status = 'rejected';
    } else {
      // Default: only show approved ads (for public web display)
      filter.status = 'approved';
    }
    
    // Filter by status (active/expired/all)
    if (req.query.status === 'active') {
      const now = new Date();
      filter.startDate = { $lte: now };
      filter.endDate = { $gte: now };
    } else if (req.query.status === 'expired') {
      filter.endDate = { $lt: new Date() };
    } else if (req.query.status === 'upcoming') {
      filter.startDate = { $gt: new Date() };
    }
    
    // Filter by search term (title or phone)
    if (req.query.search) {
      const searchTerm = req.query.search as string;
      filter.$or = [
        { title: { $regex: searchTerm, $options: 'i' } },
        { phoneNumber: { $regex: searchTerm, $options: 'i' } }
      ];
    }
    
    // PERFORMANCE: Get total count for pagination (with same filters)
    const totalAds = await Ad.countDocuments(filter);
    
    // CRITICAL: Exclude base64 image fields to prevent timeout on large datasets
    // Only fetch metadata - images are served via GridFS endpoints
    const ads = await Ad.find(filter)
      .select('-bottomImage -fullscreenImage') // Exclude large base64 fields
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limit)
      .lean()
      .exec();

    console.log(`✅ Found ${ads.length} ads (page ${page} of ${Math.ceil(totalAds / limit)})`);

    // Transform ads to include proper image URLs for admin dashboard (AWS Cloud primary)
    const imageBaseUrl = process.env.API_BASE_URL || "https://api.instantllycards.com";
    const adsWithImageUrls = ads.map((ad: any) => {
      try {
        const adId = ad._id.toString();
        
        // ALL ads (both legacy and new) now use GridFS image endpoints
        // This ensures consistent behavior and avoids sending large base64 in response
        return {
          ...ad,
          _id: adId,
          // Use GridFS endpoints for images (works for both old and new ads)
          bottomImage: ad.bottomImageGridFS 
            ? `${imageBaseUrl}/api/ads/image/${adId}/bottom`
            : `${imageBaseUrl}/api/ads/image/${adId}/bottom`, // Fallback to same endpoint
          fullscreenImage: ad.fullscreenImageGridFS 
            ? `${imageBaseUrl}/api/ads/image/${adId}/fullscreen`
            : "", // No fullscreen if not set
          bottomImageGridFS: ad.bottomImageGridFS?.toString(),
          fullscreenImageGridFS: ad.fullscreenImageGridFS?.toString()
        };
      } catch (mapError) {
        console.error('❌ Error transforming ad:', ad._id, mapError);
        // Return ad as-is if transformation fails
        return ad;
      }
    });

    console.log(`📤 Sending ${adsWithImageUrls.length} ads to admin dashboard`);
    res.json({
      success: true,
      data: adsWithImageUrls,
      pagination: {
        currentPage: page,
        totalPages: Math.ceil(totalAds / limit),
        totalAds: totalAds,
        adsPerPage: limit,
        hasNextPage: page < Math.ceil(totalAds / limit),
        hasPrevPage: page > 1
      }
    });
  } catch (error) {
    console.error("❌ GET ALL ADS ERROR:", error);
    res.status(500).json({
      success: false,
      message: "Failed to fetch ads",
      error: error instanceof Error ? error.message : 'Unknown error'
    });
  }
});

// GET /api/ads/:id - Get single ad (NO AUTH REQUIRED)
router.get("/:id", async (req: Request, res: Response) => {
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
