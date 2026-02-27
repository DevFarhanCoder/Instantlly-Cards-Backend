import { Router, Request, Response } from "express";
import mongoose from "mongoose";
import multer from "multer";
import AWS from "aws-sdk";
import { requireAuth, AuthReq } from "../middleware/auth";
import { requireAdminAuth } from "../middleware/adminAuth";
import Review from "../models/Review";
import Enquiry from "../models/Enquiry";
import BusinessPromotion from "../models/BusinessPromotion";
import User from "../models/User";
import { s3, upload } from "./channelPartnerAds";

const router = Router();

// Configure AWS S3 for review images
const s3Instance = new AWS.S3({
  region: process.env.AWS_REGION,
  credentials: {
    accessKeyId: process.env.AWS_ACCESS_KEY_ID!,
    secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY!,
  },
});

// Multer for review image uploads
const reviewImageUpload = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 5 * 1024 * 1024, // 5MB per image
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

// ============================================================================
// REVIEW ENDPOINTS
// ============================================================================

/**
 * POST /api/reviews/business/:businessId/reviews
 * Create a new review for a business with image uploads to S3
 */
router.post("/:businessId/reviews", requireAuth, reviewImageUpload.array("photos", 5), async (req: AuthReq, res: Response) => {
  try {
    console.log("Received review creation request with body:", req.body);
    const { businessId } = req.params;
    const { rating, title, message, selectedSuggestions, experience } = req.body;
    const userId = req.userId;
    const files = req.files as Express.Multer.File[];

    // Validations
    if (!rating || rating < 1 || rating > 5) {
      return res.status(400).json({
        success: false,
        error: "INVALID_RATING",
        message: "Rating must be between 1 and 5"
      });
    }

    if (title && title.length > 50) {
      return res.status(400).json({
        success: false,
        error: "TITLE_TOO_LONG",
        message: "Title must be at most 50 characters"
      });
    }

    if (message && message.length > 500) {
      return res.status(400).json({
        success: false,
        error: "MESSAGE_TOO_LONG",
        message: "Message must be at most 500 characters"
      });
    }

    if (experience && experience.length > 500) {
      return res.status(400).json({
        success: false,
        error: "EXPERIENCE_TOO_LONG",
        message: "Experience must be at most 500 characters"
      });
    }

    // Check if business exists
    const business = await BusinessPromotion.findById(businessId);
    if (!business) {
      return res.status(404).json({
        success: false,
        error: "BUSINESS_NOT_FOUND",
        message: "Business listing not found"
      });
    }

    // Check for duplicate reviews from same user
    const existingReview = await Review.findOne({
      userId,
      businessId
    });

    if (existingReview) {
      return res.status(409).json({
        success: false,
        error: "DUPLICATE_REVIEW",
        message: "You have already reviewed this business. You can edit your existing review."
      });
    }

    // Get user details
    const user = await User.findById(userId);

    // Upload photos to S3
    const photos: { url: string; cloudinary_id: string; uploadedAt: Date }[] = [];

    if (files && files.length > 0) {
      for (const file of files) {
        const key = `reviews/${businessId}/${Date.now()}-${Math.random().toString(36).substring(7)}-${file.originalname}`;

        try {
          await s3Instance.putObject({
            Bucket: process.env.S3_BUCKET!,
            Key: key,
            Body: file.buffer,
            ContentType: file.mimetype,
          }).promise();

          const photoUrl = `${process.env.CLOUDFRONT_HOST}/${key}`;
          photos.push({
            url: photoUrl,
            cloudinary_id: key, // Store S3 key for reference
            uploadedAt: new Date()
          });

          console.log(`✅ Review photo uploaded to S3: ${photoUrl}`);
        } catch (s3Error) {
          console.error(`❌ S3 upload failed for ${file.originalname}:`, s3Error);
          return res.status(500).json({
            success: false,
            error: "S3_UPLOAD_FAILED",
            message: "Failed to upload review photo to S3"
          });
        }
      }
    }
    const parsedSuggestions = typeof selectedSuggestions === "string" ? JSON.parse(selectedSuggestions) : selectedSuggestions || [];
    // Create review
    const review = new Review({
      businessId,
      userId,
      userName: user?.name || "Anonymous",
      userPhone: user?.phone,
      rating,
      title,
      message,
      selectedSuggestions: parsedSuggestions,
      experience,
      photos,
      isApproved: true,
      isSpam: false
    });

    await review.save();

    // Update business review count
    await BusinessPromotion.findByIdAndUpdate(
      businessId,
      {
        $inc: {
          "visibility.reviews": 1
        }
      },
      { new: true }
    );

    res.status(201).json({
      success: true,
      review: {
        _id: review._id,
        businessId: review.businessId,
        rating: review.rating,
        title: review.title,
        message: review.message,
        userName: review.userName,
        photosCount: photos.length,
        photos: photos,
        createdAt: review.createdAt
      }
    });
  } catch (error) {
    console.error("Error creating review:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to create review"
    });
  }
});

/**
 * POST /api/reviews/:businessId/upload-photos
 * Upload review photos to S3 and return URLs (separate endpoint for flexibility)
 */
router.post("/:businessId/upload-photos", requireAuth, reviewImageUpload.array("photos", 5), async (req: AuthReq, res: Response) => {
  try {
    const { businessId } = req.params;
    const userId = req.userId;
    const files = req.files as Express.Multer.File[];

    // Check if business exists
    const business = await BusinessPromotion.findById(businessId);
    if (!business) {
      return res.status(404).json({
        success: false,
        error: "BUSINESS_NOT_FOUND",
        message: "Business listing not found"
      });
    }

    if (!files || files.length === 0) {
      return res.status(400).json({
        success: false,
        error: "NO_FILES",
        message: "No photos provided"
      });
    }

    if (files.length > 5) {
      return res.status(400).json({
        success: false,
        error: "TOO_MANY_FILES",
        message: "Maximum 5 photos allowed"
      });
    }

    // Upload photos to S3
    const photos: { url: string; cloudinary_id: string; uploadedAt: Date }[] = [];

    for (const file of files) {
      const key = `reviews/${businessId}/${Date.now()}-${Math.random().toString(36).substring(7)}-${file.originalname}`;

      try {
        await s3Instance.putObject({
          Bucket: process.env.S3_BUCKET!,
          Key: key,
          Body: file.buffer,
          ContentType: file.mimetype,
        }).promise();

        const photoUrl = `${process.env.CLOUDFRONT_HOST}/${key}`;
        photos.push({
          url: photoUrl,
          cloudinary_id: key,
          uploadedAt: new Date()
        });

        console.log(`✅ Review photo uploaded to S3: ${photoUrl}`);
      } catch (s3Error) {
        console.error(`❌ S3 upload failed for ${file.originalname}:`, s3Error);
        return res.status(500).json({
          success: false,
          error: "S3_UPLOAD_FAILED",
          message: "Failed to upload photos to S3"
        });
      }
    }

    res.status(200).json({
      success: true,
      photos,
      message: `${photos.length} photo(s) uploaded successfully`
    });
  } catch (error) {
    console.error("Error uploading photos:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to upload photos"
    });
  }
});

/**
 * GET /api/reviews/:businessId/reviews
 * Fetch all reviews for a business with filtering and sorting
 */
router.get("/:businessId/reviews", async (req: Request, res: Response) => {
  try {
    const { businessId } = req.params;
    const { rating, sort = 'latest', page = 1, limit = 10, hasPhotos, hasOwnerReply } = req.query;

    // Build filter
    const filter: any = {
      businessId: businessId,
      isApproved: true,
      isSpam: false
    };

    if (rating) {
      filter.rating = parseInt(rating as string);
    }

    if (hasPhotos === 'true') {
      filter.photos = { $exists: true, $ne: [] };
    }

    if (hasOwnerReply === 'true') {
      filter.ownerReply = { $exists: true, $ne: null };
    }

    // Build sort
    let sortObj: any = {};
    switch (sort) {
      case 'oldest':
        sortObj = { createdAt: 1 };
        break;
      case 'rating_high':
        sortObj = { rating: -1 };
        break;
      case 'rating_low':
        sortObj = { rating: 1 };
        break;
      case 'helpful':
        sortObj = { helpful: -1, createdAt: -1 };
        break;
      case 'latest':
      default:
        sortObj = { createdAt: -1 };
    }

    const pageNum = Math.max(1, parseInt(page as string) || 1);
    const limitNum = Math.max(1, Math.min(100, parseInt(limit as string) || 10));
    const skip = (pageNum - 1) * limitNum;

    // Fetch reviews
    const reviews = await Review.find(filter)
      .sort(sortObj)
      .skip(skip)
      .limit(limitNum)
      .populate('userId', '_id name profile picture');

    const totalReviews = await Review.countDocuments(filter);

    // Calculate stats
    const allReviews = await Review.find({
      businessId: businessId,
      isApproved: true,
      isSpam: false
    });

    const stats = {
      totalReviews: allReviews.length,
      averageRating: allReviews.length > 0
        ? Number((allReviews.reduce((sum, r) => sum + r.rating, 0) / allReviews.length).toFixed(1))
        : 0,
      ratingBreakdown: {
        5: allReviews.filter(r => r.rating === 5).length,
        4: allReviews.filter(r => r.rating === 4).length,
        3: allReviews.filter(r => r.rating === 3).length,
        2: allReviews.filter(r => r.rating === 2).length,
        1: allReviews.filter(r => r.rating === 1).length
      },
      reviewsWithPhotos: allReviews.filter(r => r.photos && r.photos.length > 0).length,
      reviewsWithOwnerReply: allReviews.filter(r => r.ownerReply && r.ownerReply.message).length,
      recentTrend: (() => {
        const thirtyDaysAgo = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000);
        const recentReviews = allReviews.filter(r => new Date(r.createdAt) > thirtyDaysAgo);
        return recentReviews.length > 0
          ? (recentReviews.reduce((sum, r) => sum + r.rating, 0) / recentReviews.length).toFixed(1)
          : 0;
      })()
    };

    res.status(200).json({
      success: true,
      reviews,
      stats,
      pagination: {
        page: pageNum,
        limit: limitNum,
        total: totalReviews,
        pages: Math.ceil(totalReviews / limitNum)
      }
    });
  } catch (error) {
    console.error("Error fetching reviews:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to fetch reviews"
    });
  }
});

/**
 * PUT /api/reviews/:reviewId
 * Edit review (only by review author)
 */
router.put("/:reviewId", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { reviewId } = req.params;
    const { rating, title, message, selectedSuggestions, experience } = req.body;
    const userId = req.userId;

    if (!mongoose.Types.ObjectId.isValid(reviewId)) {
      return res.status(400).json({
        success: false,
        message: "Invalid review ID"
      });
    }

    const review = await Review.findById(reviewId);
    if (!review) {
      return res.status(404).json({
        success: false,
        message: "Review not found"
      });
    }

    if (review.userId.toString() !== userId) {
      return res.status(403).json({
        success: false,
        message: "You can only edit your own review"
      });
    }

    if (rating !== undefined) {
      const parsedRating = Number(rating);
      if (!Number.isFinite(parsedRating) || parsedRating < 1 || parsedRating > 5) {
        return res.status(400).json({
          success: false,
          message: "Rating must be between 1 and 5"
        });
      }
      review.rating = parsedRating;
    }

    if (title !== undefined && String(title).length > 50) {
      return res.status(400).json({
        success: false,
        message: "Title must be at most 50 characters"
      });
    }

    if (message !== undefined && String(message).length > 500) {
      return res.status(400).json({
        success: false,
        message: "Message must be at most 500 characters"
      });
    }

    if (experience !== undefined && String(experience).length > 500) {
      return res.status(400).json({
        success: false,
        message: "Experience must be at most 500 characters"
      });
    }

    if (title !== undefined) review.title = title;
    if (message !== undefined) review.message = message;
    if (experience !== undefined) review.experience = experience;
    if (selectedSuggestions !== undefined) {
      if (typeof selectedSuggestions === "string") {
        try {
          review.selectedSuggestions = JSON.parse(selectedSuggestions);
        } catch {
          return res.status(400).json({
            success: false,
            message: "selectedSuggestions must be a valid JSON array"
          });
        }
      } else {
        review.selectedSuggestions = selectedSuggestions;
      }
    }
    review.updatedAt = new Date();

    await review.save();

    const updatedReview = await Review.findById(reviewId).populate('userId', '_id name profile picture');

    return res.status(200).json({
      success: true,
      review: updatedReview,
      message: "Review updated successfully"
    });
  } catch (error) {
    console.error("Error updating review:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to update review"
    });
  }
});

/**
 * DELETE /api/reviews/:reviewId
 * Delete review and associated S3 photos (only by review author)
 */
router.delete("/:reviewId", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { reviewId } = req.params;
    const userId = req.userId;

    if (!mongoose.Types.ObjectId.isValid(reviewId)) {
      return res.status(400).json({
        success: false,
        message: "Invalid review ID"
      });
    }

    const review = await Review.findById(reviewId);
    if (!review) {
      return res.status(404).json({
        success: false,
        message: "Review not found"
      });
    }

    if (review.userId.toString() !== userId) {
      return res.status(403).json({
        success: false,
        message: "You can only delete your own review"
      });
    }

    if (review.photos?.length) {
      const deleteOps = review.photos
        .map((photo: any) => photo?.cloudinary_id)
        .filter((key: string) => typeof key === "string" && key.length > 0)
        .map((Key: string) => s3Instance.deleteObject({
          Bucket: process.env.S3_BUCKET!,
          Key
        }).promise());

      await Promise.all(deleteOps);
    }

    await Review.findByIdAndDelete(reviewId);

    return res.status(200).json({
      success: true,
      message: "Review deleted successfully"
    });
  } catch (error) {
    console.error("Error deleting review:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to delete review"
    });
  }
});

/**
 * POST /api/reviews/:reviewId/reply
 * Business owner replies to a review
 */
router.post("/:reviewId/reply", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { reviewId } = req.params;
    const { message } = req.body;
    const userId = req.userId;

    if (!message || message.trim().length === 0) {
      return res.status(400).json({
        success: false,
        error: "MESSAGE_REQUIRED",
        message: "Reply message is required"
      });
    }

    const review = await Review.findById(reviewId).populate('businessId');
    if (!review) {
      return res.status(404).json({
        success: false,
        error: "REVIEW_NOT_FOUND",
        message: "Review not found"
      });
    }

    // Check if user is the business owner
    const business = await BusinessPromotion.findOne({
      _id: review.businessId,
      userId
    });

    if (!business) {
      return res.status(403).json({
        success: false,
        error: "UNAUTHORIZED",
        message: "Only the business owner can reply to reviews"
      });
    }

    // Add owner reply
    review.ownerReply = {
      message,
      repliedAt: new Date(),
      repliedBy: userId
    };

    await review.save();

    res.status(200).json({
      success: true,
      ownerReply: review.ownerReply
    });
  } catch (error) {
    console.error("Error replying to review:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to reply to review"
    });
  }
});

/**
 * POST /api/reviews/:reviewId/helpful
 * Mark review as helpful or unhelpful
 */
router.post("/:reviewId/helpful", async (req: Request, res: Response) => {
  try {
    const { reviewId } = req.params;
    const { helpful } = req.body;

    const review = await Review.findById(reviewId);
    if (!review) {
      return res.status(404).json({
        success: false,
        error: "REVIEW_NOT_FOUND",
        message: "Review not found"
      });
    }

    if (helpful === true) {
      review.helpful += 1;
    } else if (helpful === false) {
      review.unhelpful += 1;
    }

    await review.save();

    res.status(200).json({
      success: true,
      helpful: review.helpful,
      unhelpful: review.unhelpful
    });
  } catch (error) {
    console.error("Error updating review helpfulness:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to update review"
    });
  }
});

/**
 * POST /api/reviews/:reviewId/report
 * Report a review as spam or inappropriate
 */
router.post("/:reviewId/report", async (req: Request, res: Response) => {
  try {
    const { reviewId } = req.params;
    const { reason, reportedBy } = req.body;

    const review = await Review.findById(reviewId);
    if (!review) {
      return res.status(404).json({
        success: false,
        error: "REVIEW_NOT_FOUND",
        message: "Review not found"
      });
    }

    review.spamReports.push({
      reportedBy: reportedBy || null,
      reason: reason || "general",
      reportedAt: new Date()
    });

    await review.save();

    res.status(200).json({
      success: true,
      message: "Review reported successfully. Our team will review it shortly."
    });
  } catch (error) {
    console.error("Error reporting review:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to report review"
    });
  }
});

/**
 * GET /api/reviews/:businessId/review-stats
 * Get review statistics for business owner dashboard
 */
router.get("/:businessId/review-stats", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { businessId } = req.params;
    const userId = req.userId;

    // Verify ownership
    const business = await BusinessPromotion.findOne({
      _id: businessId,
      userId
    });

    if (!business) {
      return res.status(403).json({
        success: false,
        error: "UNAUTHORIZED",
        message: "You don't have access to this business"
      });
    }

    const allReviews = await Review.find({
      businessId,
      isApproved: true,
      isSpam: false
    });

    const thisMonth = allReviews.filter(r =>
      new Date(r.createdAt) > new Date(Date.now() - 30 * 24 * 60 * 60 * 1000)
    );

    const stats = {
      totalReviews: allReviews.length,
      averageRating: allReviews.length > 0
        ? (allReviews.reduce((sum, r) => sum + r.rating, 0) / allReviews.length).toFixed(1)
        : 0,
      thisMonth: thisMonth.length,
      thisMonthRating: thisMonth.length > 0
        ? (thisMonth.reduce((sum, r) => sum + r.rating, 0) / thisMonth.length).toFixed(1)
        : 0,
      ratingTrend: ((thisMonth.length > 0
        ? (thisMonth.reduce((sum, r) => sum + r.rating, 0) / thisMonth.length)
        : 0) - (allReviews.length > 0
          ? (allReviews.reduce((sum, r) => sum + r.rating, 0) / allReviews.length)
          : 0)).toFixed(1),
      unrepliedReviews: allReviews.filter(r => !r.ownerReply).length,
      replyRate: allReviews.length > 0
        ? ((allReviews.filter(r => r.ownerReply).length / allReviews.length) * 100).toFixed(1)
        : 0,
      positiveReviewPercent: allReviews.length > 0
        ? ((allReviews.filter(r => r.rating >= 4).length / allReviews.length) * 100).toFixed(1)
        : 0,
      reviewsWithPhotos: allReviews.filter(r => r.photos && r.photos.length > 0).length,
      topSuggestions: (() => {
        const suggestions: { [key: string]: number } = {};
        allReviews.forEach(r => {
          r.selectedSuggestions?.forEach((s: string) => {
            suggestions[s] = (suggestions[s] || 0) + 1;
          });
        });
        return Object.entries(suggestions)
          .sort((a, b) => b[1] - a[1])
          .slice(0, 5)
          .map(([key]) => key);
      })()
    };

    res.status(200).json({
      success: true,
      stats
    });
  } catch (error) {
    console.error("Error fetching review stats:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to fetch review statistics"
    });
  }
});

// ============================================================================
// ADMIN ENDPOINTS
// ============================================================================

/**
 * GET /api/reviews?limit=50
 * Admin: Moderate reviews
 */
router.get("/admin/moderate", requireAdminAuth, async (req: Request, res: Response) => {
  try {
    const { isApproved, isSpam, spamCount = 0, page = 1, limit = 50 } = req.query;

    const filter: any = {};

    if (isApproved === 'false') {
      filter.isApproved = false;
    }

    if (isSpam === 'true') {
      filter.spamReports = { $size: parseInt(spamCount as string) || 1 };
    }

    const pageNum = Math.max(1, parseInt(page as string) || 1);
    const limitNum = Math.max(1, Math.min(100, parseInt(limit as string) || 50));
    const skip = (pageNum - 1) * limitNum;

    const reviews = await Review.find(filter)
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limitNum)
      .populate('userId', 'name phone')
      .populate('businessId', 'businessName');

    const total = await Review.countDocuments(filter);

    res.status(200).json({
      success: true,
      reviews,
      pagination: {
        total,
        page: pageNum,
        limit: limitNum,
        pages: Math.ceil(total / limitNum)
      }
    });
  } catch (error) {
    console.error("Error fetching reviews for moderation:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to fetch reviews"
    });
  }
});

/**
 * PATCH /api/reviews/:reviewId/moderate
 * Admin: Approve, reject, or mark as spam
 */
router.patch("/:reviewId/moderate", requireAdminAuth, async (req: Request, res: Response) => {
  try {
    const { reviewId } = req.params;
    const { action, reason } = req.body;

    const review = await Review.findById(reviewId);
    if (!review) {
      return res.status(404).json({
        success: false,
        error: "REVIEW_NOT_FOUND",
        message: "Review not found"
      });
    }

    if (action === 'approve') {
      review.isApproved = true;
      review.isSpam = false;
      review.spamReports = [];
    } else if (action === 'spam') {
      review.isSpam = true;
      review.isApproved = false;
    } else if (action === 'delete') {
      await Review.findByIdAndDelete(reviewId);
      return res.status(200).json({
        success: true,
        message: "Review deleted"
      });
    }

    await review.save();

    res.status(200).json({
      success: true,
      message: "Review moderated",
      status: action
    });
  } catch (error) {
    console.error("Error moderating review:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to moderate review"
    });
  }
});

/**
 * GET /api/reviews/admin/analytics
 * Admin: platform-wide review analytics
 */
router.get("/admin/analytics", requireAdminAuth, async (req: Request, res: Response) => {
  try {
    const allReviews = await Review.find({ isApproved: true, isSpam: false });
    const spamReviews = await Review.find({ isSpam: true });

    const thisMonth = allReviews.filter(r =>
      new Date(r.createdAt) > new Date(Date.now() - 30 * 24 * 60 * 60 * 1000)
    );

    // Get top businesses
    const topBusinesses = await Review.aggregate([
      { $match: { isApproved: true, isSpam: false } },
      {
        $group: {
          _id: "$businessId",
          rating: { $avg: "$rating" },
          reviews: { $sum: 1 }
        }
      },
      { $sort: { rating: -1 } },
      { $limit: 10 },
      {
        $lookup: {
          from: "businesspromotions",
          localField: "_id",
          foreignField: "_id",
          as: "business"
        }
      }
    ]);

    const analytics = {
      totalReviews: allReviews.length,
      totalBusinesses: await BusinessPromotion.countDocuments(),
      averageRating: allReviews.length > 0
        ? (allReviews.reduce((sum, r) => sum + r.rating, 0) / allReviews.length).toFixed(1)
        : 0,
      reviewsThisMonth: thisMonth.length,
      spamRate: allReviews.length > 0
        ? ((spamReviews.length / (allReviews.length + spamReviews.length)) * 100).toFixed(1)
        : 0,
      approvalRate: (allReviews.length / (allReviews.length + spamReviews.length) * 100).toFixed(1),
      topRatedBusinesses: topBusinesses.map(b => ({
        businessId: b._id,
        businessName: b.business?.[0]?.businessName || "Unknown",
        rating: b.rating.toFixed(1),
        reviews: b.reviews
      }))
    };

    res.status(200).json({
      success: true,
      analytics
    });
  } catch (error) {
    console.error("Error fetching analytics:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to fetch analytics"
    });
  }
});

export default router;
