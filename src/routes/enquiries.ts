import { Router, Request, Response } from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import { requireAdminAuth } from "../middleware/adminAuth";
import Enquiry from "../models/Enquiry";
import BusinessPromotion from "../models/BusinessPromotion";
import User from "../models/User";

const router = Router();

// ============================================================================
// ENQUIRY ENDPOINTS
// ============================================================================

/**
 * POST /api/enquiries/:businessId/enquiry
 * Send an enquiry to a business
 */
router.post("/:businessId/enquiry", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { businessId } = req.params;
    const { subject, message, phone, email } = req.body;
    const userId = req.userId;

    // Validations
    if (!subject || subject.trim().length === 0) {
      return res.status(400).json({
        success: false,
        error: "SUBJECT_REQUIRED",
        message: "Subject is required"
      });
    }

    if (subject.length > 100) {
      return res.status(400).json({
        success: false,
        error: "SUBJECT_TOO_LONG",
        message: "Subject must be at most 100 characters"
      });
    }

    if (!message || message.trim().length === 0) {
      return res.status(400).json({
        success: false,
        error: "MESSAGE_REQUIRED",
        message: "Message is required"
      });
    }

    if (message.length > 1000) {
      return res.status(400).json({
        success: false,
        error: "MESSAGE_TOO_LONG",
        message: "Message must be at most 1000 characters"
      });
    }

    if (!phone || !/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
      return res.status(400).json({
        success: false,
        error: "INVALID_PHONE",
        message: "Invalid phone number format"
      });
    }

    if (email && !/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) {
      return res.status(400).json({
        success: false,
        error: "INVALID_EMAIL",
        message: "Invalid email format"
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

    // Rate limiting: 10 enquiries per user per day
    const oneDayAgo = new Date(Date.now() - 24 * 60 * 60 * 1000);
    const userEnquiresWithinDay = await Enquiry.countDocuments({
      userId,
      createdAt: { $gte: oneDayAgo }
    });

    if (userEnquiresWithinDay >= 10) {
      return res.status(429).json({
        success: false,
        error: "RATE_LIMIT",
        message: "You have sent too many enquiries today. Please try again tomorrow."
      });
    }

    // Get user details
    const user = await User.findById(userId);

    // Create enquiry
    const enquiry = new Enquiry({
      businessId,
      userId,
      userName: user?.name || "User",
      userPhone: phone,
      userEmail: email || user?.email || null,
      subject,
      message,
      status: 'new',
      priority: 'low',
      responses: [],
      notificationSent: false
    });

    await enquiry.save();

    // Update business enquiry count
    await BusinessPromotion.findByIdAndUpdate(
      businessId,
      {
        $inc: {
          "visibility.leads": 1
        }
      },
      { new: true }
    );

    res.status(201).json({
      success: true,
      enquiry: {
        _id: enquiry._id,
        businessId: enquiry.businessId,
        status: enquiry.status,
        subject: enquiry.subject,
        createdAt: enquiry.createdAt,
        message: "Enquiry sent successfully to business"
      }
    });
  } catch (error) {
    console.error("Error creating enquiry:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to send enquiry"
    });
  }
});

/**
 * GET /api/enquiries/:businessId/enquiries
 * Business owner views enquiries for their business
 */
router.get("/:businessId/enquiries", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { businessId } = req.params;
    const { status, sort = 'latest', page = 1, priority } = req.query;

    // Verify ownership
    const business = await BusinessPromotion.findOne({
      _id: businessId,
      userId: req.userId
    });

    if (!business) {
      return res.status(403).json({
        success: false,
        error: "UNAUTHORIZED",
        message: "You don't have access to these enquiries"
      });
    }

    // Build filter
    const filter: any = { businessId };

    if (status) {
      filter.status = status;
    }

    if (priority) {
      filter.priority = priority;
    }

    // Build sort
    const sortObj: { [key: string]: 1 | -1 } = sort === 'oldest' ? { createdAt: 1 } : { createdAt: -1 };

    const pageNum = Math.max(1, parseInt(page as string) || 1);
    const limitNum = 20;
    const skip = (pageNum - 1) * limitNum;

    const enquiries = await Enquiry.find(filter)
      .sort(sortObj)
      .skip(skip)
      .limit(limitNum)
      .populate('userId', 'name phone')
      .populate('responses.respondedBy', 'name');

    const total = await Enquiry.countDocuments(filter);

    // Calculate stats
    const allEnquiries = await Enquiry.find({ businessId });

    const stats = {
      new: allEnquiries.filter(e => e.status === 'new').length,
      responded: allEnquiries.filter(e => e.status === 'responded').length,
      closed: allEnquiries.filter(e => e.status === 'closed').length,
      total: allEnquiries.length,
      avgResponseTime: (() => {
        const responded = allEnquiries.filter(e => e.lastResponseAt);
        if (responded.length === 0) return "N/A";
        const totalTime = responded.reduce((sum, e) => {
          return sum + (new Date(e.lastResponseAt!).getTime() - new Date(e.createdAt).getTime());
        }, 0);
        const avgMs = totalTime / responded.length;
        const hours = (avgMs / (1000 * 60 * 60)).toFixed(1);
        return `${hours} hours`;
      })()
    };

    res.status(200).json({
      success: true,
      enquiries,
      stats,
      pagination: {
        page: pageNum,
        limit: limitNum,
        total,
        pages: Math.ceil(total / limitNum)
      }
    });
  } catch (error) {
    console.error("Error fetching enquiries:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to fetch enquiries"
    });
  }
});

/**
 * POST /api/enquiries/:enquiryId/respond
 * Business owner responds to an enquiry
 */
router.post("/:enquiryId/respond", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { enquiryId } = req.params;
    const { message } = req.body;

    if (!message || message.trim().length === 0) {
      return res.status(400).json({
        success: false,
        error: "MESSAGE_REQUIRED",
        message: "Response message is required"
      });
    }

    const enquiry = await Enquiry.findById(enquiryId);
    if (!enquiry) {
      return res.status(404).json({
        success: false,
        error: "ENQUIRY_NOT_FOUND",
        message: "Enquiry not found"
      });
    }

    // Verify ownership
    const business = await BusinessPromotion.findOne({
      _id: enquiry.businessId,
      userId: req.userId
    });

    if (!business) {
      return res.status(403).json({
        success: false,
        error: "UNAUTHORIZED",
        message: "You don't have permission to respond to this enquiry"
      });
    }

    // Add response
    enquiry.responses.push({
      message,
      respondedBy: req.userId,
      respondedAt: new Date(),
      type: 'owner'
    });

    enquiry.status = 'responded';
    enquiry.lastResponseAt = new Date();
    enquiry.lastRespondedBy = req.userId;

    await enquiry.save();

    res.status(200).json({
      success: true,
      enquiry: {
        _id: enquiry._id,
        status: enquiry.status,
        lastResponseAt: enquiry.lastResponseAt,
        responses: enquiry.responses
      }
    });
  } catch (error) {
    console.error("Error responding to enquiry:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to respond to enquiry"
    });
  }
});

/**
 * PATCH /api/enquiries/:enquiryId/status
 * Update enquiry status
 */
router.patch("/:enquiryId/status", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { enquiryId } = req.params;
    const { status, notes } = req.body;

    if (!['new', 'responded', 'closed'].includes(status)) {
      return res.status(400).json({
        success: false,
        error: "INVALID_STATUS",
        message: "Status must be 'new', 'responded', or 'closed'"
      });
    }

    const enquiry = await Enquiry.findById(enquiryId);
    if (!enquiry) {
      return res.status(404).json({
        success: false,
        error: "ENQUIRY_NOT_FOUND",
        message: "Enquiry not found"
      });
    }

    // Verify ownership
    const business = await BusinessPromotion.findOne({
      _id: enquiry.businessId,
      userId: req.userId
    });

    if (!business) {
      return res.status(403).json({
        success: false,
        error: "UNAUTHORIZED",
        message: "You don't have permission to update this enquiry"
      });
    }

    enquiry.status = status as 'new' | 'responded' | 'closed';
    await enquiry.save();

    res.status(200).json({
      success: true,
      status: enquiry.status,
      message: `Enquiry marked as ${status}`
    });
  } catch (error) {
    console.error("Error updating enquiry status:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to update enquiry status"
    });
  }
});

/**
 * GET /api/enquiries/:businessId/enquiry-stats
 * Get enquiry statistics for business owner dashboard
 */
router.get("/:businessId/enquiry-stats", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { businessId } = req.params;

    // Verify ownership
    const business = await BusinessPromotion.findOne({
      _id: businessId,
      userId: req.userId
    });

    if (!business) {
      return res.status(403).json({
        success: false,
        error: "UNAUTHORIZED",
        message: "You don't have access to this business"
      });
    }

    const allEnquiries = await Enquiry.find({ businessId });

    const thisMonth = allEnquiries.filter(e =>
      new Date(e.createdAt) > new Date(Date.now() - 30 * 24 * 60 * 60 * 1000)
    );

    const responded = allEnquiries.filter(e => e.lastResponseAt);
    const avgResponseTime = responded.length > 0
      ? responded.reduce((sum, e) => {
        return sum + (new Date(e.lastResponseAt!).getTime() - new Date(e.createdAt).getTime());
      }, 0) / responded.length / (1000 * 60 * 60)
      : 0;

    const stats = {
      totalEnquiries: allEnquiries.length,
      newEnquiries: allEnquiries.filter(e => e.status === 'new').length,
      respondedEnquiries: allEnquiries.filter(e => e.status === 'responded').length,
      closedEnquiries: allEnquiries.filter(e => e.status === 'closed').length,
      averageResponseTime: avgResponseTime.toFixed(1),
      responseRate: allEnquiries.length > 0
        ? ((responded.length / allEnquiries.length) * 100).toFixed(1)
        : 0,
      conversionRate: allEnquiries.length > 0
        ? ((allEnquiries.filter(e => e.convertedToLead).length / allEnquiries.length) * 100).toFixed(1)
        : 0,
      thisMonth: thisMonth.length,
      thisMonthResponseTime: (() => {
        const respondedThisMonth = thisMonth.filter(e => e.lastResponseAt);
        if (respondedThisMonth.length === 0) return "0";
        const totalTime = respondedThisMonth.reduce((sum, e) => {
          return sum + (new Date(e.lastResponseAt!).getTime() - new Date(e.createdAt).getTime());
        }, 0);
        return (totalTime / respondedThisMonth.length / (1000 * 60 * 60)).toFixed(1);
      })(),
      convertedLeads: allEnquiries.filter(e => e.convertedToLead).length
    };

    res.status(200).json({
      success: true,
      stats
    });
  } catch (error) {
    console.error("Error fetching enquiry stats:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to fetch enquiry statistics"
    });
  }
});

/**
 * GET /api/enquiries/:businessId/combined-feedback
 * Get combined view of reviews and enquiries
 */
router.get("/:businessId/combined-feedback", requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { businessId } = req.params;
    const { sort = 'latest', type = 'all', page = 1 } = req.query;

    // Verify ownership
    const business = await BusinessPromotion.findOne({
      _id: businessId,
      userId: req.userId
    });

    if (!business) {
      return res.status(403).json({
        success: false,
        error: "UNAUTHORIZED",
        message: "You don't have access to this business"
      });
    }

    const pageNum = Math.max(1, parseInt(page as string) || 1);
    const limitNum = 20;
    const skip = (pageNum - 1) * limitNum;

    const feedback: any[] = [];

    if (type === 'all' || type === 'reviews') {
      const Review = require("../models/Review").default;
      const reviews = await Review.find({ businessId })
        .sort(sort === 'latest' ? { createdAt: -1 } : { createdAt: 1 })
        .limit(limitNum);

      reviews.forEach((r: any) => {
        feedback.push({
          type: 'review',
          id: r._id,
          userName: r.userName,
          rating: r.rating,
          message: r.message,
          createdAt: r.createdAt,
          status: r.ownerReply ? 'replied' : 'pending',
          hasOwnerReply: !!r.ownerReply
        });
      });
    }

    if (type === 'all' || type === 'enquiries') {
      const enquiries = await Enquiry.find({ businessId })
        .sort(sort === 'latest' ? { createdAt: -1 } : { createdAt: 1 })
        .limit(limitNum);

      enquiries.forEach(e => {
        feedback.push({
          type: 'enquiry',
          id: e._id,
          userName: e.userName,
          subject: e.subject,
          message: e.message,
          createdAt: e.createdAt,
          status: e.status,
          lastResponseAt: e.lastResponseAt
        });
      });
    }

    // Sort combined feedback
    feedback.sort((a, b) => {
      if (sort === 'latest') {
        return new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime();
      }
      return new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime();
    });

    const Review = require("../models/Review").default;
    const allReviews = await Review.find({ businessId });
    const allEnquiries = await Enquiry.find({ businessId });

    const paginatedFeedback = feedback.slice(skip, skip + limitNum);

    res.status(200).json({
      success: true,
      feedback: paginatedFeedback,
      summary: {
        totalReviews: allReviews.length,
        totalEnquiries: allEnquiries.length,
        unrepliedReviews: allReviews.filter((r: any) => !r.ownerReply).length,
        newEnquiries: allEnquiries.filter(e => e.status === 'new').length
      },
      pagination: {
        page: pageNum,
        limit: limitNum,
        total: feedback.length,
        pages: Math.ceil(feedback.length / limitNum)
      }
    });
  } catch (error) {
    console.error("Error fetching combined feedback:", error);
    res.status(500).json({
      success: false,
      error: "INTERNAL_ERROR",
      message: "Failed to fetch feedback"
    });
  }
});

export default router;
