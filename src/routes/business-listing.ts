import { Router } from "express";
import BusinessPromotion from "../models/BusinessPromotion";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

// Public listing feed: promoted + free (promoted first), no location filter for now.
router.get("/", async (req, res) => {
  try {
    const { subcategory, listingType, page = 1, limit = 20 } = req.query;
    const now = new Date();
    const pageNum = Math.max(1, parseInt(String(page), 10) || 1);
    const limitNum = Math.max(1, Math.min(50, parseInt(String(limit), 10) || 20));
    const skip = (pageNum - 1) * limitNum;

    const matchStage: any = {
      isActive: true,
      status: "active",
      $or: [{ expiryDate: null }, { expiryDate: { $gt: now } }],
    };

    if (subcategory) {
      matchStage.category = {
        $elemMatch: { $regex: String(subcategory), $options: "i" },
      };
    }

    if (listingType === "free" || listingType === "promoted") {
      matchStage.listingType = listingType;
    }

    const basePipeline: any[] = [
      { $match: matchStage },
      {
        $lookup: {
          from: "reviews",
          let: { businessId: "$_id" },
          pipeline: [
            {
              $match: {
                $expr: { $eq: ["$businessId", "$$businessId"] },
                isApproved: true,
                isSpam: false,
              },
            },
            {
              $group: {
                _id: "$businessId",
                averageRating: { $avg: "$rating" },
                totalReviews: { $sum: 1 },
              },
            },
          ],
          as: "reviewStats",
        },
      },
      {
        $addFields: {
          averageRating: {
            $round: [{ $ifNull: [{ $arrayElemAt: ["$reviewStats.averageRating", 0] }, 0] }, 1],
          },
          totalReviews: { $ifNull: [{ $arrayElemAt: ["$reviewStats.totalReviews", 0] }, 0] },
        },
      },
      {
        $addFields: {
          impressions: { $ifNull: ["$visibility.impressions", 0] },
          clicks: { $ifNull: ["$visibility.clicks", 0] },
          leads: { $ifNull: ["$visibility.leads", 0] },
          priority: { $ifNull: ["$visibility.priorityScore", 10] },
          isPromoted: { $cond: [{ $eq: ["$listingType", "promoted"] }, 1, 0] },
        },
      },
      {
        $addFields: {
          ctr: {
            $cond: [
              { $gt: ["$impressions", 0] },
              { $multiply: [{ $divide: ["$clicks", "$impressions"] }, 100] },
              0,
            ],
          },
        },
      },
      {
        $addFields: {
          // Log scaling avoids large raw click/lead counts dominating ranking.
          engagementScore: {
            $add: [
              { $multiply: [{ $ln: { $add: ["$clicks", 1] } }, 2] },
              { $multiply: [{ $ln: { $add: ["$leads", 1] } }, 4] },
            ],
          },
          finalScore: { $add: ["$priority", "$engagementScore"] },
        },
      },
      {
        $sort: {
          isPromoted: -1,
          finalScore: -1,
          createdAt: -1,
        },
      },
      {
        $project: {
          _id: 1,
          businessName: 1,
          ownerName: 1,
          description: 1,
          category: 1,
          phone: 1,
          whatsapp: 1,
          website: 1,
          businessHours: 1,
          area: 1,
          city: 1,
          state: 1,
          landmark: 1,
          pincode: 1,
          media: 1,
          listingType: 1,
          isActive: 1,
          expiryDate: 1,
          createdAt: 1,
          updatedAt: 1,
          rating: {
            averageRating: "$averageRating",
            totalReviews: "$totalReviews",
          },
          visibility: {
            impressions: "$impressions",
            clicks: "$clicks",
            leads: "$leads",
            ctr: "$ctr",
            priorityScore: "$priority",
            finalScore: "$finalScore",
          },
        },
      },
    ];

    const [listings, totalAgg] = await Promise.all([
      BusinessPromotion.aggregate([...basePipeline, { $skip: skip }, { $limit: limitNum }]),
      BusinessPromotion.aggregate([{ $match: matchStage }, { $count: "total" }]),
    ]);

    const total = totalAgg[0]?.total || 0;
    const promotedCount = listings.filter((item: any) => item.listingType === "promoted").length;
    const freeCount = listings.filter((item: any) => item.listingType === "free").length;

    res.json({
      success: true,
      data: listings,
      meta: {
        page: pageNum,
        limit: limitNum,
        total,
        pages: Math.ceil(total / limitNum),
        promotedCount,
        freeCount,
      },
    });
  } catch (error) {
    console.error("❌ Business listings error:", error);
    res.status(500).json({
      success: false,
      message: "Server error fetching listings",
    });
  }
});

// Get business details and increment impressions
router.get("/:id", async (req, res) => {
  try {
    const now = new Date();

    const business = await BusinessPromotion.findOne({
      _id: req.params.id,
      isActive: true,
      status: "active",
      $or: [{ expiryDate: null }, { expiryDate: { $gt: now } }],
    }).lean();

    if (!business) {
      return res.status(404).json({
        success: false,
        message: "Business not found or expired",
      });
    }

    await BusinessPromotion.findByIdAndUpdate(req.params.id, {
      $inc: { "visibility.impressions": 1 },
    });

    return res.json({
      success: true,
      data: business,
    });
  } catch (error) {
    console.error("❌ Detail error:", error);
    return res.status(500).json({
      success: false,
      message: "Server error",
    });
  }
});

// Lead tracking endpoint
router.post("/:id/lead", async (req, res) => {
  try {
    await BusinessPromotion.findByIdAndUpdate(req.params.id, {
      $inc: { "visibility.leads": 1 },
    });

    res.json({ success: true });
  } catch (err) {
    res.status(500).json({ success: false });
  }
});

// Impression tracking endpoint
router.post("/:id/impression", async (req, res) => {
  await BusinessPromotion.findByIdAndUpdate(req.params.id, {
    $inc: { "visibility.impressions": 1 },
  });
  res.json({ success: true });
});

// Click tracking endpoint
router.post("/:id/click", async (req, res) => {
  await BusinessPromotion.findByIdAndUpdate(req.params.id, {
    $inc: { "visibility.clicks": 1 },
  });
  res.json({ success: true });
});

export default router;
