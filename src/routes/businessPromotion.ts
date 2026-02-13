import { Router } from "express";
import BusinessPromotion from "../models/BusinessPromotion";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

// POST /api/business-promotion - Create or update business promotion
router.post("/", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    const {
      businessName,
      ownerName,
      description,
      category,
      email,
      phone,
      whatsapp,
      website,
      businessHours,
      area,
      pincode,
      plotNo,
      buildingName,
      streetName,
      landmark,
      city,
      state,
      gstNumber,
      panNumber,
      currentStep,
      progress,
      stepIndex,
      promotionId, // Optional: if updating existing promotion
    } = req.body;


    console.log(`📝 [BUSINESS-PROMOTION] ${promotionId ? 'Updating' : 'Creating'} promotion for user: ${userId}`);

    let promotion;
    if (!promotionId) {
      const existing = await BusinessPromotion.findOne({
        userId,
        status: { $in: ['draft', 'submitted'] }
      });

      if (existing) {
        return res.json({
          success: true,
          promotion: {
            _id: existing._id,
            businessName: existing.businessName,
            ownerName: existing.ownerName,
            status: existing.status,
            currentStep: existing.currentStep,
            createdAt: existing.createdAt,
            updatedAt: existing.updatedAt,
          }
        });
      }
    }


    if (promotionId) {
      // Update existing promotion
      promotion = await BusinessPromotion.findOne({ _id: promotionId, userId });

      if (!promotion) {
        console.log(`❌ [BUSINESS-PROMOTION] Promotion not found or unauthorized: ${promotionId}`);
        return res.status(404).json({
          success: false,
          message: 'Promotion not found or unauthorized'
        });
      }

      // Update fields
      // Never allow client to directly change these
      const SAFE_FIELDS = {
        businessName,
        ownerName,
        description,
        category,
        email,
        phone,
        whatsapp,
        website,
        businessHours,
        area,
        pincode,
        plotNo,
        buildingName,
        streetName,
        landmark,
        city,
        state,
        gstNumber,
        panNumber,
        currentStep,
      };

      Object.assign(promotion, SAFE_FIELDS);

      if (currentStep === 'location') {
        promotion.status = 'submitted';
      }

      await promotion.save();
      console.log(`✅ [BUSINESS-PROMOTION] Promotion updated: ${promotionId}`);

    } else {
      // Create new promotion
      promotion = new BusinessPromotion({
        userId,
        businessName,
        ownerName,
        description,
        category,
        email,
        phone,
        whatsapp,
        website,
        businessHours,
        area,
        pincode,
        plotNo,
        buildingName,
        streetName,
        landmark,
        city,
        state,
        gstNumber,
        panNumber,
        status: 'draft',
        currentStep: currentStep || 'business',
      });

      await promotion.save();
      console.log(`✅ [BUSINESS-PROMOTION] New promotion created: ${promotion._id}`);
    }

    res.json({
      success: true,
      message: `Business promotion ${promotionId ? 'updated' : 'created'} successfully`,
      promotion: {
        _id: promotion._id,
        businessName: promotion.businessName,
        ownerName: promotion.ownerName,
        status: promotion.status,
        currentStep: promotion.currentStep,
        createdAt: promotion.createdAt,
        updatedAt: promotion.updatedAt,
      },
    });

  } catch (error) {
    console.error('❌ [BUSINESS-PROMOTION] ERROR creating/updating promotion:', error);
    res.status(500).json({
      success: false,
      message: 'Server error while saving business promotion'
    });
  }
});

// POST /api/business-promotion/:id/activate-free
router.post("/:id/activate-free", requireAuth, async (req: AuthReq, res) => {
  try {
    const { id } = req.params;
    const userId = req.userId;

    const promotion = await BusinessPromotion.findOne({ _id: id, userId });

    if (!promotion) {
      return res.status(404).json({ success: false, message: 'Not found' });
    }

    if (promotion.status === 'active') {
      return res.status(400).json({
        success: false,
        message: 'Active listing cannot be edited'
      });
    }

    if (promotion.status !== 'submitted') {
      return res.status(400).json({
        success: false,
        message: 'Complete business profile before activation'
      });
    }



    promotion.listingType = 'free';
    promotion.plan = null;
    promotion.paymentStatus = 'not_required';
    promotion.status = 'active';
    promotion.isActive = true;
    promotion.expiryDate = null;
    promotion.visibility.priorityScore = 10;

    await promotion.save();

    res.json({
      success: true,
      message: 'Free listing activated',
    });

  } catch (err) {
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// POST /api/business-promotion/:id/activate-promoted
router.post("/:id/activate-promoted", requireAuth, async (req: AuthReq, res) => {
  try {
    const { id } = req.params;
    const userId = req.userId;
    const { planName, price, durationDays, paymentId } = req.body;

    const promotion = await BusinessPromotion.findOne({ _id: id, userId });

    if (!promotion) {
      return res.status(404).json({ success: false, message: 'Not found' });
    }

    if (promotion.paymentStatus === 'paid' && promotion.isActive) {
      return res.status(400).json({
        success: false,
        message: 'Listing already promoted'
      });
    }
    if (promotion.status !== 'submitted') {
      return res.status(400).json({
        success: false,
        message: 'Complete business profile before activation'
      });
    }

    if (!['basic', 'plus', 'max'].includes(planName)) {
      return res.status(400).json({
        success: false,
        message: 'Invalid plan selected'
      });
    }

    if (!durationDays || durationDays <= 0) {
      return res.status(400).json({
        success: false,
        message: 'Invalid plan duration'
      });
    }


    const activatedAt = new Date();

    promotion.listingType = 'promoted';
    promotion.plan = {
      name: planName,
      price,
      durationDays,
      activatedAt,
    };

    promotion.paymentStatus = 'paid';
    promotion.paymentId = paymentId;
    promotion.status = 'active';
    promotion.isActive = true;
    promotion.expiryDate = new Date(
      activatedAt.getTime() + durationDays * 24 * 60 * 60 * 1000
    );

    // Priority logic
    promotion.visibility.priorityScore =
      planName === 'max' ? 80 : planName === 'plus' ? 60 : 50;

    await promotion.save();

    res.json({
      success: true,
      message: 'Promoted listing activated',
    });

  } catch (err) {
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// GET /api/business-promotion/in-progress
router.get('/in-progress', requireAuth, async (req: AuthReq, res) => {
  const userId = req.userId;

  const promotion = await BusinessPromotion.findOne({
    userId,
    status: { $in: ['draft', 'submitted'] }
  }).sort({ updatedAt: -1 });

  res.json({
    success: true,
    promotion: promotion || null
  });
});



// GET /api/business-promotion - Get all user's business promotions
router.get("/", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    const { status } = req.query;

    console.log(`📋 [BUSINESS-PROMOTION] Fetching promotions for user: ${userId}`);

    const query: any = { userId };
    if (status) {
      query.status = status;
    }

    const promotions = await BusinessPromotion.find(query)
      .select('-__v')
      .sort({ createdAt: -1 })
      .lean();

    console.log(`✅ [BUSINESS-PROMOTION] Found ${promotions.length} promotions`);

    res.json({
      success: true,
      promotions,
    });

  } catch (error) {
    console.error('❌ [BUSINESS-PROMOTION] ERROR fetching promotions:', error);
    res.status(500).json({
      success: false,
      message: 'Server error while fetching promotions'
    });
  }
});

// GET /api/business-promotion/:id - Get specific business promotion
router.get("/:id", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    const { id } = req.params;

    console.log(`📋 [BUSINESS-PROMOTION] Fetching promotion: ${id} for user: ${userId}`);

    const promotion = await BusinessPromotion.findOne({ _id: id, userId }).lean();

    if (!promotion) {
      console.log(`❌ [BUSINESS-PROMOTION] Promotion not found or unauthorized: ${id}`);
      return res.status(404).json({
        success: false,
        message: 'Promotion not found or unauthorized'
      });
    }

    console.log(`✅ [BUSINESS-PROMOTION] Promotion retrieved: ${id}`);

    res.json({
      success: true,
      promotion,
    });

  } catch (error) {
    console.error('❌ [BUSINESS-PROMOTION] ERROR fetching promotion:', error);
    res.status(500).json({
      success: false,
      message: 'Server error while fetching promotion'
    });
  }
});

// DELETE /api/business-promotion/:id - Delete a business promotion
router.delete("/:id", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    const { id } = req.params;

    console.log(`🗑️ [BUSINESS-PROMOTION] Deleting promotion: ${id} for user: ${userId}`);

    const promotion = await BusinessPromotion.findOneAndDelete({ _id: id, userId });

    if (!promotion) {
      console.log(`❌ [BUSINESS-PROMOTION] Promotion not found or unauthorized: ${id}`);
      return res.status(404).json({
        success: false,
        message: 'Promotion not found or unauthorized'
      });
    }

    console.log(`✅ [BUSINESS-PROMOTION] Promotion deleted: ${id}`);

    res.json({
      success: true,
      message: 'Business promotion deleted successfully',
    });

  } catch (error) {
    console.error('❌ [BUSINESS-PROMOTION] ERROR deleting promotion:', error);
    res.status(500).json({
      success: false,
      message: 'Server error while deleting promotion'
    });
  }
});

export default router;


// Later cron job:
// if (expiryDate < now) {
//   status = 'expired';
//   isActive = false;
//   paymentStatus = 'expired';
// }
// TODO: validate price against server-side plan config
