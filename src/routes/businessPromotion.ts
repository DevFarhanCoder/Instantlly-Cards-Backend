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
      status,
      currentStep,
      promotionId, // Optional: if updating existing promotion
    } = req.body;

    console.log(`📝 [BUSINESS-PROMOTION] ${promotionId ? 'Updating' : 'Creating'} promotion for user: ${userId}`);

    let promotion;

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
      Object.assign(promotion, {
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
        status,
        currentStep,
      });

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
        status: status || 'draft',
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
