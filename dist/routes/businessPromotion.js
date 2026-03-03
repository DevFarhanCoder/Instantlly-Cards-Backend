"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const BusinessPromotion_1 = __importDefault(require("../models/BusinessPromotion"));
const PromotionOrder_1 = __importDefault(require("../models/PromotionOrder"));
const auth_1 = require("../middleware/auth");
const channelPartnerAds_1 = require("./channelPartnerAds");
const aws_sdk_1 = __importDefault(require("aws-sdk"));
const mongoose_1 = __importDefault(require("mongoose"));
const router = (0, express_1.Router)();
const normalizeListingIntent = (value) => {
    return value === 'promoted' ? 'promoted' : 'free';
};
const normalizeProgress = (value) => {
    const n = Number(value);
    if (!Number.isFinite(n))
        return 0;
    return Math.max(0, Math.min(100, n));
};
const normalizeStepIndex = (value) => {
    const n = Number(value);
    if (!Number.isFinite(n))
        return 1;
    return Math.max(1, Math.min(4, Math.floor(n)));
};
const isNonEmptyString = (value) => typeof value === 'string' && value.trim().length > 0;
const hasCategory = (value) => Array.isArray(value) && value.length > 0;
const canMarkSubmitted = (payload) => {
    // Require explicit final-step context + minimum required fields.
    if (payload.currentStep !== 'location')
        return false;
    if (payload.stepIndex < 4 && payload.progress < 100)
        return false;
    return (isNonEmptyString(payload.businessName) &&
        isNonEmptyString(payload.ownerName) &&
        hasCategory(payload.category) &&
        isNonEmptyString(payload.phone) &&
        isNonEmptyString(payload.area) &&
        isNonEmptyString(payload.pincode) &&
        isNonEmptyString(payload.city) &&
        isNonEmptyString(payload.state));
};
const hasMinimumRequiredBusinessProfile = (promotion) => {
    return (isNonEmptyString(promotion?.businessName) &&
        isNonEmptyString(promotion?.ownerName) &&
        hasCategory(promotion?.category) &&
        isNonEmptyString(promotion?.phone) &&
        isNonEmptyString(promotion?.area) &&
        isNonEmptyString(promotion?.pincode) &&
        isNonEmptyString(promotion?.city) &&
        isNonEmptyString(promotion?.state));
};
const assignPromotionEditableFields = (promotion, payload) => {
    // Never allow client to directly change status/payment/listingType lifecycle fields.
    const SAFE_FIELDS = {
        businessName: payload.businessName,
        ownerName: payload.ownerName,
        description: payload.description,
        category: payload.category,
        email: payload.email,
        phone: payload.phone,
        whatsapp: payload.whatsapp,
        website: payload.website,
        businessHours: payload.businessHours,
        area: payload.area,
        pincode: payload.pincode,
        plotNo: payload.plotNo,
        buildingName: payload.buildingName,
        streetName: payload.streetName,
        landmark: payload.landmark,
        city: payload.city,
        state: payload.state,
        gstNumber: payload.gstNumber,
        panNumber: payload.panNumber,
        currentStep: payload.currentStep,
        progress: payload.progress,
        stepIndex: payload.stepIndex,
        listingIntent: payload.listingIntent,
    };
    Object.assign(promotion, SAFE_FIELDS);
};
// POST /api/business-promotion - Create or update business promotion
router.post("/", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { businessName, ownerName, description, category, email, phone, whatsapp, website, businessHours, area, pincode, plotNo, buildingName, streetName, landmark, city, state, gstNumber, panNumber, listingType, listingIntent, currentStep, progress, stepIndex, promotionId, // Optional: if updating existing promotion
         } = req.body;
        console.log(`📝 [BUSINESS-PROMOTION] ${promotionId ? 'Updating' : 'Creating'} promotion for user: ${userId}`);
        const resolvedListingIntent = normalizeListingIntent(listingIntent || listingType);
        const resolvedProgress = normalizeProgress(progress);
        const resolvedStepIndex = normalizeStepIndex(stepIndex);
        let promotion;
        if (!promotionId) {
            const existing = await BusinessPromotion_1.default.findOne({
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
            promotion = await BusinessPromotion_1.default.findOne({ _id: promotionId, userId });
            if (!promotion) {
                console.log(`❌ [BUSINESS-PROMOTION] Promotion not found or unauthorized: ${promotionId}`);
                return res.status(404).json({
                    success: false,
                    message: 'Promotion not found or unauthorized'
                });
            }
            assignPromotionEditableFields(promotion, {
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
                progress: resolvedProgress,
                stepIndex: resolvedStepIndex,
                listingIntent: resolvedListingIntent,
            });
            if (canMarkSubmitted({
                currentStep,
                businessName,
                ownerName,
                category,
                phone,
                area,
                pincode,
                city,
                state,
                stepIndex: resolvedStepIndex,
                progress: resolvedProgress
            })) {
                promotion.status = 'submitted';
                promotion.paymentStatus =
                    resolvedListingIntent === 'promoted' ? 'pending' : 'not_required';
            }
            else if (currentStep === 'location') {
                // User reached final step but minimum required fields are incomplete.
                promotion.status = 'draft';
            }
            await promotion.save();
            console.log(`✅ [BUSINESS-PROMOTION] Promotion updated: ${promotionId}`);
        }
        else {
            // Create new promotion
            promotion = new BusinessPromotion_1.default({
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
                listingIntent: resolvedListingIntent,
                status: 'draft',
                currentStep: currentStep || 'business',
                progress: resolvedProgress,
                stepIndex: resolvedStepIndex,
                paymentStatus: resolvedListingIntent === 'promoted' ? 'pending' : 'not_required',
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
                listingIntent: promotion.listingIntent,
                status: promotion.status,
                currentStep: promotion.currentStep,
                createdAt: promotion.createdAt,
                updatedAt: promotion.updatedAt,
            },
        });
    }
    catch (error) {
        console.error('❌ [BUSINESS-PROMOTION] ERROR creating/updating promotion:', error);
        res.status(500).json({
            success: false,
            message: 'Server error while saving business promotion'
        });
    }
});
// POST /api/business-promotion/:id/upgrade-to-pro
// Upgrade existing free listing to pro intent and mark payment as pending.
router.post("/:id/upgrade-to-pro", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { id } = req.params;
        if (!mongoose_1.default.Types.ObjectId.isValid(id)) {
            return res.status(400).json({
                success: false,
                message: "Invalid promotion id",
            });
        }
        const promotion = await BusinessPromotion_1.default.findById(id);
        if (!promotion) {
            return res.status(404).json({
                success: false,
                message: "Listing not found",
            });
        }
        if (promotion.userId.toString() !== userId) {
            return res.status(403).json({
                success: false,
                message: "You are not allowed to upgrade this listing",
            });
        }
        const isAlreadyPro = promotion.listingType === "promoted" || promotion.listingIntent === "promoted";
        // Idempotent retry response while payment is still pending.
        if (isAlreadyPro && promotion.paymentStatus === "pending") {
            return res.json({
                success: true,
                message: "Listing upgraded to Pro. Payment required.",
                promotion: {
                    _id: promotion._id,
                    listingIntent: "promoted",
                    listingType: "promoted",
                    status: promotion.status,
                    paymentStatus: "pending",
                },
                nextAction: "pending_payment",
            });
        }
        if (isAlreadyPro) {
            return res.status(409).json({
                success: false,
                message: "Listing is already promoted",
            });
        }
        if (promotion.status === "draft") {
            return res.status(400).json({
                success: false,
                message: "Complete business profile before upgrading to Pro",
            });
        }
        promotion.listingIntent = "promoted";
        promotion.listingType = "promoted"; // compatibility for older listing consumers
        promotion.status = "submitted";
        promotion.paymentStatus = "pending";
        promotion.paymentId = null;
        promotion.plan = null;
        promotion.expiryDate = null;
        promotion.isActive = false;
        promotion.visibility.priorityScore = 10;
        await promotion.save();
        return res.json({
            success: true,
            message: "Listing upgraded to Pro. Payment required.",
            promotion: {
                _id: promotion._id,
                listingIntent: promotion.listingIntent,
                listingType: promotion.listingType,
                status: promotion.status,
                paymentStatus: promotion.paymentStatus,
            },
            nextAction: "pending_payment",
        });
    }
    catch (error) {
        console.error("❌ [BUSINESS-PROMOTION] ERROR upgrading listing to pro:", error);
        return res.status(500).json({
            success: false,
            message: "Server error while upgrading listing",
        });
    }
});
// POST /api/business-promotion/:id/activate-free
router.post("/:id/activate-free", auth_1.requireAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const userId = req.userId;
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId });
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
        if (promotion.listingIntent === 'promoted') {
            return res.status(400).json({
                success: false,
                message: 'This listing was created as premium. Complete payment to activate.'
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
    }
    catch (err) {
        res.status(500).json({ success: false, message: 'Server error' });
    }
});
// POST /api/business-promotion/:id/activate-promoted
router.post("/:id/activate-promoted", auth_1.requireAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const userId = req.userId;
        const { planName, price, durationDays, paymentId, orderId } = req.body;
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId });
        if (!promotion) {
            return res.status(404).json({ success: false, message: 'Not found' });
        }
        if (promotion.paymentStatus === 'paid' && promotion.isActive) {
            return res.status(400).json({
                success: false,
                message: 'Listing already promoted'
            });
        }
        if (!['submitted', 'active', 'inactive', 'expired'].includes(promotion.status)) {
            return res.status(400).json({
                success: false,
                message: 'Complete business profile before activation'
            });
        }
        // Preferred path: activate through server-side promotion order snapshot
        if (orderId) {
            const order = await PromotionOrder_1.default.findOne({
                _id: orderId,
                userId,
                businessPromotionId: id
            });
            if (!order) {
                return res.status(404).json({
                    success: false,
                    message: 'Promotion order not found'
                });
            }
            if (!['created', 'payment_pending', 'paid', 'activated'].includes(order.status)) {
                return res.status(400).json({
                    success: false,
                    message: `Cannot activate from ${order.status} order`
                });
            }
            const activatedAt = new Date();
            const expiresAt = new Date(activatedAt.getTime() + order.durationDays * 24 * 60 * 60 * 1000);
            promotion.listingType = 'promoted';
            promotion.plan = {
                name: `${order.areaType}_rank_${order.rank}`,
                price: order.amount,
                durationDays: order.durationDays,
                activatedAt,
            };
            promotion.paymentStatus = 'paid';
            promotion.paymentId = paymentId || order.paymentId || order._id.toString();
            promotion.status = 'active';
            promotion.isActive = true;
            promotion.expiryDate = expiresAt;
            promotion.visibility.priorityScore = order.priorityScore;
            await promotion.save();
            order.status = 'activated';
            order.paymentId = promotion.paymentId;
            order.paidAt = activatedAt;
            order.activatedAt = activatedAt;
            order.expiresAt = expiresAt;
            await order.save();
            return res.json({
                success: true,
                message: 'Promoted listing activated',
            });
        }
        // Legacy path (kept for backward compatibility)
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
        promotion.expiryDate = new Date(activatedAt.getTime() + durationDays * 24 * 60 * 60 * 1000);
        // Priority logic
        promotion.visibility.priorityScore =
            planName === 'max' ? 80 : planName === 'plus' ? 60 : 50;
        await promotion.save();
        res.json({
            success: true,
            message: 'Promoted listing activated',
        });
    }
    catch (err) {
        res.status(500).json({ success: false, message: 'Server error' });
    }
});
// GET /api/business-promotion/in-progress
router.get('/in-progress', auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        // Highest priority resume case: premium intent submitted but payment pending.
        const pendingPremium = await BusinessPromotion_1.default.findOne({
            userId,
            status: 'submitted',
            listingIntent: 'promoted',
            paymentStatus: 'pending'
        }).sort({ updatedAt: -1 });
        if (pendingPremium) {
            return res.json({
                success: true,
                promotion: pendingPremium,
                nextAction: 'pending_payment'
            });
        }
        // Otherwise return latest draft/submitted form to continue filling form steps.
        const promotion = await BusinessPromotion_1.default.findOne({
            userId,
            status: { $in: ['draft', 'submitted'] }
        }).sort({ updatedAt: -1 });
        return res.json({
            success: true,
            promotion: promotion || null,
            nextAction: promotion ? 'complete_form' : 'create_new'
        });
    }
    catch (error) {
        console.error('❌ [BUSINESS-PROMOTION] ERROR fetching in-progress promotion:', error);
        return res.status(500).json({
            success: false,
            message: 'Server error while fetching in-progress promotion'
        });
    }
});
// PATCH /api/business-promotion/:id/premium-edit
// Edit an already-paid premium listing without re-running payment lifecycle.
router.patch("/:id/premium-edit", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { id } = req.params;
        const { businessName, ownerName, description, category, email, phone, whatsapp, website, businessHours, area, pincode, plotNo, buildingName, streetName, landmark, city, state, gstNumber, panNumber, currentStep, progress, stepIndex, } = req.body;
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId });
        if (!promotion) {
            return res.status(404).json({
                success: false,
                message: "Promotion not found or unauthorized",
            });
        }
        if (promotion.listingType !== "promoted" || promotion.paymentStatus !== "paid") {
            return res.status(400).json({
                success: false,
                message: "Only paid premium listings can use this edit API",
            });
        }
        const resolvedProgress = normalizeProgress(progress);
        const resolvedStepIndex = normalizeStepIndex(stepIndex);
        assignPromotionEditableFields(promotion, {
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
            progress: resolvedProgress,
            stepIndex: resolvedStepIndex,
            listingIntent: "promoted",
        });
        // Keep premium listing active; no payment step is required on edit.
        promotion.listingIntent = "promoted";
        promotion.listingType = "promoted";
        promotion.paymentStatus = "paid";
        promotion.isActive = true;
        promotion.status = "active";
        await promotion.save();
        return res.json({
            success: true,
            message: "Premium listing updated successfully",
            promotion: {
                _id: promotion._id,
                status: promotion.status,
                listingType: promotion.listingType,
                paymentStatus: promotion.paymentStatus,
                updatedAt: promotion.updatedAt,
            },
        });
    }
    catch (error) {
        console.error("❌ [BUSINESS-PROMOTION] ERROR premium editing listing:", error);
        return res.status(500).json({
            success: false,
            message: "Server error while updating premium listing",
        });
    }
});
// GET /api/business-promotion - Get all user's business promotions
router.get("/", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { status } = req.query;
        console.log(`📋 [BUSINESS-PROMOTION] Fetching promotions for user: ${userId}`);
        const query = { userId };
        if (status) {
            query.status = status;
        }
        const promotions = await BusinessPromotion_1.default.find(query)
            .select('-__v')
            .sort({ createdAt: -1 })
            .lean();
        console.log(`✅ [BUSINESS-PROMOTION] Found ${promotions.length} promotions`);
        res.json({
            success: true,
            promotions,
        });
    }
    catch (error) {
        console.error('❌ [BUSINESS-PROMOTION] ERROR fetching promotions:', error);
        res.status(500).json({
            success: false,
            message: 'Server error while fetching promotions'
        });
    }
});
// GET /api/business-promotion/:id - Get specific business promotion
router.get("/:id", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { id } = req.params;
        console.log(`📋 [BUSINESS-PROMOTION] Fetching promotion: ${id} for user: ${userId}`);
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId }).lean();
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
    }
    catch (error) {
        console.error('❌ [BUSINESS-PROMOTION] ERROR fetching promotion:', error);
        res.status(500).json({
            success: false,
            message: 'Server error while fetching promotion'
        });
    }
});
// DELETE /api/business-promotion/:id - Delete a business promotion
router.delete("/:id", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { id } = req.params;
        console.log(`🗑️ [BUSINESS-PROMOTION] Deleting promotion: ${id} for user: ${userId}`);
        const promotion = await BusinessPromotion_1.default.findOneAndDelete({ _id: id, userId });
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
    }
    catch (error) {
        console.error('❌ [BUSINESS-PROMOTION] ERROR deleting promotion:', error);
        res.status(500).json({
            success: false,
            message: 'Server error while deleting promotion'
        });
    }
});
// GET /api/business-promotion/:id/analytics
router.get("/:id/analytics", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { id } = req.params;
        const promotion = await BusinessPromotion_1.default.findOne({
            _id: id,
            userId
        }).lean();
        if (!promotion) {
            return res.status(404).json({
                success: false,
                message: "Listing not found"
            });
        }
        const impressions = promotion?.visibility?.impressions || 0;
        const clicks = promotion?.visibility?.clicks || 0;
        const leads = promotion?.visibility?.leads || 0;
        const ctr = impressions > 0 ? (clicks / impressions) * 100 : 0;
        res.json({
            success: true,
            analytics: {
                impressions,
                clicks,
                leads,
                ctr: Number(ctr.toFixed(2)),
                priorityScore: promotion.visibility?.priorityScore,
                expiryDate: promotion.expiryDate,
                listingType: promotion.listingType,
                status: promotion.status
            }
        });
    }
    catch (error) {
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
router.post("/:id/media", auth_1.requireAuth, channelPartnerAds_1.upload.single("image"), async (req, res) => {
    try {
        const { id } = req.params;
        const userId = req.userId;
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId });
        if (!promotion) {
            return res.status(404).json({ success: false });
        }
        if (!req.file) {
            return res.status(400).json({ success: false, message: "No file" });
        }
        const s3 = new aws_sdk_1.default.S3({
            region: process.env.AWS_REGION,
            credentials: {
                accessKeyId: process.env.AWS_ACCESS_KEY_ID,
                secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY,
            },
        });
        const key = `business-media/${Date.now()}-${req.file.originalname}`;
        await s3
            .putObject({
            Bucket: process.env.S3_BUCKET,
            Key: key,
            Body: req.file.buffer,
            ContentType: req.file.mimetype,
        })
            .promise();
        const imageUrl = `${process.env.CLOUDFRONT_HOST}/${key}`;
        promotion.media.push({
            url: imageUrl,
            publicId: key,
        });
        await promotion.save();
        res.json({
            success: true,
            media: promotion.media,
        });
    }
    catch (err) {
        console.error(err);
        res.status(500).json({ success: false });
    }
});
router.delete("/:id/media/:mediaId", auth_1.requireAuth, async (req, res) => {
    try {
        const { id, mediaId } = req.params;
        const userId = req.userId;
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId });
        if (!promotion) {
            return res.status(404).json({ success: false });
        }
        const mediaItem = promotion.media.find((m) => m._id.toString() === mediaId);
        if (!mediaItem) {
            return res.status(404).json({ success: false, message: "Media not found" });
        }
        // Delete from S3
        if (mediaItem.publicId) {
            await channelPartnerAds_1.s3
                .deleteObject({
                Bucket: process.env.S3_BUCKET,
                Key: mediaItem.publicId,
            })
                .promise();
        }
        promotion.media = promotion.media.filter((m) => m._id.toString() !== mediaId);
        await promotion.save();
        res.json({ success: true });
    }
    catch (err) {
        console.error("❌ Delete media error:", err);
        res.status(500).json({ success: false });
    }
});
router.patch("/:id/toggle-status", auth_1.requireAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const userId = req.userId;
        const requestedIsActive = req.body?.isActive;
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId });
        if (!promotion) {
            return res.status(404).json({
                success: false,
                message: "Listing not found",
            });
        }
        // Backward compatible behavior: if isActive is not provided, keep old "toggle" semantics.
        const targetIsActive = typeof requestedIsActive === "boolean" ? requestedIsActive : !promotion.isActive;
        if (promotion.isActive === targetIsActive) {
            return res.json({
                success: true,
                message: targetIsActive ? "Listing is already active" : "Listing is already inactive",
                isActive: promotion.isActive,
                status: promotion.status,
                paymentStatus: promotion.paymentStatus,
            });
        }
        if (!targetIsActive) {
            if (promotion.status !== "active") {
                return res.status(400).json({
                    success: false,
                    message: "Only active listings can be deactivated",
                });
            }
            promotion.isActive = false;
            promotion.status = "inactive";
            await promotion.save();
            return res.json({
                success: true,
                message: "Listing deactivated",
                isActive: promotion.isActive,
                status: promotion.status,
                paymentStatus: promotion.paymentStatus,
            });
        }
        if (!hasMinimumRequiredBusinessProfile(promotion)) {
            return res.status(400).json({
                success: false,
                message: "Complete required business profile fields before activation",
            });
        }
        if (promotion.status === "draft") {
            return res.status(400).json({
                success: false,
                message: "Draft listing cannot be activated. Complete and submit listing first.",
            });
        }
        const isPromotedListing = promotion.listingType === "promoted" || promotion.listingIntent === "promoted";
        const now = new Date();
        if (isPromotedListing) {
            if (promotion.expiryDate && promotion.expiryDate <= now) {
                promotion.status = "expired";
                promotion.isActive = false;
                promotion.paymentStatus = "expired";
                await promotion.save();
                return res.status(409).json({
                    success: false,
                    message: "Premium listing has expired. Renew promotion to activate.",
                });
            }
            if (promotion.paymentStatus !== "paid") {
                const message = promotion.paymentStatus === "pending"
                    ? "Premium listing payment is pending. Complete payment to activate."
                    : "Premium listing is not paid. Complete payment to activate.";
                return res.status(400).json({ success: false, message });
            }
            if (!promotion.expiryDate) {
                return res.status(400).json({
                    success: false,
                    message: "Premium listing has no active plan. Complete promotion purchase first.",
                });
            }
        }
        else {
            if (promotion.paymentStatus !== "not_required") {
                return res.status(400).json({
                    success: false,
                    message: "Free listing payment state is invalid. Contact support.",
                });
            }
            if (promotion.expiryDate) {
                return res.status(400).json({
                    success: false,
                    message: "Free listing cannot have expiry. Contact support.",
                });
            }
        }
        promotion.isActive = true;
        promotion.status = "active";
        await promotion.save();
        return res.json({
            success: true,
            message: "Listing activated",
            isActive: promotion.isActive,
            status: promotion.status,
            paymentStatus: promotion.paymentStatus,
        });
    }
    catch (err) {
        console.error("❌ [BUSINESS-PROMOTION] toggle-status error:", err);
        res.status(500).json({
            success: false,
            message: "Server error while toggling listing status",
        });
    }
});
exports.default = router;
// Later cron job:
// if (expiryDate < now) {
//   status = 'expired';
//   isActive = false;
//   paymentStatus = 'expired';
// }
// TODO: validate price against server-side plan config
// /cron/expiry-check.ts
// import BusinessPromotion from "../models/BusinessPromotion";
// export const checkExpiredListings = async () => {
//   const now = new Date();
//   await BusinessPromotion.updateMany(
//     {
//       expiryDate: { $lt: now },
//       status: "active"
//     },
//     {
//       status: "expired",
//       isActive: false,
//       paymentStatus: "expired"
//     }
//   );
//   console.log("Expired listings updated");
// };
// Upgrade ranking to Mongo aggregation pipeline
// Add fraud detection (fake clicks prevention)
// Add geo-distance ranking
// Add AI smart boost logic
// Add daily performance charts API
