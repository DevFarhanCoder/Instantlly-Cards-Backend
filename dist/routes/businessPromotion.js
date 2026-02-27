"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const BusinessPromotion_1 = __importDefault(require("../models/BusinessPromotion"));
const auth_1 = require("../middleware/auth");
const channelPartnerAds_1 = require("./channelPartnerAds");
const aws_sdk_1 = __importDefault(require("aws-sdk"));
const router = (0, express_1.Router)();
// POST /api/business-promotion - Create or update business promotion
router.post("/", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const { businessName, ownerName, description, category, email, phone, whatsapp, website, businessHours, area, pincode, plotNo, buildingName, streetName, landmark, city, state, gstNumber, panNumber, currentStep, progress, stepIndex, promotionId, // Optional: if updating existing promotion
         } = req.body;
        console.log(`📝 [BUSINESS-PROMOTION] ${promotionId ? 'Updating' : 'Creating'} promotion for user: ${userId}`);
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
    }
    catch (error) {
        console.error('❌ [BUSINESS-PROMOTION] ERROR creating/updating promotion:', error);
        res.status(500).json({
            success: false,
            message: 'Server error while saving business promotion'
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
        const { planName, price, durationDays, paymentId } = req.body;
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
    const userId = req.userId;
    const promotion = await BusinessPromotion_1.default.findOne({
        userId,
        status: { $in: ['draft', 'submitted'] }
    }).sort({ updatedAt: -1 });
    res.json({
        success: true,
        promotion: promotion || null
    });
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
        const promotion = await BusinessPromotion_1.default.findOne({ _id: id, userId });
        if (!promotion) {
            return res.status(404).json({ success: false });
        }
        promotion.isActive = !promotion.isActive;
        promotion.status = promotion.isActive ? "active" : "inactive";
        await promotion.save();
        res.json({
            success: true,
            isActive: promotion.isActive
        });
    }
    catch (err) {
        res.status(500).json({ success: false });
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
