"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const BusinessPromotion_1 = __importDefault(require("../models/BusinessPromotion"));
const router = (0, express_1.Router)();
router.get('/', async (req, res) => {
    try {
        const { subcategory, city } = req.query;
        const now = new Date();
        const matchStage = {
            isActive: true,
            status: 'active',
            $or: [
                { expiryDate: null },
                { expiryDate: { $gt: now } }
            ]
        };
        if (city) {
            matchStage.city = city;
        }
        if (subcategory) {
            matchStage.category = {
                $regex: subcategory,
                $options: 'i'
            };
        }
        const listings = await BusinessPromotion_1.default.aggregate([
            { $match: matchStage },
            {
                $addFields: {
                    impressions: { $ifNull: ['$visibility.impressions', 0] },
                    clicks: { $ifNull: ['$visibility.clicks', 0] },
                    leads: { $ifNull: ['$visibility.leads', 0] },
                    priority: { $ifNull: ['$visibility.priorityScore', 10] }
                }
            },
            {
                $addFields: {
                    ctr: {
                        $cond: [
                            { $gt: ['$impressions', 0] },
                            { $multiply: [{ $divide: ['$clicks', '$impressions'] }, 100] },
                            0
                        ]
                    }
                }
            },
            {
                $addFields: {
                    finalScore: {
                        $add: [
                            '$priority',
                            { $multiply: ['$clicks', 0.3] },
                            { $multiply: ['$leads', 0.7] }
                        ]
                    }
                }
            },
            {
                $sort: {
                    listingType: -1, // promoted first
                    finalScore: -1
                }
            }
        ]);
        res.json({
            success: true,
            data: listings
        });
    }
    catch (error) {
        console.error("❌ Business listings error:", error);
        res.status(500).json({
            success: false,
            message: "Server error fetching listings"
        });
    }
});
// get business details and increment impressions
router.get('/:id', async (req, res) => {
    try {
        const now = new Date();
        const business = await BusinessPromotion_1.default.findOne({
            _id: req.params.id,
            isActive: true,
            status: 'active',
            $or: [
                { expiryDate: null },
                { expiryDate: { $gt: now } }
            ]
        }).lean();
        if (!business) {
            return res.status(404).json({
                success: false,
                message: 'Business not found or expired'
            });
        }
        await BusinessPromotion_1.default.findByIdAndUpdate(req.params.id, { $inc: { 'visibility.impressions': 1 } });
        res.json({
            success: true,
            data: business
        });
    }
    catch (error) {
        console.error("❌ Detail error:", error);
        res.status(500).json({
            success: false,
            message: 'Server error'
        });
    }
});
// lead tracking endpoint
router.post('/:id/lead', async (req, res) => {
    try {
        await BusinessPromotion_1.default.findByIdAndUpdate(req.params.id, { $inc: { 'visibility.leads': 1 } });
        res.json({ success: true });
    }
    catch (err) {
        res.status(500).json({ success: false });
    }
});
// click tracking endpoint
router.post('/:id/impression', async (req, res) => {
    await BusinessPromotion_1.default.findByIdAndUpdate(req.params.id, { $inc: { 'visibility.impressions': 1 } });
    res.json({ success: true });
});
// click tracking endpoint
router.post('/:id/click', async (req, res) => {
    await BusinessPromotion_1.default.findByIdAndUpdate(req.params.id, { $inc: { 'visibility.clicks': 1 } });
    res.json({ success: true });
});
exports.default = router;
