"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const Feedback_1 = __importDefault(require("../models/Feedback"));
const User_1 = __importDefault(require("../models/User"));
const auth_1 = require("../middleware/auth");
const router = (0, express_1.Router)();
// Submit feedback
router.post("/", auth_1.requireAuth, async (req, res) => {
    try {
        const { subject, message, rating } = req.body;
        const userId = req.userId;
        console.log('📝 Feedback submission received:', { userId, subject, rating });
        // Validate required fields
        if (!subject || !message) {
            return res.status(400).json({
                success: false,
                message: "Subject and message are required"
            });
        }
        // Get user details
        const user = await User_1.default.findById(userId);
        if (!user) {
            console.log('❌ User not found:', userId);
            return res.status(404).json({
                success: false,
                message: "User not found"
            });
        }
        console.log('👤 User found:', { name: user.name, phone: user.phone });
        // Create feedback
        const feedback = new Feedback_1.default({
            userId,
            name: user.name,
            phone: user.phone,
            email: user.email || "",
            subject,
            message,
            rating: rating || null,
            status: "pending"
        });
        console.log('💾 Saving feedback to database...');
        await feedback.save();
        console.log('✅ Feedback saved successfully:', feedback._id);
        res.status(201).json({
            success: true,
            message: "Feedback submitted successfully",
            data: feedback
        });
    }
    catch (error) {
        console.error("❌ Error submitting feedback:", error);
        res.status(500).json({
            success: false,
            message: "Failed to submit feedback"
        });
    }
});
// Get user's feedback history
router.get("/my-feedback", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const feedbacks = await Feedback_1.default.find({ userId })
            .sort({ createdAt: -1 })
            .select("-__v");
        res.json({
            success: true,
            data: feedbacks
        });
    }
    catch (error) {
        console.error("Error fetching feedback:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch feedback"
        });
    }
});
// Get all feedback (Admin only - you can add admin auth middleware later)
router.get("/all", auth_1.requireAuth, async (req, res) => {
    try {
        const { status, page = 1, limit = 20 } = req.query;
        console.log('📋 Fetching all feedback - Status filter:', status);
        const query = {};
        if (status) {
            query.status = status;
        }
        const skip = (Number(page) - 1) * Number(limit);
        const feedbacks = await Feedback_1.default.find(query)
            .sort({ createdAt: -1 })
            .skip(skip)
            .limit(Number(limit))
            .populate("userId", "name phone email")
            .select("-__v");
        const total = await Feedback_1.default.countDocuments(query);
        console.log(`✅ Found ${feedbacks.length} feedbacks (Total: ${total})`);
        res.json({
            success: true,
            data: feedbacks,
            pagination: {
                total,
                page: Number(page),
                limit: Number(limit),
                pages: Math.ceil(total / Number(limit))
            }
        });
    }
    catch (error) {
        console.error("❌ Error fetching all feedback:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch feedback"
        });
    }
});
// Update feedback status (Admin only)
router.patch("/:id/status", auth_1.requireAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { status, adminResponse } = req.body;
        const validStatuses = ["pending", "in-progress", "resolved", "closed"];
        if (status && !validStatuses.includes(status)) {
            return res.status(400).json({
                success: false,
                message: "Invalid status"
            });
        }
        const updateData = {};
        if (status)
            updateData.status = status;
        if (adminResponse) {
            updateData.adminResponse = adminResponse;
            updateData.respondedAt = new Date();
        }
        const feedback = await Feedback_1.default.findByIdAndUpdate(id, updateData, { new: true });
        if (!feedback) {
            return res.status(404).json({
                success: false,
                message: "Feedback not found"
            });
        }
        res.json({
            success: true,
            message: "Feedback updated successfully",
            data: feedback
        });
    }
    catch (error) {
        console.error("Error updating feedback:", error);
        res.status(500).json({
            success: false,
            message: "Failed to update feedback"
        });
    }
});
// Delete feedback
router.delete("/:id", auth_1.requireAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const userId = req.userId;
        const feedback = await Feedback_1.default.findOne({ _id: id, userId });
        if (!feedback) {
            return res.status(404).json({
                success: false,
                message: "Feedback not found or unauthorized"
            });
        }
        await Feedback_1.default.findByIdAndDelete(id);
        res.json({
            success: true,
            message: "Feedback deleted successfully"
        });
    }
    catch (error) {
        console.error("Error deleting feedback:", error);
        res.status(500).json({
            success: false,
            message: "Failed to delete feedback"
        });
    }
});
exports.default = router;
