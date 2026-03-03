"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const adminAuth_1 = require("../middleware/adminAuth");
const ReviewSuggestions_1 = __importDefault(require("../models/ReviewSuggestions"));
const router = (0, express_1.Router)();
// ============================================================================
// REVIEW SUGGESTIONS ENDPOINTS
// ============================================================================
/**
 * GET /api/suggestions/:rating
 * Fetch dynamic suggestions for specific star rating
 */
router.get("/:rating", async (req, res) => {
    try {
        const { rating } = req.params;
        const ratingNum = parseInt(rating);
        if (isNaN(ratingNum) || ratingNum < 1 || ratingNum > 5) {
            return res.status(400).json({
                success: false,
                error: "INVALID_RATING",
                message: "Rating must be between 1 and 5"
            });
        }
        const suggestions = await ReviewSuggestions_1.default.findOne({ rating: ratingNum });
        if (!suggestions) {
            return res.status(404).json({
                success: false,
                error: "SUGGESTIONS_NOT_FOUND",
                message: `No suggestions found for rating ${ratingNum}`
            });
        }
        res.status(200).json({
            success: true,
            rating: suggestions.rating,
            label: suggestions.label,
            emoji: suggestions.emoji,
            prompt: suggestions.prompt,
            suggestions: suggestions.suggestions
        });
    }
    catch (error) {
        console.error("Error fetching suggestions:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to fetch suggestions"
        });
    }
});
/**
 * GET /api/admin/suggestions
 * Admin: Get all suggestions configurations
 */
router.get("/admin/all", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const allSuggestions = await ReviewSuggestions_1.default.find({}).sort({ rating: 1 });
        const suggestionsMap = {};
        allSuggestions.forEach(s => {
            suggestionsMap[s.rating] = {
                rating: s.rating,
                label: s.label,
                emoji: s.emoji,
                prompt: s.prompt,
                suggestions: s.suggestions,
                category: s.category
            };
        });
        res.status(200).json({
            success: true,
            suggestions: suggestionsMap
        });
    }
    catch (error) {
        console.error("Error fetching all suggestions:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to fetch suggestions"
        });
    }
});
/**
 * PUT /api/admin/suggestions/:rating
 * Admin: Update suggestions for specific rating
 */
router.put("/admin/update/:rating", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const { rating } = req.params;
        const { suggestions, label, emoji, prompt, category } = req.body;
        const ratingNum = parseInt(rating);
        if (isNaN(ratingNum) || ratingNum < 1 || ratingNum > 5) {
            return res.status(400).json({
                success: false,
                error: "INVALID_RATING",
                message: "Rating must be between 1 and 5"
            });
        }
        if (!Array.isArray(suggestions)) {
            return res.status(400).json({
                success: false,
                error: "INVALID_SUGGESTIONS",
                message: "Suggestions must be an array"
            });
        }
        let suggestionDoc = await ReviewSuggestions_1.default.findOne({ rating: ratingNum });
        if (!suggestionDoc) {
            suggestionDoc = new ReviewSuggestions_1.default({
                rating: ratingNum,
                suggestions: suggestions || [],
                label: label || "",
                emoji: emoji || "",
                prompt: prompt || "",
                category: category || null
            });
        }
        else {
            if (suggestions)
                suggestionDoc.suggestions = suggestions;
            if (label)
                suggestionDoc.label = label;
            if (emoji)
                suggestionDoc.emoji = emoji;
            if (prompt)
                suggestionDoc.prompt = prompt;
            if (category)
                suggestionDoc.category = category;
        }
        await suggestionDoc.save();
        res.status(200).json({
            success: true,
            message: "Suggestions updated",
            updatedAt: suggestionDoc.updatedAt
        });
    }
    catch (error) {
        console.error("Error updating suggestions:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to update suggestions"
        });
    }
});
/**
 * POST /api/admin/suggestions/seed
 * Admin: Seed default suggestions data
 */
router.post("/admin/seed", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const defaultSuggestions = [
            {
                rating: 1,
                label: "Terrible",
                emoji: "😠",
                prompt: "What went wrong?",
                suggestions: [
                    { text: "Poor service", emoji: "👎", weight: 0.95 },
                    { text: "Rude staff", emoji: "😠", weight: 0.90 },
                    { text: "Overpriced", emoji: "💸", weight: 0.85 },
                    { text: "Unhygienic", emoji: "🤢", weight: 0.85 },
                    { text: "Waited too long", emoji: "⏰", weight: 0.80 }
                ]
            },
            {
                rating: 2,
                label: "Bad",
                emoji: "😞",
                prompt: "What could be improved?",
                suggestions: [
                    { text: "Poor quality", emoji: "⚠️", weight: 0.90 },
                    { text: "Disappointing", emoji: "😞", weight: 0.85 },
                    { text: "Not worth the money", emoji: "💰", weight: 0.80 },
                    { text: "Staff not helpful", emoji: "😒", weight: 0.75 },
                    { text: "Missing items", emoji: "❌", weight: 0.75 }
                ]
            },
            {
                rating: 3,
                label: "Average",
                emoji: "😐",
                prompt: "What could be better?",
                suggestions: [
                    { text: "Average quality", emoji: "👌", weight: 0.85 },
                    { text: "Could be faster", emoji: "⚡", weight: 0.80 },
                    { text: "Pricey", emoji: "💵", weight: 0.75 },
                    { text: "Just okay", emoji: "😐", weight: 0.70 },
                    { text: "Decent service", emoji: "👍", weight: 0.70 }
                ]
            },
            {
                rating: 4,
                label: "Good",
                emoji: "😊",
                prompt: "What did you like?",
                suggestions: [
                    { text: "Good service", emoji: "👍", weight: 0.95 },
                    { text: "Quality products", emoji: "⭐", weight: 0.90 },
                    { text: "Friendly staff", emoji: "😊", weight: 0.85 },
                    { text: "Value for money", emoji: "💰", weight: 0.80 },
                    { text: "Clean premises", emoji: "✨", weight: 0.80 }
                ]
            },
            {
                rating: 5,
                label: "Excellent",
                emoji: "🤩",
                prompt: "What did you love?",
                suggestions: [
                    { text: "Exceptional service", emoji: "⭐", weight: 0.95 },
                    { text: "Amazing quality", emoji: "🌟", weight: 0.95 },
                    { text: "Highly professional", emoji: "💼", weight: 0.90 },
                    { text: "Best in class", emoji: "🏆", weight: 0.90 },
                    { text: "Highly recommended", emoji: "👍", weight: 0.85 }
                ]
            }
        ];
        // Delete existing suggestions first
        await ReviewSuggestions_1.default.deleteMany({});
        // Insert default suggestions
        const inserted = await ReviewSuggestions_1.default.insertMany(defaultSuggestions);
        res.status(201).json({
            success: true,
            message: "Default suggestions seeded successfully",
            count: inserted.length
        });
    }
    catch (error) {
        console.error("Error seeding suggestions:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to seed suggestions"
        });
    }
});
exports.default = router;
