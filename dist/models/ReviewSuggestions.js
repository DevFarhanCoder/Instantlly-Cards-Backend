"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
const ReviewSuggestionsSchema = new mongoose_1.Schema({
    rating: {
        type: Number,
        required: true,
        min: 1,
        max: 5,
        unique: true,
        index: true
    },
    category: String, // optional - for category-specific suggestions
    suggestions: [
        {
            text: String,
            emoji: String,
            weight: Number // 0-1 for ML ranking
        }
    ],
    prompt: String, // "What went wrong?", "What did you like?", etc.
    emoji: String, // 😠, 😞, 😐, 😊, 🤩
    label: String, // Terrible, Bad, Average, Good, Excellent
    createdAt: { type: Date, default: Date.now },
    updatedAt: { type: Date, default: Date.now }
}, { timestamps: true });
exports.default = mongoose_1.models.ReviewSuggestions || (0, mongoose_1.model)("ReviewSuggestions", ReviewSuggestionsSchema);
