"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
const FeedbackSchema = new mongoose_1.Schema({
    userId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "User",
        required: true,
        index: true
    },
    name: { type: String, required: true },
    phone: { type: String, required: true },
    email: { type: String },
    subject: { type: String, required: true },
    message: { type: String, required: true },
    rating: {
        type: Number,
        min: 1,
        max: 5,
        default: null
    },
    status: {
        type: String,
        enum: ["pending", "in-progress", "resolved", "closed"],
        default: "pending"
    },
    adminResponse: { type: String },
    respondedAt: { type: Date },
}, { timestamps: true });
exports.default = mongoose_1.models.Feedback || (0, mongoose_1.model)("Feedback", FeedbackSchema);
