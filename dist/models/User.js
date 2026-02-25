"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
const UserSchema = new mongoose_1.Schema({
    name: { type: String, required: true },
    phone: {
        type: String,
        required: true,
        unique: true,
        index: true,
        validate: {
            validator: function (v) {
                return /^\+?[\d\s\-\(\)]{10,15}$/.test(v);
            },
            message: "Phone number must be between 10-15 digits",
        },
    },
    password: { type: String, required: false, select: false }, // Optional - not currently used
    // Email is completely optional - no constraints at all
    email: { type: String },
    profilePicture: { type: String, default: "" },
    about: { type: String, default: "Available" },
    gender: { type: String }, // male, female, other
    birthdate: { type: Date },
    anniversary: { type: Date },
    pushToken: { type: String },
    platform: { type: String },
    pushTokenUpdatedAt: { type: Date },
    // Credits system - 5 lac (500,000) credits on signup, valid for 1 month
    credits: { type: Number, default: 200 },
    creditsExpiryDate: { type: Date }, // Credits expire 1 month after signup
    referralCode: { type: String, unique: true, sparse: true, index: true },
    referredBy: { type: mongoose_1.Schema.Types.ObjectId, ref: "User" },
    // Service type - Selected during first-time account setup
    serviceType: {
        type: String,
        enum: ["home-based", "business-visiting"],
        default: null,
    },
    // Quiz progress tracking - supports all 30 questions
    quizProgress: {
        completed: { type: Boolean, default: false },
        currentQuestionIndex: { type: Number, default: 0 },
        answeredQuestions: {
            type: [String],
            default: [],
            validate: {
                validator: function (v) {
                    return v.length <= 30; // Maximum 30 questions
                },
                message: "Cannot have more than 30 answered questions",
            },
        }, // Array of question keys (e.g., ['married', 'haveBike'])
        answers: {
            type: Map,
            of: String,
            default: {},
        }, // Map of questionKey -> answer (stores all 30 Q&A pairs)
        creditsEarned: { type: Number, default: 0 },
        creditsRecordedInTransactions: { type: Number, default: 0 }, // Credits already saved in transactions
        startedAt: { type: Date },
        completedAt: { type: Date },
    },
    parentId: { type: mongoose_1.Schema.Types.ObjectId, ref: "User", index: true },
    level: { type: Number, default: 0, min: 0 },
    directCount: { type: Number, default: 0, min: 0 },
    downlineCount: { type: Number, default: 0, min: 0 }, // Total descendants (all levels)
}, { timestamps: true });
exports.default = mongoose_1.models.User || (0, mongoose_1.model)("User", UserSchema);
