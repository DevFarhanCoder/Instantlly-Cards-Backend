"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
const TransactionSchema = new mongoose_1.Schema({
    type: {
        type: String,
        required: true,
        enum: ['signup_bonus', 'referral_bonus', 'transfer', 'transfer_sent', 'transfer_received', 'ad_deduction', 'admin_adjustment', 'quiz_bonus', 'self_download_bonus']
    },
    transactionId: { type: String, unique: true, sparse: true },
    fromUser: { type: mongoose_1.Schema.Types.ObjectId, ref: 'User' },
    toUser: { type: mongoose_1.Schema.Types.ObjectId, ref: 'User' },
    amount: { type: Number, required: true },
    description: { type: String, required: true },
    note: { type: String }, // Optional admin reason/note
    balanceBefore: { type: Number },
    balanceAfter: { type: Number },
    relatedAd: { type: mongoose_1.Schema.Types.ObjectId, ref: 'Ad' },
    status: {
        type: String,
        default: 'completed',
        enum: ['pending', 'completed', 'failed', 'cancelled']
    }
}, { timestamps: true });
// Index for faster queries
TransactionSchema.index({ fromUser: 1, createdAt: -1 });
TransactionSchema.index({ toUser: 1, createdAt: -1 });
TransactionSchema.index({ type: 1 });
exports.default = mongoose_1.models.Transaction || (0, mongoose_1.model)("Transaction", TransactionSchema);
