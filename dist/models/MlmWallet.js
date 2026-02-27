"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
// ✅ NEW MODEL: Only tracks credit balance (no real commission/withdrawals)
// Discount-based MLM: Savings are virtual, not stored here
const MlmWalletSchema = new mongoose_1.Schema({
    userId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "User",
        required: true,
        unique: true,
        index: true,
    },
    creditBalance: { type: Number, default: 0, min: 0 },
}, { timestamps: true });
exports.default = mongoose_1.models.MlmWallet || (0, mongoose_1.model)("MlmWallet", MlmWalletSchema);
