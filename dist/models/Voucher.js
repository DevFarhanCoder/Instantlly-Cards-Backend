"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
const VoucherSchema = new mongoose_1.Schema({
    userId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "User",
        index: true,
    },
    originalOwner: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "User",
        index: true,
    },
    creditId: { type: mongoose_1.Schema.Types.ObjectId, ref: "MlmCredit" },
    voucherNumber: { type: String, required: true, unique: true, index: true },
    MRP: { type: Number, required: true, default: 1200 },
    issueDate: { type: Date, default: Date.now },
    expiryDate: { type: Date, required: true, index: true },
    redeemedStatus: {
        type: String,
        enum: ["unredeemed", "redeemed", "expired"],
        default: "unredeemed",
    },
    voucherImages: { type: [String], default: [] },
    productVideoLink: { type: String },
    redeemedAt: { type: Date },
    // Admin-created voucher template fields
    companyLogo: { type: String }, // URL to logo
    companyName: { type: String, default: "Instantlly" },
    phoneNumber: { type: String },
    address: { type: String },
    amount: { type: Number }, // Display amount
    discountPercentage: { type: Number, default: 0 }, // e.g., 40 for 40% off
    validity: { type: String }, // e.g., "Valid till August 30th, 2026"
    voucherImage: { type: String }, // Main voucher detail image from admin
    description: { type: String },
    // Publishing status
    isPublished: { type: Boolean, default: false },
    publishedAt: { type: Date },
    createdByAdmin: { type: mongoose_1.Schema.Types.ObjectId, ref: "Admin" },
    source: {
        type: String,
        enum: ["purchase", "transfer", "admin"],
        default: "purchase",
    },
    transferredFrom: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "User",
    },
    transferredAt: { type: Date },
    transferHistory: [
        {
            from: { type: mongoose_1.Schema.Types.ObjectId, ref: "User" },
            to: { type: mongoose_1.Schema.Types.ObjectId, ref: "User" },
            transferredAt: { type: Date, default: Date.now },
        },
    ],
}, { timestamps: true });
VoucherSchema.index({ userId: 1, redeemedStatus: 1 });
exports.default = mongoose_1.models.Voucher || (0, mongoose_1.model)("Voucher", VoucherSchema);
