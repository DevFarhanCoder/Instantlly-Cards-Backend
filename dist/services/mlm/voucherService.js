"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.generateVouchers = generateVouchers;
const uuid_1 = require("uuid");
const Voucher_1 = __importDefault(require("../../models/Voucher"));
const DEFAULT_VOUCHER_MRP = 1200;
const VOUCHER_EXPIRY_DAYS = 365;
async function generateVouchers(userId, creditId, quantity) {
    const now = new Date();
    const expiry = new Date(now);
    expiry.setDate(expiry.getDate() + VOUCHER_EXPIRY_DAYS);
    const vouchers = Array.from({ length: quantity }).map(() => ({
        userId,
        originalOwner: userId,
        creditId,
        voucherNumber: (0, uuid_1.v4)().replace(/-/g, "").slice(0, 12).toUpperCase(),
        MRP: DEFAULT_VOUCHER_MRP,
        issueDate: now,
        expiryDate: expiry,
        source: "purchase",
    }));
    return Voucher_1.default.insertMany(vouchers);
}
