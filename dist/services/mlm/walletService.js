"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getOrCreateWallet = getOrCreateWallet;
exports.addCredits = addCredits;
exports.subtractCredits = subtractCredits;
const MlmWallet_1 = __importDefault(require("../../models/MlmWallet"));
// ✅ SIMPLIFIED WALLET SERVICE - Only handles credit balance
// No commission/withdrawal logic (discount-based MLM model)
async function getOrCreateWallet(userId) {
    let wallet = await MlmWallet_1.default.findOne({ userId });
    if (!wallet) {
        wallet = await MlmWallet_1.default.create({ userId });
    }
    return wallet;
}
async function addCredits(userId, amount) {
    return MlmWallet_1.default.findOneAndUpdate({ userId }, { $inc: { creditBalance: amount } }, { new: true, upsert: true });
}
async function subtractCredits(userId, amount) {
    const wallet = await MlmWallet_1.default.findOne({
        userId,
        creditBalance: { $gte: amount },
    });
    if (!wallet) {
        throw new Error("Insufficient credit balance");
    }
    return MlmWallet_1.default.findOneAndUpdate({ userId, creditBalance: { $gte: amount } }, { $inc: { creditBalance: -amount } }, { new: true });
}
