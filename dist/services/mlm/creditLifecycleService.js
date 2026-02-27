"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.assignCreditUsage = assignCreditUsage;
exports.ensureWallet = ensureWallet;
const MlmCredit_1 = __importDefault(require("../../models/MlmCredit"));
const walletService_1 = require("./walletService");
async function assignCreditUsage(senderId) {
    const now = new Date();
    const activeCredit = await MlmCredit_1.default.findOne({
        receiverId: senderId,
        status: "active",
        paymentStatus: "paid",
        transferExpiresAt: { $gt: now },
        $expr: { $lt: ["$transferredCount", "$quantity"] },
    }).sort({ activatedAt: 1 });
    if (!activeCredit) {
        return null;
    }
    activeCredit.transferredCount += 1;
    await activeCredit.save();
    await (0, walletService_1.addCredits)(senderId, -1);
    return activeCredit;
}
async function ensureWallet(userId) {
    return (0, walletService_1.getOrCreateWallet)(userId);
}
