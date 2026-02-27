"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.startMlmScheduler = startMlmScheduler;
const node_cron_1 = __importDefault(require("node-cron"));
const MlmCredit_1 = __importDefault(require("../../models/MlmCredit"));
const walletService_1 = require("./walletService");
async function expirePendingCredits() {
    const now = new Date();
    const pendingCredits = await MlmCredit_1.default.find({
        status: "pending",
        paymentStatus: "pending",
        expiresAt: { $lt: now },
    }).limit(200);
    for (const credit of pendingCredits) {
        credit.status = "expired";
        credit.paymentStatus = "failed";
        await credit.save();
        await (0, walletService_1.addCredits)(credit.senderId.toString(), 1);
    }
}
async function revertUnusedCredits() {
    const now = new Date();
    const activeCredits = await MlmCredit_1.default.find({
        status: "active",
        paymentStatus: "paid",
        transferExpiresAt: { $lt: now },
        $expr: { $lt: ["$transferredCount", "$quantity"] },
    }).limit(200);
    for (const credit of activeCredits) {
        const remaining = Math.max(0, credit.quantity - credit.transferredCount);
        if (remaining <= 0)
            continue;
        credit.status = "reverted";
        credit.revertedCount = remaining;
        await credit.save();
        await (0, walletService_1.addCredits)(credit.receiverId.toString(), -remaining);
        await (0, walletService_1.addCredits)(credit.senderId.toString(), remaining);
    }
}
function startMlmScheduler() {
    node_cron_1.default.schedule("*/5 * * * *", async () => {
        await expirePendingCredits();
        await revertUnusedCredits();
    });
}
