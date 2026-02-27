"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getCreditDashboard = getCreditDashboard;
const MlmCredit_1 = __importDefault(require("../../models/MlmCredit"));
async function getCreditDashboard(userId) {
    const [received, transferred, activeCredits, recentTransfers] = await Promise.all([
        MlmCredit_1.default.countDocuments({ receiverId: userId }),
        MlmCredit_1.default.countDocuments({ senderId: userId }),
        MlmCredit_1.default.find({
            receiverId: userId,
            status: { $in: ["pending", "active"] },
        })
            .sort({ createdAt: -1 })
            .limit(10)
            .lean(),
        MlmCredit_1.default.find({ senderId: userId })
            .sort({ createdAt: -1 })
            .limit(10)
            .populate("receiverId", "name")
            .lean(),
    ]);
    const timers = activeCredits.map((credit) => ({
        creditId: credit._id.toString(),
        status: credit.status,
        paymentStatus: credit.paymentStatus,
        expiresAt: credit.expiresAt,
        transferExpiresAt: credit.transferExpiresAt,
        remainingTransfers: Math.max(0, (credit.quantity || 0) - (credit.transferredCount || 0)),
    }));
    return {
        totalCreditsReceived: received,
        totalCreditsTransferred: transferred,
        activeCredits: activeCredits.filter((credit) => credit.status === "active")
            .length,
        timers,
        recentTransfers: recentTransfers.map((credit) => {
            let status = "completed";
            if (credit.status === "pending")
                status = "pending";
            if (credit.status === "expired" || credit.status === "reverted") {
                status = "returned";
            }
            return {
                id: credit._id.toString(),
                recipientName: credit.receiverId?.name || "User",
                recipientId: credit.receiverId?._id?.toString() || "",
                amount: 1,
                date: credit.createdAt,
                status,
            };
        }),
    };
}
