"use strict";
// ✅ DISCOUNT SERVICE - Calculates level-based discounts (NOT real commission)
// Business Model: Discount on purchase price based on downline count
// "Commission" = Virtual savings shown in UI (NOT stored in wallet, NOT withdrawable)
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getUserDiscountInfo = getUserDiscountInfo;
exports.calculatePurchaseDiscount = calculatePurchaseDiscount;
exports.getDiscountSummary = getDiscountSummary;
const User_1 = __importDefault(require("../../models/User"));
const mlm_1 = require("../../utils/mlm");
/**
 * Get user's current discount level and pricing info
 * @param userId - User ID to calculate discount for
 * @returns Discount info including level, discount %, payable amount, and virtual commission
 */
async function getUserDiscountInfo(userId) {
    const user = await User_1.default.findById(userId).lean();
    if (!user) {
        throw new Error("User not found");
    }
    const downlineCount = user.downlineCount || 0;
    const unlockedLevel = (0, mlm_1.getUnlockedLevel)(downlineCount);
    const discountInfo = (0, mlm_1.getDiscountInfo)(unlockedLevel);
    const virtualCommission = (0, mlm_1.calculateVirtualCommission)(discountInfo.payableAmount);
    // Get next level target
    const nextTarget = (0, mlm_1.getNextLevelTarget)(unlockedLevel);
    const remainingDownline = nextTarget
        ? nextTarget.requiredDownline - downlineCount
        : 0;
    return {
        userId: userId.toString(),
        currentLevel: unlockedLevel,
        downlineCount,
        discountPercent: discountInfo.discountPercent,
        payableAmount: discountInfo.payableAmount,
        mrp: mlm_1.MLM_BASE_MRP,
        baseSavings: mlm_1.MLM_BASE_MRP - mlm_1.MLM_BASE_PAYABLE, // ₹2400 (60-36 = 24 hundred)
        virtualCommission, // Amount saved compared to Level 1
        nextLevel: nextTarget?.nextLevel || null,
        nextLevelRequiredDownline: nextTarget?.requiredDownline || null,
        remainingDownlineForNextLevel: Math.max(0, remainingDownline),
    };
}
/**
 * Calculate discount for a purchase (used during payment confirmation)
 * @param userId - User making the purchase
 * @returns Purchase pricing breakdown
 */
async function calculatePurchaseDiscount(userId) {
    const discountInfo = await getUserDiscountInfo(userId);
    return {
        mrp: discountInfo.mrp,
        discountPercent: discountInfo.discountPercent,
        discountAmount: discountInfo.mrp - discountInfo.payableAmount,
        payableAmount: discountInfo.payableAmount,
        level: discountInfo.currentLevel,
        virtualCommissionEarned: discountInfo.virtualCommission,
    };
}
/**
 * Get discount summary for dashboard display
 * @param userId - User ID
 * @returns Formatted discount summary for UI
 */
async function getDiscountSummary(userId) {
    try {
        const info = await getUserDiscountInfo(userId);
        return {
            currentLevel: info.currentLevel || 1,
            discountPercent: info.discountPercent || 40,
            payableAmount: info.payableAmount || 3600,
            savings: info.mrp - info.payableAmount,
            virtualCommission: info.virtualCommission || 0,
            downlineCount: info.downlineCount || 0,
            nextLevelTarget: info.nextLevel && info.remainingDownlineForNextLevel
                ? {
                    level: info.nextLevel,
                    remainingDownline: info.remainingDownlineForNextLevel,
                    targetDiscountPercent: (0, mlm_1.getDiscountInfo)(info.nextLevel)
                        .discountPercent,
                }
                : undefined,
            disclaimer: "This amount represents savings unlocked via discounts and is not withdrawable.",
        };
    }
    catch (error) {
        console.error("Error getting discount summary:", error);
        // Return safe defaults
        return {
            currentLevel: 1,
            discountPercent: 40,
            payableAmount: 3600,
            savings: 2400,
            virtualCommission: 0,
            downlineCount: 0,
            disclaimer: "This amount represents savings unlocked via discounts and is not withdrawable.",
        };
    }
}
