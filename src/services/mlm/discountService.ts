// ✅ DISCOUNT SERVICE - Calculates level-based discounts (NOT real commission)
// Business Model: Discount on purchase price based on downline count
// "Commission" = Virtual savings shown in UI (NOT stored in wallet, NOT withdrawable)

import User from "../../models/User";
import {
  getUnlockedLevel,
  getDiscountInfo,
  calculateVirtualCommission,
  getNextLevelTarget,
  MLM_BASE_MRP,
  MLM_BASE_PAYABLE,
} from "../../utils/mlm";

/**
 * Get user's current discount level and pricing info
 * @param userId - User ID to calculate discount for
 * @returns Discount info including level, discount %, payable amount, and virtual commission
 */
export async function getUserDiscountInfo(userId: string) {
  const user = await User.findById(userId).lean();
  if (!user) {
    throw new Error("User not found");
  }

  const downlineCount = (user as any).downlineCount || 0;
  const unlockedLevel = getUnlockedLevel(downlineCount);
  const discountInfo = getDiscountInfo(unlockedLevel);
  const virtualCommission = calculateVirtualCommission(
    discountInfo.payableAmount,
  );

  // Get next level target
  const nextTarget = getNextLevelTarget(unlockedLevel);
  const remainingDownline = nextTarget
    ? nextTarget.requiredDownline - downlineCount
    : 0;

  return {
    userId: userId.toString(),
    currentLevel: unlockedLevel,
    downlineCount,
    discountPercent: discountInfo.discountPercent,
    payableAmount: discountInfo.payableAmount,
    mrp: MLM_BASE_MRP,

    baseSavings: MLM_BASE_MRP - MLM_BASE_PAYABLE, // ₹2400 (60-36 = 24 hundred)
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
export async function calculatePurchaseDiscount(userId: string) {
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
export async function getDiscountSummary(userId: string) {
  try {
    const info = await getUserDiscountInfo(userId);

    return {
      currentLevel: info.currentLevel || 1,
      discountPercent: info.discountPercent || 40,
      payableAmount: info.payableAmount || 3600,
      savings: info.mrp - info.payableAmount,
      virtualCommission: info.virtualCommission || 0,
      downlineCount: info.downlineCount || 0,
      nextLevelTarget:
        info.nextLevel && info.remainingDownlineForNextLevel
          ? {
              level: info.nextLevel,
              remainingDownline: info.remainingDownlineForNextLevel,
              targetDiscountPercent: getDiscountInfo(info.nextLevel)
                .discountPercent,
            }
          : undefined,
      disclaimer:
        "This amount represents savings unlocked via discounts and is not withdrawable.",
    };
  } catch (error) {
    console.error("Error getting discount summary:", error);
    // Return safe defaults
    return {
      currentLevel: 1,
      discountPercent: 40,
      payableAmount: 3600,
      savings: 2400,
      virtualCommission: 0,
      downlineCount: 0,
      disclaimer:
        "This amount represents savings unlocked via discounts and is not withdrawable.",
    };
  }
}
