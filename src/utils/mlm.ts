// ✅ NEW BUSINESS MODEL: DISCOUNT-BASED MLM (NOT COMMISSION)
// Product MRP: ₹6000 (5 vouchers × ₹1200 each)
// Reward: Level-based discount on purchase price (NOT cash payout)
// "Commission" in UI = Virtual savings amount (NOT withdrawable)

export const MLM_BASE_MRP = 6000;
export const MLM_BASE_PAYABLE = 3600; // Level 1 payment (40% discount baseline)
export const MLM_MAX_LEVEL = 10;

// 🎯 DISCOUNT LEVEL TABLE (FROM BUSINESS SHEET - DO NOT CHANGE)
export const DISCOUNT_LEVELS: Array<{
  level: number;
  requiredDownline: number;
  discountPercent: number;
  payableAmount: number;
}> = [
  { level: 1, requiredDownline: 1, discountPercent: 40.0, payableAmount: 3600 },
  { level: 2, requiredDownline: 5, discountPercent: 55.0, payableAmount: 2700 },
  {
    level: 3,
    requiredDownline: 25,
    discountPercent: 62.5,
    payableAmount: 2250,
  },
  {
    level: 4,
    requiredDownline: 125,
    discountPercent: 66.26,
    payableAmount: 2024.4,
  },
  {
    level: 5,
    requiredDownline: 625,
    discountPercent: 68.16,
    payableAmount: 1910.4,
  },
  {
    level: 6,
    requiredDownline: 3125,
    discountPercent: 69.11,
    payableAmount: 1853.4,
  },
  {
    level: 7,
    requiredDownline: 15625,
    discountPercent: 69.58,
    payableAmount: 1825.2,
  },
  {
    level: 8,
    requiredDownline: 78125,
    discountPercent: 69.82,
    payableAmount: 1810.8,
  },
  {
    level: 9,
    requiredDownline: 390625,
    discountPercent: 69.94,
    payableAmount: 1803.6,
  },
  {
    level: 10,
    requiredDownline: 1953125,
    discountPercent: 70.0,
    payableAmount: 1800,
  },
];

// 🧠 CALCULATE USER'S UNLOCKED LEVEL (based on downline count)
export function getUnlockedLevel(downlineCount: number): number {
  let unlockedLevel = 1;
  for (const entry of DISCOUNT_LEVELS) {
    if (downlineCount >= entry.requiredDownline) {
      unlockedLevel = entry.level;
    } else {
      break;
    }
  }
  return unlockedLevel;
}

// 💰 GET DISCOUNT INFO FOR A GIVEN LEVEL
export function getDiscountInfo(level: number): {
  level: number;
  requiredDownline: number;
  discountPercent: number;
  payableAmount: number;
} {
  const match = DISCOUNT_LEVELS.find((entry) => entry.level === level);
  if (!match) {
    return DISCOUNT_LEVELS[0]; // Default to Level 1
  }
  return match;
}

// 🎁 CALCULATE "COMMISSION" (VIRTUAL SAVINGS - NOT WITHDRAWABLE)
// Formula: commissionEarned = BASE_PAYABLE - payableAmount
// This is ONLY for UI display, NOT stored in wallet
export function calculateVirtualCommission(payableAmount: number): number {
  return Math.max(0, MLM_BASE_PAYABLE - payableAmount);
}

// 🔢 GET NEXT LEVEL TARGET
export function getNextLevelTarget(
  currentLevel: number,
): { nextLevel: number; requiredDownline: number } | null {
  if (currentLevel >= MLM_MAX_LEVEL) return null;
  const nextLevel = DISCOUNT_LEVELS.find(
    (entry) => entry.level === currentLevel + 1,
  );
  return nextLevel
    ? {
        nextLevel: nextLevel.level,
        requiredDownline: nextLevel.requiredDownline,
      }
    : null;
}

// 📊 STRUCTURAL CREDIT POOL (for display only - unchanged)
export function getStructuralCreditPool(level: number): number {
  const normalizedLevel = Math.min(Math.max(level || 1, 1), MLM_MAX_LEVEL);
  const exponent = MLM_MAX_LEVEL - normalizedLevel;
  return MLM_BASE_MRP * Math.pow(5, exponent);
}
