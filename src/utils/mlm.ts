export const MLM_BASE_ENTRY_VALUE = 6000;
export const MLM_MAX_LEVEL = 10;

export const MLM_LEVEL_PERCENTAGES: Array<{
  level: number;
  percentage: number;
}> = [
  { level: 1, percentage: 0.15 },
  { level: 2, percentage: 0.075 },
  { level: 3, percentage: 0.0376 },
  { level: 4, percentage: 0.019 },
  { level: 5, percentage: 0.0095 },
  { level: 6, percentage: 0.0047 },
  { level: 7, percentage: 0.0024 },
  { level: 8, percentage: 0.0012 },
  { level: 9, percentage: 0.0006 },
  { level: 10, percentage: 0 },
];

export function getStructuralCreditPool(level: number): number {
  const normalizedLevel = Math.min(Math.max(level || 1, 1), MLM_MAX_LEVEL);
  const exponent = MLM_MAX_LEVEL - normalizedLevel;
  return MLM_BASE_ENTRY_VALUE * Math.pow(5, exponent);
}

export function getCommissionAmount(baseMrp: number, level: number): number {
  const match = MLM_LEVEL_PERCENTAGES.find((entry) => entry.level === level);
  if (!match || match.percentage <= 0) return 0;
  return Math.round(baseMrp * match.percentage);
}
