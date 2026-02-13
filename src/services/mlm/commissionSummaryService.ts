import CommissionTransaction from "../../models/CommissionTransaction";
import MlmWallet from "../../models/MlmWallet";

export async function getCommissionSummary(userId: string) {
  const wallet = (await MlmWallet.findOne({ userId }).lean()) as any;

  const levelBreakdown = await CommissionTransaction.aggregate([
    { $match: { toUserId: userId } },
    { $group: { _id: "$level", totalAmount: { $sum: "$amount" } } },
    { $sort: { _id: 1 } },
  ]);

  return {
    totalEarned:
      wallet && (wallet as any).commissionTotalEarned
        ? (wallet as any).commissionTotalEarned
        : 0,
    totalWithdrawn:
      wallet && (wallet as any).commissionTotalWithdrawn
        ? (wallet as any).commissionTotalWithdrawn
        : 0,
    availableBalance:
      wallet && (wallet as any).commissionAvailableBalance
        ? (wallet as any).commissionAvailableBalance
        : 0,
    levelBreakdown: levelBreakdown.map((entry) => ({
      level: entry._id,
      amount: entry.totalAmount,
    })),
  };
}
