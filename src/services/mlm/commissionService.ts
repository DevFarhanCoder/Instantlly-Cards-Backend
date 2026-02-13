import User from "../../models/User";
import CommissionTransaction from "../../models/CommissionTransaction";
import { MLM_BASE_ENTRY_VALUE, MLM_LEVEL_PERCENTAGES } from "../../utils/mlm";
import { addCommission } from "./walletService";

export async function distributeCommission(
  fromUserId: string,
  creditId: string,
) {
  const commissions: any[] = [];
  let currentUserId: any = fromUserId;

  for (const entry of MLM_LEVEL_PERCENTAGES) {
    if (!currentUserId) break;

    const currentUser = await User.findById(currentUserId).select("parentId");
    if (!currentUser || !currentUser.parentId) break;

    const parentId = currentUser.parentId.toString();
    if (entry.percentage > 0) {
      const amount = Math.round(MLM_BASE_ENTRY_VALUE * entry.percentage);
      commissions.push({
        fromUserId,
        toUserId: parentId,
        creditId,
        level: entry.level,
        percentage: entry.percentage,
        amount,
        baseMrp: MLM_BASE_ENTRY_VALUE,
      });

      await addCommission(parentId, amount);
    }

    currentUserId = parentId;
  }

  if (commissions.length > 0) {
    await CommissionTransaction.insertMany(commissions);
  }

  return commissions;
}
