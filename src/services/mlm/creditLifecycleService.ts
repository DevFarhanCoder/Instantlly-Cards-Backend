import MlmCredit from "../../models/MlmCredit";
import { addCredits, getOrCreateWallet } from "./walletService";

export async function assignCreditUsage(senderId: string) {
  const now = new Date();
  const activeCredit = await MlmCredit.findOne({
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
  await addCredits(senderId, -1);

  return activeCredit;
}

export async function ensureWallet(userId: string) {
  return getOrCreateWallet(userId);
}
