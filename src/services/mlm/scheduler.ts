import cron from "node-cron";
import MlmCredit from "../../models/MlmCredit";
import { addCredits } from "./walletService";

async function expirePendingCredits() {
  const now = new Date();
  const pendingCredits = await MlmCredit.find({
    status: "pending",
    paymentStatus: "pending",
    expiresAt: { $lt: now },
  }).limit(200);

  for (const credit of pendingCredits) {
    credit.status = "expired";
    credit.paymentStatus = "failed";
    await credit.save();
    await addCredits(credit.senderId.toString(), 1);
  }
}

async function revertUnusedCredits() {
  const now = new Date();
  const activeCredits = await MlmCredit.find({
    status: "active",
    paymentStatus: "paid",
    transferExpiresAt: { $lt: now },
    $expr: { $lt: ["$transferredCount", "$quantity"] },
  }).limit(200);

  for (const credit of activeCredits) {
    const remaining = Math.max(0, credit.quantity - credit.transferredCount);
    if (remaining <= 0) continue;

    credit.status = "reverted";
    credit.revertedCount = remaining;
    await credit.save();

    await addCredits(credit.receiverId.toString(), -remaining);
    await addCredits(credit.senderId.toString(), remaining);
  }
}

export function startMlmScheduler() {
  cron.schedule("*/5 * * * *", async () => {
    await expirePendingCredits();
    await revertUnusedCredits();
  });
}
