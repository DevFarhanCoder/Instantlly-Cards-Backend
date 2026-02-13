import MlmWallet from "../../models/MlmWallet";

export async function getOrCreateWallet(userId: string) {
  let wallet = await MlmWallet.findOne({ userId });
  if (!wallet) {
    wallet = await MlmWallet.create({ userId });
  }
  return wallet;
}

export async function addCredits(userId: string, amount: number) {
  return MlmWallet.findOneAndUpdate(
    { userId },
    { $inc: { creditBalance: amount } },
    { new: true, upsert: true },
  );
}

export async function addCommission(userId: string, amount: number) {
  return MlmWallet.findOneAndUpdate(
    { userId },
    {
      $inc: {
        commissionTotalEarned: amount,
        commissionAvailableBalance: amount,
      },
    },
    { new: true, upsert: true },
  );
}

export async function subtractCommission(userId: string, amount: number) {
  return MlmWallet.findOneAndUpdate(
    { userId, commissionAvailableBalance: { $gte: amount } },
    {
      $inc: {
        commissionAvailableBalance: -amount,
        commissionTotalWithdrawn: amount,
      },
    },
    { new: true },
  );
}
