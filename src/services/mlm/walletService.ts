import MlmWallet from "../../models/MlmWallet";

// ✅ SIMPLIFIED WALLET SERVICE - Only handles credit balance
// No commission/withdrawal logic (discount-based MLM model)

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

export async function subtractCredits(userId: string, amount: number) {
  const wallet = await MlmWallet.findOne({
    userId,
    creditBalance: { $gte: amount },
  });

  if (!wallet) {
    throw new Error("Insufficient credit balance");
  }

  return MlmWallet.findOneAndUpdate(
    { userId, creditBalance: { $gte: amount } },
    { $inc: { creditBalance: -amount } },
    { new: true },
  );
}
