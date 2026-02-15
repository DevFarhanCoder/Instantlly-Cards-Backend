import { Schema, model, models } from "mongoose";

// ✅ NEW MODEL: Only tracks credit balance (no real commission/withdrawals)
// Discount-based MLM: Savings are virtual, not stored here
const MlmWalletSchema = new Schema(
  {
    userId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      unique: true,
      index: true,
    },
    creditBalance: { type: Number, default: 0, min: 0 },
  },
  { timestamps: true },
);

export default models.MlmWallet || model("MlmWallet", MlmWalletSchema);
