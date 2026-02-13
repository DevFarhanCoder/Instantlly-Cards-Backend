import { Schema, model, models } from "mongoose";

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
    commissionTotalEarned: { type: Number, default: 0, min: 0 },
    commissionTotalWithdrawn: { type: Number, default: 0, min: 0 },
    commissionAvailableBalance: { type: Number, default: 0, min: 0 },
  },
  { timestamps: true },
);

export default models.MlmWallet || model("MlmWallet", MlmWalletSchema);
