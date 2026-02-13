import { Schema, model, models } from "mongoose";

const CommissionTransactionSchema = new Schema(
  {
    fromUserId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    toUserId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    creditId: { type: Schema.Types.ObjectId, ref: "MlmCredit" },
    level: { type: Number, required: true, min: 1, max: 10 },
    percentage: { type: Number, required: true, min: 0 },
    amount: { type: Number, required: true, min: 0 },
    baseMrp: { type: Number, required: true },
  },
  { timestamps: true },
);

CommissionTransactionSchema.index({ toUserId: 1, createdAt: -1 });
CommissionTransactionSchema.index({ fromUserId: 1, createdAt: -1 });
CommissionTransactionSchema.index({ level: 1 });

export default models.CommissionTransaction ||
  model("CommissionTransaction", CommissionTransactionSchema);
