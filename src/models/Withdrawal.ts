import { Schema, model, models } from "mongoose";

const WithdrawalSchema = new Schema(
  {
    userId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    amount: { type: Number, required: true, min: 0 },
    status: {
      type: String,
      enum: ["pending", "approved", "rejected", "paid"],
      default: "pending",
      index: true,
    },
    requestedAt: { type: Date, default: Date.now },
    processedAt: { type: Date },
    adminNote: { type: String },
  },
  { timestamps: true },
);

WithdrawalSchema.index({ userId: 1, requestedAt: -1 });

export default models.Withdrawal || model("Withdrawal", WithdrawalSchema);
