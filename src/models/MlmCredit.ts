import { Schema, model, models } from "mongoose";

const MlmCreditSchema = new Schema(
  {
    senderId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    receiverId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    status: {
      type: String,
      enum: ["pending", "active", "expired", "reverted"],
      default: "pending",
      index: true,
    },
    paymentStatus: {
      type: String,
      enum: ["pending", "waiting_approval", "approved", "rejected"],
      default: "pending",
      index: true,
    },
    paymentConfirmedByReceiver: { type: Boolean, default: false },
    paymentConfirmedAt: { type: Date },
    adminApprovedBy: { type: Schema.Types.ObjectId, ref: "User" },
    adminApprovedAt: { type: Date },
    adminNote: { type: String },
    createdAt: { type: Date, default: Date.now },
    expiresAt: { type: Date, required: true, index: true },
    activatedAt: { type: Date },
    transferExpiresAt: { type: Date, index: true },
    quantity: { type: Number, default: 5, min: 1 },
    transferredCount: { type: Number, default: 0, min: 0 },
    revertedCount: { type: Number, default: 0, min: 0 },
    sourceCreditId: { type: Schema.Types.ObjectId, ref: "MlmCredit" },
  },
  { timestamps: true },
);

MlmCreditSchema.index({ receiverId: 1, status: 1, paymentStatus: 1 });
MlmCreditSchema.index({ senderId: 1, createdAt: -1 });

export default models.MlmCredit || model("MlmCredit", MlmCreditSchema);
