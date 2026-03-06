import { Schema, model, models } from "mongoose";

const TransferSlotSchema = new Schema(
  {
    slotNumber: { type: Number, required: true, min: 1, max: 5 },
    amount: { type: Number, required: true, min: 0 },
    isLocked: { type: Boolean, default: true },
    isTransferred: { type: Boolean, default: false },
    transferredTo: { type: Schema.Types.ObjectId, ref: "User" },
    transferredAt: { type: Date },
  },
  { _id: false },
);

const MlmTransferSchema = new Schema(
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
    voucherId: {
      type: Schema.Types.ObjectId,
      ref: "Voucher",
      index: true,
    },
    sourceSlotId: {
      type: Schema.Types.ObjectId,
      ref: "SpecialCredit",
      index: true,
    },
    amount: { type: Number, required: true, min: 0 },
    slotCount: { type: Number, default: 5, min: 1 },
    slotAmount: { type: Number, required: true, min: 0 },
    slots: { type: [TransferSlotSchema], default: [] },
    requiredVoucherCount: { type: Number, default: 5, min: 0 },
    baselineVoucherCount: { type: Number, default: 0, min: 0 },
    currentVoucherCount: { type: Number, default: 0, min: 0 },
    unlockedSlots: { type: Number, default: 0, min: 0 },
    timerStartedAt: { type: Date, required: true, index: true },
    expiresAt: { type: Date, required: true, index: true },
    unlockedAt: { type: Date },
    returnedAt: { type: Date },
    returnReason: { type: String },
    status: {
      type: String,
      enum: [
        "pending_unlock",
        "unlocked",
        "returned_timeout",
        "partial_timeout_review",
      ],
      default: "pending_unlock",
      index: true,
    },
  },
  { timestamps: true },
);

MlmTransferSchema.index({ receiverId: 1, status: 1, expiresAt: 1 });
MlmTransferSchema.index({ senderId: 1, status: 1, createdAt: -1 });
MlmTransferSchema.index({ voucherId: 1, receiverId: 1, status: 1 });

export default models.MlmTransfer || model("MlmTransfer", MlmTransferSchema);
