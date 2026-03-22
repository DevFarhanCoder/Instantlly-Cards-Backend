import { Schema, model, models } from "mongoose";

const VoucherRedemptionSchema = new Schema(
  {
    userId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    sourceType: {
      type: String,
      enum: ["promotion", "design_fee", "ad_approval"],
      required: true,
      index: true,
    },
    sourceId: {
      type: Schema.Types.ObjectId,
      required: true,
      index: true,
    },
    voucherId: {
      type: Schema.Types.ObjectId,
      ref: "Voucher",
      required: true,
      index: true,
    },
    companyName: { type: String, default: "Instantlly" },
    qty: { type: Number, required: true, min: 1 },
    valuePerUnit: { type: Number, required: true, min: 0 },
    amount: { type: Number, required: true, min: 0 },
    currency: { type: String, default: "INR", enum: ["INR"] },
    status: {
      type: String,
      enum: ["reserved", "applied", "released"],
      default: "reserved",
      index: true,
    },
    reservedAt: { type: Date, default: Date.now },
    appliedAt: { type: Date, default: null },
    releasedAt: { type: Date, default: null },
    releaseReason: { type: String, default: null },
    metadata: {
      type: Map,
      of: Schema.Types.Mixed,
      default: {},
    },
  },
  { timestamps: true },
);

VoucherRedemptionSchema.index({ sourceType: 1, sourceId: 1 });
VoucherRedemptionSchema.index({ userId: 1, createdAt: -1 });

export default models.VoucherRedemption ||
  model("VoucherRedemption", VoucherRedemptionSchema);
