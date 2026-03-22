import { Schema, model, models } from "mongoose";

const AdPaymentOrderSchema = new Schema(
  {
    userId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    designRequestId: {
      type: Schema.Types.ObjectId,
      ref: "DesignRequest",
      default: null,
      index: true,
    },
    adId: {
      type: Schema.Types.ObjectId,
      ref: "Ad",
      default: null,
      index: true,
    },
    orderType: {
      type: String,
      enum: ["design_fee", "ad_approval"],
      required: true,
      index: true,
    },
    amount: {
      type: Number,
      required: true,
      min: 0,
    },
    payableAmount: {
      type: Number,
      default: 0,
      min: 0,
    },
    currency: {
      type: String,
      default: "INR",
      enum: ["INR"],
    },
    status: {
      type: String,
      enum: ["created", "payment_pending", "paid", "payment_failed", "cancelled"],
      default: "created",
      index: true,
    },
    paymentProvider: {
      type: String,
      default: "razorpay",
      trim: true,
    },
    paymentOrderId: {
      type: String,
      default: null,
      index: true,
    },
    paymentId: {
      type: String,
      default: null,
      index: true,
    },
    paidAt: { type: Date, default: null },

    // Voucher redemption (Instantlly-only)
    voucherId: {
      type: Schema.Types.ObjectId,
      ref: "Voucher",
      default: null,
    },
    voucherQtyApplied: { type: Number, default: 0, min: 0 },
    voucherValuePerUnit: { type: Number, default: 0, min: 0 },
    voucherAmountApplied: { type: Number, default: 0, min: 0 },
    voucherStatus: {
      type: String,
      enum: ["none", "reserved", "applied", "released"],
      default: "none",
    },
    voucherAppliedAt: { type: Date, default: null },
    voucherReleasedAt: { type: Date, default: null },

    metadata: {
      type: Map,
      of: Schema.Types.Mixed,
      default: {},
    },
  },
  { timestamps: true },
);

AdPaymentOrderSchema.index({ userId: 1, createdAt: -1 });
AdPaymentOrderSchema.index({ orderType: 1, status: 1 });
AdPaymentOrderSchema.index({ designRequestId: 1, orderType: 1 });
AdPaymentOrderSchema.index({ adId: 1, orderType: 1 });

export default models.AdPaymentOrder ||
  model("AdPaymentOrder", AdPaymentOrderSchema);

