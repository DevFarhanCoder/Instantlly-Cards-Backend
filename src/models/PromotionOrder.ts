import { Schema, model, models } from "mongoose";

const PromotionOrderSchema = new Schema(
  {
    userId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    businessPromotionId: {
      type: Schema.Types.ObjectId,
      ref: "BusinessPromotion",
      required: true,
      index: true,
    },
    pricingPlanId: {
      type: Schema.Types.ObjectId,
      ref: "PromotionPricingPlan",
      required: true,
    },
    areaType: {
      type: String,
      enum: ["pincode", "tehsil", "district", "division", "state", "zone", "india"],
      required: true,
    },
    rank: {
      type: Number,
      min: 1,
      max: 21,
      required: true,
    },
    rankLabel: {
      type: String,
      required: true,
      trim: true,
    },
    amount: {
      type: Number,
      required: true,
      min: 0,
    },
    // Voucher redemption (Instantlly-only)
    voucherId: {
      type: Schema.Types.ObjectId,
      ref: "Voucher",
      default: null,
    },
    voucherQtyApplied: {
      type: Number,
      default: 0,
      min: 0,
    },
    voucherValuePerUnit: {
      type: Number,
      default: 0,
      min: 0,
    },
    voucherAmountApplied: {
      type: Number,
      default: 0,
      min: 0,
    },
    payableAmount: {
      type: Number,
      default: 0,
      min: 0,
    },
    voucherStatus: {
      type: String,
      enum: ["none", "reserved", "applied", "released"],
      default: "none",
    },
    voucherAppliedAt: { type: Date, default: null },
    voucherReleasedAt: { type: Date, default: null },
    currency: {
      type: String,
      default: "INR",
      enum: ["INR"],
    },
    durationDays: {
      type: Number,
      required: true,
      min: 1,
    },
    priorityScore: {
      type: Number,
      required: true,
      min: 0,
      max: 100,
    },
    status: {
      type: String,
      enum: [
        "created",
        "payment_pending",
        "paid",
        "payment_failed",
        "cancelled",
        "activated",
        "expired",
      ],
      default: "created",
      index: true,
    },
    paymentProvider: {
      type: String,
      default: "manual",
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
    paidAt: {
      type: Date,
      default: null,
    },
    activatedAt: {
      type: Date,
      default: null,
    },
    expiresAt: {
      type: Date,
      default: null,
      index: true,
    },
    metadata: {
      type: Map,
      of: Schema.Types.Mixed,
      default: {},
    },
  },
  { timestamps: true },
);

PromotionOrderSchema.index({ userId: 1, createdAt: -1 });
PromotionOrderSchema.index({ businessPromotionId: 1, status: 1 });

export default models.PromotionOrder || model("PromotionOrder", PromotionOrderSchema);
