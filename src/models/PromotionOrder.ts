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
      enum: ["pincode", "tehsil", "district"],
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
