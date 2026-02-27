import { Schema, model, models } from "mongoose";

const PromotionPricingPlanSchema = new Schema(
  {
    code: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    areaType: {
      type: String,
      enum: ["pincode", "tehsil", "district"],
      required: true,
      index: true,
    },
    rank: {
      type: Number,
      min: 1,
      max: 21, // 21 => "No Rank"
      required: true,
      index: true,
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
      default: 30,
    },
    priorityScore: {
      type: Number,
      required: true,
      min: 0,
      max: 100,
      default: 10,
    },
    isActive: {
      type: Boolean,
      default: true,
      index: true,
    },
  },
  { timestamps: true },
);

PromotionPricingPlanSchema.index(
  { areaType: 1, rank: 1, durationDays: 1, isActive: 1 },
  { unique: true, partialFilterExpression: { isActive: true } },
);

export default models.PromotionPricingPlan ||
  model("PromotionPricingPlan", PromotionPricingPlanSchema);
