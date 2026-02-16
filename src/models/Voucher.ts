import { Schema, model, models } from "mongoose";

const VoucherSchema = new Schema(
  {
    userId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    originalOwner: {
      type: Schema.Types.ObjectId,
      ref: "User",
      index: true,
    },
    creditId: { type: Schema.Types.ObjectId, ref: "MlmCredit" },
    voucherNumber: { type: String, required: true, unique: true, index: true },
    MRP: { type: Number, required: true, default: 1200 },
    issueDate: { type: Date, default: Date.now },
    expiryDate: { type: Date, required: true, index: true },
    redeemedStatus: {
      type: String,
      enum: ["unredeemed", "redeemed", "expired"],
      default: "unredeemed",
    },
    voucherImages: { type: [String], default: [] },
    productVideoLink: { type: String },
    redeemedAt: { type: Date },
    source: {
      type: String,
      enum: ["purchase", "transfer"],
      default: "purchase",
    },
    transferredFrom: {
      type: Schema.Types.ObjectId,
      ref: "User",
    },
    transferredAt: { type: Date },
    transferHistory: [
      {
        from: { type: Schema.Types.ObjectId, ref: "User" },
        to: { type: Schema.Types.ObjectId, ref: "User" },
        transferredAt: { type: Date, default: Date.now },
      },
    ],
  },
  { timestamps: true },
);

VoucherSchema.index({ userId: 1, redeemedStatus: 1 });

export default models.Voucher || model("Voucher", VoucherSchema);
