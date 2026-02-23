import { Schema, model, models } from "mongoose";

/**
 * SpecialCredit - Tracks "Sales Target at Special Discount" credits
 *
 * This is separate from regular MLM credits and follows a specific pattern:
 * - Admin (level 0): 10 slots × 14,648,436,000 credits each
 * - Level 1: 5 slots × 2,929,686,000 credits each (÷5)
 * - Level 2: 5 slots × 585,936,000 credits each (÷5)
 * - And so on...
 *
 * Each slot can be sent to one user via phone number.
 * After sending, the slot shows the recipient's name and a "Call" button.
 */
const SpecialCreditSchema = new Schema(
  {
    // Owner of this special credit slot
    ownerId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },

    // Slot number (1-10 for admin, 1-5 for regular users)
    slotNumber: {
      type: Number,
      required: true,
      min: 1,
      max: 10,
    },

    // Amount of credits in this slot
    creditAmount: {
      type: Number,
      required: true,
    },

    // Status of the slot
    status: {
      type: String,
      enum: ["available", "sent", "expired"],
      default: "available",
      index: true,
    },

    // Recipient details (filled after sending)
    recipientId: {
      type: Schema.Types.ObjectId,
      ref: "User",
    },
    recipientName: { type: String },
    recipientPhone: { type: String },

    // Timestamps
    sentAt: { type: Date },
    expiresAt: { type: Date },

    // Source - who created this slot
    sourceSlotId: {
      type: Schema.Types.ObjectId,
      ref: "SpecialCredit",
    },

    // Level in the chain (0 for admin, 1 for first level, etc.)
    level: {
      type: Number,
      default: 0,
      min: 0,
      max: 10,
    },
  },
  { timestamps: true },
);

// Compound index for efficient queries
SpecialCreditSchema.index({ ownerId: 1, slotNumber: 1 });
SpecialCreditSchema.index({ ownerId: 1, status: 1 });

export default models.SpecialCredit ||
  model("SpecialCredit", SpecialCreditSchema);
