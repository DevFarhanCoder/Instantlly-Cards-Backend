import { Schema, model, models } from "mongoose";

/**
 * SlotEvent - Immutable audit log for all SpecialCredit slot state changes.
 * Every SLOT_SENT, SLOT_REFUNDED, SLOT_EXPIRED, or SLOT_REVERTED writes one record.
 * Records are never deleted.
 */
const SlotEventSchema = new Schema(
  {
    type: {
      type: String,
      enum: ["SLOT_SENT", "SLOT_REFUNDED", "SLOT_EXPIRED", "SLOT_REVERTED"],
      required: true,
      index: true,
    },
    // The SpecialCredit slot document that changed
    slotId: {
      type: Schema.Types.ObjectId,
      ref: "SpecialCredit",
      required: true,
      index: true,
    },
    // Owner of the slot
    ownerId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    // Recipient (null for non-SLOT_SENT events or before a recipient was set)
    recipientId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      default: null,
    },
    // Which voucher template this slot belongs to
    voucherId: {
      type: Schema.Types.ObjectId,
      ref: "Voucher",
      index: true,
    },
    // Credit amount at the time of the event (snapshot)
    amount: {
      type: Number,
      required: true,
    },
    // Who triggered the operation
    actorId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
    },
    // Arbitrary extra data (e.g., refund reason, expiry details)
    meta: {
      type: Schema.Types.Mixed,
      default: {},
    },
  },
  {
    timestamps: { createdAt: true, updatedAt: false }, // append-only
  },
);

// Audit queries: all events for a slot, all events for an owner
SlotEventSchema.index({ slotId: 1, createdAt: -1 });
SlotEventSchema.index({ ownerId: 1, createdAt: -1 });

export default models.SlotEvent || model("SlotEvent", SlotEventSchema);
