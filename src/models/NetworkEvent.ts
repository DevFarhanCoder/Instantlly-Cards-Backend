import { Schema, model, models } from "mongoose";

/**
 * NetworkEvent - Immutable audit log for all network topology changes.
 * Every LINK, UNLINK, REASSIGN, or SOFT_DELETE writes one record here.
 * Records are never deleted.
 */
const NetworkEventSchema = new Schema(
  {
    type: {
      type: String,
      enum: ["LINK", "UNLINK", "REASSIGN", "SOFT_DELETE"],
      required: true,
      index: true,
    },
    // The user whose position in the tree changed
    userId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
      index: true,
    },
    // Previous parent (null if user was a root before the operation)
    oldParentId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      default: null,
    },
    // New parent (null after UNLINK / SOFT_DELETE)
    newParentId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      default: null,
    },
    // Who triggered the operation (admin user or the user themselves)
    actorId: {
      type: Schema.Types.ObjectId,
      ref: "User",
      required: true,
    },
    // Human-readable reason / note
    reason: {
      type: String,
      default: "",
    },
  },
  {
    timestamps: { createdAt: true, updatedAt: false }, // append-only
  },
);

// Useful for replaying history for a specific user
NetworkEventSchema.index({ userId: 1, createdAt: -1 });

export default models.NetworkEvent || model("NetworkEvent", NetworkEventSchema);
