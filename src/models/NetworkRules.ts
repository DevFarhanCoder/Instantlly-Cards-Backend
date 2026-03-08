import { Schema, model, models } from "mongoose";

/**
 * NetworkRules - Singleton document (always _id = "default") that controls
 * structural constraints:
 *   - maxDirectByRole: how many direct children each role is allowed
 *   - maxDepth: maximum tree depth (levels below root)
 *
 * Change rules here at runtime without a code deploy.
 * Seed via the backfill script or insert manually once:
 *   db.networkrules.insertOne({ _id: "default", maxDirectByRole: { admin: 3, user: 999999 }, maxDepth: 10 })
 */
const NetworkRulesSchema = new Schema(
  {
    _id: { type: String, default: "default" },
    // Maximum direct children allowed per user role
    maxDirectByRole: {
      admin: { type: Number, default: 3 },
      user: { type: Number, default: 999999 },
    },
    // Maximum depth of the MLM tree (root is level 0)
    maxDepth: { type: Number, default: 10 },
  },
  {
    timestamps: { createdAt: false, updatedAt: true },
    _id: false, // We provide our own _id ("default")
  },
);

export interface INetworkRules {
  _id: string;
  maxDirectByRole: { admin: number; user: number };
  maxDepth: number;
  updatedAt?: Date;
}

export default models.NetworkRules ||
  model<INetworkRules>("NetworkRules", NetworkRulesSchema);
