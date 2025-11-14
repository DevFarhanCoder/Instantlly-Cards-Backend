import { Schema, model, models } from "mongoose";

const TransactionSchema = new Schema(
  {
    type: { 
      type: String, 
      required: true,
      enum: ['signup_bonus', 'referral_bonus', 'transfer_sent', 'transfer_received', 'ad_deduction', 'admin_adjustment']
    },
    fromUser: { type: Schema.Types.ObjectId, ref: 'User' },
    toUser: { type: Schema.Types.ObjectId, ref: 'User' },
    amount: { type: Number, required: true },
    description: { type: String, required: true },
    balanceBefore: { type: Number },
    balanceAfter: { type: Number },
    relatedAd: { type: Schema.Types.ObjectId, ref: 'Ad' },
    status: { 
      type: String, 
      default: 'completed',
      enum: ['pending', 'completed', 'failed', 'cancelled']
    }
  },
  { timestamps: true }
);

// Index for faster queries
TransactionSchema.index({ fromUser: 1, createdAt: -1 });
TransactionSchema.index({ toUser: 1, createdAt: -1 });
TransactionSchema.index({ type: 1 });

export default models.Transaction || model("Transaction", TransactionSchema);
