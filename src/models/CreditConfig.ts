import { Schema, model, models } from "mongoose";

/**
 * CreditConfig Model
 * Stores dynamic credit configuration for referral system
 * Admin can update these values anytime through dashboard
 */

const CreditConfigSchema = new Schema(
  {
    // There should only be ONE document in this collection
    signupBonus: { 
      type: Number, 
      required: true,
      default: 200,
      min: 0,
      description: "Credits given to new users when they sign up"
    },
    referralReward: { 
      type: Number, 
      required: true,
      default: 300,
      min: 0,
      description: "Credits given to existing users when someone uses their referral code"
    },
    lastUpdatedBy: { 
      type: String,
      description: "Email or ID of admin who last updated this config"
    },
    lastUpdatedAt: { 
      type: Date,
      default: Date.now
    }
  },
  { 
    timestamps: true,
    collection: 'creditconfig' // Ensure single collection name
  }
);

export default models.CreditConfig || model("CreditConfig", CreditConfigSchema);
