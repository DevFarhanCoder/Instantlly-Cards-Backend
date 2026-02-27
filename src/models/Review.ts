import { Schema, model, models } from "mongoose";

const ReviewSchema = new Schema(
  {
    businessId: {
      type: Schema.Types.ObjectId,
      ref: 'BusinessPromotion',
      required: true,
      index: true
    },
    userId: {
      type: Schema.Types.ObjectId,
      ref: 'User',
      required: true,
      index: true
    },
    userName: { type: String },
    userPhone: { type: String }, // For anonymous display

    // Core Review Data
    rating: {
      type: Number,
      required: true,
      min: 1,
      max: 5,
      index: true
    },
    title: {
      type: String,
      maxlength: 50
    },
    message: {
      type: String,
      maxlength: 500
    },

    // Dynamic Suggestion-based Review
    selectedSuggestions: [String], // e.g., ["Poor service", "Rude staff"]
    experience: {
      type: String,
      maxlength: 500
    },

    // Media
    photos: [
      {
        url: String,
        cloudinary_id: String,
        uploadedAt: {
          type: Date,
          default: Date.now
        }
      }
    ],

    // Response from Business Owner
    ownerReply: {
      message: String,
      repliedAt: Date,
      repliedBy: {
        type: Schema.Types.ObjectId,
        ref: 'User'
      }
    },

    // Moderation
    isApproved: {
      type: Boolean,
      default: true,
      index: true
    },
    isSpam: {
      type: Boolean,
      default: false
    },
    spamReports: [
      {
        reportedBy: Schema.Types.ObjectId,
        reason: String,
        reportedAt: Date
      }
    ],

    // Engagement
    helpful: { type: Number, default: 0 },
    unhelpful: { type: Number, default: 0 },

    createdAt: {
      type: Date,
      default: Date.now,
      index: true
    },
    updatedAt: { type: Date, default: Date.now }
  },
  { timestamps: true }
);

// Prevent duplicate reviews from same user
ReviewSchema.index({ userId: 1, businessId: 1 }, { unique: true });
ReviewSchema.index({ businessId: 1, rating: 1 });
ReviewSchema.index({ businessId: 1, createdAt: -1 });
ReviewSchema.index({ createdAt: -1 });

export default models.Review || model("Review", ReviewSchema);
