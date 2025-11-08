import mongoose from "mongoose";

const AdSchema = new mongoose.Schema(
  {
    // Ad Content
    title: {
      type: String,
      required: true,
      trim: true
    },

    // Bottom Banner Ad (Required - 624 × 174px)
    // MIGRATION: Changed from base64 string to GridFS ObjectId reference
    bottomImage: {
      type: String, // Base64 encoded image (legacy) OR GridFS ObjectId (new)
      required: true
    },

    // GridFS reference for bottom image (new field)
    bottomImageGridFS: {
      type: mongoose.Schema.Types.ObjectId,
      ref: "adImages.files"
    },

    // Full Screen Ad (Optional - 624 × 1000px)
    // MIGRATION: Changed from base64 string to GridFS ObjectId reference
    fullscreenImage: {
      type: String, // Base64 encoded image (legacy) OR GridFS ObjectId (new)
      default: ""
    },

    // GridFS reference for fullscreen image (new field)
    fullscreenImageGridFS: {
      type: mongoose.Schema.Types.ObjectId,
      ref: "adImages.files"
    },

    // Contact Information
    phoneNumber: {
      type: String,
      required: true,
      trim: true
    },

    // Scheduling
    startDate: {
      type: Date,
      required: true,
      index: true
    },

    endDate: {
      type: Date,
      required: true,
      index: true
    },

    // Analytics
    impressions: {
      type: Number,
      default: 0
    },

    clicks: {
      type: Number,
      default: 0
    },

    // Priority for rotation (1-10, higher = more priority)
    priority: {
      type: Number,
      default: 5,
      min: 1,
      max: 10
    }
  },
  {
    timestamps: true
  }
);

// Compound index for active ads within date range
AdSchema.index({ startDate: 1, endDate: 1 });

// Index for sorting by priority and createdAt (prevents memory limit error)
AdSchema.index({ priority: -1, createdAt: -1 });

// Index for admin list sorted by createdAt
AdSchema.index({ createdAt: -1 });

// Compound index for active ads query with sort optimization
AdSchema.index({ startDate: 1, endDate: 1, priority: -1, createdAt: -1 });

// SCALABILITY: Additional indexes for crores of ads
// Text search index for title and phone (enables fast search)
AdSchema.index({ title: 'text', phoneNumber: 'text' });

// Index for phone number exact lookups
AdSchema.index({ phoneNumber: 1 });

// Index for expired ads queries
AdSchema.index({ endDate: 1 });

// Index for upcoming ads queries
AdSchema.index({ startDate: 1 });

export default mongoose.model("Ad", AdSchema);
