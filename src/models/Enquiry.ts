import { Schema, model, models } from "mongoose";

const EnquirySchema = new Schema(
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
    userPhone: {
      type: String,
      required: true,
      index: true
    },
    userName: String,
    userEmail: String,

    // Enquiry Details
    subject: { type: String, required: true },
    message: { type: String, required: true },

    // Status Tracking
    status: {
      type: String,
      enum: ['new', 'responded', 'closed'],
      default: 'new',
      index: true
    },
    priority: {
      type: String,
      enum: ['low', 'medium', 'high'],
      default: 'low'
    },

    // Response Tracking
    responses: [
      {
        message: String,
        respondedBy: {
          type: Schema.Types.ObjectId,
          ref: 'User'
        },
        respondedAt: { type: Date, default: Date.now },
        type: {
          type: String,
          enum: ['owner', 'admin']
        }
      }
    ],
    lastResponseAt: Date,
    lastRespondedBy: {
      type: Schema.Types.ObjectId,
      ref: 'User'
    },

    // Assignment (for team management later)
    assignedTo: {
      type: Schema.Types.ObjectId,
      ref: 'User'
    },

    // Lead Tracking
    convertedToLead: {
      type: Boolean,
      default: false
    },
    leadValue: Number,

    // For future notifications
    notificationSent: { type: Boolean, default: false },
    notificationSentAt: Date,

    createdAt: {
      type: Date,
      default: Date.now,
      index: true
    },
    updatedAt: { type: Date, default: Date.now }
  },
  { timestamps: true }
);

EnquirySchema.index({ businessId: 1, status: 1 });
EnquirySchema.index({ businessId: 1, createdAt: -1 });
EnquirySchema.index({ userId: 1, businessId: 1 });
EnquirySchema.index({ userPhone: 1, businessId: 1 });

export default models.Enquiry || model("Enquiry", EnquirySchema);
