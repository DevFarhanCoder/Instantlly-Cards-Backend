import mongoose, { Schema, Document } from "mongoose";

export interface INotification extends Document {
  userId: mongoose.Types.ObjectId;
  type: 'CONTACT_JOINED' | 'NEW_MESSAGE' | 'CARD_SHARED' | 'GENERAL';
  title: string;
  message: string;
  data?: {
    contactId?: mongoose.Types.ObjectId;
    newUserId?: mongoose.Types.ObjectId;
    contactName?: string;
    contactPhone?: string;
    [key: string]: any;
  };
  read: boolean;
  createdAt: Date;
  updatedAt: Date;
}

const NotificationSchema = new Schema<INotification>({
  userId: {
    type: Schema.Types.ObjectId,
    ref: "User",
    required: true,
    index: true
  },
  type: {
    type: String,
    enum: ['CONTACT_JOINED', 'NEW_MESSAGE', 'CARD_SHARED', 'GENERAL'],
    required: true,
    index: true
  },
  title: {
    type: String,
    required: true,
    trim: true
  },
  message: {
    type: String,
    required: true,
    trim: true
  },
  data: {
    type: Schema.Types.Mixed,
    default: {}
  },
  read: {
    type: Boolean,
    default: false,
    index: true
  }
}, {
  timestamps: true
});

// Compound indexes for efficient queries
NotificationSchema.index({ userId: 1, read: 1, createdAt: -1 });
NotificationSchema.index({ userId: 1, type: 1, createdAt: -1 });

export default mongoose.model<INotification>("Notification", NotificationSchema);