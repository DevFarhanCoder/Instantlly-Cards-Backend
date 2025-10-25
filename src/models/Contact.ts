import mongoose, { Schema, Document } from "mongoose";

export interface IContact extends Document {
  userId: mongoose.Types.ObjectId;
  name: string;
  phoneNumber: string;
  isAppUser: boolean;
  appUserId?: mongoose.Types.ObjectId;
  lastSynced: Date;
  createdAt: Date;
  updatedAt: Date;
}

const ContactSchema = new Schema<IContact>({
  userId: {
    type: Schema.Types.ObjectId,
    ref: "User",
    required: true,
    index: true
  },
  name: {
    type: String,
    required: true,
    trim: true
  },
  phoneNumber: {
    type: String,
    required: true,
    index: true
  },
  isAppUser: {
    type: Boolean,
    default: false
  },
  appUserId: {
    type: Schema.Types.ObjectId,
    ref: "User",
    sparse: true
  },
  lastSynced: {
    type: Date,
    default: Date.now
  }
}, {
  timestamps: true
});

// Compound index for efficient queries
ContactSchema.index({ userId: 1, phoneNumber: 1 }, { unique: true });
ContactSchema.index({ userId: 1, isAppUser: 1 });

export default mongoose.model<IContact>("Contact", ContactSchema);