import mongoose, { Document, Schema } from 'mongoose';

export interface IDesignRequest extends Document {
  businessName?: string;
  email?: string;
  webLinks?: string[];
  phoneNumber?: string;
  adText?: string;
  businessAddress?: string;
  adType: 'image' | 'video';
  channelType: 'withChannel' | 'withoutChannel';
  referenceImagesGridFS?: mongoose.Types.ObjectId[];
  referenceVideosGridFS?: mongoose.Types.ObjectId[];
  referenceImagesS3?: Array<{ url: string; key: string }>;
  referenceVideosS3?: Array<{ url: string; key: string }>;
  uploaderPhone: string;
  uploaderName: string;
  userId?: string;
  status: 'pending' | 'in-progress' | 'completed' | 'cancelled';
  adminNotes?: string;
  completedAdId?: mongoose.Types.ObjectId;
  createdAt: Date;
  updatedAt: Date;
}

const DesignRequestSchema = new Schema<IDesignRequest>(
  {
    businessName: {
      type: String,
      default: '',
    },
    email: {
      type: String,
      default: '',
    },
    webLinks: {
      type: [String],
      default: [],
    },
    phoneNumber: {
      type: String,
      default: '',
    },
    adText: {
      type: String,
      default: '',
    },
    businessAddress: {
      type: String,
      default: '',
    },
    adType: {
      type: String,
      enum: ['image', 'video'],
      required: true,
    },
    channelType: {
      type: String,
      enum: ['withChannel', 'withoutChannel'],
      default: 'withoutChannel',
    },
    referenceImagesGridFS: {
      type: [Schema.Types.ObjectId],
      default: [],
    },
    referenceVideosGridFS: {
      type: [Schema.Types.ObjectId],
      default: [],
    },
    referenceImagesS3: {
      type: [{
        url: { type: String, required: true },
        key: { type: String, required: true },
      }],
      default: [],
    },
    referenceVideosS3: {
      type: [{
        url: { type: String, required: true },
        key: { type: String, required: true },
      }],
      default: [],
    },
    uploaderPhone: {
      type: String,
      required: true,
    },
    uploaderName: {
      type: String,
      default: 'Mobile User',
    },
    userId: {
      type: String,
      default: '',
    },
    status: {
      type: String,
      enum: ['pending', 'in-progress', 'completed', 'cancelled'],
      default: 'pending',
    },
    adminNotes: {
      type: String,
      default: '',
    },
    completedAdId: {
      type: Schema.Types.ObjectId,
      ref: 'Ad',
      default: null,
    },
  },
  {
    timestamps: true,
  }
);

// Indexes for efficient queries
DesignRequestSchema.index({ uploaderPhone: 1 });
DesignRequestSchema.index({ status: 1 });
DesignRequestSchema.index({ createdAt: -1 });

const DesignRequest = mongoose.model<IDesignRequest>('DesignRequest', DesignRequestSchema);

export default DesignRequest;
