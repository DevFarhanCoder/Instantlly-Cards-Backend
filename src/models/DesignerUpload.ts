import mongoose, { Document, Schema } from 'mongoose';

export interface IDesignerUpload extends Document {
  designRequestId: mongoose.Types.ObjectId;
  designerId: mongoose.Types.ObjectId;
  designerName: string;
  filesS3: Array<{ url: string; key: string; filename: string; contentType: string }>;
  notes: string;
  status: 'uploaded' | 'approved' | 'rejected' | 'sent-to-user';
  adminNotes: string;
  userFeedback: string;
  createdAt: Date;
  updatedAt: Date;
}

const DesignerUploadSchema = new Schema<IDesignerUpload>({
  designRequestId: { type: Schema.Types.ObjectId, ref: 'DesignRequest', required: true },
  designerId: { type: Schema.Types.ObjectId, ref: 'Designer', required: true },
  designerName: { type: String, required: true },
  filesS3: [{
    url: { type: String, required: true },
    key: { type: String, required: true },
    filename: { type: String },
    contentType: { type: String }
  }],
  notes: { type: String, default: '' },
  status: {
    type: String,
    enum: ['uploaded', 'approved', 'rejected', 'sent-to-user'],
    default: 'uploaded'
  },
  adminNotes: { type: String, default: '' },
  userFeedback: { type: String, default: '' }
}, { timestamps: true });

// Index for fast lookups
DesignerUploadSchema.index({ designerId: 1, createdAt: -1 });
DesignerUploadSchema.index({ designRequestId: 1 });
DesignerUploadSchema.index({ status: 1 });

const DesignerUpload = mongoose.model<IDesignerUpload>('DesignerUpload', DesignerUploadSchema);
export default DesignerUpload;
