import mongoose, { Document, Schema } from "mongoose";

export interface IVoucherTransferLog extends Document {
  senderId: mongoose.Types.ObjectId;
  senderName?: string;
  senderPhone?: string;
  recipientId: mongoose.Types.ObjectId;
  recipientName?: string;
  recipientPhone?: string;
  quantity: number;
  // Per-voucher amount at the time of transfer (snapshot). Falls back to 1200 if not set.
  amount?: number;
  transferredAt: Date;
  voucherId?: mongoose.Types.ObjectId;
}

const VoucherTransferLogSchema: Schema = new Schema(
  {
    senderId: { type: Schema.Types.ObjectId, ref: "User", required: true },
    senderName: { type: String },
    senderPhone: { type: String },
    recipientId: { type: Schema.Types.ObjectId, ref: "User", required: true },
    recipientName: { type: String },
    recipientPhone: { type: String },
    quantity: { type: Number, required: true, default: 1 },
    // Per-voucher amount snapshot (e.g. 1200 for Instantlly, different for other vouchers)
    amount: { type: Number },
    transferredAt: { type: Date, default: Date.now },
    voucherId: { type: Schema.Types.ObjectId, ref: "Voucher", index: true },
  },
  { timestamps: true },
);

// Index for fast lookup by sender and recipient
VoucherTransferLogSchema.index({ senderId: 1, transferredAt: -1 });
VoucherTransferLogSchema.index({ recipientId: 1, transferredAt: -1 });

export default mongoose.model<IVoucherTransferLog>(
  "VoucherTransferLog",
  VoucherTransferLogSchema,
);
