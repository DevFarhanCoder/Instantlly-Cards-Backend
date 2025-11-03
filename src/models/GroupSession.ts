// GroupSession Model - For Group Sharing Feature
import mongoose, { Schema, Document } from "mongoose";

export interface IParticipant {
  userId: mongoose.Types.ObjectId;
  userName: string;
  userPhone: string;
  photo?: string;
  joinedAt: Date;
  cardsToShare: mongoose.Types.ObjectId[];
  defaultCardId?: mongoose.Types.ObjectId;
  isOnline: boolean;
}

export interface IGroupSession extends Document {
  code: string;
  adminId: mongoose.Types.ObjectId;
  adminName: string;
  adminPhone: string;
  adminPhoto?: string;
  participants: IParticipant[];
  status: "waiting" | "connected" | "sharing" | "completed" | "expired";
  allowParticipantSharing: boolean; // If false, only admin shares with all, and admin receives from all
  createdAt: Date;
  expiresAt: Date;
  isActive: boolean;
}

const ParticipantSchema = new Schema<IParticipant>({
  userId: { type: Schema.Types.ObjectId, ref: "User", required: true },
  userName: { type: String, required: true },
  userPhone: { type: String, required: true },
  photo: { type: String },
  joinedAt: { type: Date, default: Date.now },
  cardsToShare: [{ type: Schema.Types.ObjectId, ref: "Card" }],
  defaultCardId: { type: Schema.Types.ObjectId, ref: "Card" },
  isOnline: { type: Boolean, default: true }
}, { _id: false });

const GroupSessionSchema = new Schema<IGroupSession>({
  code: { 
    type: String, 
    required: true, 
    unique: true,
    index: true,
    length: 4
  },
  adminId: { 
    type: Schema.Types.ObjectId, 
    ref: "User", 
    required: true,
    index: true
  },
  adminName: { type: String, required: true },
  adminPhone: { type: String, required: true },
  adminPhoto: { type: String },
  participants: [ParticipantSchema],
  status: { 
    type: String, 
    enum: ["waiting", "connected", "sharing", "completed", "expired"],
    default: "waiting",
    index: true
  },
  allowParticipantSharing: { 
    type: Boolean, 
    default: true // Default: everyone shares with everyone (current behavior)
  },
  createdAt: { type: Date, default: Date.now },
  expiresAt: { 
    type: Date, 
    required: true,
    index: true // TTL index
  },
  isActive: { type: Boolean, default: true }
}, {
  timestamps: false
});

// TTL index - auto-delete expired sessions
GroupSessionSchema.index({ expiresAt: 1 }, { expireAfterSeconds: 0 });

const GroupSession = mongoose.model<IGroupSession>("GroupSession", GroupSessionSchema);

export default GroupSession;
