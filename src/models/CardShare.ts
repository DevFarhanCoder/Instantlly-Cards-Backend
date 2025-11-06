// CardShare Model - Track shared cards for duplicate prevention
import mongoose, { Schema, Document } from "mongoose";

export interface ICardShare extends Document {
  sessionId: mongoose.Types.ObjectId;
  fromUserId: string;  // Changed to string to support temporary userIds
  toUserId: string;    // Changed to string to support temporary userIds
  cardId: mongoose.Types.ObjectId;
  sharedAt: Date;
}

const CardShareSchema = new Schema<ICardShare>({
  sessionId: { 
    type: Schema.Types.ObjectId, 
    ref: "GroupSession", 
    required: true,
    index: true
  },
  fromUserId: { 
    type: String,  // Changed from ObjectId to String
    required: true 
  },
  toUserId: { 
    type: String,  // Changed from ObjectId to String
    required: true 
  },
  cardId: { 
    type: Schema.Types.ObjectId, 
    ref: "Card", 
    required: true 
  },
  sharedAt: { type: Date, default: Date.now }
}, {
  timestamps: false
});

// Compound index for duplicate checking
CardShareSchema.index({ fromUserId: 1, toUserId: 1, cardId: 1 }, { unique: true });

const CardShare = mongoose.model<ICardShare>("CardShare", CardShareSchema);

export default CardShare;
