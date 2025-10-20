// CardShare Model - Track shared cards for duplicate prevention
import mongoose, { Schema, Document } from "mongoose";

export interface ICardShare extends Document {
  sessionId: mongoose.Types.ObjectId;
  fromUserId: mongoose.Types.ObjectId;
  toUserId: mongoose.Types.ObjectId;
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
    type: Schema.Types.ObjectId, 
    ref: "User", 
    required: true 
  },
  toUserId: { 
    type: Schema.Types.ObjectId, 
    ref: "User", 
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
