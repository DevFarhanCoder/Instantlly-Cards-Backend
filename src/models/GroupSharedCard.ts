import mongoose from "mongoose";

const groupSharedCardSchema = new mongoose.Schema({
  // Card being shared
  cardId: { 
    type: mongoose.Schema.Types.ObjectId, 
    ref: 'Card', 
    required: true 
  },
  
  // Who sent the card
  senderId: { 
    type: mongoose.Schema.Types.ObjectId, 
    ref: 'User', 
    required: true 
  },
  
  // Group where the card was shared
  groupId: { 
    type: mongoose.Schema.Types.ObjectId, 
    ref: 'Group', 
    required: true 
  },
  
  // Optional message with the card
  message: { 
    type: String, 
    maxlength: 500,
    default: "" 
  },
  
  // When the card was shared
  sentAt: { 
    type: Date, 
    default: Date.now 
  },
  
  // Cache some card info for quick queries
  cardTitle: { 
    type: String, 
    required: true 
  },
  
  // Cache sender info
  senderName: { 
    type: String, 
    required: true 
  },
  
  // Cache group info
  groupName: { 
    type: String, 
    required: true 
  },
  
}, {
  timestamps: true // Adds createdAt and updatedAt
});

// Indexes for efficient queries
groupSharedCardSchema.index({ senderId: 1, sentAt: -1 }); // For sent cards query
groupSharedCardSchema.index({ groupId: 1, sentAt: -1 }); // For group cards query
groupSharedCardSchema.index({ cardId: 1 }); // For card lookup

const GroupSharedCard = mongoose.model('GroupSharedCard', groupSharedCardSchema);

export default GroupSharedCard;