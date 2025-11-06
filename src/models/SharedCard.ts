import mongoose from "mongoose";

const sharedCardSchema = new mongoose.Schema({
  // Card being shared
  cardId: { 
    type: mongoose.Schema.Types.ObjectId, 
    ref: 'Card', 
    required: true 
  },
  
  // Who sent the card (supports both ObjectId and string userIds)
  senderId: { 
    type: String,  // Changed from ObjectId to String to support temporary userIds
    required: true 
  },
  
  // Who received the card (supports both ObjectId and string userIds)
  recipientId: { 
    type: String,  // Changed from ObjectId to String to support temporary userIds
    required: true 
  },
  
  // Optional message with the card
  message: { 
    type: String, 
    maxlength: 500,
    default: "" 
  },
  
  // Delivery status
  status: { 
    type: String, 
    enum: ['sent', 'delivered', 'viewed'], 
    default: 'sent' 
  },
  
  // When the card was shared
  sentAt: { 
    type: Date, 
    default: Date.now 
  },
  
  // When the recipient viewed the card
  viewedAt: { 
    type: Date 
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
  
  // Cache recipient info
  recipientName: { 
    type: String, 
    required: true 
  },
  
}, {
  timestamps: true // Adds createdAt and updatedAt
});

// Indexes for efficient queries
sharedCardSchema.index({ senderId: 1, sentAt: -1 }); // For sent cards query
sharedCardSchema.index({ recipientId: 1, sentAt: -1 }); // For received cards query
sharedCardSchema.index({ cardId: 1 }); // For card lookup
sharedCardSchema.index({ status: 1 }); // For status filtering

const SharedCard = mongoose.model('SharedCard', sharedCardSchema);

export default SharedCard;