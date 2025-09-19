import mongoose from 'mongoose';

interface IMessage extends mongoose.Document {
  sender: mongoose.Types.ObjectId;
  receiver: mongoose.Types.ObjectId;
  content: string;
  messageType: 'text' | 'image' | 'file';
  isRead: boolean;
  readAt?: Date;
  conversationId: string;
  createdAt: Date;
  updatedAt: Date;
  isDelivered: boolean;
  deliveredAt?: Date;
  isPendingDelivery: boolean;
  localMessageId?: string;
}

interface IMessageModel extends mongoose.Model<IMessage> {
  getConversationId(userId1: string, userId2: string): string;
}

const messageSchema = new mongoose.Schema({
  sender: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User',
    required: true
  },
  receiver: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User',
    required: true
  },
  content: {
    type: String,
    required: true,
    trim: true
  },
  messageType: {
    type: String,
    enum: ['text', 'image', 'file'],
    default: 'text'
  },
  isRead: {
    type: Boolean,
    default: false
  },
  readAt: {
    type: Date
  },
  isDelivered: {
    type: Boolean,
    default: false
  },
  deliveredAt: {
    type: Date
  },
  isPendingDelivery: {
    type: Boolean,
    default: false
  },
  localMessageId: {
    type: String
  },
  conversationId: {
    type: String,
    required: true,
    index: true
  }
}, {
  timestamps: true
});

// Index for efficient conversation queries
messageSchema.index({ conversationId: 1, createdAt: -1 });
messageSchema.index({ sender: 1, receiver: 1 });

// Generate conversation ID based on participant IDs
messageSchema.statics.getConversationId = function(userId1: string, userId2: string) {
  return [userId1, userId2].sort().join('-');
};

// Virtual for formatted timestamp
messageSchema.virtual('formattedTime').get(function() {
  const now = new Date();
  const messageTime = this.createdAt;
  const diffMs = now.getTime() - messageTime.getTime();
  const diffHours = diffMs / (1000 * 60 * 60);
  
  if (diffHours < 24) {
    return messageTime.toLocaleTimeString('en-US', { 
      hour: '2-digit', 
      minute: '2-digit',
      hour12: true 
    });
  } else if (diffHours < 168) { // Less than a week
    return messageTime.toLocaleDateString('en-US', { weekday: 'short' });
  } else {
    return messageTime.toLocaleDateString('en-US', { 
      month: 'short', 
      day: 'numeric' 
    });
  }
});

export default mongoose.model<IMessage, IMessageModel>('Message', messageSchema);
