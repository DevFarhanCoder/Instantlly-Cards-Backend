import mongoose from 'mongoose';

interface IChat extends mongoose.Document {
  participants: mongoose.Types.ObjectId[];
  lastMessage?: mongoose.Types.ObjectId;
  lastMessageTime: Date;
  isGroup: boolean;
  groupId?: mongoose.Types.ObjectId;
  unreadCount: Map<string, number>;
  isActive: boolean;
  mutedBy: mongoose.Types.ObjectId[];
  createdAt: Date;
  updatedAt: Date;
  incrementUnreadCount(userId: string): Promise<IChat>;
}

interface IChatModel extends mongoose.Model<IChat> {
  findOrCreateConversation(userId1: string, userId2: string): Promise<IChat>;
  updateLastMessage(chatId: string, messageId: string): Promise<IChat>;
  markAsRead(chatId: string, userId: string): Promise<IChat>;
}

const chatSchema = new mongoose.Schema({
  participants: [{
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User',
    required: true
  }],
  lastMessage: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'Message'
  },
  lastMessageTime: {
    type: Date,
    default: Date.now
  },
  isGroup: {
    type: Boolean,
    default: false
  },
  groupId: {
    type: mongoose.Schema.Types.ObjectId,
    ref: 'Group'
  },
  unreadCount: {
    type: Map,
    of: Number,
    default: new Map()
  },
  isActive: {
    type: Boolean,
    default: true
  },
  mutedBy: [{
    type: mongoose.Schema.Types.ObjectId,
    ref: 'User'
  }]
}, {
  timestamps: true
});

// Indexes
chatSchema.index({ participants: 1 });
chatSchema.index({ lastMessageTime: -1 });
chatSchema.index({ isGroup: 1, isActive: 1 });
chatSchema.index({ groupId: 1 });

// Find or create conversation between two users
chatSchema.statics.findOrCreateConversation = async function(userId1: string, userId2: string) {
  const participants = [
    new mongoose.Types.ObjectId(userId1),
    new mongoose.Types.ObjectId(userId2)
  ].sort();

  let chat = await this.findOne({
    participants: { $all: participants },
    isGroup: false
  });

  if (!chat) {
    chat = await this.create({
      participants,
      isGroup: false,
      unreadCount: new Map([
        [userId1, 0],
        [userId2, 0]
      ])
    });
  }

  return chat;
};

// Update last message
chatSchema.statics.updateLastMessage = async function(chatId: string, messageId: string) {
  return this.findByIdAndUpdate(
    chatId,
    {
      lastMessage: new mongoose.Types.ObjectId(messageId),
      lastMessageTime: new Date()
    },
    { new: true }
  );
};

// Mark as read for a user
chatSchema.statics.markAsRead = async function(chatId: string, userId: string) {
  const chat = await this.findById(chatId);
  if (chat) {
    chat.unreadCount.set(userId, 0);
    await chat.save();
  }
  return chat;
};

// Instance method to increment unread count
chatSchema.methods.incrementUnreadCount = function(userId: string) {
  const currentCount = this.unreadCount.get(userId) || 0;
  this.unreadCount.set(userId, currentCount + 1);
  return this.save();
};

export default mongoose.model<IChat, IChatModel>('Chat', chatSchema);