"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const chatSchema = new mongoose_1.default.Schema({
    participants: [{
            type: mongoose_1.default.Schema.Types.ObjectId,
            ref: 'User',
            required: true
        }],
    lastMessage: {
        type: mongoose_1.default.Schema.Types.ObjectId,
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
        type: mongoose_1.default.Schema.Types.ObjectId,
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
            type: mongoose_1.default.Schema.Types.ObjectId,
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
chatSchema.statics.findOrCreateConversation = async function (userId1, userId2) {
    const participants = [
        new mongoose_1.default.Types.ObjectId(userId1),
        new mongoose_1.default.Types.ObjectId(userId2)
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
chatSchema.statics.updateLastMessage = async function (chatId, messageId) {
    return this.findByIdAndUpdate(chatId, {
        lastMessage: new mongoose_1.default.Types.ObjectId(messageId),
        lastMessageTime: new Date()
    }, { new: true });
};
// Mark as read for a user
chatSchema.statics.markAsRead = async function (chatId, userId) {
    const chat = await this.findById(chatId);
    if (chat) {
        chat.unreadCount.set(userId, 0);
        await chat.save();
    }
    return chat;
};
// Instance method to increment unread count
chatSchema.methods.incrementUnreadCount = function (userId) {
    const currentCount = this.unreadCount.get(userId) || 0;
    this.unreadCount.set(userId, currentCount + 1);
    return this.save();
};
exports.default = mongoose_1.default.model('Chat', chatSchema);
