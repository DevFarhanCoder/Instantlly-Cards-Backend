"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const messageSchema = new mongoose_1.default.Schema({
    sender: {
        type: mongoose_1.default.Schema.Types.ObjectId,
        ref: 'User',
        required: true
    },
    receiver: {
        type: mongoose_1.default.Schema.Types.ObjectId,
        ref: 'User',
        required: false // Not required for group messages
    },
    groupId: {
        type: mongoose_1.default.Schema.Types.ObjectId,
        ref: 'Group',
        required: false
    },
    content: {
        type: String,
        required: true,
        trim: true
    },
    messageType: {
        type: String,
        enum: ['text', 'image', 'file', 'location'],
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
        required: false, // Not required for group messages
        index: true
    },
    isDeleted: {
        type: Boolean,
        default: false
    },
    deletedAt: {
        type: Date
    },
    readBy: [{
            userId: {
                type: mongoose_1.default.Schema.Types.ObjectId,
                ref: 'User'
            },
            readAt: {
                type: Date,
                default: Date.now
            }
        }],
    metadata: {
        fileName: String,
        fileSize: Number,
        fileUrl: String,
        latitude: Number,
        longitude: Number
    }
}, {
    timestamps: true
});
// Indexes for better performance
messageSchema.index({ conversationId: 1, createdAt: -1 });
messageSchema.index({ sender: 1, receiver: 1 });
messageSchema.index({ groupId: 1, createdAt: -1 });
messageSchema.index({ sender: 1, createdAt: -1 });
messageSchema.index({ isDeleted: 1 });
// Auto-delete messages older than 15 days (15 * 24 * 60 * 60 seconds)
messageSchema.index({ createdAt: 1 }, { expireAfterSeconds: 15 * 24 * 60 * 60 });
// Generate conversation ID based on participant IDs
messageSchema.statics.getConversationId = function (userId1, userId2) {
    return [userId1, userId2].sort().join('-');
};
// Get conversation between two users
messageSchema.statics.getConversation = function (userId1, userId2, limit = 50, page = 1) {
    const skip = (page - 1) * limit;
    return this.find({
        $or: [
            { sender: userId1, receiver: userId2 },
            { sender: userId2, receiver: userId1 }
        ],
        isDeleted: false
    })
        .sort({ createdAt: -1 })
        .limit(limit)
        .skip(skip)
        .populate('sender', 'name profilePicture email phone')
        .populate('receiver', 'name profilePicture email phone');
};
// Get group messages
messageSchema.statics.getGroupMessages = function (groupId, limit = 50, page = 1) {
    const skip = (page - 1) * limit;
    return this.find({
        groupId,
        isDeleted: false
    })
        .sort({ createdAt: -1 })
        .limit(limit)
        .skip(skip)
        .populate('sender', 'name profilePicture email phone');
};
// Get user's recent conversations
messageSchema.statics.getRecentConversations = function (userId) {
    return this.aggregate([
        {
            $match: {
                $or: [
                    { sender: new mongoose_1.default.Types.ObjectId(userId) },
                    { receiver: new mongoose_1.default.Types.ObjectId(userId) }
                ],
                isDeleted: false,
                receiver: { $exists: true, $ne: null } // Only private messages
            }
        },
        {
            $sort: { createdAt: -1 }
        },
        {
            $group: {
                _id: {
                    $cond: [
                        { $eq: ["$sender", new mongoose_1.default.Types.ObjectId(userId)] },
                        "$receiver",
                        "$sender"
                    ]
                },
                lastMessage: { $first: "$$ROOT" },
                unreadCount: {
                    $sum: {
                        $cond: [
                            {
                                $and: [
                                    { $ne: ["$sender", new mongoose_1.default.Types.ObjectId(userId)] },
                                    { $eq: ["$isRead", false] }
                                ]
                            },
                            1,
                            0
                        ]
                    }
                }
            }
        },
        {
            $lookup: {
                from: 'users',
                localField: '_id',
                foreignField: '_id',
                as: 'otherUser'
            }
        },
        {
            $unwind: '$otherUser'
        },
        {
            $sort: { 'lastMessage.createdAt': -1 }
        }
    ]);
};
// Get user's recent groups
messageSchema.statics.getRecentGroups = async function (userId) {
    // First get groups where user is a member
    const groups = await mongoose_1.default.model('Group').find({
        members: new mongoose_1.default.Types.ObjectId(userId)
    }).populate('members', 'name phone profilePicture')
        .populate('admin', 'name phone');
    const result = [];
    for (const group of groups) {
        // Get the latest message for this group
        const latestMessage = await this.findOne({
            groupId: group._id,
            isDeleted: false
        }).populate('sender', 'name profilePicture')
            .sort({ createdAt: -1 });
        // Get unread count for this user in this group
        const unreadCount = await this.countDocuments({
            groupId: group._id,
            sender: { $ne: new mongoose_1.default.Types.ObjectId(userId) },
            isDeleted: false,
            'readBy.userId': { $ne: new mongoose_1.default.Types.ObjectId(userId) }
        });
        result.push({
            _id: group._id,
            group: {
                _id: group._id,
                name: group.name,
                description: group.description,
                icon: group.icon,
                members: group.members.length,
                admin: group.admin,
                joinCode: group.joinCode,
                createdAt: group.createdAt
            },
            lastMessage: latestMessage ? {
                _id: latestMessage._id,
                content: latestMessage.content,
                messageType: latestMessage.messageType,
                timestamp: latestMessage.createdAt,
                senderId: latestMessage.sender?._id,
                sender: latestMessage.sender
            } : null,
            unreadCount
        });
    }
    // Sort by latest message time
    return result.sort((a, b) => {
        const aTime = a.lastMessage?.timestamp || a.group.createdAt;
        const bTime = b.lastMessage?.timestamp || b.group.createdAt;
        return new Date(bTime).getTime() - new Date(aTime).getTime();
    });
};
// Instance method to mark as read by user
messageSchema.methods.markAsRead = function (userId) {
    if (!this.readBy.some((r) => r.userId.toString() === userId)) {
        this.readBy.push({
            userId: new mongoose_1.default.Types.ObjectId(userId),
            readAt: new Date()
        });
        // For private messages, update isRead flag
        if (!this.groupId && this.receiver && this.receiver.toString() === userId) {
            this.isRead = true;
            this.readAt = new Date();
        }
    }
    return this.save();
};
// Virtual for formatted timestamp
messageSchema.virtual('formattedTime').get(function () {
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
    }
    else if (diffHours < 168) { // Less than a week
        return messageTime.toLocaleDateString('en-US', { weekday: 'short' });
    }
    else {
        return messageTime.toLocaleDateString('en-US', {
            month: 'short',
            day: 'numeric'
        });
    }
});
exports.default = mongoose_1.default.model('Message', messageSchema);
