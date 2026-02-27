"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const mongoose_1 = __importDefault(require("mongoose"));
const Message_1 = __importDefault(require("../models/Message"));
const Chat_1 = __importDefault(require("../models/Chat"));
const Group_1 = __importDefault(require("../models/Group"));
const auth_1 = require("../middleware/auth");
const router = express_1.default.Router();
// Get conversations list for a user
router.get('/conversations', auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        // Get recent private conversations
        const conversations = await Message_1.default.getRecentConversations(userId);
        res.json({
            success: true,
            conversations
        });
    }
    catch (error) {
        console.error('Error fetching conversations:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to fetch conversations'
        });
    }
});
// Get conversation messages between two users
router.get('/conversation/:userId', auth_1.requireAuth, async (req, res) => {
    try {
        const currentUserId = req.userId;
        const otherUserId = req.params.userId;
        const page = parseInt(req.query.page) || 1;
        const limit = parseInt(req.query.limit) || 50;
        const messages = await Message_1.default.getConversation(currentUserId, otherUserId, limit, page);
        // Mark messages as read
        await Message_1.default.updateMany({
            sender: otherUserId,
            receiver: currentUserId,
            isRead: false
        }, {
            isRead: true,
            readAt: new Date()
        });
        res.json({
            success: true,
            messages: messages.reverse(), // Reverse to show oldest first
            page,
            hasMore: messages.length === limit
        });
    }
    catch (error) {
        console.error('Error fetching conversation:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to fetch conversation'
        });
    }
});
// Get user's groups list
router.get('/groups', auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        // Get recent groups
        const groups = await Message_1.default.getRecentGroups(userId);
        res.json({
            success: true,
            groups
        });
    }
    catch (error) {
        console.error('Error fetching groups:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to fetch groups'
        });
    }
});
// Get group messages
router.get('/group/:groupId', auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const groupId = req.params.groupId;
        const page = parseInt(req.query.page) || 1;
        const limit = parseInt(req.query.limit) || 50;
        // Verify user is member of group
        const group = await Group_1.default.findById(groupId);
        if (!group || !group.members.includes(new mongoose_1.default.Types.ObjectId(userId))) {
            return res.status(403).json({
                success: false,
                error: 'You are not a member of this group'
            });
        }
        const messages = await Message_1.default.getGroupMessages(groupId, limit, page);
        // Mark messages as read by this user
        const unreadMessages = messages.filter(msg => msg.sender.toString() !== userId &&
            !msg.readBy.some((r) => r.userId.toString() === userId));
        for (const message of unreadMessages) {
            if (message.markAsRead) {
                await message.markAsRead(userId);
            }
        }
        res.json({
            success: true,
            messages: messages.reverse(), // Reverse to show oldest first
            page,
            hasMore: messages.length === limit,
            group: {
                _id: group._id,
                name: group.name,
                description: group.description,
                icon: group.icon,
                members: group.members.length,
                admin: group.admin
            }
        });
    }
    catch (error) {
        console.error('Error fetching group messages:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to fetch group messages'
        });
    }
});
// Send a message (fallback for non-socket clients)
router.post('/send', auth_1.requireAuth, async (req, res) => {
    try {
        const { receiverId, groupId, content, messageType = 'text', metadata } = req.body;
        const senderId = req.userId;
        if (!content) {
            return res.status(400).json({
                success: false,
                error: 'Message content is required'
            });
        }
        if (!receiverId && !groupId) {
            return res.status(400).json({
                success: false,
                error: 'Either receiverId or groupId is required'
            });
        }
        const messageData = {
            sender: senderId,
            content,
            messageType,
            metadata
        };
        if (receiverId) {
            // Private message
            messageData.receiver = receiverId;
            const chat = await Chat_1.default.findOrCreateConversation(senderId, receiverId);
            messageData.conversationId = chat._id?.toString();
        }
        else if (groupId) {
            // Group message
            const group = await Group_1.default.findById(groupId);
            if (!group || !group.members.includes(new mongoose_1.default.Types.ObjectId(senderId))) {
                return res.status(403).json({
                    success: false,
                    error: 'You are not a member of this group'
                });
            }
            messageData.groupId = groupId;
        }
        const message = new Message_1.default(messageData);
        await message.save();
        await message.populate('sender', 'name profilePicture email');
        res.json({
            success: true,
            message
        });
    }
    catch (error) {
        console.error('Error sending message:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to send message'
        });
    }
});
// Mark message as read
router.put('/read/:messageId', auth_1.requireAuth, async (req, res) => {
    try {
        const messageId = req.params.messageId;
        const userId = req.userId;
        const message = await Message_1.default.findById(messageId);
        if (!message) {
            return res.status(404).json({
                success: false,
                error: 'Message not found'
            });
        }
        if (message.markAsRead) {
            await message.markAsRead(userId);
        }
        res.json({
            success: true,
            message: 'Message marked as read'
        });
    }
    catch (error) {
        console.error('Error marking message as read:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to mark message as read'
        });
    }
});
// Delete message
router.delete('/:messageId', auth_1.requireAuth, async (req, res) => {
    try {
        const messageId = req.params.messageId;
        const userId = req.userId;
        const message = await Message_1.default.findById(messageId);
        if (!message) {
            return res.status(404).json({
                success: false,
                error: 'Message not found'
            });
        }
        // Only sender can delete message
        if (message.sender.toString() !== userId) {
            return res.status(403).json({
                success: false,
                error: 'You can only delete your own messages'
            });
        }
        message.isDeleted = true;
        message.deletedAt = new Date();
        await message.save();
        res.json({
            success: true,
            message: 'Message deleted successfully'
        });
    }
    catch (error) {
        console.error('Error deleting message:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to delete message'
        });
    }
});
// Search messages
router.get('/search', auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const query = req.query.q;
        const type = req.query.type; // 'private' or 'group'
        const page = parseInt(req.query.page) || 1;
        const limit = parseInt(req.query.limit) || 20;
        if (!query) {
            return res.status(400).json({
                success: false,
                error: 'Search query is required'
            });
        }
        const searchFilter = {
            content: { $regex: query, $options: 'i' },
            isDeleted: false,
            $or: [
                { sender: userId },
                { receiver: userId }
            ]
        };
        if (type === 'private') {
            searchFilter.receiver = { $exists: true };
        }
        else if (type === 'group') {
            searchFilter.groupId = { $exists: true };
            // Add group membership check here if needed
        }
        const messages = await Message_1.default.find(searchFilter)
            .sort({ createdAt: -1 })
            .limit(limit)
            .skip((page - 1) * limit)
            .populate('sender', 'name profilePicture')
            .populate('receiver', 'name profilePicture')
            .populate('groupId', 'name icon');
        res.json({
            success: true,
            messages,
            page,
            hasMore: messages.length === limit
        });
    }
    catch (error) {
        console.error('Error searching messages:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to search messages'
        });
    }
});
// Get unread message count
router.get('/unread-count', auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        // Count unread private messages
        const privateUnread = await Message_1.default.countDocuments({
            receiver: userId,
            isRead: false,
            isDeleted: false
        });
        // Count unread group messages
        const groupUnread = await Message_1.default.countDocuments({
            groupId: { $exists: true },
            sender: { $ne: userId },
            isDeleted: false,
            'readBy.userId': { $ne: userId }
        });
        res.json({
            success: true,
            privateUnread,
            groupUnread,
            totalUnread: privateUnread + groupUnread
        });
    }
    catch (error) {
        console.error('Error getting unread count:', error);
        res.status(500).json({
            success: false,
            error: 'Failed to get unread count'
        });
    }
});
exports.default = router;
