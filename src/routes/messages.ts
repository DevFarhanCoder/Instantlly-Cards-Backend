import { Router, Request, Response } from 'express';
import mongoose from 'mongoose';
import Message from '../models/Message';
import User from '../models/User';
import { requireAuth, AuthReq } from '../middleware/auth';
import { sendMessageNotification } from '../services/pushNotifications';

const router = Router();

// Helper function to generate conversation ID
const getConversationId = (userId1: string, userId2: string): string => {
  return [userId1, userId2].sort().join('-');
};

// Send a local message (P2P style - with temporary backend storage for delivery)
router.post('/send-local', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { receiverId, content, timestamp, messageId } = req.body;
    const senderId = req.userId;

    if (!receiverId || !content || !senderId) {
      return res.status(400).json({ 
        error: 'Receiver ID and content are required' 
      });
    }

    // Verify receiver exists
    const receiver = await User.findById(receiverId);
    if (!receiver) {
      return res.status(404).json({ error: 'Receiver not found' });
    }

    const sender = await User.findById(senderId);
    if (!sender) {
      return res.status(404).json({ error: 'Sender not found' });
    }

    // Store message temporarily for delivery (will be deleted after delivery)
    const message = new Message({
      _id: new mongoose.Types.ObjectId(),
      sender: senderId,
      receiver: receiverId,
      content: content.trim(),
      messageType: 'text',
      conversationId: getConversationId(senderId, receiverId),
      createdAt: new Date(timestamp),
      isDelivered: false, // Track delivery status
      isPendingDelivery: true, // Mark as pending for delivery
      localMessageId: messageId // Store the frontend message ID
    });

    await message.save();
    
    console.log(`Local message from ${sender.name} to ${receiver.name}: ${content}`);

    // Send push notification to the receiver if they have a push token
    if (receiver.pushToken && receiver.pushToken !== 'expo-go-local-mode') {
      try {
        await sendMessageNotification(
          receiver.pushToken,
          sender.name,
          content,
          senderId
        );
        console.log(`📱 Push notification sent to ${receiver.name}`);
      } catch (error) {
        console.error('Failed to send push notification:', error);
      }
    } else if (receiver.pushToken === 'expo-go-local-mode') {
      console.log(`📱 Receiver ${receiver.name} is using Expo Go - notification will be handled locally`);
    } else {
      console.log(`📱 No push token for ${receiver.name}, skipping push notification`);
    }

    res.status(200).json({
      success: true,
      message: 'Message queued for delivery',
      messageId,
      timestamp,
      receiverName: receiver.name,
      senderName: sender.name,
      backendMessageId: message._id
    });
  } catch (error) {
    console.error('Send local message error:', error);
    res.status(500).json({ error: 'Failed to send message' });
  }
});

// Send a new message (Original database-based messaging - kept for compatibility)
router.post('/send', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { receiverId, content, messageType = 'text' } = req.body;
    const senderId = req.userId; // Use userId from AuthReq

    if (!receiverId || !content || !senderId) {
      return res.status(400).json({ 
        error: 'Receiver ID and content are required' 
      });
    }

    // Verify receiver exists
    const receiver = await User.findById(receiverId);
    if (!receiver) {
      return res.status(404).json({ error: 'Receiver not found' });
    }

    // Generate conversation ID
    const conversationId = getConversationId(senderId, receiverId);

    // Create message
    const message = new Message({
      sender: senderId,
      receiver: receiverId,
      content: content.trim(),
      messageType,
      conversationId
    });

    await message.save();

    // Populate sender info for response
    await message.populate('sender', 'name phone profilePicture');
    await message.populate('receiver', 'name phone profilePicture');

    res.status(201).json({
      success: true,
      message,
      conversationId
    });
  } catch (error) {
    console.error('Send message error:', error);
    res.status(500).json({ error: 'Failed to send message' });
  }
});

// Check for pending messages (FIXED - Don't mark as delivered immediately)
router.get('/check-pending/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // Find messages pending delivery to the current user from the specified userId
    const pendingMessages = await Message.find({
      receiver: currentUserId,
      sender: userId,
      isPendingDelivery: true,
      isDelivered: false
    }).populate('sender', 'name phone profilePicture');

    // DON'T mark as delivered immediately - only return the messages
    console.log(`📥 Found ${pendingMessages.length} pending messages from ${userId} to ${currentUserId}`);

    // Format messages for frontend
    const formattedMessages = pendingMessages.map((msg: any) => ({
      id: msg.localMessageId || msg._id.toString(),
      text: msg.content,
      timestamp: msg.createdAt,
      isFromMe: false,
      status: 'pending',
      senderName: msg.sender.name,
      senderId: msg.sender._id.toString(),
      backendMessageId: msg._id.toString()
    }));
    
    res.json({
      success: true,
      messages: formattedMessages
    });
  } catch (error) {
    console.error('Check pending messages error:', error);
    res.status(500).json({ error: 'Failed to check pending messages' });
  }
});

// Check for all pending messages for current user (FIXED - Don't mark as delivered immediately)
router.get('/check-all-pending', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // Find all messages pending delivery to the current user
    const pendingMessages = await Message.find({
      receiver: currentUserId,
      isPendingDelivery: true,
      isDelivered: false
    }).populate('sender', 'name phone profilePicture');

    // Group messages by sender
    const messagesBySender = pendingMessages.reduce((acc: any, msg: any) => {
      const senderId = msg.sender._id.toString();
      if (!acc[senderId]) {
        acc[senderId] = {
          senderId,
          senderName: msg.sender.name,
          messages: []
        };
      }
      
      acc[senderId].messages.push({
        id: msg.localMessageId || msg._id.toString(),
        text: msg.content,
        timestamp: msg.createdAt,
        isFromMe: false,
        status: 'pending',
        backendMessageId: msg._id.toString()
      });
      
      return acc;
    }, {});

    // DON'T mark as delivered yet - only when specifically confirmed by the receiving device
    console.log(`📥 Found ${pendingMessages.length} pending messages for user ${currentUserId}`);
    
    res.json({
      success: true,
      messagesBySender: Object.values(messagesBySender),
      totalMessages: pendingMessages.length
    });
  } catch (error) {
    console.error('Check all pending messages error:', error);
    res.status(500).json({ error: 'Failed to check all pending messages' });
  }
});

// Update message status (mark as read, etc.)
router.put('/update-status/:messageId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { messageId } = req.params;
    const { status } = req.body;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    const updateData: any = {};
    
    if (status === 'read') {
      updateData.isRead = true;
      updateData.readAt = new Date();
    }

    await Message.findOneAndUpdate(
      {
        _id: messageId,
        receiver: currentUserId
      },
      updateData
    );

    res.json({
      success: true,
      message: 'Message status updated'
    });
  } catch (error) {
    console.error('Update message status error:', error);
    res.status(500).json({ error: 'Failed to update message status' });
  }
});

// Mark messages as delivered (called after the receiving device actually processes them)
router.post('/mark-delivered', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { messageIds } = req.body;
    const currentUserId = req.userId;

    if (!currentUserId || !messageIds || !Array.isArray(messageIds)) {
      return res.status(400).json({ error: 'User ID and message IDs are required' });
    }

    // Mark messages as delivered
    const result = await Message.updateMany(
      {
        receiver: currentUserId,
        _id: { $in: messageIds },
        isPendingDelivery: true,
        isDelivered: false
      },
      {
        isDelivered: true,
        deliveredAt: new Date(),
        isPendingDelivery: false
      }
    );

    console.log(`✅ Marked ${result.modifiedCount} messages as delivered for user ${currentUserId}`);
    
    res.json({
      success: true,
      markedCount: result.modifiedCount
    });
  } catch (error) {
    console.error('Mark delivered error:', error);
    res.status(500).json({ error: 'Failed to mark messages as delivered' });
  }
});

// Get conversation history between two users
router.get('/conversation/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const currentUserId = req.userId; // Use userId from AuthReq
    const { page = 1, limit = 50 } = req.query;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // Generate conversation ID
    const conversationId = getConversationId(currentUserId, userId);

    // Get messages with pagination
    const messages = await Message.find({ conversationId })
      .populate('sender', 'name phone profilePicture')
      .populate('receiver', 'name phone profilePicture')
      .sort({ createdAt: -1 })
      .limit(Number(limit))
      .skip((Number(page) - 1) * Number(limit));

    // Mark messages as read where current user is receiver
    await Message.updateMany(
      { 
        conversationId,
        receiver: currentUserId,
        isRead: false 
      },
      { 
        isRead: true,
        readAt: new Date()
      }
    );

    res.json({
      success: true,
      messages: messages.reverse(), // Reverse to show oldest first
      page: Number(page),
      hasMore: messages.length === Number(limit)
    });
  } catch (error) {
    console.error('Get conversation error:', error);
    res.status(500).json({ error: 'Failed to get conversation' });
  }
});

// Get all conversations for current user
router.get('/conversations', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const currentUserId = req.userId; // Use userId from AuthReq

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // Get all messages where user is sender or receiver
    const conversations = await Message.aggregate([
      {
        $match: {
          $or: [
            { sender: new mongoose.Types.ObjectId(currentUserId) },
            { receiver: new mongoose.Types.ObjectId(currentUserId) }
          ]
        }
      },
      {
        $sort: { createdAt: -1 }
      },
      {
        $group: {
          _id: "$conversationId",
          lastMessage: { $first: "$$ROOT" },
          unreadCount: {
            $sum: {
              $cond: [
                { 
                  $and: [
                    { $eq: ["$receiver", new mongoose.Types.ObjectId(currentUserId)] },
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
        $sort: { "lastMessage.createdAt": -1 }
      }
    ]);

    // Populate user details for each conversation
    const populatedConversations = await Promise.all(
      conversations.map(async (conv) => {
        const lastMessage = conv.lastMessage;
        
        // Determine the other participant
        const otherUserId = lastMessage.sender.toString() === currentUserId 
          ? lastMessage.receiver 
          : lastMessage.sender;

        const otherUser = await User.findById(otherUserId)
          .select('name phone profilePicture');

        return {
          conversationId: conv._id,
          otherUser,
          lastMessage: {
            content: lastMessage.content,
            createdAt: lastMessage.createdAt,
            sender: lastMessage.sender.toString(),
            messageType: lastMessage.messageType
          },
          unreadCount: conv.unreadCount
        };
      })
    );

    res.json({
      success: true,
      conversations: populatedConversations
    });
  } catch (error) {
    console.error('Get conversations error:', error);
    res.status(500).json({ error: 'Failed to get conversations' });
  }
});

// Mark messages as read
router.put('/mark-read/:conversationId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { conversationId } = req.params;
    const currentUserId = req.userId; // Use userId from AuthReq

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    await Message.updateMany(
      { 
        conversationId,
        receiver: currentUserId,
        isRead: false 
      },
      { 
        isRead: true,
        readAt: new Date()
      }
    );

    res.json({ success: true });
  } catch (error) {
    console.error('Mark read error:', error);
    res.status(500).json({ error: 'Failed to mark messages as read' });
  }
});

// Delete a message
router.delete('/:messageId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { messageId } = req.params;
    const currentUserId = req.userId; // Use userId from AuthReq

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    const message = await Message.findOne({
      _id: messageId,
      sender: currentUserId
    });

    if (!message) {
      return res.status(404).json({ 
        error: 'Message not found or you do not have permission to delete it' 
      });
    }

    await Message.findByIdAndDelete(messageId);

    res.json({ 
      success: true, 
      message: 'Message deleted successfully' 
    });
  } catch (error) {
    console.error('Delete message error:', error);
    res.status(500).json({ error: 'Failed to delete message' });
  }
});

// Search messages in a conversation
router.get('/search/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const { q } = req.query;
    const currentUserId = req.userId; // Use userId from AuthReq

    if (!q) {
      return res.status(400).json({ error: 'Search query is required' });
    }

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    const conversationId = getConversationId(currentUserId, userId);

    const messages = await Message.find({
      conversationId,
      content: { $regex: q, $options: 'i' }
    })
    .populate('sender', 'name phone profilePicture')
    .populate('receiver', 'name phone profilePicture')
    .sort({ createdAt: -1 })
    .limit(20);

    res.json({
      success: true,
      messages,
      query: q
    });
  } catch (error) {
    console.error('Search messages error:', error);
    res.status(500).json({ error: 'Failed to search messages' });
  }
});

// Store for typing status (in-memory for now)
const typingStatus = new Map<string, { isTyping: boolean, lastUpdate: Date }>();

// Set typing status
router.post('/typing-status/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const { isTyping } = req.body;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    const conversationId = getConversationId(currentUserId, userId);
    const key = `${conversationId}-${currentUserId}`;

    typingStatus.set(key, {
      isTyping: Boolean(isTyping),
      lastUpdate: new Date()
    });

    // Clean up old typing statuses (older than 10 seconds)
    const now = new Date();
    for (const [statusKey, status] of typingStatus.entries()) {
      if (now.getTime() - status.lastUpdate.getTime() > 10000) {
        typingStatus.delete(statusKey);
      }
    }

    console.log(`💬 User ${currentUserId} typing status: ${isTyping} in conversation with ${userId}`);

    res.json({
      success: true,
      message: 'Typing status updated'
    });
  } catch (error) {
    console.error('Set typing status error:', error);
    res.status(500).json({ error: 'Failed to set typing status' });
  }
});

// Get typing status
router.get('/typing-status/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    const conversationId = getConversationId(currentUserId, userId);
    const key = `${conversationId}-${userId}`;

    const status = typingStatus.get(key);
    
    // Clean up old typing statuses
    const now = new Date();
    if (status && now.getTime() - status.lastUpdate.getTime() > 10000) {
      typingStatus.delete(key);
    }

    const isTyping = status ? status.isTyping && (now.getTime() - status.lastUpdate.getTime() < 10000) : false;

    res.json({
      success: true,
      isTyping,
      userId
    });
  } catch (error) {
    console.error('Get typing status error:', error);
    res.status(500).json({ error: 'Failed to get typing status' });
  }
});

export default router;
