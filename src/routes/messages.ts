import { Router, Request, Response } from 'express';
import mongoose from 'mongoose';
import Message from '../models/Message';
import User from '../models/User';
import { requireAuth, AuthReq } from '../middleware/auth';

const router = Router();

// Helper function to generate conversation ID
const getConversationId = (userId1: string, userId2: string): string => {
  return [userId1, userId2].sort().join('-');
};

// Send a local message (P2P style - no database storage)
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

    // In a real implementation, you would send a push notification here
    // For now, we'll just return success to indicate the message was "sent"
    // The frontend will handle local storage
    
    console.log(`Local message from ${sender.name} to ${receiver.name}: ${content}`);

    res.status(200).json({
      success: true,
      message: 'Message sent locally',
      messageId,
      timestamp,
      receiverName: receiver.name,
      senderName: sender.name
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

// Check for pending messages (simulate message delivery)
router.get('/check-pending/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // In a real implementation, you would check for pending messages
    // For now, we'll return empty to indicate no pending messages
    // This would be replaced with a proper message queue or push notification system
    
    res.json({
      success: true,
      messages: [] // No pending messages for demo
    });
  } catch (error) {
    console.error('Check pending messages error:', error);
    res.status(500).json({ error: 'Failed to check pending messages' });
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

export default router;
