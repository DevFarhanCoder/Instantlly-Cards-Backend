import { Router, Request, Response } from 'express';
import mongoose from 'mongoose';
import User from '../models/User';
import TempMessage from '../models/TempMessage';
import { requireAuth, AuthReq } from '../middleware/auth';
import { sendIndividualMessageNotification, sendGroupMessageNotification } from '../services/pushNotifications';

const router = Router();

// Note: Messages are NOT stored in database - only notifications and typing effects are handled

// Send message to individual user (stores temporarily for notifications)
router.post('/send', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { receiverId, text, messageId } = req.body;
    const senderId = req.userId;

    if (!receiverId || !text || !messageId || !senderId) {
      return res.status(400).json({ 
        error: 'Receiver ID, message text, and message ID are required' 
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

    // Store message temporarily for notification delivery tracking
    try {
      await TempMessage.create({
        senderId,
        receiverId,
        text: text.trim(),
        messageId,
        isDelivered: false
      });
      
      console.log(`� Message stored temporarily for notification tracking: ${messageId}`);
    } catch (dbError) {
      console.error('Failed to store temp message:', dbError);
      // Continue with notification even if storage fails
    }

    console.log(`📱 Sending message from ${sender.name} to ${receiver.name}: ${text}`);

    // Send push notification to the receiver if they have a push token
    if (receiver.pushToken && receiver.pushToken !== 'expo-go-local-mode') {
      try {
        await sendIndividualMessageNotification(
          receiver.pushToken,
          sender.name,
          text,
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
      message: 'Message sent successfully',
      messageId,
      receiverName: receiver.name,
      senderName: sender.name,
      timestamp: new Date().toISOString(),
      note: 'Message stored temporarily for 15 days for notification purposes only'
    });
  } catch (error) {
    console.error('Send message error:', error);
    res.status(500).json({ error: 'Failed to send message' });
  }
});

// Send group message (stores temporarily for notifications)
router.post('/send-group', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { groupId, text, messageId } = req.body;
    const senderId = req.userId;

    if (!groupId || !text || !messageId || !senderId) {
      return res.status(400).json({ 
        error: 'Group ID, message text, and message ID are required' 
      });
    }

    const sender = await User.findById(senderId);
    if (!sender) {
      return res.status(404).json({ error: 'Sender not found' });
    }

    // Store message temporarily for notification delivery tracking
    try {
      await TempMessage.create({
        senderId,
        groupId,
        text: text.trim(),
        messageId,
        isDelivered: false
      });
      
      console.log(`� Group message stored temporarily for notification tracking: ${messageId}`);
    } catch (dbError) {
      console.error('Failed to store temp group message:', dbError);
      // Continue with notification even if storage fails
    }

    console.log(`📱 Sending group message from ${sender.name} to group ${groupId}: ${text}`);

    res.status(200).json({
      success: true,
      message: 'Group message sent successfully',
      messageId,
      senderName: sender.name,
      timestamp: new Date().toISOString(),
      note: 'Message stored temporarily for 15 days for notification purposes only'
    });
  } catch (error) {
    console.error('Send group message error:', error);
    res.status(500).json({ error: 'Failed to send group message' });
  }
});

// Get online status of users (for real-time features)
router.post('/check-online-status', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userIds } = req.body;
    const currentUserId = req.userId;

    if (!userIds || !Array.isArray(userIds)) {
      return res.status(400).json({ error: 'User IDs array is required' });
    }

    // For simplicity, we'll just return basic user info
    // In a real implementation, you might track last seen timestamps
    const users = await User.find({
      _id: { $in: userIds }
    }).select('_id name profilePicture updatedAt');

    const onlineStatus = users.map(user => ({
      userId: user._id,
      name: user.name,
      profilePicture: user.profilePicture,
      isOnline: false, // You can implement real online tracking here
      lastSeen: user.updatedAt
    }));

    res.json({
      success: true,
      onlineStatus
    });
  } catch (error) {
    console.error('Check online status error:', error);
    res.status(500).json({ error: 'Failed to check online status' });
  }
});

// Store for typing status (in-memory for now - not stored in database)
const typingStatus = new Map<string, { isTyping: boolean, lastUpdate: Date }>();

// Set typing status for individual chat
router.post('/typing-status/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const { isTyping } = req.body;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // Generate conversation ID (sorted user IDs)
    const conversationId = [currentUserId, userId].sort().join('-');
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
      message: 'Typing status updated',
      conversationId
    });
  } catch (error) {
    console.error('Set typing status error:', error);
    res.status(500).json({ error: 'Failed to set typing status' });
  }
});

// Get typing status for individual chat
router.get('/typing-status/:userId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    const conversationId = [currentUserId, userId].sort().join('-');
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
      userId,
      conversationId
    });
  } catch (error) {
    console.error('Get typing status error:', error);
    res.status(500).json({ error: 'Failed to get typing status' });
  }
});

// Set typing status for group chat
router.post('/group-typing-status/:groupId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { groupId } = req.params;
    const { isTyping } = req.body;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    const key = `group-${groupId}-${currentUserId}`;

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

    console.log(`💬 User ${currentUserId} typing status: ${isTyping} in group ${groupId}`);

    res.json({
      success: true,
      message: 'Group typing status updated',
      groupId
    });
  } catch (error) {
    console.error('Set group typing status error:', error);
    res.status(500).json({ error: 'Failed to set group typing status' });
  }
});

// Get typing status for group chat
router.get('/group-typing-status/:groupId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { groupId } = req.params;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // Find all users typing in this group (excluding current user)
    const now = new Date();
    const typingUsers: string[] = [];

    for (const [statusKey, status] of typingStatus.entries()) {
      if (statusKey.startsWith(`group-${groupId}-`) && 
          statusKey !== `group-${groupId}-${currentUserId}` &&
          status.isTyping && 
          (now.getTime() - status.lastUpdate.getTime() < 10000)) {
        
        const userId = statusKey.split('-').pop();
        if (userId) {
          typingUsers.push(userId);
        }
      }
    }

    // Clean up old statuses
    for (const [statusKey, status] of typingStatus.entries()) {
      if (now.getTime() - status.lastUpdate.getTime() > 10000) {
        typingStatus.delete(statusKey);
      }
    }

    res.json({
      success: true,
      typingUsers,
      groupId,
      isAnyoneTyping: typingUsers.length > 0
    });
  } catch (error) {
    console.error('Get group typing status error:', error);
    res.status(500).json({ error: 'Failed to get group typing status' });
  }
});

// GET /check-all-pending - Get undelivered messages for current user
router.get('/check-all-pending', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const currentUserId = req.userId;
    
    if (!currentUserId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    // Find all undelivered messages for this user
    const pendingMessages = await TempMessage.find({
      receiverId: currentUserId,
      isDelivered: false
    }).populate('senderId', 'name profilePicture').sort({ createdAt: 1 });

    // Group messages by sender
    const messagesBySender: any[] = [];
    const senderMap = new Map();

    for (const message of pendingMessages) {
      const senderData = message.senderId as any;
      const senderId = senderData._id.toString();
      
      if (!senderMap.has(senderId)) {
        senderMap.set(senderId, {
          senderId,
          senderName: senderData.name,
          senderProfilePicture: senderData.profilePicture,
          messages: []
        });
        messagesBySender.push(senderMap.get(senderId));
      }
      
      senderMap.get(senderId).messages.push({
        id: message.messageId,
        backendMessageId: message._id,
        text: message.text,
        timestamp: message.createdAt,
        messageId: message.messageId
      });
    }

    // Only log if there are pending messages to avoid spam
    if (pendingMessages.length > 0) {
      console.log(`📬 Found ${pendingMessages.length} pending messages for user ${currentUserId} from ${messagesBySender.length} senders`);
    }

    res.status(200).json({
      success: true,
      messagesBySender,
      totalPendingMessages: pendingMessages.length,
      note: 'Messages will auto-delete after 15 days'
    });
  } catch (error) {
    console.error('Error in check-all-pending:', error);
    res.status(500).json({ error: 'Failed to check pending messages' });
  }
});

// Mark messages as delivered
router.post('/mark-delivered', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { messageIds, senderId } = req.body;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    if (!messageIds || !Array.isArray(messageIds)) {
      return res.status(400).json({ error: 'Message IDs array is required' });
    }

    // Mark messages as delivered
    const result = await TempMessage.updateMany(
      {
        receiverId: currentUserId,
        messageId: { $in: messageIds },
        isDelivered: false
      },
      {
        $set: {
          isDelivered: true,
          deliveredAt: new Date()
        }
      }
    );

    console.log(`📬 Marked ${result.modifiedCount} messages as delivered for user ${currentUserId}`);

    res.json({
      success: true,
      markedCount: result.modifiedCount,
      message: 'Messages marked as delivered'
    });
  } catch (error) {
    console.error('Mark delivered error:', error);
    res.status(500).json({ error: 'Failed to mark messages as delivered' });
  }
});

// Clean up expired messages manually (MongoDB TTL should handle this automatically)
router.delete('/cleanup-expired', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const now = new Date();
    const result = await TempMessage.deleteMany({
      expiresAt: { $lt: now }
    });

    console.log(`🧹 Cleaned up ${result.deletedCount} expired messages`);

    res.json({
      success: true,
      deletedCount: result.deletedCount,
      message: 'Expired messages cleaned up'
    });
  } catch (error) {
    console.error('Cleanup error:', error);
    res.status(500).json({ error: 'Failed to cleanup expired messages' });
  }
});

// Add endpoint to clear typing statuses (useful for debugging)
router.delete('/typing-status/clear', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const currentUserId = req.userId;
    
    if (!currentUserId) {
      return res.status(401).json({ error: 'Unauthorized' });
    }

    // Clear all typing statuses for current user
    for (const [key] of typingStatus.entries()) {
      if (key.includes(currentUserId)) {
        typingStatus.delete(key);
      }
    }

    res.json({
      success: true,
      message: 'Typing statuses cleared'
    });
  } catch (error) {
    console.error('Clear typing status error:', error);
    res.status(500).json({ error: 'Failed to clear typing status' });
  }
});

// GET /check-group-pending/:groupId - Get undelivered messages for a specific group
router.get('/check-group-pending/:groupId', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const currentUserId = req.userId;
    const { groupId } = req.params;
    
    if (!currentUserId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    if (!groupId) {
      return res.status(400).json({ error: 'Group ID is required' });
    }

    // Check if user is member of this group
    const Group = mongoose.model('Group');
    const group = await Group.findById(groupId);
    if (!group || !group.members.includes(currentUserId)) {
      return res.status(403).json({ error: 'Access denied' });
    }

    // Find all undelivered messages for this group (excluding messages sent by current user)
    const pendingMessages = await TempMessage.find({
      groupId: groupId,
      senderId: { $ne: currentUserId }, // Exclude messages sent by current user
      isDelivered: false
    }).populate('senderId', 'name profilePicture').sort({ createdAt: 1 });

    const messages = pendingMessages.map(message => {
      const senderData = message.senderId as any;
      return {
        id: message.messageId,
        backendMessageId: message._id,
        senderId: senderData._id.toString(),
        senderName: senderData.name,
        senderProfilePicture: senderData.profilePicture,
        text: message.text,
        timestamp: message.createdAt,
        messageId: message.messageId,
        groupId: message.groupId
      };
    });

    // Only log if there are pending messages to avoid spam
    if (pendingMessages.length > 0) {
      console.log(`📬 Found ${pendingMessages.length} pending group messages for user ${currentUserId} in group ${groupId}`);
    }

    res.status(200).json({
      success: true,
      messages,
      totalPendingMessages: pendingMessages.length,
      groupId,
      note: 'Messages will auto-delete after 15 days'
    });
  } catch (error) {
    console.error('Error in check-group-pending:', error);
    res.status(500).json({ error: 'Failed to check pending group messages' });
  }
});

// Mark group messages as delivered
router.post('/mark-group-delivered', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { messageIds, groupId } = req.body;
    const currentUserId = req.userId;

    if (!currentUserId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    if (!messageIds || !Array.isArray(messageIds) || !groupId) {
      return res.status(400).json({ error: 'Message IDs array and group ID are required' });
    }

    // Mark group messages as delivered
    const result = await TempMessage.updateMany(
      {
        groupId: groupId,
        messageId: { $in: messageIds },
        isDelivered: false
      },
      {
        $set: {
          isDelivered: true,
          deliveredAt: new Date()
        }
      }
    );

    console.log(`📬 Marked ${result.modifiedCount} group messages as delivered for group ${groupId}`);

    res.json({
      success: true,
      markedCount: result.modifiedCount,
      message: 'Group messages marked as delivered'
    });
  } catch (error) {
    console.error('Mark group delivered error:', error);
    res.status(500).json({ error: 'Failed to mark group messages as delivered' });
  }
});

export default router;
