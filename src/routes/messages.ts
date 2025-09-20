import { Router, Request, Response } from 'express';
import User from '../models/User';
import { requireAuth, AuthReq } from '../middleware/auth';
import { sendIndividualMessageNotification, sendGroupMessageNotification } from '../services/pushNotifications';

const router = Router();

// Note: Messages are NOT stored in database - only notifications and typing effects are handled

// Send notification for individual message (no message storage)
router.post('/send-notification', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { receiverId, messagePreview, messageId } = req.body;
    const senderId = req.userId;

    if (!receiverId || !messagePreview || !senderId) {
      return res.status(400).json({ 
        error: 'Receiver ID and message preview are required' 
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

    console.log(`📱 Sending notification from ${sender.name} to ${receiver.name}: ${messagePreview}`);

    // Send push notification to the receiver if they have a push token
    if (receiver.pushToken && receiver.pushToken !== 'expo-go-local-mode') {
      try {
        await sendIndividualMessageNotification(
          receiver.pushToken,
          sender.name,
          messagePreview,
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
      message: 'Notification sent successfully',
      messageId,
      receiverName: receiver.name,
      senderName: sender.name,
      note: 'Message was not stored in database - only notification sent'
    });
  } catch (error) {
    console.error('Send notification error:', error);
    res.status(500).json({ error: 'Failed to send notification' });
  }
});

// Send notification for group message (no message storage)
router.post('/send-group-notification', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { groupName, groupId, memberIds, messagePreview, messageId } = req.body;
    const senderId = req.userId;

    if (!groupName || !memberIds || !Array.isArray(memberIds) || !messagePreview || !senderId) {
      return res.status(400).json({ 
        error: 'Group name, member IDs, and message preview are required' 
      });
    }

    const sender = await User.findById(senderId);
    if (!sender) {
      return res.status(404).json({ error: 'Sender not found' });
    }

    console.log(`📱 Sending group notification from ${sender.name} to group ${groupName}`);

    // Send notifications to all group members except sender
    const otherMemberIds = memberIds.filter((id: string) => id !== senderId);
    let notificationsSent = 0;
    
    for (const memberId of otherMemberIds) {
      try {
        const member = await User.findById(memberId);
        if (member?.pushToken && member.pushToken !== 'expo-go-local-mode') {
          await sendGroupMessageNotification(
            member.pushToken,
            groupName,
            sender.name,
            messagePreview,
            groupId || 'group-id-placeholder',
            senderId
          );
          console.log(`📱 Group notification sent to ${member.name}`);
          notificationsSent++;
        }
      } catch (error) {
        console.error(`Failed to send group notification to member ${memberId}:`, error);
      }
    }

    res.status(200).json({
      success: true,
      message: 'Group notifications sent successfully',
      messageId,
      senderName: sender.name,
      notificationsSent,
      note: 'Message was not stored in database - only notifications sent'
    });
  } catch (error) {
    console.error('Send group notification error:', error);
    res.status(500).json({ error: 'Failed to send group notifications' });
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

// GET /check-all-pending - Since messages aren't stored, return empty
router.get('/check-all-pending', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    res.status(200).json({
      success: true,
      message: 'No pending messages - messages are not stored in database',
      pendingMessages: []
    });
  } catch (error) {
    console.error('Error in check-all-pending:', error);
    res.status(500).json({ error: 'Failed to check pending messages' });
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

export default router;
