import express from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import User from "../models/User";
import Message from "../models/Message";

const router = express.Router();

// Send a message
router.post("/send", requireAuth, async (req: AuthReq, res) => {
  try {
    const { receiverId, text } = req.body;
    const senderId = req.userId;

    if (!receiverId || !text) {
      return res.status(400).json({ error: "Missing required fields: receiverId, text" });
    }

    // Check if receiver exists
    const receiver = await User.findById(receiverId);
    if (!receiver) {
      return res.status(404).json({ error: "Receiver not found" });
    }

    // Create conversation ID (consistent between users)
    const conversationId = [senderId, receiverId].sort().join('-');

    // Create message
    const message = await Message.create({
      conversationId,
      senderId,
      receiverId,
      text: text.trim(),
      timestamp: new Date(),
      isRead: false
    });

    // Populate sender info
    const populatedMessage = await Message.findById(message._id)
      .populate('senderId', 'name profilePicture')
      .populate('receiverId', 'name profilePicture');

    res.status(201).json({ 
      success: true, 
      message: "Message sent successfully",
      data: populatedMessage 
    });
  } catch (error) {
    console.error("Error sending message:", error);
    res.status(500).json({ error: "Failed to send message" });
  }
});

// Get messages for a conversation
router.get("/conversation/:userId", requireAuth, async (req: AuthReq, res) => {
  try {
    const { userId: otherUserId } = req.params;
    const currentUserId = req.userId;

    // Create conversation ID
    const conversationId = [currentUserId, otherUserId].sort().join('-');

    // Get messages for this conversation
    const messages = await Message.find({ conversationId })
      .populate('senderId', 'name profilePicture')
      .populate('receiverId', 'name profilePicture')
      .sort({ timestamp: 1 }) // Oldest first
      .limit(100); // Limit to last 100 messages

    // Mark messages as read
    await Message.updateMany(
      { 
        conversationId, 
        receiverId: currentUserId, 
        isRead: false 
      },
      { isRead: true }
    );

    res.json({ success: true, data: messages });
  } catch (error) {
    console.error("Error fetching messages:", error);
    res.status(500).json({ error: "Failed to fetch messages" });
  }
});

// Get all conversations for current user
router.get("/conversations", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;

    // Get all messages where user is sender or receiver
    const messages = await Message.aggregate([
      {
        $match: {
          $or: [
            { senderId: userId },
            { receiverId: userId }
          ]
        }
      },
      {
        $sort: { timestamp: -1 }
      },
      {
        $group: {
          _id: "$conversationId",
          lastMessage: { $first: "$text" },
          lastMessageTime: { $first: "$timestamp" },
          senderId: { $first: "$senderId" },
          receiverId: { $first: "$receiverId" },
          unreadCount: {
            $sum: {
              $cond: [
                { 
                  $and: [
                    { $eq: ["$receiverId", userId] },
                    { $eq: ["$isRead", false] }
                  ]
                },
                1,
                0
              ]
            }
          }
        }
      }
    ]);

    // Get other user details for each conversation
    const conversations = await Promise.all(
      messages.map(async (msg) => {
        const otherUserId = msg.senderId.toString() === userId ? msg.receiverId : msg.senderId;
        const otherUser = await User.findById(otherUserId).select('name profilePicture phone');
        
        return {
          id: msg._id,
          userId: otherUserId,
          name: otherUser?.name || 'Unknown User',
          profilePicture: otherUser?.profilePicture,
          lastMessage: msg.lastMessage,
          lastMessageTime: msg.lastMessageTime,
          unreadCount: msg.unreadCount,
          isOnline: false // Could implement real-time status later
        };
      })
    );

    res.json({ success: true, data: conversations });
  } catch (error) {
    console.error("Error fetching conversations:", error);
    res.status(500).json({ error: "Failed to fetch conversations" });
  }
});

export default router;