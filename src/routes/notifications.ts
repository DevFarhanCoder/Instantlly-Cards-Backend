import express from "express";
import mongoose from "mongoose";
import { requireAuth, AuthReq } from "../middleware/auth";
import Notification from "../models/Notification";

const router = express.Router();

// GET /api/notifications - Get user's notifications
router.get("/", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    if (!userId) {
      return res.status(401).json({ error: "User not authenticated" });
    }
    
    const { page = 1, limit = 20, unreadOnly = false } = req.query;
    
    const pageNum = parseInt(page as string);
    const limitNum = parseInt(limit as string);
    const skip = (pageNum - 1) * limitNum;
    
    // Build query
    const query: any = { userId };
    if (unreadOnly === 'true') {
      query.read = false;
    }
    
    // Get notifications with pagination
    const notifications = await Notification.find(query)
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(limitNum)
      .lean();
    
    // Get total count for pagination
    const total = await Notification.countDocuments(query);
    const unreadCount = await Notification.countDocuments({ userId, read: false });
    
    res.json({
      success: true,
      data: notifications,
      pagination: {
        page: pageNum,
        limit: limitNum,
        total,
        pages: Math.ceil(total / limitNum)
      },
      unreadCount
    });
  } catch (error) {
    console.error("Error fetching notifications:", error);
    res.status(500).json({ error: "Failed to fetch notifications" });
  }
});

// PUT /api/notifications/:id/read - Mark notification as read
router.put("/:id/read", requireAuth, async (req: AuthReq, res) => {
  try {
    const { id } = req.params;
    const userId = req.userId;
    if (!userId) {
      return res.status(401).json({ error: "User not authenticated" });
    }
    
    const notification = await Notification.findOneAndUpdate(
      { _id: id, userId },
      { read: true },
      { new: true }
    );
    
    if (!notification) {
      return res.status(404).json({ error: "Notification not found" });
    }
    
    res.json({ success: true, data: notification });
  } catch (error) {
    console.error("Error marking notification as read:", error);
    res.status(500).json({ error: "Failed to update notification" });
  }
});

// PUT /api/notifications/read-all - Mark all notifications as read
router.put("/read-all", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    if (!userId) {
      return res.status(401).json({ error: "User not authenticated" });
    }
    
    const result = await Notification.updateMany(
      { userId, read: false },
      { read: true }
    );
    
    res.json({ 
      success: true, 
      message: `Marked ${result.modifiedCount} notifications as read` 
    });
  } catch (error) {
    console.error("Error marking all notifications as read:", error);
    res.status(500).json({ error: "Failed to update notifications" });
  }
});

// DELETE /api/notifications/:id - Delete notification
router.delete("/:id", requireAuth, async (req: AuthReq, res) => {
  try {
    const { id } = req.params;
    const userId = req.userId;
    if (!userId) {
      return res.status(401).json({ error: "User not authenticated" });
    }
    
    const notification = await Notification.findOneAndDelete({ _id: id, userId });
    
    if (!notification) {
      return res.status(404).json({ error: "Notification not found" });
    }
    
    res.json({ success: true, message: "Notification deleted" });
  } catch (error) {
    console.error("Error deleting notification:", error);
    res.status(500).json({ error: "Failed to delete notification" });
  }
});

// GET /api/notifications/stats - Get notification statistics
router.get("/stats", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    if (!userId) {
      return res.status(401).json({ error: "User not authenticated" });
    }
    
    const stats = await Notification.aggregate([
      { $match: { userId: new mongoose.Types.ObjectId(userId) } },
      {
        $group: {
          _id: "$type",
          count: { $sum: 1 },
          unreadCount: {
            $sum: { $cond: [{ $eq: ["$read", false] }, 1, 0] }
          }
        }
      }
    ]);
    
    const totalUnread = await Notification.countDocuments({ userId, read: false });
    const totalCount = await Notification.countDocuments({ userId });
    
    res.json({
      success: true,
      data: {
        total: totalCount,
        unread: totalUnread,
        byType: stats
      }
    });
  } catch (error) {
    console.error("Error fetching notification stats:", error);
    res.status(500).json({ error: "Failed to fetch notification stats" });
  }
});

// POST /api/notifications/register-token - Register push notification token
router.post("/register-token", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    if (!userId) {
      return res.status(401).json({ error: "User not authenticated" });
    }

    const { pushToken, platform, deviceInfo } = req.body;
    
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📱 [TOKEN-REGISTER] New push token registration request');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('User ID:', userId);
    console.log('Platform:', platform || 'not specified');
    console.log('Push Token:', pushToken ? pushToken.substring(0, 30) + '...' : 'MISSING');
    if (deviceInfo) {
      console.log('Device Info:', JSON.stringify(deviceInfo, null, 2));
    }
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    if (!pushToken) {
      console.error('❌ [TOKEN-REGISTER] No push token provided!');
      return res.status(400).json({ error: "Push token is required" });
    }

    // Validate token format
    if (pushToken === 'expo-go-local-mode') {
      console.error('⚠️  [TOKEN-REGISTER] Received expo-go-local-mode token - rejecting!');
      return res.status(400).json({ 
        error: "expo-go-local-mode tokens are not valid for production",
        message: "Please use a production APK build"
      });
    }

    if (!pushToken.startsWith('ExponentPushToken[')) {
      console.error('⚠️  [TOKEN-REGISTER] Invalid token format:', pushToken.substring(0, 50));
      return res.status(400).json({ 
        error: "Invalid push token format",
        message: "Token must start with 'ExponentPushToken['"
      });
    }

    // Update user with push token
    const User = mongoose.model('User');
    const updatedUser = await User.findByIdAndUpdate(
      userId,
      {
        pushToken,
        platform: platform || 'unknown',
        pushTokenUpdatedAt: new Date(),
        ...(deviceInfo && { deviceInfo })
      },
      { new: true }
    );

    if (!updatedUser) {
      console.error('❌ [TOKEN-REGISTER] User not found:', userId);
      return res.status(404).json({ error: "User not found" });
    }

    console.log('✅ [TOKEN-REGISTER] Push token registered successfully!');
    console.log('✅ [TOKEN-REGISTER] User:', updatedUser.name || updatedUser.phone);
    console.log('✅ [TOKEN-REGISTER] Token:', pushToken.substring(0, 20) + '...');
    console.log('✅ [TOKEN-REGISTER] Platform:', platform || 'unknown');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

    res.json({
      success: true,
      message: "Push token registered successfully"
    });
  } catch (error) {
    console.error("❌ [TOKEN-REGISTER] Error registering push token:", error);
    console.error("❌ [TOKEN-REGISTER] Error stack:", error instanceof Error ? error.stack : 'No stack trace');
    res.status(500).json({ error: "Failed to register push token" });
  }
});

// DIAGNOSTIC ENDPOINT: Ping when attempting registration
router.post("/ping-registration-attempt", async (req, res) => {
  const { phone, timestamp, hasModule, hasFunction } = req.body;
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('🔔 [DIAGNOSTIC] Mobile app attempting to register push token');
  console.log('📱 Phone:', phone);
  console.log('⏰ Timestamp:', timestamp);
  console.log('📦 Has notification module:', hasModule);
  console.log('🔧 Has registerForPushNotifications function:', hasFunction);
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  res.json({ received: true });
});

// DIAGNOSTIC ENDPOINT: Report registration errors
router.post("/registration-error", async (req, res) => {
  const { phone, error, stack } = req.body;
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('❌ [DIAGNOSTIC] Push token registration ERROR reported from mobile');
  console.log('📱 Phone:', phone);
  console.log('🚨 Error:', error);
  console.log('📋 Stack:', stack);
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  res.json({ received: true });
});

export default router;