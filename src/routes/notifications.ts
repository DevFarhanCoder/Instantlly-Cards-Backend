import express from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import Notification from "../models/Notification";

const router = express.Router();

// GET /api/notifications - Get user's notifications
router.get("/", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
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
    
    const stats = await Notification.aggregate([
      { $match: { userId: userId } },
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

export default router;