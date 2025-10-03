// src/routes/admin.ts
import express, { Request, Response, NextFunction } from "express";
import User from "../models/User";
import Card from "../models/Card";
import Message from "../models/Message";
import Chat from "../models/Chat";
import Group from "../models/Group";
import Contact from "../models/Contact";
import Notification from "../models/Notification";

const router = express.Router();

// Simple admin authentication middleware (you should replace with proper auth)
const adminAuth = (req: Request, res: Response, next: NextFunction) => {
  const adminKey = req.headers['x-admin-key'];
  
  // TODO: Replace with your secure admin key
  if (adminKey === process.env.ADMIN_SECRET_KEY || adminKey === 'your-secure-admin-key-here') {
    next();
  } else {
    res.status(401).json({ error: 'Unauthorized' });
  }
};

// Dashboard Overview Stats
router.get("/stats", adminAuth, async (req: Request, res: Response) => {
  try {
    const [
      totalUsers,
      totalCards,
      totalMessages,
      totalGroups,
      totalContacts,
      totalNotifications
    ] = await Promise.all([
      User.countDocuments(),
      Card.countDocuments(),
      Message.countDocuments(),
      Group.countDocuments(),
      Contact.countDocuments(),
      Notification.countDocuments()
    ]);

    // Get users created in last 7 days
    const sevenDaysAgo = new Date();
    sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);
    
    const newUsersThisWeek = await User.countDocuments({
      createdAt: { $gte: sevenDaysAgo }
    });

    // Get active users (users who have cards or sent messages)
    const activeUsers = await User.countDocuments({
      $or: [
        { _id: { $in: await Card.distinct('userId') } },
        { _id: { $in: await Message.distinct('senderId') } }
      ]
    });

    res.json({
      totalUsers,
      totalCards,
      totalMessages,
      totalGroups,
      totalContacts,
      totalNotifications,
      newUsersThisWeek,
      activeUsers
    });
  } catch (error) {
    console.error('Error fetching stats:', error);
    res.status(500).json({ error: 'Failed to fetch statistics' });
  }
});

// Get all users with pagination
router.get("/users", adminAuth, async (req: Request, res: Response) => {
  try {
    const page = parseInt(req.query.page as string) || 1;
    const limit = parseInt(req.query.limit as string) || 50;
    const search = req.query.search as string || '';
    const skip = (page - 1) * limit;

    // Build search query
    const searchQuery: any = {};
    if (search) {
      searchQuery.$or = [
        { name: { $regex: search, $options: 'i' } },
        { phone: { $regex: search, $options: 'i' } },
        { email: { $regex: search, $options: 'i' } }
      ];
    }

    const [users, total] = await Promise.all([
      User.find(searchQuery)
        .select('name phone email profilePicture about createdAt')
        .sort({ createdAt: -1 })
        .skip(skip)
        .limit(limit),
      User.countDocuments(searchQuery)
    ]);

    // Get additional stats for each user
    const usersWithStats = await Promise.all(
      users.map(async (user: any) => {
        const [cardCount, messageCount, contactCount] = await Promise.all([
          Card.countDocuments({ userId: user._id }),
          Message.countDocuments({ senderId: user._id }),
          Contact.countDocuments({ userId: user._id })
        ]);

        return {
          ...user.toObject(),
          stats: {
            cards: cardCount,
            messages: messageCount,
            contacts: contactCount
          }
        };
      })
    );

    res.json({
      users: usersWithStats,
      pagination: {
        page,
        limit,
        total,
        totalPages: Math.ceil(total / limit)
      }
    });
  } catch (error) {
    console.error('Error fetching users:', error);
    res.status(500).json({ error: 'Failed to fetch users' });
  }
});

// Export all users to CSV
router.get("/users/export", adminAuth, async (req: Request, res: Response) => {
  try {
    const users = await User.find()
      .select('name phone email about createdAt')
      .sort({ createdAt: -1 });

    // Create CSV content
    const headers = ['Name', 'Phone', 'Email', 'About', 'Created At'];
    const csvRows = [headers.join(',')];

    users.forEach((user: any) => {
      const row = [
        `"${user.name || ''}"`,
        `"${user.phone || ''}"`,
        `"${user.email || ''}"`,
        `"${user.about || ''}"`,
        `"${user.createdAt?.toISOString() || ''}"`
      ];
      csvRows.push(row.join(','));
    });

    const csvContent = csvRows.join('\n');

    // Set headers for CSV download
    res.setHeader('Content-Type', 'text/csv');
    res.setHeader('Content-Disposition', `attachment; filename=users-${Date.now()}.csv`);
    res.send(csvContent);
  } catch (error) {
    console.error('Error exporting users:', error);
    res.status(500).json({ error: 'Failed to export users' });
  }
});

// Export phone numbers only
router.get("/users/export-phones", adminAuth, async (req: Request, res: Response) => {
  try {
    const users = await User.find({ phone: { $exists: true, $ne: '' } })
      .select('name phone')
      .sort({ createdAt: -1 });

    // Create CSV content
    const headers = ['Name', 'Phone Number'];
    const csvRows = [headers.join(',')];

    users.forEach((user: any) => {
      const row = [
        `"${user.name || ''}"`,
        `"${user.phone || ''}"`
      ];
      csvRows.push(row.join(','));
    });

    const csvContent = csvRows.join('\n');

    // Set headers for CSV download
    res.setHeader('Content-Type', 'text/csv');
    res.setHeader('Content-Disposition', `attachment; filename=phone-numbers-${Date.now()}.csv`);
    res.send(csvContent);
  } catch (error) {
    console.error('Error exporting phone numbers:', error);
    res.status(500).json({ error: 'Failed to export phone numbers' });
  }
});

// Get user growth data (for charts)
router.get("/analytics/user-growth", adminAuth, async (req: Request, res: Response) => {
  try {
    const days = parseInt(req.query.days as string) || 30;
    const startDate = new Date();
    startDate.setDate(startDate.getDate() - days);

    const userGrowth = await User.aggregate([
      {
        $match: {
          createdAt: { $gte: startDate }
        }
      },
      {
        $group: {
          _id: {
            $dateToString: { format: "%Y-%m-%d", date: "$createdAt" }
          },
          count: { $sum: 1 }
        }
      },
      {
        $sort: { _id: 1 }
      }
    ]);

    res.json(userGrowth);
  } catch (error) {
    console.error('Error fetching user growth:', error);
    res.status(500).json({ error: 'Failed to fetch user growth data' });
  }
});

// Get recent activity
router.get("/activity/recent", adminAuth, async (req: Request, res: Response) => {
  try {
    const limit = parseInt(req.query.limit as string) || 20;

    const [recentUsers, recentCards, recentMessages] = await Promise.all([
      User.find()
        .select('name phone createdAt')
        .sort({ createdAt: -1 })
        .limit(limit),
      Card.find()
        .populate('userId', 'name phone')
        .select('companyName createdAt userId')
        .sort({ createdAt: -1 })
        .limit(limit),
      Message.find()
        .populate('senderId', 'name')
        .populate('receiverId', 'name')
        .select('message senderId receiverId createdAt')
        .sort({ createdAt: -1 })
        .limit(limit)
    ]);

    res.json({
      recentUsers,
      recentCards,
      recentMessages
    });
  } catch (error) {
    console.error('Error fetching recent activity:', error);
    res.status(500).json({ error: 'Failed to fetch recent activity' });
  }
});

export default router;
