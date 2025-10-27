// src/routes/admin.ts
import express, { Request, Response, NextFunction } from "express";
import User from "../models/User";
import Card from "../models/Card";
import Message from "../models/Message";
import Chat from "../models/Chat";
import Group from "../models/Group";
import Contact from "../models/Contact";
import Notification from "../models/Notification";
import SharedCard from "../models/SharedCard";

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

    // Google Play Console stats (update these values manually or via API integration)
    // You can update these from your Google Play Console dashboard
    const totalDownloads = 123; // User acquisition from Google Play Console
    const downloadsTrend = "+623.5% vs previous 30 days"; // Update from Google Play Console
    const installedAudience = 89; // Currently installed apps

    res.json({
      totalUsers,
      totalCards,
      totalMessages,
      totalGroups,
      totalContacts,
      totalNotifications,
      newUsersThisWeek,
      activeUsers,
      totalDownloads,
      downloadsTrend,
      installedAudience
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
        { phone: { $regex: search, $options: 'i' } }
      ];
    }

    const [users, total] = await Promise.all([
      User.find(searchQuery)
        .select('name phone profilePicture about createdAt')
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
      .select('name phone about createdAt')
      .sort({ createdAt: -1 });

    // Create CSV content
    const headers = ['Name', 'Phone', 'About', 'Created At'];
    const csvRows = [headers.join(',')];

    users.forEach((user: any) => {
      const row = [
        `"${user.name || ''}"`,
        `"${user.phone || ''}"`,
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

// Export specific user's contacts
router.get("/users/:userId/contacts/export", adminAuth, async (req: Request, res: Response) => {
  try {
    const { userId } = req.params;
    
    // Get user info
    const user = await User.findById(userId).select('name phone');
    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    // Get all contacts for this user
    const contacts = await Contact.find({ userId })
      .sort({ name: 1 });

    // Create CSV content
    const headers = ['Contact Name', 'Phone Number', 'Email', 'Added On'];
    const csvRows = [headers.join(',')];

    contacts.forEach((contact: any) => {
      const row = [
        `"${contact.name || ''}"`,
        `"${contact.phoneNumber || ''}"`,
        `"${contact.email || ''}"`,
        `"${contact.createdAt?.toISOString() || ''}"`
      ];
      csvRows.push(row.join(','));
    });

    const csvContent = csvRows.join('\n');

    // Set headers for CSV download
    const fileName = `${user.name || user.phone || 'user'}-contacts-${Date.now()}.csv`;
    res.setHeader('Content-Type', 'text/csv');
    res.setHeader('Content-Disposition', `attachment; filename="${fileName}"`);
    res.send(csvContent);
  } catch (error) {
    console.error('Error exporting user contacts:', error);
    res.status(500).json({ error: 'Failed to export user contacts' });
  }
});

// Delete user and all related data
router.delete("/users/:userId", adminAuth, async (req: Request, res: Response) => {
  try {
    const { userId } = req.params;
    
    // Check if user exists
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    // Delete all related data in parallel
    await Promise.all([
      // Delete user's cards
      Card.deleteMany({ userId }),
      
      // Delete user's contacts
      Contact.deleteMany({ userId }),
      
      // Delete messages where user is sender or receiver
      Message.deleteMany({ 
        $or: [
          { senderId: userId },
          { receiverId: userId }
        ]
      }),
      
      // Delete user's notifications
      Notification.deleteMany({ userId }),
      
      // Delete user's shared cards
      SharedCard.deleteMany({ 
        $or: [
          { senderId: userId },
          { receiverId: userId }
        ]
      }),
      
      // Delete user's group memberships
      Group.updateMany(
        { members: userId },
        { $pull: { members: userId } }
      ),
      
      // Delete groups created by user (where they are the only member or admin)
      Group.deleteMany({ createdBy: userId }),
      
      // Delete user's chats
      Chat.deleteMany({
        participants: userId
      }),
    ]);

    // Finally, delete the user
    await User.findByIdAndDelete(userId);

    res.json({ 
      success: true, 
      message: 'User and all related data deleted successfully',
      deletedUser: {
        id: user._id,
        name: user.name,
        phone: user.phone
      }
    });
  } catch (error) {
    console.error('Error deleting user:', error);
    res.status(500).json({ error: 'Failed to delete user' });
  }
});

export default router;
