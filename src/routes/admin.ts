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
import Ad from "../models/Ad";
import Transaction from "../models/Transaction";
import { requireAdminAuth, AdminAuthReq } from "../middleware/adminAuth";

const router = express.Router();

// Simple admin authentication middleware (you should replace with proper auth)
const adminAuth = (req: Request, res: Response, next: NextFunction) => {
  const adminKey = req.headers['x-admin-key'];
  
  // Accept configured admin key from environment or known keys
  const validKeys = [
    process.env.ADMIN_SECRET_KEY,
    'your-secure-admin-key-here',
    'Farhan_90'  // Admin dashboard key
  ].filter(Boolean);
  
  if (adminKey && validKeys.includes(adminKey as string)) {
    next();
  } else {
    console.log(`❌ Admin auth failed. Received key: ${adminKey}, Expected one of: ${validKeys.join(', ')}`);
    res.status(401).json({ error: 'Unauthorized', message: 'Invalid admin key' });
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
    const sortBy = req.query.sortBy as string || '';
    const sortOrder = req.query.sortOrder as string || '';
    const skip = (page - 1) * limit;

    console.log('📊 Admin /users request:', { page, limit, search, sortBy, sortOrder });

    // Build search query
    const searchQuery: any = {};
    if (search) {
      searchQuery.$or = [
        { name: { $regex: search, $options: 'i' } },
        { phone: { $regex: search, $options: 'i' } }
      ];
    }

    // Build sort query
    let sortQuery: any = { createdAt: -1 }; // Default sort by creation date
    if (sortBy === 'credits' && (sortOrder === 'asc' || sortOrder === 'desc')) {
      sortQuery = { credits: sortOrder === 'asc' ? 1 : -1 };
      console.log('💳 Sorting by credits:', sortQuery);
    }

    const [users, total] = await Promise.all([
      User.find(searchQuery)
        .select('name phone profilePicture about createdAt credits')
        .sort(sortQuery)
        .skip(skip)
        .limit(limit),
      User.countDocuments(searchQuery)
    ]);

    console.log(`✅ Found ${users.length} users (Total: ${total}). First user credits: ${users[0]?.credits}, Last user credits: ${users[users.length - 1]?.credits}`);


    // Get additional stats for each user
    const usersWithStats = await Promise.all(
      users.map(async (user: any) => {
        const [cardCount, messageCount, contactCount] = await Promise.all([
          Card.countDocuments({ userId: user._id }),
          Message.countDocuments({ senderId: user._id }),
          Contact.countDocuments({ userId: user._id })
        ]);

        // User.credits is the current live balance (already includes all transactions)
        const totalCredits = user.credits || 0;

        return {
          ...user.toObject(),
          stats: {
            cards: cardCount,
            messages: messageCount,
            contacts: contactCount,
            credits: totalCredits
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

// ==================== AD APPROVAL WORKFLOW ====================

/**
 * GET /api/admin/ads/pending
 * Get all pending ads awaiting approval
 */
router.get("/ads/pending", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    console.log(`📋 Admin ${req.adminUsername} fetching pending ads`);

    const pendingAds = await Ad.find({ status: 'pending' })
      .sort({ createdAt: -1 }) // Most recent first
      .select('-__v');

    console.log(`✅ Found ${pendingAds.length} pending ads`);

    const adsWithDetails = pendingAds.map((ad) => ({
      id: ad._id,
      title: ad.title,
      phoneNumber: ad.phoneNumber,
      startDate: ad.startDate,
      endDate: ad.endDate,
      status: ad.status,
      uploadedBy: ad.uploadedBy,
      uploaderName: ad.uploaderName,
      priority: ad.priority,
      bottomImageId: ad.bottomImageGridFS,
      fullscreenImageId: ad.fullscreenImageGridFS,
      impressions: ad.impressions,
      clicks: ad.clicks,
      createdAt: ad.createdAt,
      updatedAt: ad.updatedAt,
    }));

    res.json({
      success: true,
      count: adsWithDetails.length,
      ads: adsWithDetails,
    });
  } catch (error) {
    console.error('❌ Error fetching pending ads:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to fetch pending ads',
    });
  }
});

/**
 * POST /api/admin/ads/:id/approve
 * Approve a pending ad and deduct 1200 credits from creator
 */
router.post("/ads/:id/approve", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    const { id } = req.params;
    const { priority } = req.body; // Optional: admin can set priority during approval

    console.log(`✅ Admin ${req.adminUsername} approving ad ${id}`);

    const ad = await Ad.findById(id);

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: 'Advertisement not found',
      });
    }

    if (ad.status !== 'pending') {
      return res.status(400).json({
        success: false,
        message: `Advertisement is already ${ad.status}`,
      });
    }

    // Find the user who created the ad (by phone number in uploadedBy field)
    let creditsDeducted = false;
    let paymentRequired = 0;
    let userCredits = 0;

    if (ad.uploadedBy && ad.uploadedBy !== 'admin') {
      const creator = await User.findOne({ phone: ad.uploadedBy });
      
      if (creator) {
        const currentCredits = (creator as any).credits || 0;
        const deductionAmount = 1200;

        // Check if user has enough credits
        if (currentCredits >= deductionAmount) {
          // Deduct credits
          creator.set({ credits: currentCredits - deductionAmount });
          await creator.save();

          // Create transaction record
          await Transaction.create({
            type: 'ad_deduction',
            fromUser: creator._id,
            amount: deductionAmount,
            description: `Credits deducted for ad: ${ad.title}`,
            balanceBefore: currentCredits,
            balanceAfter: currentCredits - deductionAmount,
            relatedAd: ad._id,
            status: 'completed'
          });

          creditsDeducted = true;
          userCredits = currentCredits - deductionAmount;
          
          // Calculate 15% payment required (15% of 1200 = 180)
          paymentRequired = Math.round(deductionAmount * 0.15);

          console.log(`💰 Deducted ${deductionAmount} credits from ${creator.name}. New balance: ${userCredits}. Payment required: ₹${paymentRequired}`);
        } else {
          console.log(`⚠️ User ${creator.name} has insufficient credits (${currentCredits}). Ad approval blocked.`);
          return res.status(400).json({
            success: false,
            message: `Insufficient credits. User has ${currentCredits} credits but needs ${deductionAmount} credits to approve this ad.`,
            userCredits: currentCredits,
            required: deductionAmount
          });
        }
      } else {
        console.log(`⚠️ Could not find user with phone ${ad.uploadedBy} for credits deduction`);
      }
    }

    // Update ad status to approved
    ad.status = 'approved';
    ad.approvedBy = req.adminId || req.adminUsername || 'admin';
    ad.approvalDate = new Date();
    
    if (priority !== undefined) {
      ad.priority = Math.min(Math.max(parseInt(priority), 1), 10); // Clamp between 1-10
    }

    await ad.save();

    console.log(`✅ Ad ${id} approved by ${req.adminUsername}`);

    res.json({
      success: true,
      message: 'Advertisement approved successfully',
      ad: {
        id: ad._id,
        title: ad.title,
        status: ad.status,
        approvedBy: ad.approvedBy,
        approvalDate: ad.approvalDate,
        priority: ad.priority,
      },
      credits: {
        deducted: creditsDeducted,
        amount: creditsDeducted ? 1200 : 0,
        remainingBalance: creditsDeducted ? userCredits : null,
        paymentRequired: creditsDeducted ? paymentRequired : 0,
        paymentDetails: creditsDeducted ? {
          description: `15% payment for ad approval`,
          amount: paymentRequired,
          currency: 'INR'
        } : null
      }
    });
  } catch (error) {
    console.error('❌ Error approving ad:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to approve advertisement',
    });
  }
});

/**
 * POST /api/admin/ads/:id/reject
 * Reject a pending ad with reason
 */
router.post("/ads/:id/reject", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    const { id } = req.params;
    const { reason } = req.body;

    console.log(`❌ Admin ${req.adminUsername} rejecting ad ${id}`);

    if (!reason || reason.trim() === '') {
      return res.status(400).json({
        success: false,
        message: 'Rejection reason is required',
      });
    }

    const ad = await Ad.findById(id);

    if (!ad) {
      return res.status(404).json({
        success: false,
        message: 'Advertisement not found',
      });
    }

    if (ad.status !== 'pending') {
      return res.status(400).json({
        success: false,
        message: `Advertisement is already ${ad.status}`,
      });
    }

    // Update ad status to rejected
    ad.status = 'rejected';
    ad.approvedBy = req.adminId || req.adminUsername || 'admin';
    ad.approvalDate = new Date();
    ad.rejectionReason = reason.trim();

    await ad.save();

    console.log(`❌ Ad ${id} rejected by ${req.adminUsername}: ${reason}`);

    res.json({
      success: true,
      message: 'Advertisement rejected',
      ad: {
        id: ad._id,
        title: ad.title,
        status: ad.status,
        approvedBy: ad.approvedBy,
        approvalDate: ad.approvalDate,
        rejectionReason: ad.rejectionReason,
      },
    });
  } catch (error) {
    console.error('❌ Error rejecting ad:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to reject advertisement',
    });
  }
});

/**
 * GET /api/admin/ads/all
 * Get all ads with filtering options
 */
router.get("/ads/all", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    const { status, uploadedBy } = req.query;
    
    const filter: any = {};
    if (status) {
      filter.status = status;
    }
    if (uploadedBy) {
      filter.uploadedBy = uploadedBy;
    }

    console.log(`📋 Admin ${req.adminUsername} fetching ads with filter:`, filter);

    const ads = await Ad.find(filter)
      .sort({ createdAt: -1 })
      .select('-__v');

    const adsWithDetails = ads.map((ad) => ({
      id: ad._id,
      title: ad.title,
      phoneNumber: ad.phoneNumber,
      startDate: ad.startDate,
      endDate: ad.endDate,
      status: ad.status,
      uploadedBy: ad.uploadedBy,
      uploaderName: ad.uploaderName,
      approvedBy: ad.approvedBy,
      approvalDate: ad.approvalDate,
      rejectionReason: ad.rejectionReason,
      priority: ad.priority,
      bottomImageId: ad.bottomImageGridFS,
      fullscreenImageId: ad.fullscreenImageGridFS,
      impressions: ad.impressions,
      clicks: ad.clicks,
      createdAt: ad.createdAt,
      updatedAt: ad.updatedAt,
    }));

    res.json({
      success: true,
      count: adsWithDetails.length,
      ads: adsWithDetails,
    });
  } catch (error) {
    console.error('❌ Error fetching all ads:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to fetch advertisements',
    });
  }
});

// GET /api/admin/users-stats - Get user statistics with total credits
router.get("/users-stats", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    const totalUsers = await User.countDocuments();
    
    // Calculate total credits across all users
    const creditStats = await User.aggregate([
      {
        $group: {
          _id: null,
          totalCredits: { $sum: "$credits" }
        }
      }
    ]);

    const totalCredits = creditStats.length > 0 ? creditStats[0].totalCredits : 0;

    res.json({
      success: true,
      totalUsers,
      totalCredits
    });
  } catch (error) {
    console.error('❌ Error fetching user stats:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to fetch user statistics'
    });
  }
});

// GET /api/admin/all-transactions - Get all credit transactions
router.get("/all-transactions", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    const { limit = 500, skip = 0, type, userId } = req.query;
    
    const query: any = {};
    if (type) query.type = type;
    if (userId) {
      query.$or = [
        { fromUser: userId },
        { toUser: userId }
      ];
    }

    const transactions = await Transaction.find(query)
      .populate('fromUser', 'name phone profilePicture')
      .populate('toUser', 'name phone profilePicture')
      .sort({ createdAt: -1 })
      .limit(Number(limit))
      .skip(Number(skip));

    const total = await Transaction.countDocuments(query);

    res.json({
      success: true,
      transactions,
      total,
      limit: Number(limit),
      skip: Number(skip)
    });
  } catch (error) {
    console.error('❌ Error fetching all transactions:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to fetch transactions'
    });
  }
});

// GET /api/admin/user/:userId - Get specific user details including balance
router.get("/user/:userId", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    const { userId } = req.params;
    
    const user = await User.findById(userId).select('name phone email credits profilePicture createdAt');
    
    if (!user) {
      return res.status(404).json({
        success: false,
        message: 'User not found'
      });
    }

    res.json({
      success: true,
      user: {
        _id: user._id,
        name: user.name,
        phone: user.phone,
        email: user.email,
        credits: (user as any).credits || 0,
        profilePicture: user.profilePicture,
        createdAt: user.createdAt
      }
    });
  } catch (error) {
    console.error('❌ Error fetching user:', error);
    res.status(500).json({
      success: false,
      message: 'Failed to fetch user details'
    });
  }
});

// POST /api/admin/transfer-credits - Admin transfer credits to any user
router.post("/transfer-credits", requireAdminAuth, async (req: AdminAuthReq, res: Response) => {
  try {
    const { toUserId, amount, description } = req.body;
    
    // Validation
    if (!toUserId || !amount) {
      return res.status(400).json({ 
        success: false,
        message: "Recipient and amount are required" 
      });
    }

    if (amount <= 0) {
      return res.status(400).json({ 
        success: false,
        message: "Amount must be greater than 0" 
      });
    }

    // Get recipient
    const recipient = await User.findById(toUserId);
    if (!recipient) {
      return res.status(404).json({ 
        success: false,
        message: "Recipient not found" 
      });
    }

    // Add credits to recipient
    const recipientCredits = (recipient as any).credits || 0;
    recipient.set({ credits: recipientCredits + amount });
    await recipient.save();

    // Create transaction record
    const transferDesc = description || `Admin credit - ${amount.toLocaleString('en-IN')} credits`;
    
    await Transaction.create({
      type: 'admin_adjustment',
      toUser: recipient._id,
      amount: amount,
      description: transferDesc,
      balanceBefore: recipientCredits,
      balanceAfter: recipientCredits + amount,
      status: 'completed'
    });

    console.log(`✅ Admin transferred ${amount} credits to ${recipient.name}`);

    res.json({
      success: true,
      message: `Successfully transferred ${amount} credits to ${recipient.name}`,
      newBalance: recipientCredits + amount,
      transfer: {
        amount,
        to: {
          _id: recipient._id,
          name: recipient.name,
          phone: recipient.phone
        }
      }
    });
  } catch (error) {
    console.error('❌ Admin transfer error:', error);
    res.status(500).json({ 
      success: false,
      message: "Server error during transfer" 
    });
  }
});

// Credit Transfer endpoint (alternative path for admin dashboard)
router.post("/credits/transfer", adminAuth, async (req: Request, res: Response) => {
  try {
    const { userId, amount, reason } = req.body;
    
    // Validation
    if (!userId || !amount) {
      return res.status(400).json({ 
        success: false,
        message: "User ID and amount are required" 
      });
    }

    if (amount <= 0) {
      return res.status(400).json({ 
        success: false,
        message: "Amount must be greater than 0" 
      });
    }

    // Get recipient
    const recipient = await User.findById(userId);
    if (!recipient) {
      return res.status(404).json({ 
        success: false,
        message: "User not found" 
      });
    }

    // Add credits to recipient
    const recipientCredits = (recipient as any).credits || 0;
    recipient.set({ credits: recipientCredits + amount });
    await recipient.save();

    // Create transaction record
    const transferDesc = reason || `Admin credit transfer - ${amount.toLocaleString('en-IN')} credits`;
    
    await Transaction.create({
      type: 'admin_adjustment',
      toUser: recipient._id,
      amount: amount,
      description: transferDesc,
      balanceBefore: recipientCredits,
      balanceAfter: recipientCredits + amount,
      status: 'completed'
    });

    console.log(`✅ Admin transferred ${amount} credits to ${recipient.name}`);

    res.json({
      success: true,
      message: `Successfully transferred ${amount} credits to ${recipient.name}`,
      newBalance: recipientCredits + amount
    });
  } catch (error) {
    console.error('❌ Admin credit transfer error:', error);
    res.status(500).json({ 
      success: false,
      message: "Server error during transfer" 
    });
  }
});

// Edit Application Info (Name & Phone)
router.put("/applications/:id/edit", adminAuth, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const { name, phone } = req.body;

    console.log(`📝 Editing application ${id}:`, { name, phone });

    // Find and update the application (assuming you have an Application model)
    // For now, update the User model if the application is approved
    const user = await User.findById(id);
    
    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    // Update user info
    if (name) user.name = name;
    if (phone) user.phone = phone;
    await user.save();

    console.log(`✅ Updated user: ${user.name} (${user.phone})`);

    res.json({
      success: true,
      message: "Application updated successfully",
      user: {
        _id: user._id,
        name: user.name,
        phone: user.phone
      }
    });
  } catch (error) {
    console.error('❌ Edit application error:', error);
    res.status(500).json({ message: "Server error" });
  }
});

// Transfer Position
router.put("/applications/:id/transfer", adminAuth, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const { newPositionId } = req.body;

    console.log(`🔄 Transferring application ${id} to position:`, newPositionId);

    // Update user's position (you may need to adjust based on your data model)
    const user = await User.findById(id);
    
    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    // Update position (assuming you store position in user model)
    (user as any).positionId = newPositionId;
    await user.save();

    console.log(`✅ Transferred ${user.name} to ${newPositionId}`);

    res.json({
      success: true,
      message: "Position transferred successfully",
      user: {
        _id: user._id,
        name: user.name,
        positionId: newPositionId
      }
    });
  } catch (error) {
    console.error('❌ Transfer position error:', error);
    res.status(500).json({ message: "Server error" });
  }
});

// GET /api/admin/referral-tracking - Get comprehensive referral statistics
router.get("/referral-tracking", adminAuth, async (req: Request, res: Response) => {
  try {
    const days = parseInt(req.query.days as string) || 30;
    
    // Calculate date filter
    let dateFilter: any = {};
    if (days > 0) {
      const startDate = new Date();
      startDate.setDate(startDate.getDate() - days);
      dateFilter = { createdAt: { $gte: startDate } };
    }

    // Get all users who have made referrals
    const usersWithReferrals = await User.find({
      referralCode: { $exists: true, $ne: null }
    }).select('_id name phone referralCode createdAt').lean();

    // Count referrals for each user
    const topReferrersData = await Promise.all(
      usersWithReferrals.map(async (user: any) => {
        // Count users referred by this user
        const referralCount = await User.countDocuments({
          referredBy: user._id,
          ...dateFilter
        });

        // Get total credits earned from referrals
        const referralTransactions = await Transaction.find({
          type: 'referral_bonus',
          toUser: user._id,
          ...dateFilter
        }).select('amount').lean();

        const creditsEarned = referralTransactions.reduce((sum, tx: any) => sum + (tx.amount || 0), 0);

        return {
          userId: user._id.toString(),
          name: user.name,
          phone: user.phone,
          referralCode: user.referralCode,
          totalReferrals: referralCount,
          creditsEarned,
          joinedDate: user.createdAt
        };
      })
    );

    // Filter out users with 0 referrals and sort by total referrals
    const topReferrers = topReferrersData
      .filter(r => r.totalReferrals > 0)
      .sort((a, b) => b.totalReferrals - a.totalReferrals)
      .slice(0, 100); // Top 100 referrers

    // Get overall statistics
    const totalReferrals = await User.countDocuments({
      referredBy: { $exists: true, $ne: null },
      ...dateFilter
    });

    const totalReferralTransactions = await Transaction.find({
      type: 'referral_bonus',
      ...dateFilter
    }).select('amount').lean();

    const totalReferralCreditsGiven = totalReferralTransactions.reduce((sum, tx: any) => sum + (tx.amount || 0), 0);

    const uniqueReferrers = topReferrers.length;
    const averageReferralsPerUser = uniqueReferrers > 0 ? totalReferrals / uniqueReferrers : 0;

    // Get recent referral activity
    const recentReferredUsers = await User.find({
      referredBy: { $exists: true, $ne: null },
      ...dateFilter
    })
      .select('name phone referredBy createdAt')
      .populate('referredBy', 'name referralCode')
      .sort({ createdAt: -1 })
      .limit(50)
      .lean();

    // Get corresponding transactions for credits awarded
    const recentActivity = await Promise.all(
      recentReferredUsers.map(async (user: any) => {
        const transaction = await Transaction.findOne({
          type: 'referral_bonus',
          fromUser: user._id
        }).select('amount').lean();

        return {
          referrerName: user.referredBy?.name || 'Unknown',
          referrerCode: user.referredBy?.referralCode || 'N/A',
          newUserName: user.name,
          newUserPhone: user.phone,
          date: user.createdAt,
          creditsAwarded: (transaction as any)?.amount || 0
        };
      })
    );

    // Get referral trends (daily count for the period)
    const referralTrends: Array<{ date: string; count: number }> = [];
    if (days > 0 && days <= 90) {
      const trendDays = days > 30 ? 30 : days;
      for (let i = trendDays - 1; i >= 0; i--) {
        const date = new Date();
        date.setDate(date.getDate() - i);
        date.setHours(0, 0, 0, 0);
        
        const nextDate = new Date(date);
        nextDate.setDate(nextDate.getDate() + 1);

        const count = await User.countDocuments({
          referredBy: { $exists: true, $ne: null },
          createdAt: { $gte: date, $lt: nextDate }
        });

        referralTrends.push({
          date: date.toISOString().split('T')[0],
          count
        });
      }
    }

    res.json({
      success: true,
      data: {
        totalReferrals,
        totalReferralCreditsGiven,
        uniqueReferrers,
        averageReferralsPerUser,
        topReferrers,
        recentActivity,
        referralTrends
      }
    });
  } catch (error) {
    console.error('❌ Referral tracking error:', error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// GET /api/admin/referral-chain/:userId - Get detailed referral chain for a specific user
router.get("/referral-chain/:userId", adminAuth, async (req: Request, res: Response) => {
  try {
    const { userId } = req.params;
    
    console.log(`📊 Fetching referral chain for user: ${userId}`);

    // Get the user's details
    const user = await User.findById(userId).select('name phone referralCode createdAt').lean() as any;
    
    if (!user) {
      return res.status(404).json({
        success: false,
        message: "User not found"
      });
    }

    // Find all users referred by this user
    const referredUsers = await User.find({ referredBy: userId })
      .select('_id name phone referralCode createdAt referredBy credits')
      .sort({ createdAt: -1 })
      .lean() as any[];

    // For each referred user, count their own referrals
    const referredUsersWithStats = await Promise.all(
      referredUsers.map(async (refUser: any) => {
        const refUserReferralCount = await User.countDocuments({ referredBy: refUser._id });
        
        // Use the User.credits field directly (it's the live balance)
        const creditsEarned = refUser.credits || 0;

        return {
          userId: refUser._id.toString(),
          name: refUser.name,
          phone: refUser.phone,
          referralCode: refUser.referralCode,
          totalReferrals: refUserReferralCount,
          creditsEarned,
          joinedDate: refUser.createdAt
        };
      })
    );

    res.json({
      success: true,
      data: {
        user: {
          userId: user._id.toString(),
          name: user.name,
          phone: user.phone,
          referralCode: user.referralCode,
          joinedDate: user.createdAt
        },
        referredUsers: referredUsersWithStats,
        totalCount: referredUsersWithStats.length
      }
    });

  } catch (error: any) {
    console.error("❌ Error fetching referral chain:", error);
    res.status(500).json({
      success: false,
      message: error.message || "Failed to fetch referral chain"
    });
  }
});

export default router;
