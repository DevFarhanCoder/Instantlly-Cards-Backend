import { Router } from "express";
import User from "../models/User";
import Transaction from "../models/Transaction";
import CreditConfig from "../models/CreditConfig";
import { requireAuth, AuthReq } from "../middleware/auth";
// Force language server refresh

const router = Router();

// Helper function to generate referral code
function generateReferralCode(): string {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let code = '';
  for (let i = 0; i < 6; i++) {
    code += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return code;
}

// DEBUG ENDPOINT - Get sample phone numbers from database
router.get("/debug-phones", requireAuth, async (req: AuthReq, res) => {
  try {
    const users = await User.find()
      .select('name phone')
      .limit(10)
      .lean();
    
    console.log('📱 [DEBUG] Sample phone numbers in database:', users);
    
    res.json({
      success: true,
      count: users.length,
      phones: users.map(u => ({
        name: u.name,
        phone: u.phone,
        phoneLength: u.phone?.length || 0
      }))
    });
  } catch (error) {
    console.error("DEBUG PHONES ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// GET /api/credits/balance - Get current credits balance
router.get("/balance", requireAuth, async (req: AuthReq, res) => {
  try {
    const user = await User.findById(req.userId).select('credits creditsExpiryDate');
    
    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    const currentCredits = (user as any).credits || 0;
    const expiryDate = (user as any).creditsExpiryDate;
    
    // Check if credits have expired
    let activeCredits = currentCredits;
    let isExpired = false;
    
    if (expiryDate && new Date() > new Date(expiryDate)) {
      // Credits expired, set to 0
      isExpired = true;
      activeCredits = 0;
      
      // Update user credits to 0 if not already
      if (currentCredits > 0) {
        (user as any).credits = 0;
        await user.save();
        console.log(`⏰ Credits expired for user ${req.userId}, reset to 0`);
      }
    }

    res.json({
      success: true,
      credits: activeCredits,
      creditsExpiryDate: expiryDate,
      isExpired: isExpired,
      daysRemaining: expiryDate ? Math.max(0, Math.ceil((new Date(expiryDate).getTime() - Date.now()) / (1000 * 60 * 60 * 24))) : null
    });
  } catch (error) {
    console.error("GET BALANCE ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// GET /api/credits/transactions - Get user's credit transaction history
router.get("/transactions", requireAuth, async (req: AuthReq, res) => {
  try {
    const { limit = 50, skip = 0 } = req.query;
    const userId = req.userId;
    const userIdStr = userId?.toString();
    
    // Fetch all potential transactions first
    const allTransactions = await Transaction.find({
      $or: [
        { fromUser: userId },
        { toUser: userId }
      ]
    })
    .populate('fromUser', 'name phone')
    .populate('toUser', 'name phone')
    .sort({ createdAt: -1 })
    .lean();

    // Filter transactions based on type and user role
    // This ensures each user only sees their relevant transaction record
    const filteredTransactions = allTransactions.filter((txn: any) => {
      // Safely extract IDs as strings - handle both populated and non-populated refs
      const fromUserId = txn.fromUser?._id?.toString() || (typeof txn.fromUser === 'object' ? txn.fromUser?.toString() : txn.fromUser);
      const toUserId = txn.toUser?._id?.toString() || (typeof txn.toUser === 'object' ? txn.toUser?.toString() : txn.toUser);
      
      switch (txn.type) {
        case 'signup_bonus':
        case 'referral_bonus':
        case 'quiz_bonus':
        case 'self_download_bonus':
          // User should be the receiver
          return toUserId === userIdStr;
        
        case 'transfer_sent':
          // Only sender sees this transaction
          return fromUserId === userIdStr;
        
        case 'transfer_received':
          // Only receiver sees this transaction
          return toUserId === userIdStr;
        
        case 'ad_deduction':
          // User should be the one being deducted
          return fromUserId === userIdStr;
        
        default:
          // For any other type, show if user is involved
          return fromUserId === userIdStr || toUserId === userIdStr;
      }
    });

    // Apply pagination
    const paginatedTransactions = filteredTransactions.slice(Number(skip), Number(skip) + Number(limit));
    const total = filteredTransactions.length;

    res.json({
      success: true,
      transactions: paginatedTransactions,
      total,
      limit: Number(limit),
      skip: Number(skip)
    });
  } catch (error) {
    console.error("GET TRANSACTIONS ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// GET /api/credits/history - Get detailed credits history with breakdown
router.get("/history", requireAuth, async (req: AuthReq, res) => {
  try {
    const { limit = 100, skip = 0 } = req.query;
    
    // Get user's balance
    const user = await User.findById(req.userId).select('credits');
    if (!user) {
      return res.status(404).json({ 
        success: false,
        message: "User not found" 
      });
    }

    // Get all transactions - filter properly to only show user's transactions
    // For credits earned (quiz, referral, signup, self_download): show if user is toUser
    // For transfers sent: show if user is fromUser
    // For transfers received: show if user is toUser
    
    const userId = req.userId;
    
    // Fetch all potential transactions first, then filter properly
    const allTransactions = await Transaction.find({
      $or: [
        { toUser: userId },
        { fromUser: userId }
      ]
    })
    .populate('fromUser', 'name phone')
    .populate('toUser', 'name phone')
    .sort({ createdAt: -1 })
    .lean();

    // Filter transactions based on type and user role
    // Ensure userId is a string for comparison
    const userIdStr = userId?.toString();
    
    const transactions = allTransactions.filter((txn: any) => {
      // Safely extract IDs as strings - handle both populated and non-populated refs
      const fromUserId = txn.fromUser?._id?.toString() || (typeof txn.fromUser === 'object' ? txn.fromUser?.toString() : txn.fromUser);
      const toUserId = txn.toUser?._id?.toString() || (typeof txn.toUser === 'object' ? txn.toUser?.toString() : txn.toUser);
      
      switch (txn.type) {
        case 'signup_bonus':
        case 'referral_bonus':
        case 'quiz_bonus':
        case 'self_download_bonus':
          // User should be the receiver
          return toUserId === userIdStr;
        
        case 'transfer_sent':
          // User should be the sender (fromUser) - only sender sees this
          return fromUserId === userIdStr;
        
        case 'transfer_received':
          // User should be the receiver (toUser) - only receiver sees this
          return toUserId === userIdStr;
        
        case 'ad_deduction':
          // User should be the one being deducted (fromUser)
          return fromUserId === userIdStr;
        
        default:
          return false;
      }
    }).slice(0, Number(limit));

    // Calculate breakdown by type - use the already filtered transactions
    const breakdown = {
      quizCredits: 0,
      referralCredits: 0,
      signupBonus: 0,
      selfDownloadCredits: 0,
      transferReceived: 0,
      transferSent: 0,
      adDeductions: 0,
    };

    // Use filtered allTransactions for breakdown
    allTransactions.forEach((txn: any) => {
      // Safely extract IDs as strings - handle both populated and non-populated refs
      const fromUserId = txn.fromUser?._id?.toString() || (typeof txn.fromUser === 'object' ? txn.fromUser?.toString() : txn.fromUser);
      const toUserId = txn.toUser?._id?.toString() || (typeof txn.toUser === 'object' ? txn.toUser?.toString() : txn.toUser);
      
      switch (txn.type) {
        case 'quiz_bonus':
          if (toUserId === userIdStr) breakdown.quizCredits += txn.amount;
          break;
        case 'referral_bonus':
          if (toUserId === userIdStr) breakdown.referralCredits += txn.amount;
          break;
        case 'signup_bonus':
          if (toUserId === userIdStr) breakdown.signupBonus += txn.amount;
          break;
        case 'self_download_bonus':
          if (toUserId === userIdStr) breakdown.selfDownloadCredits += txn.amount;
          break;
        case 'transfer_received':
          if (toUserId === userIdStr) breakdown.transferReceived += txn.amount;
          break;
        case 'transfer_sent':
          if (fromUserId === userIdStr) breakdown.transferSent += Math.abs(txn.amount);
          break;
        case 'ad_deduction':
          if (fromUserId === userIdStr) breakdown.adDeductions += Math.abs(txn.amount);
          break;
      }
    });

    const total = transactions.length;

    res.json({
      success: true,
      totalCredits: (user as any).credits || 0,
      breakdown,
      transactions,
      total,
      limit: Number(limit),
      skip: Number(skip)
    });
  } catch (error) {
    console.error("GET CREDITS HISTORY ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// POST /api/credits/search-users - Search users by phone number only
router.post("/search-users", requireAuth, async (req: AuthReq, res) => {
  try {
    // Support both 'query' (new) and 'phonePrefix' (old) for backward compatibility
    const { query, phonePrefix } = req.body;
    let searchTerm = query || phonePrefix;
    
    console.log('🔍 [SEARCH] Raw input:', searchTerm);
    
    if (!searchTerm) {
      return res.status(400).json({ 
        success: false,
        message: "Phone number is required" 
      });
    }

    // Clean the search term - keep only digits
    searchTerm = searchTerm.replace(/[^0-9]/g, '');
    console.log('🔍 [SEARCH] Cleaned to digits only:', searchTerm);
    
    // Simple search: Find any phone that CONTAINS these digits
    // This works regardless of phone format (+8801xxx, +880xxx, or any other format)
    const users = await User.find({
      _id: { $ne: req.userId }, // Exclude current user
      phone: { $regex: searchTerm, $options: 'i' } // Contains search
    })
    .select('name phone profilePicture credits')
    .limit(20)
    .lean();

    console.log(`✅ [SEARCH] Found ${users.length} users`);
    if (users.length > 0) {
      console.log('📱 [SEARCH] Sample results:', users.slice(0, 3).map(u => ({ name: u.name, phone: u.phone })));
    } else {
      // Log all phones in DB for debugging (first 5)
      const allUsers = await User.find().select('phone').limit(5).lean();
      console.log('📱 [SEARCH] Sample phones in DB:', allUsers.map(u => u.phone));
    }

    res.json({
      success: true,
      users: users.map(user => ({
        _id: user._id,
        name: user.name,
        phone: user.phone,
        profilePicture: user.profilePicture,
        credits: (user as any).credits || 0,
        // Display phone as-is
        displayPhone: user.phone
      }))
    });
  } catch (error) {
    console.error("❌ [SEARCH] ERROR:", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// POST /api/credits/transfer - Transfer credits to another user
router.post("/transfer", requireAuth, async (req: AuthReq, res) => {
  try {
    const { toUserId, amount, note } = req.body;
    
    // Validation
    if (!toUserId || !amount) {
      return res.status(400).json({ 
        success: false,
        message: "Recipient and amount are required" 
      });
    }

    if (amount <= 0 || amount < 10) {
      return res.status(400).json({ 
        success: false,
        message: "Minimum transfer amount is 10 credits" 
      });
    }

    if (amount > 10000) {
      return res.status(400).json({ 
        success: false,
        message: "Maximum transfer amount is 10,000 credits" 
      });
    }

    if (toUserId === req.userId) {
      return res.status(400).json({ 
        success: false,
        message: "Cannot transfer credits to yourself" 
      });
    }

    // Get sender
    const sender = await User.findById(req.userId);
    if (!sender) {
      return res.status(404).json({ 
        success: false,
        message: "Sender not found" 
      });
    }

    // Check if sender's credits have expired
    const senderExpiryDate = (sender as any).creditsExpiryDate;
    if (senderExpiryDate && new Date() > new Date(senderExpiryDate)) {
      // Credits expired, set to 0
      (sender as any).credits = 0;
      await sender.save();
      
      return res.status(400).json({ 
        success: false,
        message: "Your credits have expired (valid for 1 month from signup). Please contact support to renew." 
      });
    }

    // Check if sender has enough credits
    const senderCredits = (sender as any).credits || 0;
    if (senderCredits < amount) {
      return res.status(400).json({ 
        success: false,
        message: `Insufficient credits. You have ${senderCredits} credits` 
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

    // Generate unique transaction ID
    const generateTransactionId = () => {
      const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
      let id = 'TXN';
      for (let i = 0; i < 9; i++) {
        id += chars.charAt(Math.floor(Math.random() * chars.length));
      }
      return id;
    };
    
    const transactionId = generateTransactionId();

    // Perform transfer
    const recipientCredits = (recipient as any).credits || 0;
    
    sender.set({ credits: senderCredits - amount });
    recipient.set({ credits: recipientCredits + amount });
    
    await sender.save();
    await recipient.save();

    // Create transaction records
    const transferDesc = note ? `Transfer to ${recipient.name}: ${note}` : `Transfer to ${recipient.name}`;
    const receiveDesc = note ? `Transfer from ${sender.name}: ${note}` : `Transfer from ${sender.name}`;
    
    // Transaction for sender (deduction) - store as NEGATIVE amount
    const senderTransaction = await Transaction.create({
      type: 'transfer_sent',
      fromUser: sender._id,
      toUser: recipient._id,
      amount: -amount,  // Negative for deduction
      description: transferDesc,
      balanceBefore: senderCredits,
      balanceAfter: senderCredits - amount,
      status: 'completed'
    });

    // Transaction for recipient (addition) - store as POSITIVE amount
    const recipientTransaction = await Transaction.create({
      type: 'transfer_received',
      fromUser: sender._id,
      toUser: recipient._id,
      amount: amount,  // Positive for addition
      description: receiveDesc,
      balanceBefore: recipientCredits,
      balanceAfter: recipientCredits + amount,
      status: 'completed'
    });

    console.log(`✅ Transfer successful: ${sender.name} sent ${amount} credits to ${recipient.name} (${transactionId})`);

    res.json({
      success: true,
      message: `Successfully transferred ${amount} credits to ${recipient.name}`,
      newBalance: senderCredits - amount,
      transaction: {
        id: transactionId,
        transactionId: transactionId,
        amount,
        date: new Date().toLocaleString('en-US', {
          month: 'short',
          day: 'numeric',
          year: 'numeric',
          hour: 'numeric',
          minute: '2-digit',
          hour12: true,
        }),
        to: {
          _id: recipient._id,
          name: recipient.name,
          phone: recipient.phone
        },
        note: note || ''
      }
    });
  } catch (error) {
    console.error("TRANSFER CREDITS ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error during transfer" 
    });
  }
});

// GET /api/credits/config - Get current credit configuration (PUBLIC - no auth required)
router.get("/config", async (req, res) => {
  try {
    // Find the credit config (should only be one document)
    let config = await CreditConfig.findOne();
    
    // If no config exists, create default one
    if (!config) {
      config = await CreditConfig.create({
        signupBonus: 200,
        referralReward: 300,
        lastUpdatedBy: 'system',
        lastUpdatedAt: new Date()
      });
      console.log('✅ Created default credit config:', config);
    }

    res.json({
      success: true,
      config: {
        signupBonus: config.signupBonus,
        referralReward: config.referralReward,
        lastUpdatedAt: config.lastUpdatedAt,
        lastUpdatedBy: config.lastUpdatedBy
      }
    });
  } catch (error) {
    console.error("GET CREDIT CONFIG ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// GET /api/credits/referral-stats - Get user's referral stats and history (REQUIRES AUTH)
router.get("/referral-stats", requireAuth, async (req: AuthReq, res) => {
  try {
    // Get current user
    let user = await User.findById(req.userId).select('name referralCode');
    
    if (!user) {
      return res.status(404).json({ 
        success: false,
        message: "User not found" 
      });
    }

    // If user doesn't have a referral code, generate one now
    if (!(user as any).referralCode) {
      console.log('⚠️ User missing referral code, generating now...');
      let newReferralCode = generateReferralCode();
      let codeExists = await User.findOne({ referralCode: newReferralCode });
      
      // Ensure uniqueness
      while (codeExists) {
        newReferralCode = generateReferralCode();
        codeExists = await User.findOne({ referralCode: newReferralCode });
      }
      
      // Update user with new referral code
      (user as any).referralCode = newReferralCode;
      await user.save();
      console.log(`✅ Generated referral code for user ${req.userId}: ${newReferralCode}`);
    }

    // Count total referrals (users who used this user's referral code)
    const referralCount = await User.countDocuments({ referredBy: req.userId });

    // Get referral history with details
    const referredUsers = await User.find({ referredBy: req.userId })
      .select('name phone createdAt')
      .sort({ createdAt: -1 })
      .limit(50)
      .lean();

    // Get referral bonus transactions
    const referralTransactions = await Transaction.find({
      type: 'referral_bonus',
      toUser: req.userId
    })
      .populate('fromUser', 'name phone')
      .sort({ createdAt: -1 })
      .limit(50)
      .lean();

    // Calculate total credits earned from referrals
    const totalReferralCredits = referralTransactions.reduce((sum, tx) => sum + (tx.amount || 0), 0);

    // Format referral history
    const referralHistory = referralTransactions.map(tx => ({
      name: (tx.fromUser as any)?.name || 'Unknown',
      phone: (tx.fromUser as any)?.phone,
      date: tx.createdAt,
      credits: tx.amount,
      status: 'credited'
    }));

    res.json({
      success: true,
      referralCode: (user as any).referralCode,
      totalReferrals: referralCount,
      totalCreditsEarned: totalReferralCredits,
      recentReferrals: referralHistory
    });
  } catch (error) {
    console.error("GET REFERRAL STATS ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// PUT /api/credits/config - Update credit configuration (ADMIN ONLY)
router.put("/config", async (req, res) => {
  try {
    // Simple admin authentication - check for admin key in header
    const adminKey = req.headers['x-admin-key'] as string;
    
    if (adminKey !== process.env.ADMIN_SECRET_KEY && adminKey !== 'your-secure-admin-key-here') {
      return res.status(401).json({ 
        success: false,
        message: "Unauthorized - Admin access required" 
      });
    }

    const { signupBonus, referralReward, updatedBy } = req.body;

    // Validation
    if (signupBonus !== undefined && (signupBonus < 0 || !Number.isInteger(signupBonus))) {
      return res.status(400).json({ 
        success: false,
        message: "Signup bonus must be a non-negative integer" 
      });
    }

    if (referralReward !== undefined && (referralReward < 0 || !Number.isInteger(referralReward))) {
      return res.status(400).json({ 
        success: false,
        message: "Referral reward must be a non-negative integer" 
      });
    }

    // Find existing config or create new one
    let config = await CreditConfig.findOne();
    
    if (!config) {
      config = new CreditConfig({
        signupBonus: signupBonus || 200,
        referralReward: referralReward || 300,
        lastUpdatedBy: updatedBy || 'admin',
        lastUpdatedAt: new Date()
      });
    } else {
      if (signupBonus !== undefined) config.signupBonus = signupBonus;
      if (referralReward !== undefined) config.referralReward = referralReward;
      config.lastUpdatedBy = updatedBy || 'admin';
      config.lastUpdatedAt = new Date();
    }

    await config.save();

    console.log(`✅ Credit config updated by ${config.lastUpdatedBy}:`, {
      signupBonus: config.signupBonus,
      referralReward: config.referralReward
    });

    res.json({
      success: true,
      message: "Credit configuration updated successfully",
      config: {
        signupBonus: config.signupBonus,
        referralReward: config.referralReward,
        lastUpdatedAt: config.lastUpdatedAt,
        lastUpdatedBy: config.lastUpdatedBy
      }
    });
  } catch (error) {
    console.error("UPDATE CREDIT CONFIG ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

export default router;