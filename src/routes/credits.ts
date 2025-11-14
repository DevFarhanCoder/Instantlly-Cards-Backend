import { Router } from "express";
import User from "../models/User";
import Transaction from "../models/Transaction";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

// GET /api/credits/balance - Get current credits balance
router.get("/balance", requireAuth, async (req: AuthReq, res) => {
  try {
    const user = await User.findById(req.userId).select('credits');
    
    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    res.json({
      success: true,
      credits: (user as any).credits || 0
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
    
    const transactions = await Transaction.find({
      $or: [
        { fromUser: req.userId },
        { toUser: req.userId }
      ]
    })
    .populate('fromUser', 'name phone')
    .populate('toUser', 'name phone')
    .sort({ createdAt: -1 })
    .limit(Number(limit))
    .skip(Number(skip));

    const total = await Transaction.countDocuments({
      $or: [
        { fromUser: req.userId },
        { toUser: req.userId }
      ]
    });

    res.json({
      success: true,
      transactions,
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

// POST /api/credits/search-users - Search users by phone number (starting with 88)
router.post("/search-users", requireAuth, async (req: AuthReq, res) => {
  try {
    const { phonePrefix } = req.body;
    
    if (!phonePrefix) {
      return res.status(400).json({ 
        success: false,
        message: "Phone prefix is required" 
      });
    }

    // Search for users whose phone contains the prefix (after +country code)
    // For Bangladesh, phone numbers are like +8801XXXXXXXXX or +88XXXXXXXXXX
    const users = await User.find({
      $and: [
        { _id: { $ne: req.userId } }, // Exclude current user
        { 
          $or: [
            { phone: { $regex: phonePrefix, $options: 'i' } },
            { phone: { $regex: `\\+88${phonePrefix}`, $options: 'i' } },
            { phone: { $regex: `\\+880${phonePrefix}`, $options: 'i' } }
          ]
        }
      ]
    })
    .select('name phone profilePicture')
    .limit(20);

    res.json({
      success: true,
      users: users.map(user => ({
        _id: user._id,
        name: user.name,
        phone: user.phone,
        profilePicture: user.profilePicture,
        // Extract just the phone number part after +88 for display
        displayPhone: user.phone.replace(/^\+880?/, '')
      }))
    });
  } catch (error) {
    console.error("SEARCH USERS ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error" 
    });
  }
});

// POST /api/credits/transfer - Transfer credits to another user
router.post("/transfer", requireAuth, async (req: AuthReq, res) => {
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

    // Perform transfer
    const recipientCredits = (recipient as any).credits || 0;
    
    sender.set({ credits: senderCredits - amount });
    recipient.set({ credits: recipientCredits + amount });
    
    await sender.save();
    await recipient.save();

    // Create transaction records
    const transferDesc = description || `Transfer to ${recipient.name}`;
    
    // Transaction for sender (deduction)
    await Transaction.create({
      type: 'transfer_sent',
      fromUser: sender._id,
      toUser: recipient._id,
      amount: amount,
      description: transferDesc,
      balanceBefore: senderCredits,
      balanceAfter: senderCredits - amount,
      status: 'completed'
    });

    // Transaction for recipient (addition)
    await Transaction.create({
      type: 'transfer_received',
      fromUser: sender._id,
      toUser: recipient._id,
      amount: amount,
      description: `Transfer from ${sender.name}`,
      balanceBefore: recipientCredits,
      balanceAfter: recipientCredits + amount,
      status: 'completed'
    });

    console.log(`✅ Transfer successful: ${sender.name} sent ${amount} credits to ${recipient.name}`);

    res.json({
      success: true,
      message: `Successfully transferred ${amount} credits to ${recipient.name}`,
      newBalance: senderCredits - amount,
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
    console.error("TRANSFER CREDITS ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error during transfer" 
    });
  }
});

export default router;
