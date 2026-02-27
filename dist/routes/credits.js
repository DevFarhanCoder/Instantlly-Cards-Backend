"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const User_1 = __importDefault(require("../models/User"));
const Transaction_1 = __importDefault(require("../models/Transaction"));
const CreditConfig_1 = __importDefault(require("../models/CreditConfig"));
const auth_1 = require("../middleware/auth");
const pushNotifications_1 = require("../services/pushNotifications");
const Notification_1 = __importDefault(require("../models/Notification"));
// Force language server refresh
// Force language server refresh
const router = (0, express_1.Router)();
// Helper function to generate referral code
function generateReferralCode() {
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
    let code = '';
    for (let i = 0; i < 6; i++) {
        code += chars.charAt(Math.floor(Math.random() * chars.length));
    }
    return code;
}
// DEBUG ENDPOINT - Get sample phone numbers from database
router.get("/debug-phones", auth_1.requireAuth, async (req, res) => {
    try {
        const users = await User_1.default.find()
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
    }
    catch (error) {
        console.error("DEBUG PHONES ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// GET /api/credits/balance - Get current credits balance
router.get("/balance", auth_1.requireAuth, async (req, res) => {
    try {
        const user = await User_1.default.findById(req.userId).select('credits creditsExpiryDate');
        if (!user) {
            return res.status(404).json({ message: "User not found" });
        }
        const currentCredits = user.credits || 0;
        // Fixed expiry date: March 31, 2026
        const fixedExpiryDate = new Date('2026-03-31T23:59:59');
        // Check if credits have expired
        let activeCredits = currentCredits;
        let isExpired = false;
        if (new Date() > fixedExpiryDate) {
            // Credits expired, set to 0
            isExpired = true;
            activeCredits = 0;
            // Update user credits to 0 if not already
            if (currentCredits > 0) {
                user.credits = 0;
                await user.save();
                console.log(`⏰ Credits expired for user ${req.userId}, reset to 0`);
            }
        }
        res.json({
            success: true,
            credits: activeCredits,
            creditsExpiryDate: fixedExpiryDate.toISOString(),
            isExpired: isExpired,
            daysRemaining: Math.max(0, Math.ceil((fixedExpiryDate.getTime() - Date.now()) / (1000 * 60 * 60 * 24)))
        });
    }
    catch (error) {
        console.error("GET BALANCE ERROR", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
// GET /api/credits/transactions - Get user's credit transaction history
router.get("/transactions", auth_1.requireAuth, async (req, res) => {
    try {
        const { limit = 50, skip = 0 } = req.query;
        const userId = req.userId;
        const userIdStr = userId?.toString();
        console.log(`📊 [TRANSACTIONS] Fetching for user: ${userIdStr}`);
        // Fetch all potential transactions first
        const allTransactions = await Transaction_1.default.find({
            $or: [
                { fromUser: userId },
                { toUser: userId }
            ]
        })
            .populate('fromUser', 'name phone')
            .populate('toUser', 'name phone')
            .sort({ createdAt: -1 })
            .lean();
        console.log(`📊 [TRANSACTIONS] Found ${allTransactions.length} raw transactions`);
        // Filter transactions based on type and user role
        // This ensures each user only sees their relevant transaction record
        const filteredTransactions = allTransactions.map((txn) => {
            // Safely extract IDs as strings - handle both populated and non-populated refs
            const fromUserId = txn.fromUser?._id?.toString() || (typeof txn.fromUser === 'object' ? txn.fromUser?.toString() : txn.fromUser);
            const toUserId = txn.toUser?._id?.toString() || (typeof txn.toUser === 'object' ? txn.toUser?.toString() : txn.toUser);
            const isSender = fromUserId === userIdStr;
            const isReceiver = toUserId === userIdStr;
            console.log(`   Type: ${txn.type}, fromUser: ${fromUserId}, toUser: ${toUserId}, isSender: ${isSender}, isReceiver: ${isReceiver}`);
            // For 'transfer' type (new single record), transform based on viewer's perspective
            if (txn.type === 'transfer') {
                // Check if description is just the default "Credit transfer" or a custom note
                const isDefaultNote = !txn.description || txn.description === 'Credit transfer';
                if (isSender) {
                    return {
                        ...txn,
                        _id: txn._id,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        type: 'transfer_sent',
                        amount: -Math.abs(txn.amount),
                        // Always show "Transfer to [Name]" as main description
                        description: `Transfer to ${txn.toUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description
                    };
                }
                else if (isReceiver) {
                    return {
                        ...txn,
                        _id: txn._id,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        type: 'transfer_received',
                        amount: Math.abs(txn.amount),
                        // Always show "Transfer from [Name]" as main description
                        description: `Transfer from ${txn.fromUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description
                    };
                }
                return null;
            }
            // Handle legacy transfer_sent - ONLY show to the actual sender (fromUser)
            if (txn.type === 'transfer_sent') {
                if (isSender) {
                    console.log(`   ✓ Showing transfer_sent to sender`);
                    // Transform legacy description to proper format
                    const isDefaultNote = !txn.description || txn.description === 'Credit transfer';
                    return {
                        ...txn,
                        _id: txn._id,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        amount: -Math.abs(txn.amount), // Ensure negative for sent
                        description: `Transfer to ${txn.toUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description
                    };
                }
                console.log(`   ✗ Hiding transfer_sent from receiver`);
                return null;
            }
            // Handle legacy transfer_received - ONLY show to the actual receiver (toUser)
            if (txn.type === 'transfer_received') {
                // IMPORTANT: Only show to receiver AND NOT to sender
                if (isReceiver && !isSender) {
                    console.log(`   ✓ Showing transfer_received to receiver (NOT sender)`);
                    // Transform legacy description to proper format
                    const isDefaultNote = !txn.description || txn.description === 'Credit transfer';
                    return {
                        ...txn,
                        _id: txn._id,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        amount: Math.abs(txn.amount), // Ensure positive for received
                        description: `Transfer from ${txn.fromUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description
                    };
                }
                console.log(`   ✗ Hiding transfer_received (isSender=${isSender}, isReceiver=${isReceiver})`);
                return null;
            }
            // For bonus types, user must be the receiver
            if (['signup_bonus', 'referral_bonus', 'quiz_bonus', 'self_download_bonus'].includes(txn.type)) {
                return isReceiver ? txn : null;
            }
            // For ad_deduction, user must be fromUser
            if (txn.type === 'ad_deduction') {
                return isSender ? txn : null;
            }
            // For admin_adjustment, user must be the receiver (toUser)
            if (txn.type === 'admin_adjustment') {
                return isReceiver ? txn : null;
            }
            return null;
        }).filter((txn) => txn !== null);
        console.log(`📊 [TRANSACTIONS] After filtering: ${filteredTransactions.length} transactions`);
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
    }
    catch (error) {
        console.error("GET CREDITS HISTORY ERROR", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
// GET /api/credits/history - Get detailed credits history with breakdown
router.get("/history", auth_1.requireAuth, async (req, res) => {
    try {
        const { limit = 100, skip = 0 } = req.query;
        // Get user's balance
        const user = await User_1.default.findById(req.userId).select('credits');
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found"
            });
        }
        const userId = req.userId;
        const userIdStr = userId?.toString();
        console.log(`📊 [HISTORY] Fetching for user: ${userIdStr}`);
        // Fetch all potential transactions first, then filter properly
        const allTransactions = await Transaction_1.default.find({
            $or: [
                { toUser: userId },
                { fromUser: userId }
            ]
        })
            .populate('fromUser', 'name phone')
            .populate('toUser', 'name phone')
            .sort({ createdAt: -1 })
            .lean();
        console.log(`📊 [HISTORY] Found ${allTransactions.length} raw transactions`);
        // Filter transactions based on type and user role
        const transactions = allTransactions.map((txn) => {
            // Safely extract IDs as strings
            const fromUserId = txn.fromUser?._id?.toString() || (typeof txn.fromUser === 'object' ? txn.fromUser?.toString() : txn.fromUser);
            const toUserId = txn.toUser?._id?.toString() || (typeof txn.toUser === 'object' ? txn.toUser?.toString() : txn.toUser);
            const isSender = fromUserId === userIdStr;
            const isReceiver = toUserId === userIdStr;
            // DEBUG: Log transaction details
            console.log(`📊 [HISTORY] Processing txn: type=${txn.type}, fromUser=${txn.fromUser?.name}, toUser=${txn.toUser?.name}, isSender=${isSender}, isReceiver=${isReceiver}, originalDesc=${txn.description}`);
            // For 'transfer' type (new single record), transform based on viewer's perspective
            if (txn.type === 'transfer') {
                // Check if description is just the default "Credit transfer" or a custom note
                const isDefaultNote = !txn.description || txn.description === 'Credit transfer';
                if (isSender) {
                    return {
                        ...txn,
                        _id: txn._id,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        type: 'transfer_sent',
                        amount: -Math.abs(txn.amount),
                        // Always show "Transfer to [Name]" as main description
                        description: `Transfer to ${txn.toUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description
                    };
                }
                else if (isReceiver) {
                    return {
                        ...txn,
                        _id: txn._id,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        type: 'transfer_received',
                        amount: Math.abs(txn.amount),
                        // Always show "Transfer from [Name]" as main description
                        description: `Transfer from ${txn.fromUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description
                    };
                }
                return null;
            }
            // Handle legacy transfer_sent - ONLY show to the actual sender
            if (txn.type === 'transfer_sent') {
                if (isSender) {
                    // Transform legacy description to proper format
                    const isDefaultNote = !txn.description || txn.description === 'Credit transfer';
                    const transformedAmount = -Math.abs(txn.amount);
                    console.log(`🔄 [TRANSFORM] transfer_sent: ${txn.amount} → ${transformedAmount}`);
                    return {
                        _id: txn._id,
                        type: txn.type,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        amount: transformedAmount, // Explicitly set negative amount
                        description: `Transfer to ${txn.toUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description,
                        fromUser: txn.fromUser,
                        toUser: txn.toUser
                    };
                }
                return null;
            }
            // Handle legacy transfer_received - ONLY show to the actual receiver  
            if (txn.type === 'transfer_received') {
                // IMPORTANT: Only show to receiver AND NOT to sender
                if (isReceiver && !isSender) {
                    // Transform legacy description to proper format
                    const isDefaultNote = !txn.description || txn.description === 'Credit transfer';
                    const transformedAmount = Math.abs(txn.amount);
                    console.log(`🔄 [TRANSFORM] transfer_received: ${txn.amount} → ${transformedAmount}`);
                    return {
                        _id: txn._id,
                        type: txn.type,
                        transactionId: txn.transactionId,
                        createdAt: txn.createdAt,
                        amount: transformedAmount, // Explicitly set positive amount
                        description: `Transfer from ${txn.fromUser?.name || 'User'}`,
                        note: isDefaultNote ? undefined : txn.description,
                        fromUser: txn.fromUser,
                        toUser: txn.toUser
                    };
                }
                return null;
            }
            // For bonus types, user must be the receiver
            if (['signup_bonus', 'referral_bonus', 'quiz_bonus', 'self_download_bonus'].includes(txn.type)) {
                return isReceiver ? txn : null;
            }
            // For ad_deduction, user must be fromUser
            if (txn.type === 'ad_deduction') {
                return isSender ? txn : null;
            }
            // For admin_adjustment, user must be the receiver (toUser)
            if (txn.type === 'admin_adjustment') {
                return isReceiver ? txn : null;
            }
            return null;
        }).filter((txn) => txn !== null).slice(0, Number(limit));
        console.log(`📊 [HISTORY] After filtering: ${transactions.length} transactions`);
        // Calculate breakdown by type
        const breakdown = {
            quizCredits: 0,
            referralCredits: 0,
            signupBonus: 0,
            selfDownloadCredits: 0,
            transferReceived: 0,
            transferSent: 0,
            adDeductions: 0,
        };
        // Use allTransactions for breakdown but with proper filtering
        allTransactions.forEach((txn) => {
            const fromUserId = txn.fromUser?._id?.toString() || (typeof txn.fromUser === 'object' ? txn.fromUser?.toString() : txn.fromUser);
            const toUserId = txn.toUser?._id?.toString() || (typeof txn.toUser === 'object' ? txn.toUser?.toString() : txn.toUser);
            const isSender = fromUserId === userIdStr;
            const isReceiver = toUserId === userIdStr;
            switch (txn.type) {
                case 'quiz_bonus':
                    if (isReceiver)
                        breakdown.quizCredits += txn.amount;
                    break;
                case 'referral_bonus':
                    if (isReceiver)
                        breakdown.referralCredits += txn.amount;
                    break;
                case 'signup_bonus':
                    if (isReceiver)
                        breakdown.signupBonus += txn.amount;
                    break;
                case 'self_download_bonus':
                    if (isReceiver)
                        breakdown.selfDownloadCredits += txn.amount;
                    break;
                case 'transfer':
                    // Handle new unified transfer type
                    if (isSender) {
                        breakdown.transferSent += Math.abs(txn.amount);
                    }
                    else if (isReceiver) {
                        breakdown.transferReceived += Math.abs(txn.amount);
                    }
                    break;
                case 'transfer_received':
                    // Legacy type - only count if user is actual receiver
                    if (isReceiver)
                        breakdown.transferReceived += Math.abs(txn.amount);
                    break;
                case 'transfer_sent':
                    // Legacy type - only count if user is actual sender
                    if (isSender)
                        breakdown.transferSent += Math.abs(txn.amount);
                    break;
                case 'ad_deduction':
                    if (isSender)
                        breakdown.adDeductions += Math.abs(txn.amount);
                    break;
            }
        });
        const total = transactions.length;
        res.json({
            success: true,
            totalCredits: user.credits || 0,
            breakdown,
            transactions,
            total,
            limit: Number(limit),
            skip: Number(skip)
        });
    }
    catch (error) {
        console.error("GET CREDITS HISTORY ERROR", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
// POST /api/credits/search-users - Search users by phone number only
router.post("/search-users", auth_1.requireAuth, async (req, res) => {
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
        const users = await User_1.default.find({
            _id: { $ne: req.userId }, // Exclude current user
            phone: { $regex: searchTerm, $options: 'i' } // Contains search
        })
            .select('name phone profilePicture credits')
            .limit(20)
            .lean();
        console.log(`✅ [SEARCH] Found ${users.length} users`);
        if (users.length > 0) {
            console.log('📱 [SEARCH] Sample results:', users.slice(0, 3).map(u => ({ name: u.name, phone: u.phone })));
        }
        else {
            // Log all phones in DB for debugging (first 5)
            const allUsers = await User_1.default.find().select('phone').limit(5).lean();
            console.log('📱 [SEARCH] Sample phones in DB:', allUsers.map(u => u.phone));
        }
        res.json({
            success: true,
            users: users.map(user => ({
                _id: user._id,
                name: user.name,
                phone: user.phone,
                profilePicture: user.profilePicture,
                credits: user.credits || 0,
                // Display phone as-is
                displayPhone: user.phone
            }))
        });
    }
    catch (error) {
        console.error("❌ [SEARCH] ERROR:", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
// POST /api/credits/transfer - Transfer credits to another user
router.post("/transfer", auth_1.requireAuth, async (req, res) => {
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
        if (toUserId === req.userId) {
            return res.status(400).json({
                success: false,
                message: "Cannot transfer credits to yourself"
            });
        }
        // Get sender
        const sender = await User_1.default.findById(req.userId);
        if (!sender) {
            return res.status(404).json({
                success: false,
                message: "Sender not found"
            });
        }
        // Check if sender's credits have expired
        const senderExpiryDate = sender.creditsExpiryDate;
        if (senderExpiryDate && new Date() > new Date(senderExpiryDate)) {
            // Credits expired, set to 0
            sender.credits = 0;
            await sender.save();
            return res.status(400).json({
                success: false,
                message: "Your credits have expired (valid for 1 month from signup). Please contact support to renew."
            });
        }
        // Check if sender has enough credits
        const senderCredits = sender.credits || 0;
        if (senderCredits < amount) {
            return res.status(400).json({
                success: false,
                message: `Insufficient credits. You have ${senderCredits} credits`
            });
        }
        // Get recipient (include pushToken for notifications)
        const recipient = await User_1.default.findById(toUserId).select('+pushToken');
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
        const recipientCredits = recipient.credits || 0;
        sender.set({ credits: senderCredits - amount });
        recipient.set({ credits: recipientCredits + amount });
        await sender.save();
        await recipient.save();
        // Create SINGLE transaction record for the transfer
        // This will be interpreted differently based on who is viewing it
        const transferTransaction = await Transaction_1.default.create({
            type: 'transfer',
            transactionId: transactionId,
            fromUser: sender._id,
            toUser: recipient._id,
            amount: amount, // Store as positive amount
            description: note || 'Credit transfer',
            balanceBefore: senderCredits,
            balanceAfter: senderCredits - amount,
            status: 'completed'
        });
        console.log(`✅ Transfer successful: ${sender.name} sent ${amount} credits to ${recipient.name} (${transactionId})`);
        // Send push notification to recipient
        try {
            console.log('🔔 [CREDIT TRANSFER] Checking for pushToken...');
            console.log(`📱 Recipient pushToken: ${recipient.pushToken ? 'EXISTS' : 'NOT FOUND'}`);
            if (recipient.pushToken) {
                const notificationTitle = "💰 Credits Received!";
                const notificationMessage = `Received ${amount.toLocaleString('en-IN')} credits from ${sender.name}`;
                console.log(`📤 [CREDIT TRANSFER] Sending push notification to ${recipient.name}`);
                console.log(`   Title: ${notificationTitle}`);
                console.log(`   Message: ${notificationMessage}`);
                console.log(`   Token: ${recipient.pushToken.substring(0, 30)}...`);
                const notificationSent = await (0, pushNotifications_1.sendPushNotification)(recipient.pushToken, notificationTitle, notificationMessage, {
                    type: 'CREDIT_RECEIVED',
                    senderId: sender._id.toString(),
                    senderName: sender.name,
                    amount,
                    transactionId
                }, 'default' // Use default channel for backward compatibility with old APKs
                );
                console.log(`📬 Push notification result: ${notificationSent ? 'SUCCESS' : 'FAILED'}`);
                // Save notification to database
                await Notification_1.default.create({
                    userId: recipient._id,
                    type: 'GENERAL',
                    title: notificationTitle,
                    message: notificationMessage,
                    data: {
                        senderId: sender._id,
                        senderName: sender.name,
                        amount,
                        transactionId
                    },
                    read: false
                });
                console.log(`✅ Notification saved to database for ${recipient.name}`);
            }
            else {
                console.log(`⚠️ [CREDIT TRANSFER] Recipient ${recipient.name} has no push token - skipping notification`);
            }
        }
        catch (notifError) {
            console.error('❌ [CREDIT TRANSFER] Failed to send notification:', notifError);
            console.error('❌ Error details:', notifError.message);
            console.error('❌ Stack:', notifError.stack);
            // Don't fail the transfer if notification fails
        }
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
    }
    catch (error) {
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
        let config = await CreditConfig_1.default.findOne();
        // If no config exists, create default one
        if (!config) {
            config = await CreditConfig_1.default.create({
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
    }
    catch (error) {
        console.error("GET CREDIT CONFIG ERROR", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
// GET /api/credits/referral-stats - Get user's referral stats and history (REQUIRES AUTH)
router.get("/referral-stats", auth_1.requireAuth, async (req, res) => {
    try {
        // Get current user
        let user = await User_1.default.findById(req.userId).select('name referralCode');
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found"
            });
        }
        // If user doesn't have a referral code, generate one now
        if (!user.referralCode) {
            console.log('⚠️ User missing referral code, generating now...');
            let newReferralCode = generateReferralCode();
            let codeExists = await User_1.default.findOne({ referralCode: newReferralCode });
            // Ensure uniqueness
            while (codeExists) {
                newReferralCode = generateReferralCode();
                codeExists = await User_1.default.findOne({ referralCode: newReferralCode });
            }
            // Update user with new referral code
            user.referralCode = newReferralCode;
            await user.save();
            console.log(`✅ Generated referral code for user ${req.userId}: ${newReferralCode}`);
        }
        // Count total referrals (users who used this user's referral code)
        const referralCount = await User_1.default.countDocuments({ referredBy: req.userId });
        // Get referral history with details
        const referredUsers = await User_1.default.find({ referredBy: req.userId })
            .select('name phone createdAt')
            .sort({ createdAt: -1 })
            .limit(50)
            .lean();
        // Get referral bonus transactions
        const referralTransactions = await Transaction_1.default.find({
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
            name: tx.fromUser?.name || 'Unknown',
            phone: tx.fromUser?.phone,
            date: tx.createdAt,
            credits: tx.amount,
            status: 'credited'
        }));
        res.json({
            success: true,
            referralCode: user.referralCode,
            totalReferrals: referralCount,
            totalCreditsEarned: totalReferralCredits,
            recentReferrals: referralHistory
        });
    }
    catch (error) {
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
        const adminKey = req.headers['x-admin-key'];
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
        let config = await CreditConfig_1.default.findOne();
        if (!config) {
            config = new CreditConfig_1.default({
                signupBonus: signupBonus || 200,
                referralReward: referralReward || 300,
                lastUpdatedBy: updatedBy || 'admin',
                lastUpdatedAt: new Date()
            });
        }
        else {
            if (signupBonus !== undefined)
                config.signupBonus = signupBonus;
            if (referralReward !== undefined)
                config.referralReward = referralReward;
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
    }
    catch (error) {
        console.error("UPDATE CREDIT CONFIG ERROR", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
// GET /api/credits/config - Get current credit configuration (PUBLIC - no auth required)
router.get("/config", async (req, res) => {
    try {
        // Find the credit config (should only be one document)
        let config = await CreditConfig_1.default.findOne();
        // If no config exists, create default one
        if (!config) {
            config = await CreditConfig_1.default.create({
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
    }
    catch (error) {
        console.error("GET CREDIT CONFIG ERROR", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
// GET /api/credits/referral-stats - Get user's referral stats and history (REQUIRES AUTH)
router.get("/referral-stats", auth_1.requireAuth, async (req, res) => {
    try {
        // Get current user
        let user = await User_1.default.findById(req.userId).select('name referralCode');
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found"
            });
        }
        // If user doesn't have a referral code, generate one now
        if (!user.referralCode) {
            console.log('⚠️ User missing referral code, generating now...');
            let newReferralCode = generateReferralCode();
            let codeExists = await User_1.default.findOne({ referralCode: newReferralCode });
            // Ensure uniqueness
            while (codeExists) {
                newReferralCode = generateReferralCode();
                codeExists = await User_1.default.findOne({ referralCode: newReferralCode });
            }
            // Update user with new referral code
            user.referralCode = newReferralCode;
            await user.save();
            console.log(`✅ Generated referral code for user ${req.userId}: ${newReferralCode}`);
        }
        // Count total referrals (users who used this user's referral code)
        const referralCount = await User_1.default.countDocuments({ referredBy: req.userId });
        // Get referral history with details
        const referredUsers = await User_1.default.find({ referredBy: req.userId })
            .select('name phone createdAt')
            .sort({ createdAt: -1 })
            .limit(50)
            .lean();
        // Get referral bonus transactions
        const referralTransactions = await Transaction_1.default.find({
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
            name: tx.fromUser?.name || 'Unknown',
            phone: tx.fromUser?.phone,
            date: tx.createdAt,
            credits: tx.amount,
            status: 'credited'
        }));
        res.json({
            success: true,
            referralCode: user.referralCode,
            totalReferrals: referralCount,
            totalCreditsEarned: totalReferralCredits,
            recentReferrals: referralHistory
        });
    }
    catch (error) {
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
        const adminKey = req.headers['x-admin-key'];
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
        let config = await CreditConfig_1.default.findOne();
        if (!config) {
            config = new CreditConfig_1.default({
                signupBonus: signupBonus || 200,
                referralReward: referralReward || 300,
                lastUpdatedBy: updatedBy || 'admin',
                lastUpdatedAt: new Date()
            });
        }
        else {
            if (signupBonus !== undefined)
                config.signupBonus = signupBonus;
            if (referralReward !== undefined)
                config.referralReward = referralReward;
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
    }
    catch (error) {
        console.error("UPDATE CREDIT CONFIG ERROR", error);
        res.status(500).json({
            success: false,
            message: "Server error"
        });
    }
});
exports.default = router;
