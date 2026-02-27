"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const mongoose_1 = require("mongoose");
const auth_1 = require("../middleware/auth");
const User_1 = __importDefault(require("../models/User"));
const MlmCredit_1 = __importDefault(require("../models/MlmCredit"));
const Voucher_1 = __importDefault(require("../models/Voucher"));
const mlm_1 = require("../utils/mlm");
const walletService_1 = require("../services/mlm/walletService");
const creditLifecycleService_1 = require("../services/mlm/creditLifecycleService");
const voucherService_1 = require("../services/mlm/voucherService");
const treeService_1 = require("../services/mlm/treeService");
const creditDashboardService_1 = require("../services/mlm/creditDashboardService");
const directBuyersService_1 = require("../services/mlm/directBuyersService");
const discountService_1 = require("../services/mlm/discountService");
const downlineService_1 = require("../services/mlm/downlineService");
const router = (0, express_1.Router)();
const CREDIT_EXPIRY_MINUTES = 60;
const TRANSFER_EXPIRY_HOURS = 48;
const VOUCHER_PURCHASE_TIMEOUT_MINUTES = 60; // 1 hour to complete purchase
const CONNECTION_TIMEOUT_HOURS = 48; // 2 days to connect 5 people
const MIN_VOUCHERS_TO_UNLOCK = 5; // Must share 5 vouchers to unlock credit transfer
// ============================================
// DISTRIBUTION CREDITS
// ============================================
// Get distribution credits for MLM user (to be transferred to downline)
router.get("/distribution-credits", auth_1.requireAuth, async (req, res) => {
    try {
        const user = await User_1.default.findById(req.userId).select("parentId");
        // Only MLM users (those with introducers) get distribution credits
        if (!user?.parentId) {
            return res.json({ success: true, credits: [] });
        }
        // Get user's direct children (first level downline)
        const directChildren = await User_1.default.find({ parentId: req.userId })
            .select("name phone createdAt")
            .lean();
        // Calculate distribution credits for each child
        const distributionCredits = await Promise.all(directChildren.map(async (child, index) => {
            // Count vouchers shared with this child
            const vouchersShared = await Voucher_1.default.countDocuments({
                userId: child._id,
                source: "transfer",
                transferredFrom: req.userId,
            });
            // Check if credits are locked (need 5 vouchers shared)
            const isLocked = vouchersShared < MIN_VOUCHERS_TO_UNLOCK;
            // Calculate time left to connect (2 days from creation)
            const createdAt = new Date(child.createdAt);
            const timeoutAt = new Date(createdAt.getTime() + CONNECTION_TIMEOUT_HOURS * 60 * 60 * 1000);
            const timeLeft = Math.max(0, timeoutAt.getTime() - Date.now());
            const hoursLeft = Math.floor(timeLeft / (1000 * 60 * 60));
            const minutesLeft = Math.floor((timeLeft % (1000 * 60 * 60)) / (1000 * 60));
            return {
                level: 1, // Direct children are level 1
                creditsToTransfer: 293, // Example credit amount from the table
                recipientName: child.name,
                recipientPhone: child.phone,
                recipientId: child._id.toString(),
                vouchersShared,
                isLocked,
                timeLeft: `${hoursLeft}h ${minutesLeft}m`,
            };
        }));
        res.json({ success: true, credits: distributionCredits });
    }
    catch (error) {
        console.error("MLM DISTRIBUTION CREDITS ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// VOUCHER PURCHASE WITH TIMER
// ============================================
router.post("/vouchers/purchase", auth_1.requireAuth, async (req, res) => {
    try {
        const { quantity, totalAmount, paymentMethod } = req.body;
        // Validate quantity (must be multiple of 5)
        if (!quantity || quantity < 5 || quantity % 5 !== 0) {
            return res.status(400).json({
                success: false,
                message: "Quantity must be at least 5 and in multiples of 5",
            });
        }
        // Calculate expected amount (₹3600 for 5 vouchers with 40% discount)
        const VOUCHER_SET_PRICE = 3600; // Discounted price for 5 vouchers
        const sets = quantity / 5;
        const expectedAmount = sets * VOUCHER_SET_PRICE;
        if (totalAmount !== expectedAmount) {
            return res.status(400).json({
                success: false,
                message: `Invalid amount. Expected ₹${expectedAmount}`,
            });
        }
        // TODO: Integrate actual payment gateway (Razorpay, etc.)
        // For now, we'll simulate successful payment
        // Generate vouchers for user
        const user = await User_1.default.findById(req.userId);
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        // Create vouchers
        const vouchers = [];
        for (let i = 0; i < quantity; i++) {
            const voucher = await Voucher_1.default.create({
                userId: req.userId,
                voucherNumber: `INS-${Date.now()}-${i + 1}`,
                MRP: 1200, // ₹1200 per voucher
                amount: 1200,
                issueDate: new Date(),
                expiryDate: new Date(Date.now() + 365 * 24 * 60 * 60 * 1000), // 1 year
                redeemedStatus: "unredeemed",
                source: "purchase",
            });
            vouchers.push(voucher);
        }
        // Set connection timeout (2 days to connect 5 people)
        const connectionExpiresAt = new Date(Date.now() + CONNECTION_TIMEOUT_HOURS * 60 * 60 * 1000);
        res.json({
            success: true,
            message: `Successfully purchased ${quantity} vouchers`,
            vouchers: vouchers.map((v) => ({
                id: v._id.toString(),
                voucherNumber: v.voucherNumber,
                amount: v.amount,
            })),
            connectionExpiresAt,
            connectionTimeoutHours: CONNECTION_TIMEOUT_HOURS,
        });
    }
    catch (error) {
        console.error("MLM VOUCHER PURCHASE ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// WALLET & CREDITS
// ============================================
router.get("/wallet", auth_1.requireAuth, async (req, res) => {
    try {
        const wallet = await (0, walletService_1.getOrCreateWallet)(req.userId);
        res.json({ success: true, wallet });
    }
    catch (error) {
        console.error("MLM WALLET ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/credits/dashboard", auth_1.requireAuth, async (req, res) => {
    try {
        const wallet = await (0, walletService_1.getOrCreateWallet)(req.userId);
        const dashboard = await (0, creditDashboardService_1.getCreditDashboard)(req.userId);
        res.json({
            success: true,
            creditBalance: wallet.creditBalance,
            ...dashboard,
        });
    }
    catch (error) {
        console.error("MLM CREDIT DASHBOARD ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.post("/credits/transfer", auth_1.requireAuth, async (req, res) => {
    try {
        const { receiverId, amount } = req.body;
        const creditCount = Number(amount) || 1;
        if (!receiverId) {
            return res
                .status(400)
                .json({ success: false, message: "Receiver is required" });
        }
        if (creditCount <= 0 || creditCount > 5) {
            return res.status(400).json({
                success: false,
                message: "Transfer amount must be between 1 and 5 credits",
            });
        }
        if (receiverId === req.userId) {
            return res
                .status(400)
                .json({ success: false, message: "Cannot transfer to yourself" });
        }
        const senderWallet = await (0, walletService_1.getOrCreateWallet)(req.userId);
        const senderUser = await User_1.default.findById(req.userId).select("level downlineCount");
        if (!senderUser) {
            return res
                .status(404)
                .json({ success: false, message: "Sender not found" });
        }
        if (senderWallet.creditBalance < creditCount) {
            return res
                .status(400)
                .json({ success: false, message: "Insufficient credit balance" });
        }
        const receiver = await User_1.default.findById(receiverId);
        if (!receiver) {
            return res
                .status(404)
                .json({ success: false, message: "Receiver not found" });
        }
        // NEW: Check if sender has shared 5 vouchers with receiver
        const vouchersShared = await Voucher_1.default.countDocuments({
            userId: receiverId,
            source: "transfer",
            transferredFrom: req.userId,
        });
        if (vouchersShared < MIN_VOUCHERS_TO_UNLOCK) {
            return res.status(400).json({
                success: false,
                message: `Credit transfer locked. You must share ${MIN_VOUCHERS_TO_UNLOCK} vouchers first (currently shared: ${vouchersShared})`,
                vouchersShared,
                vouchersRequired: MIN_VOUCHERS_TO_UNLOCK,
                isLocked: true,
            });
        }
        // Link receiver to sender if not already linked
        if (receiver.parentId && receiver.parentId.toString() !== req.userId) {
            return res.status(400).json({
                success: false,
                message: "Receiver already linked to another parent",
            });
        }
        if (!receiver.parentId) {
            receiver.parentId = req.userId;
            receiver.level = Math.min(senderUser?.level || 0, 9) + 1;
            receiver.directCount = receiver.directCount || 0;
            await receiver.save();
            // Update sender's direct count
            await User_1.default.findByIdAndUpdate(req.userId, { $inc: { directCount: 1 } });
            // Update downline counts for all ancestors
            await (0, downlineService_1.updateAncestorDownlineCounts)(receiverId);
        }
        // Create credits (payment pending state)
        const createdCredits = [];
        for (let i = 0; i < creditCount; i += 1) {
            const sourceCredit = await (0, creditLifecycleService_1.assignCreditUsage)(req.userId);
            if (!sourceCredit) {
                break;
            }
            const expiresAt = new Date(Date.now() + CREDIT_EXPIRY_MINUTES * 60 * 1000);
            const credit = await MlmCredit_1.default.create({
                senderId: req.userId,
                receiverId,
                expiresAt,
                paymentStatus: "pending", // Receiver must confirm payment
                sourceCreditId: sourceCredit._id,
            });
            createdCredits.push({ creditId: credit._id.toString(), expiresAt });
        }
        if (createdCredits.length === 0) {
            return res.status(400).json({
                success: false,
                message: "No active credits available to transfer",
            });
        }
        res.json({
            success: true,
            credits: createdCredits,
            message: "Credits transferred. Waiting for receiver to confirm payment.",
            vouchersShared,
            isUnlocked: true,
        });
    }
    catch (error) {
        console.error("MLM CREDIT TRANSFER ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ NEW: Receiver confirms "I Have Paid" → changes to waiting_approval
router.post("/credits/:creditId/confirm-payment", auth_1.requireAuth, async (req, res) => {
    try {
        const { creditId } = req.params;
        const credit = await MlmCredit_1.default.findById(creditId);
        if (!credit) {
            return res
                .status(404)
                .json({ success: false, message: "Credit not found" });
        }
        if (credit.receiverId.toString() !== req.userId) {
            return res
                .status(403)
                .json({ success: false, message: "Not authorized" });
        }
        if (credit.paymentStatus !== "pending") {
            return res.json({
                success: true,
                message: `Payment already in ${credit.paymentStatus} state`,
            });
        }
        // Update to waiting for admin approval
        credit.paymentStatus = "waiting_approval";
        credit.paymentConfirmedByReceiver = true;
        credit.paymentConfirmedAt = new Date();
        await credit.save();
        res.json({
            success: true,
            message: "Payment confirmation received. Waiting for admin approval.",
            credit: {
                id: credit._id.toString(),
                status: credit.status,
                paymentStatus: credit.paymentStatus,
            },
        });
    }
    catch (error) {
        console.error("MLM PAYMENT CONFIRM ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// DISCOUNT INFO (REPLACES COMMISSION)
// ============================================
router.get("/discount/info", auth_1.requireAuth, async (req, res) => {
    try {
        const discountInfo = await (0, discountService_1.getUserDiscountInfo)(req.userId);
        res.json({ success: true, discountInfo });
    }
    catch (error) {
        console.error("MLM DISCOUNT INFO ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/discount/summary", auth_1.requireAuth, async (req, res) => {
    try {
        const summary = await (0, discountService_1.getDiscountSummary)(req.userId);
        res.json({ success: true, summary });
    }
    catch (error) {
        console.error("MLM DISCOUNT SUMMARY ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// VOUCHERS
// ============================================
router.get("/vouchers", auth_1.requireAuth, async (req, res) => {
    try {
        const { status, limit = 20, skip = 0, source } = req.query;
        // If requesting admin vouchers specifically
        if (source === "admin") {
            const query = {
                source: "admin",
                isPublished: true,
            };
            const adminVouchers = await Voucher_1.default.find(query)
                .sort({ publishedAt: -1 })
                .skip(Number(skip))
                .limit(Number(limit))
                .lean();
            return res.json({ success: true, vouchers: adminVouchers });
        }
        // Regular user vouchers
        const query = { userId: req.userId };
        if (status)
            query.redeemedStatus = status;
        const userVouchers = await Voucher_1.default.find(query)
            .sort({ issueDate: -1 })
            .skip(Number(skip))
            .limit(Number(limit))
            .lean();
        res.json({ success: true, vouchers: userVouchers });
    }
    catch (error) {
        console.error("MLM VOUCHERS ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.post("/vouchers/:voucherId/redeem", auth_1.requireAuth, async (req, res) => {
    try {
        const { voucherId } = req.params;
        const voucher = await Voucher_1.default.findOne({
            _id: voucherId,
            userId: req.userId,
        });
        if (!voucher) {
            return res
                .status(404)
                .json({ success: false, message: "Voucher not found" });
        }
        if (voucher.redeemedStatus === "redeemed") {
            return res
                .status(400)
                .json({ success: false, message: "Voucher already redeemed" });
        }
        if (voucher.expiryDate < new Date()) {
            voucher.redeemedStatus = "expired";
            await voucher.save();
            return res
                .status(400)
                .json({ success: false, message: "Voucher expired" });
        }
        voucher.redeemedStatus = "redeemed";
        voucher.redeemedAt = new Date();
        await voucher.save();
        res.json({ success: true, voucher });
    }
    catch (error) {
        console.error("MLM VOUCHER REDEEM ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.post("/vouchers/:voucherId/transfer", auth_1.requireAuth, async (req, res) => {
    try {
        const { voucherId } = req.params;
        const { recipientPhone } = req.body;
        if (!(0, mongoose_1.isValidObjectId)(voucherId)) {
            return res
                .status(400)
                .json({ success: false, message: "Invalid voucher id" });
        }
        if (!recipientPhone) {
            return res
                .status(400)
                .json({ success: false, message: "Recipient phone is required" });
        }
        // Find voucher
        const voucher = await Voucher_1.default.findOne({
            _id: voucherId,
            userId: req.userId,
        });
        if (!voucher) {
            return res
                .status(404)
                .json({ success: false, message: "Voucher not found" });
        }
        if (voucher.redeemedStatus !== "unredeemed") {
            return res.status(400).json({
                success: false,
                message: `Cannot transfer ${voucher.redeemedStatus} voucher`,
            });
        }
        if (voucher.expiryDate < new Date()) {
            return res
                .status(400)
                .json({ success: false, message: "Cannot transfer expired voucher" });
        }
        // Find recipient by phone
        const recipient = await User_1.default.findOne({ phone: recipientPhone });
        if (!recipient) {
            return res
                .status(404)
                .json({ success: false, message: "Recipient not found" });
        }
        if (recipient._id.toString() === req.userId) {
            return res.status(400).json({
                success: false,
                message: "Cannot transfer voucher to yourself",
            });
        }
        // Transfer voucher
        const previousOwner = voucher.userId;
        voucher.userId = recipient._id;
        voucher.source = "transfer";
        voucher.transferredFrom = previousOwner;
        voucher.transferredAt = new Date();
        if (!Array.isArray(voucher.transferHistory)) {
            voucher.transferHistory = [];
        }
        voucher.transferHistory.push({
            from: previousOwner,
            to: recipient._id,
            transferredAt: new Date(),
        });
        await voucher.save();
        // Populate recipient info for response
        await voucher.populate("userId", "name phone");
        res.json({
            success: true,
            message: `Voucher transferred to ${recipient.name}`,
            voucher,
        });
    }
    catch (error) {
        console.error("MLM VOUCHER TRANSFER ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/vouchers/history", auth_1.requireAuth, async (req, res) => {
    try {
        const { limit = 50, skip = 0 } = req.query;
        // Get all vouchers where user is original owner OR received via transfer
        const vouchers = await Voucher_1.default.find({
            $or: [
                { originalOwner: req.userId },
                { userId: req.userId },
                { "transferHistory.from": req.userId },
                { "transferHistory.to": req.userId },
            ],
        })
            .populate("userId", "name phone")
            .populate("originalOwner", "name phone")
            .populate("transferredFrom", "name phone")
            .populate("transferHistory.from", "name phone")
            .populate("transferHistory.to", "name phone")
            .sort({ createdAt: -1 })
            .skip(Number(skip))
            .limit(Number(limit))
            .lean();
        // Categorize vouchers
        const purchased = vouchers.filter((v) => v.originalOwner?._id?.toString() === req.userId &&
            v.source === "purchase");
        const received = vouchers.filter((v) => v.userId?._id?.toString() === req.userId && v.source === "transfer");
        const sent = vouchers.filter((v) => v.transferHistory?.some((t) => t.from?.toString() === req.userId && t.to?.toString() !== req.userId));
        res.json({
            success: true,
            history: {
                purchased: purchased.length,
                received: received.length,
                sent: sent.length,
                all: vouchers,
            },
        });
    }
    catch (error) {
        console.error("MLM VOUCHER HISTORY ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/vouchers/received", auth_1.requireAuth, async (req, res) => {
    try {
        const { limit = 20, skip = 0 } = req.query;
        const vouchers = await Voucher_1.default.find({
            userId: req.userId,
            source: "transfer",
        })
            .populate("transferredFrom", "name phone")
            .sort({ transferredAt: -1 })
            .skip(Number(skip))
            .limit(Number(limit))
            .lean();
        res.json({ success: true, vouchers });
    }
    catch (error) {
        console.error("MLM VOUCHERS RECEIVED ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/vouchers/sent", auth_1.requireAuth, async (req, res) => {
    try {
        const { limit = 20, skip = 0 } = req.query;
        const vouchers = await Voucher_1.default.find({
            "transferHistory.from": req.userId,
        })
            .populate("userId", "name phone")
            .populate("transferredFrom", "name phone")
            .sort({ transferredAt: -1 })
            .skip(Number(skip))
            .limit(Number(limit))
            .lean();
        res.json({ success: true, vouchers });
    }
    catch (error) {
        console.error("MLM VOUCHERS SENT ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// NETWORK TREE & MEMBERS
// ============================================
router.get("/network/tree", auth_1.requireAuth, async (req, res) => {
    try {
        const depth = Math.min(Number(req.query.depth) || 2, 4);
        const perParentLimit = Math.min(Number(req.query.perParentLimit) || 5, 10);
        const tree = await (0, treeService_1.buildNetworkTree)(req.userId, depth, perParentLimit);
        res.json({ success: true, tree });
    }
    catch (error) {
        console.error("MLM TREE ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/network/children", auth_1.requireAuth, async (req, res) => {
    try {
        const { parentId, limit = 10, skip = 0 } = req.query;
        const targetParent = parentId || req.userId;
        const children = await User_1.default.find({ parentId: targetParent })
            .select("name phone level directCount downlineCount createdAt")
            .sort({ createdAt: -1 })
            .skip(Number(skip))
            .limit(Number(limit))
            .lean();
        const formatted = children.map((child) => ({
            id: child._id.toString(),
            name: child.name,
            phone: child.phone,
            level: child.level || 0,
            directCount: child.directCount || 0,
            downlineCount: child.downlineCount || 0,
            joinedDate: child.createdAt,
            structuralCreditPool: (0, mlm_1.getStructuralCreditPool)(child.level || 1),
        }));
        res.json({ success: true, children: formatted });
    }
    catch (error) {
        console.error("MLM CHILDREN ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/network/direct-buyers", auth_1.requireAuth, async (req, res) => {
    try {
        const limit = Math.min(Number(req.query.limit) || 10, 50);
        const skip = Number(req.query.skip) || 0;
        const buyers = await (0, directBuyersService_1.getDirectBuyers)(req.userId, limit, skip);
        res.json({ success: true, buyers });
    }
    catch (error) {
        console.error("MLM DIRECT BUYERS ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.get("/network/structural-pool", auth_1.requireAuth, async (req, res) => {
    try {
        const user = await User_1.default.findById(req.userId).select("level");
        if (!user) {
            return res
                .status(404)
                .json({ success: false, message: "User not found" });
        }
        const pool = (0, mlm_1.getStructuralCreditPool)(user.level || 1);
        res.json({ success: true, structuralCreditPool: pool });
    }
    catch (error) {
        console.error("MLM STRUCTURAL POOL ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// OVERVIEW DASHBOARD
// ============================================
router.get("/overview", auth_1.requireAuth, async (req, res) => {
    try {
        const user = await User_1.default.findById(req.userId).select("name phone level directCount downlineCount parentId createdAt");
        if (!user) {
            return res
                .status(404)
                .json({ success: false, message: "User not found" });
        }
        const wallet = await (0, walletService_1.getOrCreateWallet)(req.userId);
        const creditDashboard = await (0, creditDashboardService_1.getCreditDashboard)(req.userId);
        const discountSummary = await (0, discountService_1.getDiscountSummary)(req.userId);
        const totalNetworkUsers = await User_1.default.countDocuments({
            parentId: req.userId,
        });
        const structuralCreditPool = (0, mlm_1.getStructuralCreditPool)(user.level || 1);
        res.json({
            success: true,
            user: {
                id: user._id.toString(),
                name: user.name,
                phone: user.phone,
                level: user.level || 0,
                directCount: user.directCount || 0,
                downlineCount: user.downlineCount || 0,
                parentId: user.parentId,
                joinedDate: user.createdAt,
            },
            wallet,
            creditDashboard,
            discountSummary, // Replaces commission summary
            metrics: {
                availableCredits: wallet.creditBalance,
                totalVouchersTransferred: creditDashboard.totalCreditsTransferred || 0,
                totalNetworkUsers,
                virtualCommission: discountSummary.virtualCommission || 0, // Discount equivalent
                currentDiscountPercent: discountSummary.discountPercent || 0,
            },
            structuralCreditPool,
            baseMrp: mlm_1.MLM_BASE_MRP,
        });
    }
    catch (error) {
        console.error("MLM OVERVIEW ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// ADMIN ENDPOINTS
// ============================================
router.post("/admin/credits/seed", async (req, res) => {
    try {
        const adminKey = req.headers["x-admin-key"];
        if (adminKey !== process.env.ADMIN_SECRET_KEY &&
            adminKey !== "your-secure-admin-key-here") {
            return res.status(401).json({ success: false, message: "Unauthorized" });
        }
        const { userId, amount } = req.body;
        if (!userId || !amount || amount <= 0) {
            return res
                .status(400)
                .json({ success: false, message: "Invalid request" });
        }
        const wallet = await (0, walletService_1.addCredits)(userId, amount);
        res.json({ success: true, wallet });
    }
    catch (error) {
        console.error("MLM ADMIN SEED ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ NEW: Admin approves payment (activates vouchers & applies discount)
router.post("/admin/credits/:creditId/approve-payment", async (req, res) => {
    try {
        const adminKey = req.headers["x-admin-key"];
        if (adminKey !== process.env.ADMIN_SECRET_KEY &&
            adminKey !== "your-secure-admin-key-here") {
            return res.status(401).json({ success: false, message: "Unauthorized" });
        }
        const { creditId } = req.params;
        const { adminId } = req.body; // Optional: track which admin approved
        const credit = await MlmCredit_1.default.findById(creditId);
        if (!credit) {
            return res
                .status(404)
                .json({ success: false, message: "Credit not found" });
        }
        if (credit.paymentStatus !== "waiting_approval") {
            return res.status(400).json({
                success: false,
                message: `Credit is in ${credit.paymentStatus} state, cannot approve`,
            });
        }
        // Approve payment
        credit.paymentStatus = "approved";
        credit.status = "active";
        credit.activatedAt = new Date();
        credit.transferExpiresAt = new Date(Date.now() + TRANSFER_EXPIRY_HOURS * 60 * 60 * 1000);
        credit.adminApprovedBy = adminId || null;
        credit.adminApprovedAt = new Date();
        await credit.save();
        // Add credits to receiver's wallet
        await (0, walletService_1.addCredits)(credit.receiverId.toString(), credit.quantity);
        // Generate vouchers for receiver
        const vouchers = await (0, voucherService_1.generateVouchers)(credit.receiverId.toString(), credit._id.toString(), credit.quantity);
        res.json({
            success: true,
            message: "Payment approved. Vouchers generated.",
            vouchersGenerated: vouchers.length,
            credit: {
                id: credit._id.toString(),
                status: credit.status,
                paymentStatus: credit.paymentStatus,
            },
        });
    }
    catch (error) {
        console.error("MLM ADMIN APPROVE PAYMENT ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ NEW: Admin rejects payment (reverts credit)
router.post("/admin/credits/:creditId/reject-payment", async (req, res) => {
    try {
        const adminKey = req.headers["x-admin-key"];
        if (adminKey !== process.env.ADMIN_SECRET_KEY &&
            adminKey !== "your-secure-admin-key-here") {
            return res.status(401).json({ success: false, message: "Unauthorized" });
        }
        const { creditId } = req.params;
        const { adminId, note } = req.body;
        const credit = await MlmCredit_1.default.findById(creditId);
        if (!credit) {
            return res
                .status(404)
                .json({ success: false, message: "Credit not found" });
        }
        if (credit.paymentStatus !== "waiting_approval") {
            return res.status(400).json({
                success: false,
                message: `Credit is in ${credit.paymentStatus} state, cannot reject`,
            });
        }
        // Reject payment
        credit.paymentStatus = "rejected";
        credit.status = "reverted";
        credit.adminApprovedBy = adminId || null;
        credit.adminApprovedAt = new Date();
        credit.adminNote = note || "Payment not confirmed by sender";
        await credit.save();
        // Refund credit to sender
        await (0, walletService_1.addCredits)(credit.senderId.toString(), 1);
        res.json({
            success: true,
            message: "Payment rejected. Credit refunded to sender.",
            credit: {
                id: credit._id.toString(),
                status: credit.status,
                paymentStatus: credit.paymentStatus,
            },
        });
    }
    catch (error) {
        console.error("MLM ADMIN REJECT PAYMENT ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ NEW: List credits pending admin approval
router.get("/admin/credits/pending-approval", async (req, res) => {
    try {
        const adminKey = req.headers["x-admin-key"];
        if (adminKey !== process.env.ADMIN_SECRET_KEY &&
            adminKey !== "your-secure-admin-key-here") {
            return res.status(401).json({ success: false, message: "Unauthorized" });
        }
        const { limit = 50, skip = 0 } = req.query;
        const credits = await MlmCredit_1.default.find({ paymentStatus: "waiting_approval" })
            .populate("senderId", "name phone")
            .populate("receiverId", "name phone")
            .sort({ paymentConfirmedAt: -1 })
            .skip(Number(skip))
            .limit(Number(limit))
            .lean();
        res.json({
            success: true,
            credits: credits.map((c) => ({
                id: c._id.toString(),
                sender: c.senderId,
                receiver: c.receiverId,
                quantity: c.quantity,
                paymentConfirmedAt: c.paymentConfirmedAt,
                createdAt: c.createdAt,
            })),
        });
    }
    catch (error) {
        console.error("MLM ADMIN PENDING APPROVAL ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ NEW: Admin MLM Transfer - Transfer credits & vouchers to anyone by phone number
// Admin becomes root (level 0), transferred users become level 1
router.post("/admin/mlm-transfer", async (req, res) => {
    try {
        const adminKey = req.headers["x-admin-key"];
        if (adminKey !== process.env.ADMIN_SECRET_KEY &&
            adminKey !== "your-secure-admin-key-here") {
            return res.status(401).json({ success: false, message: "Unauthorized" });
        }
        const { phone, amount, adminUserId } = req.body;
        // Validation
        if (!phone || !amount || !adminUserId) {
            return res.status(400).json({
                success: false,
                message: "Phone number, amount, and adminUserId are required",
            });
        }
        if (amount <= 0) {
            return res.status(400).json({
                success: false,
                message: "Amount must be greater than 0",
            });
        }
        // Find recipient by phone number
        const recipient = await User_1.default.findOne({ phone });
        if (!recipient) {
            return res.status(404).json({
                success: false,
                message: `No user found with phone number: ${phone}`,
            });
        }
        // Get or create Admin user
        let adminUser = await User_1.default.findById(adminUserId);
        if (!adminUser) {
            return res.status(404).json({
                success: false,
                message: "Admin user not found",
            });
        }
        // Ensure Admin is at root level (level 0)
        if (adminUser.level !== 0) {
            adminUser.level = 0;
            adminUser.parentId = null;
            await adminUser.save();
        }
        // Check if recipient already has a parent
        if (recipient.parentId && recipient.parentId.toString() !== adminUserId) {
            return res.status(400).json({
                success: false,
                message: `${recipient.name} is already linked to another parent in the network`,
            });
        }
        // Link recipient to Admin (if not already linked)
        let isNewLink = false;
        if (!recipient.parentId) {
            recipient.parentId = adminUserId;
            recipient.level = 1; // One level below Admin
            await recipient.save();
            // Update Admin's direct count
            await User_1.default.findByIdAndUpdate(adminUserId, { $inc: { directCount: 1 } });
            // Update downline counts for all ancestors (Admin in this case)
            await (0, downlineService_1.updateAncestorDownlineCounts)(recipient._id.toString());
            isNewLink = true;
        }
        // Add credits to recipient's MLM wallet
        await (0, walletService_1.addCredits)(recipient._id.toString(), amount);
        // Generate vouchers for the recipient
        // Create a temporary credit record for voucher generation
        const adminCredit = await MlmCredit_1.default.create({
            senderId: adminUserId,
            receiverId: recipient._id,
            status: "active",
            paymentStatus: "approved",
            quantity: amount,
            activatedAt: new Date(),
            transferExpiresAt: new Date(Date.now() + TRANSFER_EXPIRY_HOURS * 60 * 60 * 1000),
        });
        const vouchers = await (0, voucherService_1.generateVouchers)(recipient._id.toString(), adminCredit._id.toString(), amount);
        res.json({
            success: true,
            message: `Successfully transferred ${amount} credits and generated ${vouchers.length} vouchers to ${recipient.name}`,
            transfer: {
                admin: {
                    id: adminUser._id,
                    name: adminUser.name,
                    level: adminUser.level,
                },
                recipient: {
                    id: recipient._id,
                    name: recipient.name,
                    phone: recipient.phone,
                    level: recipient.level,
                    isNewLink,
                },
                creditsTransferred: amount,
                vouchersGenerated: vouchers.length,
            },
        });
    }
    catch (error) {
        console.error("MLM ADMIN TRANSFER ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
exports.default = router;
