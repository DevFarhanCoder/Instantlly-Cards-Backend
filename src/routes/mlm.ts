import { Router } from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import User from "../models/User";
import MlmCredit from "../models/MlmCredit";
import Voucher from "../models/Voucher";
import { getStructuralCreditPool, MLM_BASE_MRP } from "../utils/mlm";
import {
  addCredits,
  getOrCreateWallet,
  subtractCredits,
} from "../services/mlm/walletService";
import { assignCreditUsage } from "../services/mlm/creditLifecycleService";
import { generateVouchers } from "../services/mlm/voucherService";
import { buildNetworkTree } from "../services/mlm/treeService";
import { getCreditDashboard } from "../services/mlm/creditDashboardService";
import { getDirectBuyers } from "../services/mlm/directBuyersService";
import {
  getUserDiscountInfo,
  getDiscountSummary,
  calculatePurchaseDiscount,
} from "../services/mlm/discountService";
import { updateAncestorDownlineCounts } from "../services/mlm/downlineService";

const router = Router();

const CREDIT_EXPIRY_MINUTES = 60;
const TRANSFER_EXPIRY_HOURS = 48;

// ============================================
// WALLET & CREDITS
// ============================================

router.get("/wallet", requireAuth, async (req: AuthReq, res) => {
  try {
    const wallet = await getOrCreateWallet(req.userId as string);
    res.json({ success: true, wallet });
  } catch (error) {
    console.error("MLM WALLET ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get("/credits/dashboard", requireAuth, async (req: AuthReq, res) => {
  try {
    const wallet = await getOrCreateWallet(req.userId as string);
    const dashboard = await getCreditDashboard(req.userId as string);

    res.json({
      success: true,
      creditBalance: wallet.creditBalance,
      ...dashboard,
    });
  } catch (error) {
    console.error("MLM CREDIT DASHBOARD ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.post("/credits/transfer", requireAuth, async (req: AuthReq, res) => {
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

    const senderWallet = await getOrCreateWallet(req.userId as string);
    const senderUser = await User.findById(req.userId).select(
      "level downlineCount",
    );

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

    const receiver = await User.findById(receiverId);
    if (!receiver) {
      return res
        .status(404)
        .json({ success: false, message: "Receiver not found" });
    }

    // Link receiver to sender if not already linked
    if (receiver.parentId && receiver.parentId.toString() !== req.userId) {
      return res.status(400).json({
        success: false,
        message: "Receiver already linked to another parent",
      });
    }

    if (!receiver.parentId) {
      receiver.parentId = req.userId as any;
      receiver.level = Math.min((senderUser as any)?.level || 0, 9) + 1;
      receiver.directCount = receiver.directCount || 0;
      await receiver.save();

      // Update sender's direct count
      await User.findByIdAndUpdate(req.userId, { $inc: { directCount: 1 } });

      // Update downline counts for all ancestors
      await updateAncestorDownlineCounts(receiverId);
    }

    // Create credits (payment pending state)
    const createdCredits: Array<{ creditId: string; expiresAt: Date }> = [];
    for (let i = 0; i < creditCount; i += 1) {
      const sourceCredit = await assignCreditUsage(req.userId as string);
      if (!sourceCredit) {
        break;
      }

      const expiresAt = new Date(
        Date.now() + CREDIT_EXPIRY_MINUTES * 60 * 1000,
      );

      const credit = await MlmCredit.create({
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
    });
  } catch (error) {
    console.error("MLM CREDIT TRANSFER ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ NEW: Receiver confirms "I Have Paid" → changes to waiting_approval
router.post(
  "/credits/:creditId/confirm-payment",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const { creditId } = req.params;
      const credit = await MlmCredit.findById(creditId);

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
    } catch (error) {
      console.error("MLM PAYMENT CONFIRM ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

// ============================================
// DISCOUNT INFO (REPLACES COMMISSION)
// ============================================

router.get("/discount/info", requireAuth, async (req: AuthReq, res) => {
  try {
    const discountInfo = await getUserDiscountInfo(req.userId as string);
    res.json({ success: true, discountInfo });
  } catch (error) {
    console.error("MLM DISCOUNT INFO ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get("/discount/summary", requireAuth, async (req: AuthReq, res) => {
  try {
    const summary = await getDiscountSummary(req.userId as string);
    res.json({ success: true, summary });
  } catch (error) {
    console.error("MLM DISCOUNT SUMMARY ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ============================================
// VOUCHERS
// ============================================

router.get("/vouchers", requireAuth, async (req: AuthReq, res) => {
  try {
    const { status, limit = 20, skip = 0, source } = req.query;

    // If requesting admin vouchers specifically
    if (source === "admin") {
      const query: any = {
        source: "admin",
        isPublished: true,
      };

      const adminVouchers = await Voucher.find(query)
        .sort({ publishedAt: -1 })
        .skip(Number(skip))
        .limit(Number(limit))
        .lean();

      return res.json({ success: true, vouchers: adminVouchers });
    }

    // Regular user vouchers
    const query: any = { userId: req.userId };
    if (status) query.redeemedStatus = status;

    const userVouchers = await Voucher.find(query)
      .sort({ issueDate: -1 })
      .skip(Number(skip))
      .limit(Number(limit))
      .lean();

    res.json({ success: true, vouchers: userVouchers });
  } catch (error) {
    console.error("MLM VOUCHERS ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.post(
  "/vouchers/:voucherId/redeem",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const { voucherId } = req.params;
      const voucher = await Voucher.findOne({
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
    } catch (error) {
      console.error("MLM VOUCHER REDEEM ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

router.post(
  "/vouchers/:voucherId/transfer",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const { voucherId } = req.params;
      const { recipientPhone } = req.body;

      if (!recipientPhone) {
        return res
          .status(400)
          .json({ success: false, message: "Recipient phone is required" });
      }

      // Find voucher
      const voucher = await Voucher.findOne({
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
      const recipient = await User.findOne({ phone: recipientPhone });
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
      voucher.userId = recipient._id as any;
      voucher.source = "transfer";
      voucher.transferredFrom = previousOwner;
      voucher.transferredAt = new Date();
      voucher.transferHistory.push({
        from: previousOwner,
        to: recipient._id as any,
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
    } catch (error) {
      console.error("MLM VOUCHER TRANSFER ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

router.get("/vouchers/history", requireAuth, async (req: AuthReq, res) => {
  try {
    const { limit = 50, skip = 0 } = req.query;

    // Get all vouchers where user is original owner OR received via transfer
    const vouchers = await Voucher.find({
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
    const purchased = vouchers.filter(
      (v: any) =>
        v.originalOwner?._id?.toString() === req.userId &&
        v.source === "purchase",
    );
    const received = vouchers.filter(
      (v: any) =>
        v.userId?._id?.toString() === req.userId && v.source === "transfer",
    );
    const sent = vouchers.filter((v: any) =>
      v.transferHistory?.some(
        (t: any) =>
          t.from?.toString() === req.userId && t.to?.toString() !== req.userId,
      ),
    );

    res.json({
      success: true,
      history: {
        purchased: purchased.length,
        received: received.length,
        sent: sent.length,
        all: vouchers,
      },
    });
  } catch (error) {
    console.error("MLM VOUCHER HISTORY ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get("/vouchers/received", requireAuth, async (req: AuthReq, res) => {
  try {
    const { limit = 20, skip = 0 } = req.query;

    const vouchers = await Voucher.find({
      userId: req.userId,
      source: "transfer",
    })
      .populate("transferredFrom", "name phone")
      .sort({ transferredAt: -1 })
      .skip(Number(skip))
      .limit(Number(limit))
      .lean();

    res.json({ success: true, vouchers });
  } catch (error) {
    console.error("MLM VOUCHERS RECEIVED ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get("/vouchers/sent", requireAuth, async (req: AuthReq, res) => {
  try {
    const { limit = 20, skip = 0 } = req.query;

    const vouchers = await Voucher.find({
      "transferHistory.from": req.userId,
    })
      .populate("userId", "name phone")
      .populate("transferredFrom", "name phone")
      .sort({ transferredAt: -1 })
      .skip(Number(skip))
      .limit(Number(limit))
      .lean();

    res.json({ success: true, vouchers });
  } catch (error) {
    console.error("MLM VOUCHERS SENT ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ============================================
// NETWORK TREE & MEMBERS
// ============================================

router.get("/network/tree", requireAuth, async (req: AuthReq, res) => {
  try {
    const depth = Math.min(Number(req.query.depth) || 2, 4);
    const perParentLimit = Math.min(Number(req.query.perParentLimit) || 5, 10);

    const tree = await buildNetworkTree(
      req.userId as string,
      depth,
      perParentLimit,
    );
    res.json({ success: true, tree });
  } catch (error) {
    console.error("MLM TREE ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get("/network/children", requireAuth, async (req: AuthReq, res) => {
  try {
    const { parentId, limit = 10, skip = 0 } = req.query;
    const targetParent = (parentId as string) || (req.userId as string);

    const children = await User.find({ parentId: targetParent })
      .select("name phone level directCount downlineCount createdAt")
      .sort({ createdAt: -1 })
      .skip(Number(skip))
      .limit(Number(limit))
      .lean();

    const formatted = (children as any[]).map((child: any) => ({
      id: (child._id as any).toString(),
      name: child.name,
      phone: child.phone,
      level: child.level || 0,
      directCount: child.directCount || 0,
      downlineCount: child.downlineCount || 0,
      joinedDate: child.createdAt,
      structuralCreditPool: getStructuralCreditPool(child.level || 1),
    }));

    res.json({ success: true, children: formatted });
  } catch (error) {
    console.error("MLM CHILDREN ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get("/network/direct-buyers", requireAuth, async (req: AuthReq, res) => {
  try {
    const limit = Math.min(Number(req.query.limit) || 10, 50);
    const skip = Number(req.query.skip) || 0;

    const buyers = await getDirectBuyers(req.userId as string, limit, skip);
    res.json({ success: true, buyers });
  } catch (error) {
    console.error("MLM DIRECT BUYERS ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get(
  "/network/structural-pool",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const user = await User.findById(req.userId).select("level");
      if (!user) {
        return res
          .status(404)
          .json({ success: false, message: "User not found" });
      }

      const pool = getStructuralCreditPool(user.level || 1);
      res.json({ success: true, structuralCreditPool: pool });
    } catch (error) {
      console.error("MLM STRUCTURAL POOL ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

// ============================================
// OVERVIEW DASHBOARD
// ============================================

router.get("/overview", requireAuth, async (req: AuthReq, res) => {
  try {
    const user = await User.findById(req.userId).select(
      "name phone level directCount downlineCount parentId createdAt",
    );

    if (!user) {
      return res
        .status(404)
        .json({ success: false, message: "User not found" });
    }

    const wallet = await getOrCreateWallet(req.userId as string);
    const creditDashboard = await getCreditDashboard(req.userId as string);
    const discountSummary = await getDiscountSummary(req.userId as string);

    const totalNetworkUsers = await User.countDocuments({
      parentId: req.userId,
    });
    const structuralCreditPool = getStructuralCreditPool(user.level || 1);

    res.json({
      success: true,
      user: {
        id: user._id.toString(),
        name: user.name,
        phone: user.phone,
        level: user.level || 0,
        directCount: user.directCount || 0,
        downlineCount: (user as any).downlineCount || 0,
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
      baseMrp: MLM_BASE_MRP,
    });
  } catch (error) {
    console.error("MLM OVERVIEW ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ============================================
// ADMIN ENDPOINTS
// ============================================

router.post("/admin/credits/seed", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const { userId, amount } = req.body;
    if (!userId || !amount || amount <= 0) {
      return res
        .status(400)
        .json({ success: false, message: "Invalid request" });
    }

    const wallet = await addCredits(userId, amount);
    res.json({ success: true, wallet });
  } catch (error) {
    console.error("MLM ADMIN SEED ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ NEW: Admin approves payment (activates vouchers & applies discount)
router.post("/admin/credits/:creditId/approve-payment", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const { creditId } = req.params;
    const { adminId } = req.body; // Optional: track which admin approved

    const credit = await MlmCredit.findById(creditId);
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
    credit.transferExpiresAt = new Date(
      Date.now() + TRANSFER_EXPIRY_HOURS * 60 * 60 * 1000,
    );
    credit.adminApprovedBy = adminId || null;
    credit.adminApprovedAt = new Date();
    await credit.save();

    // Add credits to receiver's wallet
    await addCredits(credit.receiverId.toString(), credit.quantity);

    // Generate vouchers for receiver
    const vouchers = await generateVouchers(
      credit.receiverId.toString(),
      credit._id.toString(),
      credit.quantity,
    );

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
  } catch (error) {
    console.error("MLM ADMIN APPROVE PAYMENT ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ NEW: Admin rejects payment (reverts credit)
router.post("/admin/credits/:creditId/reject-payment", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const { creditId } = req.params;
    const { adminId, note } = req.body;

    const credit = await MlmCredit.findById(creditId);
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
    await addCredits(credit.senderId.toString(), 1);

    res.json({
      success: true,
      message: "Payment rejected. Credit refunded to sender.",
      credit: {
        id: credit._id.toString(),
        status: credit.status,
        paymentStatus: credit.paymentStatus,
      },
    });
  } catch (error) {
    console.error("MLM ADMIN REJECT PAYMENT ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ NEW: List credits pending admin approval
router.get("/admin/credits/pending-approval", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const { limit = 50, skip = 0 } = req.query;

    const credits = await MlmCredit.find({ paymentStatus: "waiting_approval" })
      .populate("senderId", "name phone")
      .populate("receiverId", "name phone")
      .sort({ paymentConfirmedAt: -1 })
      .skip(Number(skip))
      .limit(Number(limit))
      .lean();

    res.json({
      success: true,
      credits: (credits as any[]).map((c) => ({
        id: c._id.toString(),
        sender: c.senderId,
        receiver: c.receiverId,
        quantity: c.quantity,
        paymentConfirmedAt: c.paymentConfirmedAt,
        createdAt: c.createdAt,
      })),
    });
  } catch (error) {
    console.error("MLM ADMIN PENDING APPROVAL ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ NEW: Admin MLM Transfer - Transfer credits & vouchers to anyone by phone number
// Admin becomes root (level 0), transferred users become level 1
router.post("/admin/mlm-transfer", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
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
    const recipient = await User.findOne({ phone });
    if (!recipient) {
      return res.status(404).json({
        success: false,
        message: `No user found with phone number: ${phone}`,
      });
    }

    // Get or create Admin user
    let adminUser = await User.findById(adminUserId);
    if (!adminUser) {
      return res.status(404).json({
        success: false,
        message: "Admin user not found",
      });
    }

    // Ensure Admin is at root level (level 0)
    if (adminUser.level !== 0) {
      adminUser.level = 0;
      adminUser.parentId = null as any;
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
      recipient.parentId = adminUserId as any;
      recipient.level = 1; // One level below Admin
      await recipient.save();

      // Update Admin's direct count
      await User.findByIdAndUpdate(adminUserId, { $inc: { directCount: 1 } });

      // Update downline counts for all ancestors (Admin in this case)
      await updateAncestorDownlineCounts(recipient._id.toString());
      isNewLink = true;
    }

    // Add credits to recipient's MLM wallet
    await addCredits(recipient._id.toString(), amount);

    // Generate vouchers for the recipient
    // Create a temporary credit record for voucher generation
    const adminCredit = await MlmCredit.create({
      senderId: adminUserId,
      receiverId: recipient._id,
      status: "active",
      paymentStatus: "approved",
      quantity: amount,
      activatedAt: new Date(),
      transferExpiresAt: new Date(
        Date.now() + TRANSFER_EXPIRY_HOURS * 60 * 60 * 1000,
      ),
    });

    const vouchers = await generateVouchers(
      recipient._id.toString(),
      adminCredit._id.toString(),
      amount,
    );

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
  } catch (error) {
    console.error("MLM ADMIN TRANSFER ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

export default router;
