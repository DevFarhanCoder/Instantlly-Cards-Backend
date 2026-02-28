import { Router } from "express";
import { isValidObjectId } from "mongoose";
import { requireAuth, AuthReq } from "../middleware/auth";
import User from "../models/User";
import MlmCredit from "../models/MlmCredit";
import Voucher from "../models/Voucher";
import SpecialCredit from "../models/SpecialCredit";
import VoucherTransferLog from "../models/VoucherTransferLog";
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
const VOUCHER_PURCHASE_TIMEOUT_MINUTES = 60; // 1 hour to complete purchase
const CONNECTION_TIMEOUT_HOURS = 48; // 2 days to connect 5 people
const MIN_VOUCHERS_TO_UNLOCK = 5; // Must share 5 vouchers to unlock credit transfer
const DISTRIBUTION_CREDIT_AMOUNT = 146484360000; // Credits per distribution entry

// ============================================
// DISTRIBUTION CREDITS
// ============================================

// Get distribution credits for MLM user (to be transferred to downline)
router.get("/distribution-credits", requireAuth, async (req: AuthReq, res) => {
  try {
    const user = await User.findById(req.userId).select("parentId");

    // Only MLM users (those with introducers) get distribution credits
    if (!user?.parentId) {
      return res.json({ success: true, credits: [] });
    }

    // Get user's direct children (first level downline)
    const directChildren = await User.find({ parentId: req.userId })
      .select("name phone createdAt")
      .lean();

    // Calculate distribution credits for each child
    const distributionCredits = await Promise.all(
      directChildren.map(async (child: any, index: number) => {
        // Count vouchers shared with this child
        const vouchersShared = await Voucher.countDocuments({
          userId: child._id,
          source: "transfer",
          transferredFrom: req.userId,
        });

        // Check if credits are locked (need 5 vouchers shared)
        const isLocked = vouchersShared < MIN_VOUCHERS_TO_UNLOCK;

        // Calculate time left to connect (2 days from creation)
        const createdAt = new Date(child.createdAt);
        const timeoutAt = new Date(
          createdAt.getTime() + CONNECTION_TIMEOUT_HOURS * 60 * 60 * 1000,
        );
        const timeLeft = Math.max(0, timeoutAt.getTime() - Date.now());
        const hoursLeft = Math.floor(timeLeft / (1000 * 60 * 60));
        const minutesLeft = Math.floor(
          (timeLeft % (1000 * 60 * 60)) / (1000 * 60),
        );

        return {
          level: 1, // Direct children are level 1
          creditsToTransfer: DISTRIBUTION_CREDIT_AMOUNT, // 146,484,360,000 credits per child
          recipientName: child.name,
          recipientPhone: child.phone,
          recipientId: child._id.toString(),
          vouchersShared,
          isLocked,
          timeLeft: `${hoursLeft}h ${minutesLeft}m`,
        };
      }),
    );

    res.json({ success: true, credits: distributionCredits });
  } catch (error) {
    console.error("MLM DISTRIBUTION CREDITS ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ============================================
// VOUCHER PURCHASE WITH TIMER
// ============================================

router.post("/vouchers/purchase", requireAuth, async (req: AuthReq, res) => {
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
    const user = await User.findById(req.userId);
    if (!user) {
      return res.status(404).json({
        success: false,
        message: "User not found",
      });
    }

    // Create vouchers
    const vouchers = [];
    for (let i = 0; i < quantity; i++) {
      const voucher = await Voucher.create({
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
    const connectionExpiresAt = new Date(
      Date.now() + CONNECTION_TIMEOUT_HOURS * 60 * 60 * 1000,
    );

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
  } catch (error) {
    console.error("MLM VOUCHER PURCHASE ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

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

    // NEW: Check if sender has shared 5 vouchers with receiver
    const vouchersShared = await Voucher.countDocuments({
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
      vouchersShared,
      isUnlocked: true,
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

    // Check if user is voucher admin
    const user = await User.findById(req.userId).select(
      "isVoucherAdmin level specialCredits voucherBalance",
    );
    const isVoucherAdmin = user?.isVoucherAdmin === true;
    const userVoucherBalance = (user as any)?.voucherBalance || 0;

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

    // Add special Instantlly voucher at the beginning for ALL users
    const allVouchers = [...userVouchers];

    // Create Instantlly voucher (shown for all users)
    const specialVoucher: any = {
      _id: "instantlly-special-credits",
      voucherNumber: "INSTANTLLY-SPECIAL",
      companyName: "Instantlly",
      phoneNumber: "+91 98674 77227",
      address: "Jogeshwari, Mumbai",
      title: "Sales Target at Special Discount",
      description: "",
      MRP: 100,
      amount: 100,
      discountPercentage: 70,
      issueDate: new Date(),
      expiryDate: new Date("2026-08-30"), // August 30, 2026
      redeemedStatus: "unredeemed",
      source: "instantlly-special",
      isSpecialCreditsVoucher: true,
    };

    // For admin users, add special credits info
    if (isVoucherAdmin && user?.specialCredits?.availableSlots) {
      specialVoucher.vouchersFigure = 122070300; // Show credit amount for admin
      specialVoucher.specialCredits = {
        totalSlots: user.specialCredits.availableSlots,
        usedSlots: user.specialCredits.usedSlots || 0,
        creditPerSlot: getSpecialCreditsForLevel(user.level || 0),
      };
    }

    allVouchers.unshift(specialVoucher);

    res.json({
      success: true,
      vouchers: allVouchers,
      voucherBalance: userVoucherBalance,
    });
  } catch (error) {
    console.error("MLM VOUCHERS ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ============================================
// VOUCHER HISTORY ROUTES (must be before :voucherId)
// ============================================

router.get("/vouchers/history", requireAuth, async (req: AuthReq, res) => {
  try {
    const { limit = 50, skip = 0 } = req.query;

    // Get all vouchers where user is involved
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
      .populate({
        path: "transferHistory.from",
        select: "name phone",
      })
      .populate({
        path: "transferHistory.to",
        select: "name phone",
      })
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
        v.userId?._id?.toString() === req.userId &&
        (v.source === "transfer" || v.source === "admin"),
    );

    // Count sent vouchers by checking transfer history
    let sentCount = 0;
    const allTransfers = new Set();
    vouchers.forEach((v: any) => {
      if (v.transferHistory && v.transferHistory.length > 0) {
        v.transferHistory.forEach((t: any) => {
          if (
            t.from?.toString() === req.userId ||
            t.from?._id?.toString() === req.userId
          ) {
            const key = `${v._id}-${t.to?._id || t.to}-${t.transferredAt}`;
            if (!allTransfers.has(key)) {
              allTransfers.add(key);
              sentCount++;
            }
          }
        });
      }
    });

    res.json({
      success: true,
      history: {
        purchased: purchased.length,
        received: received.length,
        sent: sentCount,
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

// Get voucher details (including special Instantlly voucher)
router.get("/vouchers/:voucherId", requireAuth, async (req: AuthReq, res) => {
  try {
    const { voucherId } = req.params;

    // Handle special Instantlly voucher (available to ALL users)
    if (voucherId === "instantlly-special-credits") {
      const user = await User.findById(req.userId).select(
        "name phone level isVoucherAdmin specialCredits",
      );
      const isAdmin = user?.isVoucherAdmin || user?.level === 0;

      const specialVoucher: any = {
        _id: "instantlly-special-credits",
        voucherNumber: "INSTANTLLY-SPECIAL",
        companyName: "Instantlly",
        phoneNumber: "+91 98674 77227",
        address: "Jogeshwari, Mumbai",
        title: "Sales Target at Special Discount",
        description: "",
        MRP: 100,
        amount: 100,
        discountPercentage: 70,
        issueDate: new Date(),
        expiryDate: new Date("2027-02-24"),
        redeemedStatus: "unredeemed",
        source: "instantlly-special",
        isSpecialCreditsVoucher: true,
        canContinueToDashboard: true,
        isAdmin: isAdmin, // Flag to differentiate admin from regular users
      };

      // For admin users, add special credits info
      if (isAdmin && user?.specialCredits?.availableSlots) {
        specialVoucher.vouchersFigure = 122070300; // Show credit amount for admin
        specialVoucher.specialCredits = {
          totalSlots: user.specialCredits.availableSlots || 0,
          usedSlots: user.specialCredits.usedSlots || 0,
          availableSlots:
            (user.specialCredits.availableSlots || 0) -
            (user.specialCredits.usedSlots || 0),
          creditPerSlot: getSpecialCreditsForLevel(user?.level || 0),
        };
      }

      return res.json({ success: true, voucher: specialVoucher });
    }

    // Regular voucher lookup
    const voucher = await Voucher.findOne({
      _id: voucherId,
      userId: req.userId,
    })
      .populate("transferredFrom", "name phone")
      .lean();

    if (!voucher) {
      return res.status(404).json({
        success: false,
        message: "Voucher not found",
      });
    }

    res.json({ success: true, voucher });
  } catch (error) {
    console.error("MLM VOUCHER DETAILS ERROR", error);
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

      // Handle multiple uses
      const remainingUses = voucher.remainingUses || 1;

      if (remainingUses <= 0) {
        return res
          .status(400)
          .json({ success: false, message: "Voucher has no remaining uses" });
      }

      // Decrement remaining uses
      voucher.remainingUses = remainingUses - 1;

      // Add to usage history
      if (!voucher.usageHistory) {
        voucher.usageHistory = [];
      }
      voucher.usageHistory.push({
        usedAt: new Date(),
        usedBy: req.userId as any,
      });

      // Mark as redeemed only when no uses remain
      if (voucher.remainingUses <= 0) {
        voucher.redeemedStatus = "redeemed";
        voucher.redeemedAt = new Date();
      }

      await voucher.save();

      res.json({
        success: true,
        voucher,
        message:
          voucher.remainingUses > 0
            ? `Voucher used successfully. ${voucher.remainingUses} use${voucher.remainingUses > 1 ? "s" : ""} remaining.`
            : "Voucher fully redeemed",
      });
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
      const { recipientPhone, quantity = 1 } = req.body;

      // Prevent transferring special voucher
      if (voucherId === "instantlly-special-credits") {
        return res.status(400).json({
          success: false,
          message:
            "Cannot transfer special credits voucher. Use the special credits transfer feature instead.",
        });
      }

      if (!isValidObjectId(voucherId)) {
        return res
          .status(400)
          .json({ success: false, message: "Invalid voucher id" });
      }

      if (!recipientPhone) {
        return res
          .status(400)
          .json({ success: false, message: "Recipient phone is required" });
      }

      // Validate quantity
      const qty = parseInt(quantity, 10);
      if (isNaN(qty) || qty < 1 || qty > 100) {
        return res.status(400).json({
          success: false,
          message: "Quantity must be between 1 and 100",
        });
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

      // Transfer voucher with quantity
      const previousOwner = voucher.userId;
      voucher.userId = recipient._id as any;
      voucher.source = "transfer";
      voucher.transferredFrom = previousOwner;
      voucher.transferredAt = new Date();
      voucher.maxUses = qty;
      voucher.remainingUses = qty;
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
        message: `Voucher transferred to ${recipient.name} with ${qty} use${qty > 1 ? "s" : ""}`,
        voucher,
      });
    } catch (error) {
      console.error("MLM VOUCHER TRANSFER ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

// Admin voucher transfer - create new vouchers for recipient
router.post(
  "/vouchers/admin-transfer",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const { recipientPhone, quantity = 1 } = req.body;

      // Check if user is admin
      const admin = await User.findById(req.userId).select(
        "isVoucherAdmin level name phone",
      );
      const isAdmin = admin?.isVoucherAdmin || admin?.level === 0;

      if (!isAdmin) {
        return res.status(403).json({
          success: false,
          message: "Only admins can use this feature",
        });
      }

      if (!recipientPhone) {
        return res
          .status(400)
          .json({ success: false, message: "Recipient phone is required" });
      }

      // Validate quantity
      const qty = parseInt(quantity, 10);
      if (isNaN(qty) || qty < 1 || qty > 100) {
        return res.status(400).json({
          success: false,
          message: "Quantity must be between 1 and 100",
        });
      }

      // Find recipient by phone
      const recipient = await User.findOne({ phone: recipientPhone });
      if (!recipient) {
        return res
          .status(404)
          .json({ success: false, message: "Recipient not found" });
      }

      // Increment recipient's voucherBalance (no physical documents created)
      await User.findByIdAndUpdate(recipient._id, {
        $inc: { voucherBalance: qty },
      });

      // Deduct from admin's voucherBalance
      await User.findByIdAndUpdate(req.userId, {
        $inc: { voucherBalance: -qty },
      });

      // Save transfer log for history tracking
      await VoucherTransferLog.create({
        senderId: req.userId,
        senderName: (admin as any).name,
        senderPhone: (admin as any).phone,
        recipientId: recipient._id,
        recipientName: recipient.name,
        recipientPhone: recipient.phone,
        quantity: qty,
        transferredAt: new Date(),
      });

      res.json({
        success: true,
        message: `Transferred ${qty} voucher${qty > 1 ? "s" : ""} to ${recipient.name}`,
        count: qty,
      });
    } catch (error) {
      console.error("MLM ADMIN VOUCHER TRANSFER ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

// ============================================
// TRANSFER HISTORY (Special Credits + Vouchers)
// ============================================

router.get("/transfer-history", requireAuth, async (req: AuthReq, res) => {
  try {
    const { limit = 50 } = req.query;

    // Get special credits sent by this user
    const specialCreditsSent = await SpecialCredit.find({
      ownerId: req.userId,
      status: "sent",
    })
      .populate("recipientId", "name phone")
      .sort({ sentAt: -1 })
      .limit(Number(limit))
      .lean();

    // Get special credits received by this user
    const specialCreditsReceived = await SpecialCredit.find({
      recipientId: req.userId,
      status: "sent",
    })
      .populate("ownerId", "name phone")
      .sort({ sentAt: -1 })
      .limit(Number(limit))
      .lean();

    // Get balance-based voucher transfer logs (admin-transfer uses voucherBalance counters)
    const voucherLogsSent = await VoucherTransferLog.find({
      senderId: req.userId,
    })
      .sort({ transferredAt: -1 })
      .limit(Number(limit))
      .lean();

    const voucherLogsReceived = await VoucherTransferLog.find({
      recipientId: req.userId,
    })
      .sort({ transferredAt: -1 })
      .limit(Number(limit))
      .lean();

    // Get vouchers sent (check transferHistory)
    const vouchersSent = await Voucher.find({
      "transferHistory.from": req.userId,
    })
      .populate("userId", "name phone")
      .populate({
        path: "transferHistory.from",
        select: "name phone",
      })
      .populate({
        path: "transferHistory.to",
        select: "name phone",
      })
      .sort({ "transferHistory.transferredAt": -1 })
      .limit(Number(limit))
      .lean();

    // Get vouchers received
    const vouchersReceived = await Voucher.find({
      userId: req.userId,
      source: { $in: ["transfer", "admin"] },
    })
      .populate("transferredFrom", "name phone")
      .sort({ transferredAt: -1 })
      .limit(Number(limit))
      .lean();

    // Format the response
    const history = {
      specialCredits: {
        sent: specialCreditsSent.map((sc: any) => ({
          type: "special_credit",
          direction: "sent",
          amount: sc.creditAmount,
          recipient: {
            id: sc.recipientId?._id,
            name: sc.recipientName || sc.recipientId?.name,
            phone: sc.recipientPhone || sc.recipientId?.phone,
          },
          slotNumber: sc.slotNumber,
          transferredAt: sc.sentAt,
        })),
        received: specialCreditsReceived.map((sc: any) => ({
          type: "special_credit",
          direction: "received",
          amount: sc.creditAmount,
          sender: {
            id: sc.ownerId?._id,
            name: sc.ownerId?.name,
            phone: sc.ownerId?.phone,
          },
          slotNumber: sc.slotNumber,
          transferredAt: sc.sentAt,
        })),
      },
      vouchers: {
        sent: [] as any[],
        received: vouchersReceived.map((v: any) => ({
          type: "voucher",
          direction: "received",
          voucherNumber: v.voucherNumber,
          companyName: v.companyName || "Instantlly",
          amount: v.amount || v.MRP,
          sender: v.transferredFrom
            ? {
                id: v.transferredFrom._id,
                name: v.transferredFrom.name,
                phone: v.transferredFrom.phone,
              }
            : null,
          transferredAt: v.transferredAt,
          source: v.source,
        })),
      },
    };

    // Extract sent vouchers from transferHistory
    const vouchersSentMap = new Map();
    vouchersSent.forEach((v: any) => {
      if (v.transferHistory && v.transferHistory.length > 0) {
        v.transferHistory.forEach((th: any) => {
          const fromId = th.from?._id?.toString() || th.from?.toString();
          if (fromId === req.userId) {
            const recipientId = th.to?._id?.toString() || th.to?.toString();
            const transferDate = new Date(th.transferredAt);
            // Create a key based on recipient and transfer time (rounded to minute)
            const transferMinute = new Date(
              transferDate.getFullYear(),
              transferDate.getMonth(),
              transferDate.getDate(),
              transferDate.getHours(),
              transferDate.getMinutes(),
            ).getTime();
            const key = `${recipientId}-${transferMinute}`;

            if (vouchersSentMap.has(key)) {
              // Add to existing grouped transfer
              const existing = vouchersSentMap.get(key);
              existing.count++;
              existing.voucherNumbers.push(v.voucherNumber);
              existing.totalAmount += v.amount || v.MRP;
            } else {
              // Create new grouped transfer
              vouchersSentMap.set(key, {
                type: "voucher",
                direction: "sent",
                voucherNumber: v.voucherNumber,
                voucherNumbers: [v.voucherNumber],
                companyName: v.companyName || "Instantlly",
                amount: v.amount || v.MRP,
                totalAmount: v.amount || v.MRP,
                count: 1,
                recipient: {
                  id: recipientId,
                  name: th.to?.name || v.userId?.name,
                  phone: th.to?.phone || v.userId?.phone,
                },
                transferredAt: th.transferredAt,
              });
            }
          }
        });
      }
    });

    // Convert map to array
    history.vouchers.sent = Array.from(vouchersSentMap.values());

    // Merge balance-based transfer logs into sent/received
    const VOUCHER_MRP = 1200; // ₹1200 per voucher
    const logSentEntries = voucherLogsSent.map((log: any) => ({
      type: "voucher",
      direction: "sent",
      voucherNumber: null,
      voucherNumbers: [],
      companyName: "Instantlly",
      amount: VOUCHER_MRP,
      totalAmount: VOUCHER_MRP * log.quantity,
      count: log.quantity,
      recipient: {
        id: log.recipientId?.toString(),
        name: log.recipientName,
        phone: log.recipientPhone,
      },
      transferredAt: log.transferredAt,
      source: "admin-balance",
    }));

    const logReceivedEntries = voucherLogsReceived.map((log: any) => ({
      type: "voucher",
      direction: "received",
      voucherNumber: null,
      companyName: "Instantlly",
      amount: VOUCHER_MRP,
      totalAmount: VOUCHER_MRP * log.quantity,
      count: log.quantity,
      sender: {
        id: log.senderId?.toString(),
        name: log.senderName,
        phone: log.senderPhone,
      },
      transferredAt: log.transferredAt,
      source: "admin-balance",
    }));

    history.vouchers.sent = [...history.vouchers.sent, ...logSentEntries];
    history.vouchers.received = [
      ...history.vouchers.received,
      ...logReceivedEntries,
    ];

    // Combine and sort all transfers by date
    const allTransfers = [
      ...history.specialCredits.sent,
      ...history.specialCredits.received,
      ...history.vouchers.sent,
      ...history.vouchers.received,
    ].sort((a, b) => {
      const dateA = new Date(a.transferredAt || 0).getTime();
      const dateB = new Date(b.transferredAt || 0).getTime();
      return dateB - dateA;
    });

    res.json({
      success: true,
      history: {
        all: allTransfers,
        specialCredits: history.specialCredits,
        vouchers: history.vouchers,
        summary: {
          specialCreditsSent: history.specialCredits.sent.length,
          specialCreditsReceived: history.specialCredits.received.length,
          vouchersSent: history.vouchers.sent.reduce(
            (sum, v: any) => sum + (v.count || 1),
            0,
          ),
          vouchersReceived: history.vouchers.received.length,
          totalTransfers: allTransfers.length,
        },
      },
    });
  } catch (error) {
    console.error("MLM TRANSFER HISTORY ERROR", error);
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
      "name phone level directCount downlineCount parentId createdAt isVoucherAdmin specialCredits voucherBalance",
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

    // Add special credits data for voucher admin
    let specialCreditsData = null;
    const isVoucherAdmin = (user as any).isVoucherAdmin === true;

    if (isVoucherAdmin) {
      const specialCreditSlots = await SpecialCredit.find({
        ownerId: req.userId,
      })
        .populate("recipientId", "name phone")
        .sort({ slotNumber: 1 })
        .lean();

      const totalSlots = getSlotsForUser(true);
      const creditPerSlot = getSpecialCreditsForLevel((user as any).level || 0);
      const availableSlots = specialCreditSlots.filter(
        (s: any) => s.status === "available",
      ).length;
      const usedSlots = specialCreditSlots.filter(
        (s: any) => s.status === "sent",
      ).length;

      specialCreditsData = {
        vouchersFigure: (user as any).voucherBalance || 0,
        totalSlots,
        availableSlots,
        usedSlots,
        creditPerSlot,
        label: "Sales Target at Special Discount",
      };
    }

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
        isVoucherAdmin: isVoucherAdmin,
        specialCredits: (user as any).specialCredits || {
          availableSlots: 0,
          usedSlots: 0,
        },
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
        // Include vouchersFigure for any user who has a voucherBalance > 0
        ...((user as any).voucherBalance > 0
          ? { vouchersFigure: (user as any).voucherBalance }
          : {}),
      },
      structuralCreditPool,
      baseMrp: MLM_BASE_MRP,
      specialCredits: specialCreditsData,
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

// ============================================
// SPECIAL CREDITS - "Sales Target at Special Discount"
// ============================================

// Credit calculation per level (divides by 5 each level)
const SPECIAL_CREDIT_CHAIN = [
  14648436000, // Level 0 (Admin)
  2929686000, // Level 1
  585936000, // Level 2
  117186000, // Level 3
  23436000, // Level 4
  4686000, // Level 5
  936000, // Level 6
  186000, // Level 7
  36000, // Level 8
  6000, // Level 9
];

// Calculate credits for a given level
function getSpecialCreditsForLevel(level: number): number {
  if (level < 0 || level >= SPECIAL_CREDIT_CHAIN.length) return 0;
  return SPECIAL_CREDIT_CHAIN[level];
}

// Get number of slots for user (10 for admin, 5 for others)
function getSlotsForUser(isAdmin: boolean): number {
  return isAdmin ? 10 : 5;
}

// ✅ Initialize Admin's Special Credit Slots
router.post("/special-credits/admin/initialize", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const { adminUserId } = req.body;

    if (!adminUserId) {
      return res.status(400).json({
        success: false,
        message: "adminUserId is required",
      });
    }

    // Find admin user
    const admin = await User.findById(adminUserId);
    if (!admin) {
      return res.status(404).json({
        success: false,
        message: "Admin user not found",
      });
    }

    // Mark as voucher admin
    admin.isVoucherAdmin = true;
    admin.level = 0; // Admin is level 0
    admin.parentId = null as any;

    // Initialize special credits
    if (!admin.specialCredits) {
      admin.specialCredits = {
        balance: 0,
        totalReceived: 0,
        totalSent: 0,
        availableSlots: 10,
        usedSlots: 0,
      };
    } else {
      admin.specialCredits.availableSlots = 10;
    }

    await admin.save();

    // Check if slots already exist
    const existingSlots = await SpecialCredit.countDocuments({
      ownerId: adminUserId,
    });

    if (existingSlots >= 10) {
      return res.json({
        success: true,
        message: "Admin already has special credit slots initialized",
        slots: existingSlots,
      });
    }

    // Create 10 slots for admin
    const slots = [];
    const adminCreditAmount = getSpecialCreditsForLevel(0); // 14,648,436,000

    for (let i = 1; i <= 10; i++) {
      const slot = await SpecialCredit.create({
        ownerId: adminUserId,
        slotNumber: i,
        creditAmount: adminCreditAmount,
        status: "available",
        level: 0,
      });
      slots.push(slot);
    }

    res.json({
      success: true,
      message: "Admin special credit slots initialized",
      admin: {
        id: admin._id,
        name: admin.name,
        phone: admin.phone,
        isVoucherAdmin: true,
      },
      slots: slots.length,
      creditPerSlot: adminCreditAmount,
    });
  } catch (error) {
    console.error("SPECIAL CREDITS ADMIN INIT ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ Get Special Credit Slots for current user
router.get("/special-credits/slots", requireAuth, async (req: AuthReq, res) => {
  try {
    const user = await User.findById(req.userId).select(
      "name phone level isVoucherAdmin specialCredits",
    );

    if (!user) {
      return res.status(404).json({
        success: false,
        message: "User not found",
      });
    }

    // Get user's slots
    const slots = await SpecialCredit.find({ ownerId: req.userId })
      .populate("recipientId", "name phone")
      .sort({ slotNumber: 1 })
      .lean();

    // Calculate expected slots
    const isAdmin = user.isVoucherAdmin || user.level === 0;
    const expectedSlots = getSlotsForUser(isAdmin);
    const creditAmount = getSpecialCreditsForLevel(user.level || 0);

    // Format slots response
    const formattedSlots = [];
    for (let i = 1; i <= expectedSlots; i++) {
      const slot = slots.find((s: any) => s.slotNumber === i);

      if (slot) {
        formattedSlots.push({
          slotNumber: i,
          status: (slot as any).status,
          creditAmount: (slot as any).creditAmount,
          recipientName: (slot as any).recipientName || null,
          recipientPhone: (slot as any).recipientPhone || null,
          recipientId: (slot as any).recipientId?._id?.toString() || null,
          sentAt: (slot as any).sentAt || null,
          isAvailable: (slot as any).status === "available",
        });
      } else {
        // Placeholder for empty slot
        formattedSlots.push({
          slotNumber: i,
          status: "available",
          creditAmount,
          recipientName: null,
          recipientPhone: null,
          recipientId: null,
          sentAt: null,
          isAvailable: true,
        });
      }
    }

    res.json({
      success: true,
      user: {
        id: user._id,
        name: user.name,
        phone: user.phone,
        level: user.level,
        isVoucherAdmin: user.isVoucherAdmin || false,
      },
      slots: formattedSlots,
      summary: {
        totalSlots: expectedSlots,
        availableSlots: formattedSlots.filter((s) => s.isAvailable).length,
        usedSlots: formattedSlots.filter((s) => !s.isAvailable).length,
        creditPerSlot: creditAmount,
      },
    });
  } catch (error) {
    console.error("SPECIAL CREDITS SLOTS ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ Send Special Credits to a user by phone number
router.post("/special-credits/send", requireAuth, async (req: AuthReq, res) => {
  try {
    const { recipientPhone, slotNumber } = req.body;

    if (!recipientPhone) {
      return res.status(400).json({
        success: false,
        message: "Recipient phone number is required",
      });
    }

    if (!slotNumber || slotNumber < 1) {
      return res.status(400).json({
        success: false,
        message: "Valid slot number is required",
      });
    }

    // Get sender
    const sender = await User.findById(req.userId).select(
      "name phone level isVoucherAdmin specialCredits voucherBalance",
    );

    if (!sender) {
      return res.status(404).json({
        success: false,
        message: "Sender not found",
      });
    }

    // Check slot availability
    const isAdmin = sender.isVoucherAdmin || sender.level === 0;
    const maxSlots = getSlotsForUser(isAdmin);

    // Non-admin users need at least 5 vouchers to send special credits
    if (!isAdmin) {
      const voucherDocCount = await Voucher.countDocuments({
        userId: req.userId,
        redeemedStatus: { $ne: "redeemed" },
      });
      const totalVouchers =
        voucherDocCount + ((sender as any).voucherBalance || 0);

      if (totalVouchers < 5) {
        return res.status(403).json({
          success: false,
          message: `You need at least 5 vouchers to send special credits. Current vouchers: ${totalVouchers}`,
          requiredVouchers: 5,
          currentVouchers: totalVouchers,
        });
      }
    }

    if (slotNumber > maxSlots) {
      return res.status(400).json({
        success: false,
        message: `Invalid slot number. Maximum slots: ${maxSlots}`,
      });
    }

    // Find or create the slot
    let slot = await SpecialCredit.findOne({
      ownerId: req.userId,
      slotNumber,
    });

    // If slot doesn't exist, create it
    if (!slot) {
      const creditAmount = getSpecialCreditsForLevel(sender.level || 0);
      slot = await SpecialCredit.create({
        ownerId: req.userId,
        slotNumber,
        creditAmount,
        status: "available",
        level: sender.level || 0,
      });
    }

    // Check if slot is already used
    if (slot.status === "sent") {
      return res.status(400).json({
        success: false,
        message: `Slot ${slotNumber} already used. Sent to ${slot.recipientName}`,
        slot: {
          recipientName: slot.recipientName,
          recipientPhone: slot.recipientPhone,
          sentAt: slot.sentAt,
        },
      });
    }

    // Find recipient by phone - try multiple formats
    const phonePatterns = [
      recipientPhone, // As is
      `+91${recipientPhone}`, // With +91
      recipientPhone.replace(/^\+91/, ""), // Remove +91 if present
      recipientPhone.replace(/^91/, ""), // Remove 91 prefix
    ];

    const recipient = await User.findOne({
      $or: [
        { phone: phonePatterns[0] },
        { phone: phonePatterns[1] },
        { phone: phonePatterns[2] },
        { phone: phonePatterns[3] },
      ],
    });

    if (!recipient) {
      return res.status(404).json({
        success: false,
        message: `No user found with phone number: ${recipientPhone}`,
      });
    }

    // Can't send to yourself
    if (recipient._id.toString() === req.userId) {
      return res.status(400).json({
        success: false,
        message: "Cannot send special credits to yourself",
      });
    }

    // Update slot
    slot.status = "sent";
    slot.recipientId = recipient._id as any;
    slot.recipientName = recipient.name;
    slot.recipientPhone = recipient.phone;
    slot.sentAt = new Date();
    await slot.save();

    // Update sender's special credits stats
    if (!sender.specialCredits) {
      sender.specialCredits = {
        balance: 0,
        totalReceived: 0,
        totalSent: 0,
        availableSlots: maxSlots,
        usedSlots: 0,
      };
    }
    sender.specialCredits.totalSent += slot.creditAmount;
    sender.specialCredits.usedSlots += 1;
    await sender.save();

    // Initialize recipient's special credits if not exists
    if (!recipient.specialCredits) {
      recipient.specialCredits = {
        balance: 0,
        totalReceived: 0,
        totalSent: 0,
        availableSlots: 0,
        usedSlots: 0,
      };
    }

    // Add credits to recipient
    recipient.specialCredits.balance += slot.creditAmount;
    recipient.specialCredits.totalReceived += slot.creditAmount;
    recipient.specialCredits.availableSlots = 5; // Recipients get 5 slots

    // Set recipient's level if not already set
    if (!recipient.level || recipient.level === 0) {
      recipient.level = (sender.level || 0) + 1;
    }

    // Link recipient to sender if not already linked
    if (!recipient.parentId) {
      recipient.parentId = req.userId as any;
      sender.directCount = (sender.directCount || 0) + 1;
      await sender.save();
    }

    await recipient.save();

    // Add credits to recipient's WALLET
    await addCredits(recipient._id.toString(), slot.creditAmount);

    // Create 5 slots for recipient with EQUAL amounts
    // Each slot gets: (received amount) / 5
    const recipientLevel = recipient.level;
    const recipientSlots = [];
    const slotCreditAmount = slot.creditAmount / 5; // Divide equally among 5 slots

    for (let i = 1; i <= 5; i++) {
      const existingSlot = await SpecialCredit.findOne({
        ownerId: recipient._id,
        slotNumber: i,
      });

      if (!existingSlot) {
        const newSlot = await SpecialCredit.create({
          ownerId: recipient._id,
          slotNumber: i,
          creditAmount: slotCreditAmount, // All slots get the same amount
          status: "available",
          level: recipientLevel,
          sourceSlotId: slot._id,
        });
        recipientSlots.push(newSlot);
      }
    }

    res.json({
      success: true,
      message: `Successfully sent ${slot.creditAmount.toLocaleString()} special credits to ${recipient.name}`,
      transfer: {
        sender: {
          id: sender._id,
          name: sender.name,
          phone: sender.phone,
        },
        recipient: {
          id: recipient._id,
          name: recipient.name,
          phone: recipient.phone,
          level: recipient.level,
        },
        slot: {
          slotNumber,
          creditAmount: slot.creditAmount,
          sentAt: slot.sentAt,
        },
        recipientSlotsCreated: recipientSlots.length,
        recipientSlotAmount: slotCreditAmount,
        recipientSlots: recipientSlots.map((s) => ({
          slotNumber: s.slotNumber,
          creditAmount: s.creditAmount,
        })),
      },
    });
  } catch (error) {
    console.error("SPECIAL CREDITS SEND ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

// ✅ Get Special Credits Dashboard
router.get(
  "/special-credits/dashboard",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const user = await User.findById(req.userId).select(
        "name phone level isVoucherAdmin specialCredits parentId voucherBalance",
      );

      if (!user) {
        return res.status(404).json({
          success: false,
          message: "User not found",
        });
      }

      // Get all slots
      const slots = await SpecialCredit.find({ ownerId: req.userId })
        .populate("recipientId", "name phone")
        .sort({ slotNumber: 1 })
        .lean();

      // Calculate stats
      const isAdmin = user.isVoucherAdmin || user.level === 0;
      const totalSlots = getSlotsForUser(isAdmin);
      const creditPerSlot = getSpecialCreditsForLevel(user.level || 0);

      const availableSlots = slots.filter(
        (s: any) => s.status === "available",
      ).length;
      const usedSlots = slots.filter((s: any) => s.status === "sent").length;

      // Calculate total available credits
      const totalAvailableCredits = availableSlots * creditPerSlot;
      const totalSentCredits = user.specialCredits?.totalSent || 0;

      // Get network users who received special credits from this user
      const recipientIds = slots
        .filter((s: any) => s.recipientId)
        .map((s: any) => s.recipientId._id);

      const networkUsers = await User.find({
        _id: { $in: recipientIds },
      })
        .select("name phone level specialCredits")
        .lean();

      // Provide vouchers figure for admin and users with special credits
      let vouchersFigure = 0;
      if (isAdmin) {
        // Admin uses voucherBalance field (a stored number, not actual documents)
        vouchersFigure = user.voucherBalance || 0;
      } else if (user.specialCredits?.availableSlots > 0) {
        // Regular users with special credits: count physical voucher docs + balance-based transfers
        const physicalCount = await Voucher.countDocuments({
          userId: req.userId,
          redeemedStatus: { $ne: "redeemed" },
        });
        vouchersFigure = physicalCount + ((user as any).voucherBalance || 0);
      }

      res.json({
        success: true,
        user: {
          id: user._id,
          name: user.name,
          phone: user.phone,
          level: user.level,
          isVoucherAdmin: user.isVoucherAdmin || false,
        },
        dashboard: {
          vouchersFigure, // Only for admin
          specialCredits: {
            balance: totalAvailableCredits, // Total available credits
            totalReceived: user.specialCredits?.totalReceived || 0,
            totalSent: totalSentCredits, // Credits sent to others
            creditPerSlot,
          },
          slots: {
            total: totalSlots,
            available: availableSlots,
            used: usedSlots,
          },
          label: "Sales Target at Special Discount",
        },
        networkUsers: networkUsers.map((nu: any) => ({
          id: nu._id.toString(),
          name: nu.name,
          phone: nu.phone,
          level: nu.level,
          creditsReceived: nu.specialCredits?.totalReceived || 0,
        })),
      });
    } catch (error) {
      console.error("SPECIAL CREDITS DASHBOARD ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

// ✅ Get network users with special credits info (for admin view)
router.get(
  "/special-credits/network",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const user = await User.findById(req.userId).select(
        "name phone level isVoucherAdmin",
      );

      if (!user) {
        return res.status(404).json({
          success: false,
          message: "User not found",
        });
      }

      // Get slots with recipients
      const slots = await SpecialCredit.find({
        ownerId: req.userId,
        status: "sent",
      })
        .populate("recipientId", "name phone level specialCredits")
        .sort({ slotNumber: 1 })
        .lean();

      // Get slots info
      const allSlots = await SpecialCredit.find({ ownerId: req.userId })
        .sort({ slotNumber: 1 })
        .lean();

      const isAdmin = user.isVoucherAdmin || user.level === 0;
      const totalSlots = getSlotsForUser(isAdmin);
      const creditPerSlot = getSpecialCreditsForLevel(user.level || 0);

      // Format network users
      const networkUsers = slots.map((slot: any, index: number) => ({
        slotNumber: slot.slotNumber,
        name: slot.recipientName || "",
        phone: slot.recipientPhone || "",
        credits: slot.creditAmount,
        sentAt: slot.sentAt,
        recipientLevel: slot.recipientId?.level || 0,
      }));

      // Add placeholders for unused slots
      const placeholders = [];
      for (let i = 1; i <= totalSlots; i++) {
        const existingSlot = allSlots.find((s: any) => s.slotNumber === i);
        if (!existingSlot || existingSlot.status === "available") {
          placeholders.push({
            slotNumber: i,
            name: "",
            phone: "",
            credits: creditPerSlot,
            sentAt: null,
            recipientLevel: null,
            isPlaceholder: true,
          });
        }
      }

      res.json({
        success: true,
        networkUsers: [...networkUsers, ...placeholders].sort(
          (a, b) => a.slotNumber - b.slotNumber,
        ),
        summary: {
          totalSlots,
          usedSlots: networkUsers.length,
          availableSlots: placeholders.length,
          creditPerSlot,
        },
      });
    } catch (error) {
      console.error("SPECIAL CREDITS NETWORK ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

export default router;
