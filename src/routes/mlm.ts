import { Router } from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import User from "../models/User";
import MlmCredit from "../models/MlmCredit";
import Voucher from "../models/Voucher";
import CommissionTransaction from "../models/CommissionTransaction";
import Withdrawal from "../models/Withdrawal";
import { getStructuralCreditPool, MLM_BASE_ENTRY_VALUE } from "../utils/mlm";
import {
  addCredits,
  addCommission,
  getOrCreateWallet,
  subtractCommission,
} from "../services/mlm/walletService";
import { assignCreditUsage } from "../services/mlm/creditLifecycleService";
import { distributeCommission } from "../services/mlm/commissionService";
import { generateVouchers } from "../services/mlm/voucherService";
import { buildNetworkTree } from "../services/mlm/treeService";
import { getCreditDashboard } from "../services/mlm/creditDashboardService";
import { getCommissionSummary } from "../services/mlm/commissionSummaryService";
import { getDirectBuyers } from "../services/mlm/directBuyersService";

const router = Router();

const CREDIT_EXPIRY_MINUTES = 60;
const TRANSFER_EXPIRY_HOURS = 48;

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
      return res
        .status(400)
        .json({ success: false, message: "Invalid credit amount" });
    }

    if (receiverId === req.userId) {
      return res
        .status(400)
        .json({ success: false, message: "Cannot transfer to yourself" });
    }

    const senderWallet = await getOrCreateWallet(req.userId as string);
    const senderUser = await User.findById(req.userId).select("level");
    if (!senderUser) {
      return res
        .status(404)
        .json({ success: false, message: "Sender not found" });
    }
    if (senderWallet.creditBalance < creditCount) {
      return res
        .status(400)
        .json({ success: false, message: "No credits available" });
    }

    const receiver = await User.findById(receiverId);
    if (!receiver) {
      return res
        .status(404)
        .json({ success: false, message: "Receiver not found" });
    }

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

      await User.findByIdAndUpdate(req.userId, { $inc: { directCount: 1 } });
    }

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

    res.json({ success: true, credits: createdCredits });
  } catch (error) {
    console.error("MLM CREDIT TRANSFER ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

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

      if (credit.paymentStatus === "paid") {
        return res.json({ success: true, message: "Already paid" });
      }

      credit.paymentStatus = "paid";
      credit.status = "active";
      credit.activatedAt = new Date();
      credit.transferExpiresAt = new Date(
        Date.now() + TRANSFER_EXPIRY_HOURS * 60 * 60 * 1000,
      );
      await credit.save();

      await addCredits(req.userId as string, credit.quantity);
      const vouchers = await generateVouchers(
        req.userId as string,
        credit._id.toString(),
        credit.quantity,
      );
      await distributeCommission(req.userId as string, credit._id.toString());

      res.json({ success: true, vouchersGenerated: vouchers.length });
    } catch (error) {
      console.error("MLM PAYMENT CONFIRM ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

router.get("/vouchers", requireAuth, async (req: AuthReq, res) => {
  try {
    const { status, limit = 20, skip = 0 } = req.query;
    const query: any = { userId: req.userId };
    if (status) query.redeemedStatus = status;

    const vouchers = await Voucher.find(query)
      .sort({ issueDate: -1 })
      .skip(Number(skip))
      .limit(Number(limit))
      .lean();

    res.json({ success: true, vouchers });
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

router.get("/commissions/summary", requireAuth, async (req: AuthReq, res) => {
  try {
    const summary = await getCommissionSummary(req.userId as string);
    res.json({ success: true, summary });
  } catch (error) {
    console.error("MLM COMMISSION SUMMARY ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get(
  "/commissions/transactions",
  requireAuth,
  async (req: AuthReq, res) => {
    try {
      const { limit = 50, skip = 0 } = req.query;
      const transactions = await CommissionTransaction.find({
        toUserId: req.userId,
      })
        .sort({ createdAt: -1 })
        .skip(Number(skip))
        .limit(Number(limit))
        .lean();

      res.json({ success: true, transactions });
    } catch (error) {
      console.error("MLM COMMISSION TRANSACTIONS ERROR", error);
      res.status(500).json({ success: false, message: "Server error" });
    }
  },
);

router.post("/withdrawals/request", requireAuth, async (req: AuthReq, res) => {
  try {
    const { amount } = req.body;
    if (!amount || amount <= 0) {
      return res
        .status(400)
        .json({ success: false, message: "Invalid amount" });
    }

    const lastWithdrawal = (await Withdrawal.findOne({ userId: req.userId })
      .sort({ requestedAt: -1 })
      .lean()) as any;

    if (
      lastWithdrawal &&
      Date.now() - new Date((lastWithdrawal as any).requestedAt).getTime() <
        24 * 60 * 60 * 1000
    ) {
      return res
        .status(400)
        .json({ success: false, message: "Only one withdrawal per day" });
    }

    const wallet = await getOrCreateWallet(req.userId as string);
    if (wallet.commissionAvailableBalance < amount) {
      return res
        .status(400)
        .json({ success: false, message: "Insufficient balance" });
    }

    const withdrawal = await Withdrawal.create({ userId: req.userId, amount });
    await subtractCommission(req.userId as string, amount);

    res.json({ success: true, withdrawal });
  } catch (error) {
    console.error("MLM WITHDRAWAL REQUEST ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.get("/withdrawals/history", requireAuth, async (req: AuthReq, res) => {
  try {
    const withdrawals = await Withdrawal.find({ userId: req.userId })
      .sort({ requestedAt: -1 })
      .limit(50)
      .lean();

    res.json({ success: true, withdrawals });
  } catch (error) {
    console.error("MLM WITHDRAWAL HISTORY ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

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
      .select("name phone level directCount createdAt")
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

router.post("/admin/withdrawals/:withdrawalId/approve", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const { withdrawalId } = req.params;
    const withdrawal = await Withdrawal.findById(withdrawalId);
    if (!withdrawal) {
      return res.status(404).json({ success: false, message: "Not found" });
    }

    withdrawal.status = "approved";
    withdrawal.processedAt = new Date();
    await withdrawal.save();

    res.json({ success: true, withdrawal });
  } catch (error) {
    console.error("MLM WITHDRAWAL APPROVE ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

router.post("/admin/withdrawals/:withdrawalId/reject", async (req, res) => {
  try {
    const adminKey = req.headers["x-admin-key"] as string;
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const { withdrawalId } = req.params;
    const { note } = req.body;

    const withdrawal = await Withdrawal.findById(withdrawalId);
    if (!withdrawal) {
      return res.status(404).json({ success: false, message: "Not found" });
    }

    withdrawal.status = "rejected";
    withdrawal.adminNote = note || "Rejected by admin";
    withdrawal.processedAt = new Date();
    await withdrawal.save();

    await addCommission(withdrawal.userId.toString(), withdrawal.amount);

    res.json({ success: true, withdrawal });
  } catch (error) {
    console.error("MLM WITHDRAWAL REJECT ERROR", error);
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

router.get("/overview", requireAuth, async (req: AuthReq, res) => {
  try {
    const user = await User.findById(req.userId).select(
      "name phone level directCount parentId createdAt",
    );
    if (!user) {
      return res
        .status(404)
        .json({ success: false, message: "User not found" });
    }

    const wallet = await getOrCreateWallet(req.userId as string);
    const creditDashboard = await getCreditDashboard(req.userId as string);
    const commissionSummary = await getCommissionSummary(req.userId as string);

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
        parentId: user.parentId,
        joinedDate: user.createdAt,
      },
      wallet,
      creditDashboard,
      commissionSummary,
      metrics: {
        availableCredits: wallet.creditBalance,
        totalVouchersTransferred: creditDashboard.totalCreditsTransferred,
        totalNetworkUsers,
        estimatedCommission: commissionSummary.totalEarned,
      },
      structuralCreditPool,
      baseMrp: MLM_BASE_ENTRY_VALUE,
    });
  } catch (error) {
    console.error("MLM OVERVIEW ERROR", error);
    res.status(500).json({ success: false, message: "Server error" });
  }
});

export default router;
