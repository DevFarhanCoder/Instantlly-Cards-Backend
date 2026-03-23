import { Router } from "express";
import mongoose from "mongoose";
import crypto from "crypto";
import axios from "axios";
import { requireAuth, AuthReq } from "../middleware/auth";
import BusinessPromotion from "../models/BusinessPromotion";
import PromotionOrder from "../models/PromotionOrder";
import PromotionPricingPlan from "../models/PromotionPricingPlan";
import User from "../models/User";
import Voucher from "../models/Voucher";
import VoucherRedemption from "../models/VoucherRedemption";

const router = Router();

type AreaType = "pincode" | "tehsil" | "district" | "division" | "state" | "zone" | "india";

type PricingRow = {
  rank: number;
  pincode: number;
  tehsil: number;
  district: number;
  division: number;
  state: number;
  zone: number;
  india: number;
};

const buildPricingRow = (rank: number, pincode: number): PricingRow => {
  const tehsil = pincode * 6;
  const district = tehsil * 6;
  const division = district * 6;
  const state = division * 6;
  const zone = state * 6;
  const india = zone * 6;
  return {
    rank,
    pincode,
    tehsil,
    district,
    division,
    state,
    zone,
    india,
  };
};

const getPincodeForRank = (rank: number): number => {
  if (rank === 21) return 600;
  return 2600 - (rank - 1) * 100;
};

const DEFAULT_PRICING_ROWS: PricingRow[] = (() => {
  const rows: PricingRow[] = [];
  rows.push(buildPricingRow(21, getPincodeForRank(21)));
  for (let rank = 20; rank >= 1; rank -= 1) {
    rows.push(buildPricingRow(rank, getPincodeForRank(rank)));
  }
  return rows;
})();

const AREA_TYPES: AreaType[] = [
  "pincode",
  "tehsil",
  "district",
  "division",
  "state",
  "zone",
  "india",
];

const INSTANTLLY_COMPANY_NAME = "Instantlly";
const PAYMENT_PENDING_TIMEOUT_MINUTES = Math.max(
  5,
  Number(process.env.PROMOTION_PAYMENT_TIMEOUT_MINUTES || 30),
);
const PROMO_STALE_SWEEP_MINUTES = Math.max(
  2,
  Number(process.env.PROMOTION_STALE_SWEEP_MINUTES || 10),
);

function priorityFromRank(rank: number): number {
  if (rank === 21) {
    return 10;
  }
  return Math.max(20, 100 - rank * 3);
}

function getRankLabel(rank: number): string {
  return rank === 21 ? "No Rank" : `Rank ${rank}`;
}

function getAdminKey(req: AuthReq): string | undefined {
  const headerValue = req.headers["x-admin-key"];
  return typeof headerValue === "string" ? headerValue : undefined;
}

function getRazorpayCredentials() {
  const keyId =
    process.env.RAZORPAY_KEY_ID ||
    process.env.RAZORPAY_KEY_ID_TEST ||
    "";
  const keySecret =
    process.env.RAZORPAY_KEY_SECRET ||
    process.env.RAZORPAY_KEY_SECRET_TEST ||
    "";

  return { keyId, keySecret };
}

async function createRazorpayOrder(params: {
  amountRupees: number;
  currency: string;
  receipt: string;
  notes: Record<string, string>;
}) {
  const { keyId, keySecret } = getRazorpayCredentials();
  if (!keyId || !keySecret) {
    throw new Error("Razorpay credentials are not configured");
  }

  const amountPaise = Math.round(params.amountRupees * 100);
  const payload = {
    amount: amountPaise,
    currency: params.currency,
    receipt: params.receipt,
    notes: params.notes,
  };

  const auth = Buffer.from(`${keyId}:${keySecret}`).toString("base64");

  const response = await axios.post("https://api.razorpay.com/v1/orders", payload, {
    headers: {
      Authorization: `Basic ${auth}`,
      "Content-Type": "application/json",
    },
    timeout: 15000,
  });

  return response.data as {
    id: string;
    amount: number;
    currency: string;
    receipt: string;
    status: string;
  };
}

function verifyRazorpaySignature(params: {
  razorpayOrderId: string;
  razorpayPaymentId: string;
  razorpaySignature: string;
}) {
  const { keySecret } = getRazorpayCredentials();
  if (!keySecret) {
    throw new Error("Razorpay secret is not configured");
  }

  const payload = `${params.razorpayOrderId}|${params.razorpayPaymentId}`;
  const expectedSignature = crypto
    .createHmac("sha256", keySecret)
    .update(payload)
    .digest("hex");

  return crypto.timingSafeEqual(
    Buffer.from(expectedSignature),
    Buffer.from(params.razorpaySignature),
  );
}

async function getInstantllyVoucherTemplate(): Promise<any | null> {
  const configuredId = process.env.INSTANTLLY_VOUCHER_ID;
  if (configuredId && mongoose.Types.ObjectId.isValid(configuredId)) {
    const voucher = (await Voucher.findById(configuredId).lean()) as any;
    if (voucher && voucher.companyName === INSTANTLLY_COMPANY_NAME) {
      return voucher;
    }
  }

  // Fallback: latest published Instantlly template (admin-level, no userId)
  const fallback = (await Voucher.findOne({
    companyName: INSTANTLLY_COMPANY_NAME,
    isPublished: true,
    $or: [{ userId: { $exists: false } }, { userId: null }],
  })
    .sort({ publishedAt: -1, createdAt: -1 })
    .lean()) as any;
  return fallback || null;
}

function getVoucherBalanceFromUser(user: any, voucherId: string): number {
  if (!user) return 0;
  const balanceMap = user.voucherBalances;
  if (balanceMap instanceof Map) {
    return Number(balanceMap.get(String(voucherId)) || 0);
  }
  if (balanceMap && typeof balanceMap === "object") {
    return Number(balanceMap[String(voucherId)] || 0);
  }
  return 0;
}

async function reserveUserVouchers(params: {
  userId: string;
  voucherId: string;
  qty: number;
}) {
  const key = `voucherBalances.${params.voucherId}`;
  const updated = await User.findOneAndUpdate(
    { _id: params.userId, [key]: { $gte: params.qty } },
    { $inc: { [key]: -params.qty } },
    { new: true },
  );
  return updated;
}

async function releaseUserVouchers(params: {
  userId: string;
  voucherId: string;
  qty: number;
}) {
  const key = `voucherBalances.${params.voucherId}`;
  await User.findByIdAndUpdate(params.userId, {
    $inc: { [key]: params.qty },
  });
}

async function releaseReservedVouchersForOrder(
  order: any,
  reason: string,
): Promise<void> {
  if (!order?.voucherQtyApplied || !order?.voucherId) return;
  if (order.voucherStatus !== "reserved") return;

  await releaseUserVouchers({
    userId: order.userId.toString(),
    voucherId: order.voucherId.toString(),
    qty: Number(order.voucherQtyApplied || 0),
  });

  await VoucherRedemption.findOneAndUpdate(
    {
      $or: [
        { sourceType: "promotion", sourceId: order._id, status: "reserved" },
        { orderId: order._id, status: "reserved" },
      ],
    },
    {
      $set: {
        status: "released",
        releasedAt: new Date(),
        releaseReason: reason,
      },
    },
  );

  order.voucherStatus = "released";
  order.voucherReleasedAt = new Date();
  await order.save();
}

async function expireStaleOrdersForUser(userId: string): Promise<void> {
  const cutoff = new Date(
    Date.now() - PAYMENT_PENDING_TIMEOUT_MINUTES * 60 * 1000,
  );

  const staleOrders = await PromotionOrder.find({
    userId,
    status: { $in: ["payment_pending", "created"] },
    createdAt: { $lt: cutoff },
  });

  if (staleOrders.length === 0) return;

  for (const order of staleOrders) {
    await releaseReservedVouchersForOrder(order, "payment_timeout");
    order.status = "payment_failed";
    if (order.metadata && typeof order.metadata.set === "function") {
      order.metadata.set("paymentTimeoutAt", new Date().toISOString());
    }
    await order.save();
  }
}

async function expireStaleOrdersGlobally(): Promise<void> {
  const cutoff = new Date(
    Date.now() - PAYMENT_PENDING_TIMEOUT_MINUTES * 60 * 1000,
  );

  const staleOrders = await PromotionOrder.find({
    status: { $in: ["payment_pending", "created"] },
    createdAt: { $lt: cutoff },
  });

  if (staleOrders.length === 0) return;

  for (const order of staleOrders) {
    await releaseReservedVouchersForOrder(order, "payment_timeout");
    order.status = "payment_failed";
    if (order.metadata && typeof order.metadata.set === "function") {
      order.metadata.set("paymentTimeoutAt", new Date().toISOString());
    }
    await order.save();
  }
}

async function activateOrderAndPromotion(params: {
  order: any;
  promotion: any;
  paymentId?: string;
  paymentOrderId?: string;
  metadata?: Record<string, unknown>;
}) {
  const now = new Date();
  const expiryDate = new Date(
    now.getTime() + params.order.durationDays * 24 * 60 * 60 * 1000,
  );

  params.order.status = "activated";
  params.order.paymentId = params.paymentId || params.order.paymentId;
  params.order.paymentOrderId = params.paymentOrderId || params.order.paymentOrderId;
  params.order.paidAt = now;
  params.order.activatedAt = now;
  params.order.expiresAt = expiryDate;
  if (params.metadata) {
    Object.entries(params.metadata).forEach(([key, value]) => {
      params.order.metadata.set(key, value);
    });
  }
  await params.order.save();

  params.promotion.listingType = "promoted";
  params.promotion.plan = {
    name: `${params.order.areaType}_rank_${params.order.rank}`,
    price: params.order.amount,
    durationDays: params.order.durationDays,
    activatedAt: now,
  };
  params.promotion.paymentStatus = "paid";
  params.promotion.paymentId = params.order.paymentId || params.order._id.toString();
  params.promotion.status = "active";
  params.promotion.isActive = true;
  params.promotion.expiryDate = expiryDate;
  params.promotion.visibility.priorityScore = params.order.priorityScore;
  await params.promotion.save();

  return { now, expiryDate };
}

function startPromotionStaleSweep(): void {
  if (process.env.NODE_ENV === "test") return;

  const globalKey = "__promotionStaleSweepStarted";
  const globalRef = global as typeof globalThis & Record<string, unknown>;
  if (globalRef[globalKey]) return;
  globalRef[globalKey] = true;

  const sweep = async () => {
    try {
      await expireStaleOrdersGlobally();
    } catch (error) {
      console.error("❌ [PROMOTION-PRICING] Stale sweep error:", error);
    }
  };

  setTimeout(sweep, 30 * 1000);
  setInterval(sweep, PROMO_STALE_SWEEP_MINUTES * 60 * 1000);
}

startPromotionStaleSweep();

// Admin-only: seed default pricing catalog
router.post("/seed-default", requireAuth, async (req: AuthReq, res) => {
  try {
    const adminKey = getAdminKey(req);
    if (
      adminKey !== process.env.ADMIN_SECRET_KEY &&
      adminKey !== "your-secure-admin-key-here"
    ) {
      return res.status(401).json({ success: false, message: "Unauthorized" });
    }

    const operations = DEFAULT_PRICING_ROWS.flatMap((row) =>
      AREA_TYPES.map((areaType) => {
        const amount = row[areaType];
        const code = `BL_${areaType.toUpperCase()}_R${row.rank}_30D`;
        return {
          updateOne: {
            filter: { code },
            update: {
              $set: {
                code,
                areaType,
                rank: row.rank,
                rankLabel: getRankLabel(row.rank),
                amount,
                currency: "INR",
                durationDays: 30,
                priorityScore: priorityFromRank(row.rank),
                isActive: true,
              },
            },
            upsert: true,
          },
        };
      }),
    );

    if (operations.length > 0) {
      await PromotionPricingPlan.bulkWrite(operations);
    }

    return res.json({
      success: true,
      message: "Default promotion pricing catalog seeded",
      plansUpserted: operations.length,
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Seed error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to seed promotion pricing catalog",
    });
  }
});

// Public catalog: used by frontend pricing screen
router.get("/catalog", async (_req, res) => {
  try {
    const plans = await PromotionPricingPlan.find({ isActive: true })
      .select("-__v")
      .sort({ rank: -1, areaType: 1 })
      .lean();

    return res.json({
      success: true,
      plans,
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Catalog error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to fetch promotion pricing catalog",
    });
  }
});

// Public quote endpoint (server-side price authority)
router.get("/quote", async (req, res) => {
  try {
    const { areaType, rank = "21", durationDays = "30" } = req.query;
    const area = String(areaType || "");
    const parsedRank = Number(rank);
    const parsedDuration = Number(durationDays);

    if (!AREA_TYPES.includes(area as AreaType)) {
      return res.status(400).json({
        success: false,
        message:
          "Invalid areaType. Use pincode, tehsil, district, division, state, zone, or india",
      });
    }

    if (!Number.isInteger(parsedRank) || parsedRank < 1 || parsedRank > 21) {
      return res.status(400).json({
        success: false,
        message: "Invalid rank. Allowed range is 1 to 21",
      });
    }

    if (!Number.isInteger(parsedDuration) || parsedDuration <= 0) {
      return res.status(400).json({
        success: false,
        message: "Invalid durationDays",
      });
    }

    const plan = await PromotionPricingPlan.findOne({
      areaType: area,
      rank: parsedRank,
      durationDays: parsedDuration,
      isActive: true,
    }).lean();

    if (!plan) {
      return res.status(404).json({
        success: false,
        message: "No pricing plan found for selected options",
      });
    }

    return res.json({ success: true, quote: plan });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Quote error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to generate quote",
    });
  }
});

// Get Instantlly voucher balance for promotion redeem
router.get("/voucher-balance", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    await expireStaleOrdersForUser(userId);
    const [user, voucher] = await Promise.all([
      User.findById(userId).select("voucherBalances").lean(),
      getInstantllyVoucherTemplate(),
    ]);

    if (!voucher) {
      return res.json({
        success: true,
        voucher: null,
        balance: 0,
        totalValue: 0,
      });
    }

    if (voucher.companyName !== INSTANTLLY_COMPANY_NAME) {
      return res.status(400).json({
        success: false,
        message: "Instantlly voucher template not configured correctly",
      });
    }

    if (voucher.expiryDate && new Date(voucher.expiryDate) < new Date()) {
      return res.json({
        success: true,
        voucher: {
          id: voucher._id,
          companyName: voucher.companyName,
          mrp: voucher.MRP || 0,
          expiresAt: voucher.expiryDate,
        },
        balance: 0,
        totalValue: 0,
        expired: true,
      });
    }

    const balance = getVoucherBalanceFromUser(user, voucher._id.toString());
    const mrp = Number(voucher.MRP || 0);
    const totalValue = Math.max(0, balance * mrp);

    return res.json({
      success: true,
      voucher: {
        id: voucher._id,
        companyName: voucher.companyName,
        mrp,
        expiresAt: voucher.expiryDate,
      },
      balance,
      totalValue,
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Voucher balance error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to fetch voucher balance",
    });
  }
});

// Create order for a business promotion
router.post("/orders", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    const {
      businessPromotionId,
      areaType,
      rank,
      durationDays = 30,
      paymentProvider = "razorpay",
      deferPaymentOrder = false,
    } = req.body as {
      businessPromotionId?: string;
      areaType?: AreaType;
      rank?: number;
      durationDays?: number;
      paymentProvider?: string;
      deferPaymentOrder?: boolean;
    };

    if (!businessPromotionId || !mongoose.Types.ObjectId.isValid(businessPromotionId)) {
      return res.status(400).json({
        success: false,
        message: "Valid businessPromotionId is required",
      });
    }

    if (!areaType || !AREA_TYPES.includes(areaType)) {
      return res.status(400).json({
        success: false,
        message:
          "Invalid areaType. Use pincode, tehsil, district, division, state, zone, or india",
      });
    }

    if (!Number.isInteger(rank) || (rank as number) < 1 || (rank as number) > 21) {
      return res.status(400).json({
        success: false,
        message: "Invalid rank. Allowed range is 1 to 21",
      });
    }

    if (!Number.isInteger(durationDays) || (durationDays as number) <= 0) {
      return res.status(400).json({
        success: false,
        message: "Invalid durationDays",
      });
    }

    const promotion = await BusinessPromotion.findOne({
      _id: businessPromotionId,
      userId,
    });

    if (!promotion) {
      return res.status(404).json({
        success: false,
        message: "Business promotion not found or unauthorized",
      });
    }

    if (promotion.status === "draft") {
      return res.status(400).json({
        success: false,
        message: "Complete business profile before creating promotion order",
      });
    }

    if (promotion.listingIntent !== "promoted") {
      return res.status(400).json({
        success: false,
        message:
          "Listing is not marked as premium. Please select premium listing type first.",
      });
    }

    const existingPending = await PromotionOrder.findOne({
      userId,
      businessPromotionId,
      status: { $in: ["payment_pending", "created"] },
    }).sort({ createdAt: -1 });

    if (existingPending) {
      const sameSelection =
        existingPending.areaType === areaType &&
        Number(existingPending.rank) === Number(rank) &&
        Number(existingPending.durationDays) === Number(durationDays);

      if (sameSelection) {
        return res.status(200).json({
          success: true,
          order: existingPending,
          message: "Pending order already exists for this selection.",
        });
      }

      return res.status(409).json({
        success: false,
        message:
          "You already have a pending order for this promotion. Please resume or cancel it before creating a new one.",
        order: existingPending,
      });
    }

    const pricingPlan = await PromotionPricingPlan.findOne({
      areaType,
      rank,
      durationDays,
      isActive: true,
    });

    if (!pricingPlan) {
      return res.status(404).json({
        success: false,
        message: "Pricing plan not found for selected options",
      });
    }

    const order = await PromotionOrder.create({
      userId,
      businessPromotionId,
      pricingPlanId: pricingPlan._id,
      areaType,
      rank,
      rankLabel: pricingPlan.rankLabel,
      amount: pricingPlan.amount,
      payableAmount: pricingPlan.amount,
      currency: pricingPlan.currency,
      durationDays: pricingPlan.durationDays,
      priorityScore: pricingPlan.priorityScore,
      status: "payment_pending",
      paymentProvider,
    });

    let checkout: any = null;
    if (paymentProvider === "razorpay" && !deferPaymentOrder) {
      const rzOrder = await createRazorpayOrder({
        amountRupees: pricingPlan.amount,
        currency: pricingPlan.currency,
        receipt: `promo_${String(order._id).slice(-12)}`,
        notes: {
          orderId: String(order._id),
          businessPromotionId: String(businessPromotionId),
          userId: String(userId),
        },
      });

      order.paymentOrderId = rzOrder.id;
      await order.save();

      const { keyId } = getRazorpayCredentials();
      checkout = {
        keyId,
        amount: rzOrder.amount,
        currency: rzOrder.currency,
        razorpayOrderId: rzOrder.id,
      };
    }

    return res.status(201).json({
      success: true,
      order,
      checkout,
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Create order error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to create promotion order",
    });
  }
});

// Get latest pending order for a promotion/selection
router.get("/orders/pending", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    const {
      businessPromotionId,
      areaType,
      rank,
      durationDays,
    } = req.query as {
      businessPromotionId?: string;
      areaType?: AreaType;
      rank?: string;
      durationDays?: string;
    };

    if (!businessPromotionId || !mongoose.Types.ObjectId.isValid(businessPromotionId)) {
      return res.status(400).json({
        success: false,
        message: "Valid businessPromotionId is required",
      });
    }

    if (!areaType || !AREA_TYPES.includes(areaType)) {
      return res.status(400).json({
        success: false,
        message:
          "Invalid areaType. Use pincode, tehsil, district, division, state, zone, or india",
      });
    }

    const parsedRank = Number(rank);
    const parsedDuration = Number(durationDays);
    if (!Number.isInteger(parsedRank) || parsedRank < 1 || parsedRank > 21) {
      return res.status(400).json({ success: false, message: "Invalid rank" });
    }
    if (!Number.isInteger(parsedDuration) || parsedDuration <= 0) {
      return res.status(400).json({ success: false, message: "Invalid durationDays" });
    }

    await expireStaleOrdersForUser(userId);

    const order = await PromotionOrder.findOne({
      userId,
      businessPromotionId,
      areaType,
      rank: parsedRank,
      durationDays: parsedDuration,
      status: { $in: ["payment_pending", "created"] },
    })
      .sort({ createdAt: -1 })
      .lean();

    return res.json({ success: true, order: order || null });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Pending order lookup error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to fetch pending order",
    });
  }
});

// Get latest pending order for a promotion (any selection)
router.get("/orders/pending-by-promotion", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    const { businessPromotionId } = req.query as { businessPromotionId?: string };

    if (!businessPromotionId || !mongoose.Types.ObjectId.isValid(businessPromotionId)) {
      return res.status(400).json({
        success: false,
        message: "Valid businessPromotionId is required",
      });
    }

    await expireStaleOrdersForUser(userId);

    const order = await PromotionOrder.findOne({
      userId,
      businessPromotionId,
      status: { $in: ["payment_pending", "created"] },
    })
      .sort({ createdAt: -1 })
      .lean();

    return res.json({ success: true, order: order || null });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Pending by promotion error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to fetch pending order",
    });
  }
});

// Cancel a pending order and release vouchers
router.post("/orders/:orderId/cancel", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    const { orderId } = req.params;

    if (!mongoose.Types.ObjectId.isValid(orderId)) {
      return res.status(400).json({ success: false, message: "Invalid orderId" });
    }

    const order = await PromotionOrder.findOne({ _id: orderId, userId });
    if (!order) {
      return res.status(404).json({ success: false, message: "Promotion order not found" });
    }

    if (!["payment_pending", "created"].includes(order.status)) {
      return res.status(400).json({
        success: false,
        message: `Order is in ${order.status} state`,
      });
    }

    await releaseReservedVouchersForOrder(order, "user_cancelled");
    order.status = "cancelled";
    if (order.metadata && typeof order.metadata.set === "function") {
      order.metadata.set("cancelledAt", new Date().toISOString());
    }
    await order.save();

    return res.json({ success: true, message: "Order cancelled", order });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Cancel order error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to cancel order",
    });
  }
});

// Apply Instantlly vouchers to a promotion order (reserve on apply)
router.post("/orders/:orderId/apply-vouchers", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    const { orderId } = req.params;
    const { qty } = req.body as { qty?: number };

    if (!mongoose.Types.ObjectId.isValid(orderId)) {
      return res.status(400).json({ success: false, message: "Invalid orderId" });
    }

    const order = await PromotionOrder.findOne({ _id: orderId, userId });
    if (!order) {
      return res.status(404).json({ success: false, message: "Promotion order not found" });
    }

    if (!["payment_pending", "created"].includes(order.status)) {
      return res.status(400).json({
        success: false,
        message: `Order is in ${order.status} state`,
      });
    }

    if (order.voucherStatus === "reserved") {
      return res.json({
        success: true,
        message: "Vouchers already applied",
        order,
      });
    }

    const voucher = await getInstantllyVoucherTemplate();
    if (!voucher || voucher.companyName !== INSTANTLLY_COMPANY_NAME) {
      return res.status(400).json({
        success: false,
        message: "Instantlly voucher not available",
      });
    }

    if (voucher.expiryDate && new Date(voucher.expiryDate) < new Date()) {
      return res.status(400).json({
        success: false,
        message: "Instantlly voucher has expired",
      });
    }

    const user = await User.findById(userId).select("voucherBalances").lean();
    const balance = getVoucherBalanceFromUser(user, voucher._id.toString());
    const mrp = Number(voucher.MRP || 0);
    const maxByAmount = mrp > 0 ? Math.floor(order.amount / mrp) : 0;
    const maxQty = Math.max(0, Math.min(balance, maxByAmount));

    const requestedQty = qty !== undefined ? Math.max(0, Math.floor(qty)) : maxQty;
    const qtyToUse = Math.min(requestedQty, maxQty);

    if (qtyToUse <= 0) {
      return res.status(400).json({
        success: false,
        message: "No vouchers available to apply",
        balance,
        maxRedeemable: maxQty,
      });
    }

    const reserved = await reserveUserVouchers({
      userId,
      voucherId: voucher._id.toString(),
      qty: qtyToUse,
    });

    if (!reserved) {
      return res.status(400).json({
        success: false,
        message: "Insufficient voucher balance",
      });
    }

    const voucherAmount = qtyToUse * mrp;
    const payableAmount = Math.max(0, order.amount - voucherAmount);

    await VoucherRedemption.create({
      userId,
      sourceType: "promotion",
      sourceId: order._id,
      voucherId: voucher._id,
      companyName: voucher.companyName,
      qty: qtyToUse,
      valuePerUnit: mrp,
      amount: voucherAmount,
      currency: order.currency || "INR",
      status: "reserved",
    });

    order.voucherId = voucher._id as any;
    order.voucherQtyApplied = qtyToUse;
    order.voucherValuePerUnit = mrp;
    order.voucherAmountApplied = voucherAmount;
    order.payableAmount = payableAmount;
    order.voucherStatus = "reserved";
    order.voucherAppliedAt = new Date();
    await order.save();

    return res.json({
      success: true,
      message: "Vouchers applied",
      order,
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Apply vouchers error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to apply vouchers",
    });
  }
});

// Create a Razorpay payment order for the payable amount
router.post("/orders/:orderId/create-payment-order", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    const { orderId } = req.params;

    if (!mongoose.Types.ObjectId.isValid(orderId)) {
      return res.status(400).json({ success: false, message: "Invalid orderId" });
    }

    const order = await PromotionOrder.findOne({ _id: orderId, userId });
    if (!order) {
      return res.status(404).json({ success: false, message: "Promotion order not found" });
    }

    if (!["payment_pending", "created"].includes(order.status)) {
      return res.status(400).json({
        success: false,
        message: `Order is in ${order.status} state`,
      });
    }

    const payableAmount = Math.max(
      0,
      Number(
        order.payableAmount !== undefined && order.payableAmount !== null
          ? order.payableAmount
          : order.amount || 0,
      ),
    );
    if (payableAmount <= 0) {
      return res.status(400).json({
        success: false,
        message: "No payable amount remaining for this order",
      });
    }

    const rzOrder = await createRazorpayOrder({
      amountRupees: payableAmount,
      currency: order.currency || "INR",
      receipt: `promo_${String(order._id).slice(-12)}`,
      notes: {
        orderId: String(order._id),
        businessPromotionId: String(order.businessPromotionId),
        userId: String(userId),
      },
    });

    order.paymentProvider = "razorpay";
    order.paymentOrderId = rzOrder.id;
    await order.save();

    const { keyId } = getRazorpayCredentials();
    return res.json({
      success: true,
      checkout: {
        keyId,
        amount: rzOrder.amount,
        currency: rzOrder.currency,
        razorpayOrderId: rzOrder.id,
      },
      order,
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Create payment order error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to create payment order",
    });
  }
});

// Confirm promotion order using vouchers only (payable amount = 0)
router.post("/orders/:orderId/confirm-voucher", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    const { orderId } = req.params;

    if (!mongoose.Types.ObjectId.isValid(orderId)) {
      return res.status(400).json({ success: false, message: "Invalid orderId" });
    }

    const order = await PromotionOrder.findOne({ _id: orderId, userId });
    if (!order) {
      return res.status(404).json({ success: false, message: "Promotion order not found" });
    }

    if (!["payment_pending", "created"].includes(order.status)) {
      return res.status(400).json({
        success: false,
        message: `Order is in ${order.status} state`,
      });
    }

    const payableAmount = Math.max(
      0,
      Number(
        order.payableAmount !== undefined && order.payableAmount !== null
          ? order.payableAmount
          : order.amount || 0,
      ),
    );
    if (payableAmount > 0) {
      return res.status(400).json({
        success: false,
        message: "Order still has payable amount",
      });
    }

    if (order.voucherStatus !== "reserved") {
      return res.status(400).json({
        success: false,
        message: "No reserved vouchers for this order",
      });
    }

    const promotion = await BusinessPromotion.findOne({
      _id: order.businessPromotionId,
      userId,
    });

    if (!promotion) {
      return res.status(404).json({
        success: false,
        message: "Business promotion not found",
      });
    }

      await VoucherRedemption.findOneAndUpdate(
        { sourceType: "promotion", sourceId: order._id, status: "reserved" },
        { $set: { status: "applied", appliedAt: new Date() } },
      );

    order.voucherStatus = "applied";
    await activateOrderAndPromotion({
      order,
      promotion,
      paymentId: `voucher_redeem_${order._id.toString()}`,
      metadata: { gateway: "voucher" },
    });

    return res.json({
      success: true,
      message: "Voucher redemption confirmed and listing activated",
      order,
      promotion: {
        id: promotion._id,
        listingType: promotion.listingType,
        status: promotion.status,
        priorityScore: promotion.visibility.priorityScore,
        expiryDate: promotion.expiryDate,
      },
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Confirm voucher error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to confirm voucher redemption",
    });
  }
});

// Verify Razorpay payment signature and activate listing
router.post("/orders/:orderId/verify-payment", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    await expireStaleOrdersForUser(userId);
    const { orderId } = req.params;
    const { razorpay_order_id, razorpay_payment_id, razorpay_signature } = req.body as {
      razorpay_order_id?: string;
      razorpay_payment_id?: string;
      razorpay_signature?: string;
    };

    if (!mongoose.Types.ObjectId.isValid(orderId)) {
      return res.status(400).json({ success: false, message: "Invalid orderId" });
    }

    if (!razorpay_order_id || !razorpay_payment_id || !razorpay_signature) {
      return res.status(400).json({
        success: false,
        message: "razorpay_order_id, razorpay_payment_id and razorpay_signature are required",
      });
    }

    const order = await PromotionOrder.findOne({ _id: orderId, userId });
    if (!order) {
      return res.status(404).json({ success: false, message: "Promotion order not found" });
    }

    if (order.paymentProvider !== "razorpay") {
      return res.status(400).json({
        success: false,
        message: "Order is not created with Razorpay",
      });
    }

    if (!["payment_pending", "created", "paid"].includes(order.status)) {
      return res.status(400).json({
        success: false,
        message: `Order is in ${order.status} state`,
      });
    }

    if (order.paymentOrderId && order.paymentOrderId !== razorpay_order_id) {
      return res.status(400).json({
        success: false,
        message: "Razorpay order ID mismatch",
      });
    }

    const isValidSignature = verifyRazorpaySignature({
      razorpayOrderId: razorpay_order_id,
      razorpayPaymentId: razorpay_payment_id,
      razorpaySignature: razorpay_signature,
    });

    if (!isValidSignature) {
      order.status = "payment_failed";
      await order.save();
      await releaseReservedVouchersForOrder(order, "payment_failed");
      return res.status(400).json({
        success: false,
        message: "Invalid payment signature",
      });
    }

    const promotion = await BusinessPromotion.findOne({
      _id: order.businessPromotionId,
      userId,
    });

    if (!promotion) {
      return res.status(404).json({
        success: false,
        message: "Business promotion not found",
      });
    }

    await activateOrderAndPromotion({
      order,
      promotion,
      paymentId: razorpay_payment_id,
      paymentOrderId: razorpay_order_id,
      metadata: {
        gateway: "razorpay",
        signatureVerified: true,
      },
    });

    if (order.voucherStatus === "reserved") {
    await VoucherRedemption.findOneAndUpdate(
      { sourceType: "promotion", sourceId: order._id, status: "reserved" },
      { $set: { status: "applied", appliedAt: new Date() } },
    );
      order.voucherStatus = "applied";
      await order.save();
    }

    return res.json({
      success: true,
      message: "Payment verified and listing activated",
      order,
      promotion: {
        id: promotion._id,
        listingType: promotion.listingType,
        status: promotion.status,
        priorityScore: promotion.visibility.priorityScore,
        expiryDate: promotion.expiryDate,
      },
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Verify payment error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to verify payment",
    });
  }
});

// User: get own orders
router.get("/orders/me", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    await expireStaleOrdersForUser(userId);

    const orders = await PromotionOrder.find({ userId })
      .populate("businessPromotionId", "businessName city category")
      .sort({ createdAt: -1 })
      .lean();

    return res.json({ success: true, orders });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] List orders error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to fetch promotion orders",
    });
  }
});

// Confirm payment and activate listing using server-side order snapshot
router.post("/orders/:orderId/confirm-payment", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
    await expireStaleOrdersForUser(userId);
    const { orderId } = req.params;
    const { paymentId, paymentOrderId, metadata } = req.body as {
      paymentId?: string;
      paymentOrderId?: string;
      metadata?: Record<string, unknown>;
    };

    if (!mongoose.Types.ObjectId.isValid(orderId)) {
      return res.status(400).json({
        success: false,
        message: "Invalid orderId",
      });
    }

    const order = await PromotionOrder.findOne({ _id: orderId, userId });
    if (!order) {
      return res.status(404).json({
        success: false,
        message: "Promotion order not found",
      });
    }

    if (!["payment_pending", "created"].includes(order.status)) {
      return res.status(400).json({
        success: false,
        message: `Order is in ${order.status} state`,
      });
    }

    if (order.paymentProvider === "razorpay") {
      return res.status(400).json({
        success: false,
        message:
          "Use /orders/:orderId/verify-payment for Razorpay orders",
      });
    }

    const promotion = await BusinessPromotion.findOne({
      _id: order.businessPromotionId,
      userId,
    });

    if (!promotion) {
      return res.status(404).json({
        success: false,
        message: "Business promotion not found",
      });
    }

    if (promotion.status === "draft") {
      return res.status(400).json({
        success: false,
        message: "Complete business profile before activation",
      });
    }

    await activateOrderAndPromotion({
      order,
      promotion,
      paymentId,
      paymentOrderId,
      metadata,
    });

    if (order.voucherStatus === "reserved") {
    await VoucherRedemption.findOneAndUpdate(
      { sourceType: "promotion", sourceId: order._id, status: "reserved" },
      { $set: { status: "applied", appliedAt: new Date() } },
    );
      order.voucherStatus = "applied";
      await order.save();
    }

    return res.json({
      success: true,
      message: "Promotion payment confirmed and listing activated",
      order,
      promotion: {
        id: promotion._id,
        listingType: promotion.listingType,
        status: promotion.status,
        priorityScore: promotion.visibility.priorityScore,
        expiryDate: promotion.expiryDate,
      },
    });
  } catch (error) {
    console.error("❌ [PROMOTION-PRICING] Confirm payment error:", error);
    return res.status(500).json({
      success: false,
      message: "Failed to confirm payment",
    });
  }
});

export default router;
