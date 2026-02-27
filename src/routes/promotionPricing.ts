import { Router } from "express";
import mongoose from "mongoose";
import crypto from "crypto";
import axios from "axios";
import { requireAuth, AuthReq } from "../middleware/auth";
import BusinessPromotion from "../models/BusinessPromotion";
import PromotionOrder from "../models/PromotionOrder";
import PromotionPricingPlan from "../models/PromotionPricingPlan";

const router = Router();

type AreaType = "pincode" | "tehsil" | "district";

type PricingRow = {
  rank: number;
  pincode: number;
  tehsil: number;
  district: number;
};

const DEFAULT_PRICING_ROWS: PricingRow[] = [
  { rank: 21, pincode: 100, tehsil: 600, district: 3600 }, // No Rank
  { rank: 20, pincode: 200, tehsil: 1200, district: 7200 },
  { rank: 19, pincode: 300, tehsil: 1800, district: 10800 },
  { rank: 18, pincode: 400, tehsil: 2400, district: 14400 },
  { rank: 17, pincode: 500, tehsil: 3000, district: 18000 },
  { rank: 16, pincode: 600, tehsil: 3600, district: 21600 },
  { rank: 15, pincode: 700, tehsil: 4200, district: 25200 },
  { rank: 14, pincode: 800, tehsil: 4800, district: 28800 },
  { rank: 13, pincode: 900, tehsil: 5400, district: 32400 },
  { rank: 12, pincode: 1000, tehsil: 6000, district: 36000 },
  { rank: 11, pincode: 1100, tehsil: 6600, district: 39600 },
  { rank: 10, pincode: 1200, tehsil: 7200, district: 43200 },
  { rank: 9, pincode: 1300, tehsil: 7800, district: 46800 },
  { rank: 8, pincode: 1400, tehsil: 8400, district: 50400 },
  { rank: 7, pincode: 1500, tehsil: 9000, district: 54000 },
  { rank: 6, pincode: 1600, tehsil: 9600, district: 57600 },
  { rank: 5, pincode: 1700, tehsil: 10200, district: 61200 },
  { rank: 4, pincode: 1800, tehsil: 10800, district: 64800 },
  { rank: 3, pincode: 1900, tehsil: 11400, district: 68400 },
  { rank: 2, pincode: 2000, tehsil: 12000, district: 72000 },
  { rank: 1, pincode: 2100, tehsil: 12600, district: 75600 },
];

const AREA_TYPES: AreaType[] = ["pincode", "tehsil", "district"];

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
        message: "Invalid areaType. Use pincode, tehsil, or district",
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
    } = req.body as {
      businessPromotionId?: string;
      areaType?: AreaType;
      rank?: number;
      durationDays?: number;
      paymentProvider?: string;
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
        message: "Invalid areaType. Use pincode, tehsil, or district",
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
      currency: pricingPlan.currency,
      durationDays: pricingPlan.durationDays,
      priorityScore: pricingPlan.priorityScore,
      status: "payment_pending",
      paymentProvider,
    });

    let checkout: any = null;
    if (paymentProvider === "razorpay") {
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

// Verify Razorpay payment signature and activate listing
router.post("/orders/:orderId/verify-payment", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId as string;
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
