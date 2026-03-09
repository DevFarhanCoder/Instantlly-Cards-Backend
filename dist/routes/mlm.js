"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.startAutoRefundScheduler = startAutoRefundScheduler;
const express_1 = require("express");
const mongoose_1 = require("mongoose");
const auth_1 = require("../middleware/auth");
const User_1 = __importDefault(require("../models/User"));
const MlmCredit_1 = __importDefault(require("../models/MlmCredit"));
const MlmTransfer_1 = __importDefault(require("../models/MlmTransfer"));
const Voucher_1 = __importDefault(require("../models/Voucher"));
const SpecialCredit_1 = __importDefault(require("../models/SpecialCredit"));
const VoucherTransferLog_1 = __importDefault(require("../models/VoucherTransferLog"));
const mlm_1 = require("../utils/mlm");
const walletService_1 = require("../services/mlm/walletService");
const creditLifecycleService_1 = require("../services/mlm/creditLifecycleService");
const voucherService_1 = require("../services/mlm/voucherService");
const treeService_1 = require("../services/mlm/treeService");
const creditDashboardService_1 = require("../services/mlm/creditDashboardService");
const directBuyersService_1 = require("../services/mlm/directBuyersService");
const discountService_1 = require("../services/mlm/discountService");
const networkService_1 = require("../services/mlm/networkService");
const slotService_1 = require("../services/mlm/slotService");
const router = (0, express_1.Router)();
const CREDIT_EXPIRY_MINUTES = 60;
const TRANSFER_EXPIRY_HOURS = 48;
const VOUCHER_PURCHASE_TIMEOUT_MINUTES = Math.max(1, Number(process.env.SPECIAL_CREDIT_UNLOCK_TIMEOUT_MINUTES || 5));
const AUTO_REFUND_CHECK_INTERVAL_MS = Math.max(15 * 1000, Math.min(60 * 1000, Math.floor((VOUCHER_PURCHASE_TIMEOUT_MINUTES * 60 * 1000) / 10)));
const CONNECTION_TIMEOUT_HOURS = 48; // 2 days to connect 5 people
const MIN_VOUCHERS_TO_UNLOCK = 5; // Must share 5 vouchers to unlock credit transfer
const DEFAULT_REQUIRED_VOUCHERS_PER_TEMPLATE = 5;
const DISTRIBUTION_CREDIT_AMOUNT = 146484360000; // Credits per distribution entry
const MLM_LOG_REDACT_KEYS = new Set([
    "password",
    "token",
    "authorization",
    "otp",
    "accessToken",
    "refreshToken",
]);
function sanitizeMlmLogValue(value, depth = 0) {
    if (depth > 3) {
        return "[max-depth]";
    }
    if (value === null || value === undefined) {
        return value;
    }
    if (Array.isArray(value)) {
        if (value.length > 20) {
            return {
                length: value.length,
                preview: value.slice(0, 20).map((item) => sanitizeMlmLogValue(item, depth + 1)),
            };
        }
        return value.map((item) => sanitizeMlmLogValue(item, depth + 1));
    }
    if (value instanceof Date) {
        return value.toISOString();
    }
    if (typeof value === "object") {
        const entries = Object.entries(value).slice(0, 50);
        const sanitized = {};
        for (const [key, nestedValue] of entries) {
            if (MLM_LOG_REDACT_KEYS.has(key)) {
                sanitized[key] = "[redacted]";
                continue;
            }
            sanitized[key] = sanitizeMlmLogValue(nestedValue, depth + 1);
        }
        return sanitized;
    }
    if (typeof value === "string" && value.length > 300) {
        return `${value.slice(0, 300)}...[truncated ${value.length - 300} chars]`;
    }
    return value;
}
router.use((req, res, next) => {
    const startedAt = Date.now();
    const requestMeta = {
        method: req.method,
        path: req.originalUrl,
        userId: req.userId || null,
        query: sanitizeMlmLogValue(req.query),
        body: sanitizeMlmLogValue(req.body),
        params: sanitizeMlmLogValue(req.params),
    };
    console.log("[MLM API REQUEST]", requestMeta);
    res.on("finish", () => {
        console.log("[MLM API RESPONSE]", {
            method: req.method,
            path: req.originalUrl,
            userId: req.userId || null,
            statusCode: res.statusCode,
            durationMs: Date.now() - startedAt,
        });
    });
    next();
});
function getVoucherTimeoutReason() {
    if (VOUCHER_PURCHASE_TIMEOUT_MINUTES % 60 === 0) {
        return `required_vouchers_not_met_within_${VOUCHER_PURCHASE_TIMEOUT_MINUTES / 60}h`;
    }
    return `required_vouchers_not_met_within_${VOUCHER_PURCHASE_TIMEOUT_MINUTES}m`;
}
function asObjectId(id) {
    if (!id)
        return null;
    try {
        return new mongoose_1.Types.ObjectId(id);
    }
    catch {
        return null;
    }
}
async function resolveVoucherScope(voucherId) {
    const requestedVoucherId = voucherId || null;
    if (!voucherId) {
        const scope = {
            requestedVoucherId,
            detectedType: "none",
            canonicalVoucherId: null,
            matchedVoucherId: null,
        };
        console.log("[MLM VOUCHER SCOPE]", scope);
        return scope;
    }
    if (voucherId === "instantlly-special-credits") {
        const publishedTemplate = await Voucher_1.default.findOne({
            isPublished: true,
            $or: [{ userId: { $exists: false } }, { userId: null }],
        })
            .sort({ publishedAt: -1, createdAt: -1 })
            .select("_id templateId")
            .lean();
        if (!publishedTemplate) {
            const scope = {
                requestedVoucherId,
                detectedType: "legacy",
                canonicalVoucherId: null,
                matchedVoucherId: null,
            };
            console.log("[MLM VOUCHER SCOPE]", scope);
            return scope;
        }
        const canonical = publishedTemplate.templateId ||
            publishedTemplate._id;
        const scope = {
            requestedVoucherId,
            detectedType: "legacy",
            canonicalVoucherId: canonical,
            matchedVoucherId: publishedTemplate._id,
        };
        console.log("[MLM VOUCHER SCOPE]", {
            ...scope,
            canonicalVoucherId: scope.canonicalVoucherId?.toString() || null,
            matchedVoucherId: scope.matchedVoucherId?.toString() || null,
        });
        return scope;
    }
    const objectId = asObjectId(voucherId);
    if (!objectId) {
        const scope = {
            requestedVoucherId,
            detectedType: "invalid",
            canonicalVoucherId: null,
            matchedVoucherId: null,
        };
        console.log("[MLM VOUCHER SCOPE]", scope);
        return scope;
    }
    const voucherDoc = await Voucher_1.default.findById(objectId)
        .select("_id templateId")
        .lean();
    if (!voucherDoc) {
        // Accept campaign/template id directly even if this collection row is absent.
        const scope = {
            requestedVoucherId,
            detectedType: "campaign_or_template_id",
            canonicalVoucherId: objectId,
            matchedVoucherId: null,
        };
        console.log("[MLM VOUCHER SCOPE]", {
            ...scope,
            canonicalVoucherId: scope.canonicalVoucherId?.toString() || null,
        });
        return scope;
    }
    const templateId = voucherDoc.templateId;
    const detectedType = templateId && templateId.toString() !== voucherDoc._id.toString()
        ? "instance"
        : "template";
    const scope = {
        requestedVoucherId,
        detectedType,
        canonicalVoucherId: templateId || voucherDoc._id,
        matchedVoucherId: voucherDoc._id,
    };
    console.log("[MLM VOUCHER SCOPE]", {
        ...scope,
        canonicalVoucherId: scope.canonicalVoucherId?.toString() || null,
        matchedVoucherId: scope.matchedVoucherId?.toString() || null,
        templateId: templateId?.toString() || null,
    });
    return scope;
}
async function getVoucherTemplateRequiredCount(voucherId) {
    const objectId = asObjectId(voucherId);
    if (!objectId)
        return DEFAULT_REQUIRED_VOUCHERS_PER_TEMPLATE;
    const voucher = await Voucher_1.default.findById(objectId)
        .select("minVouchersRequired")
        .lean();
    return Math.max(0, Number(voucher?.minVouchersRequired) ||
        DEFAULT_REQUIRED_VOUCHERS_PER_TEMPLATE);
}
async function getVoucherCountForTemplate(userId, voucherId) {
    if (!voucherId) {
        const [physicalCount, user] = await Promise.all([
            Voucher_1.default.countDocuments({
                userId: new mongoose_1.Types.ObjectId(userId),
                redeemedStatus: { $ne: "redeemed" },
            }),
            User_1.default.findById(userId).select("voucherBalance").lean(),
        ]);
        const total = Math.max(0, physicalCount + Number(user?.voucherBalance || 0));
        console.log("[MLM VOUCHER COUNT]", {
            userId,
            voucherId: null,
            mode: "global",
            physicalCount,
            voucherBalance: Number(user?.voucherBalance || 0),
            total,
        });
        return total;
    }
    const [physicalCount, user] = await Promise.all([
        Voucher_1.default.countDocuments({
            userId: new mongoose_1.Types.ObjectId(userId),
            redeemedStatus: { $ne: "redeemed" },
            $or: [{ templateId: voucherId }, { _id: voucherId }],
        }),
        User_1.default.findById(userId).select("voucherBalances").lean(),
    ]);
    let balanceCount = 0;
    const balanceMap = user?.voucherBalances;
    if (balanceMap instanceof Map) {
        balanceCount = Number(balanceMap.get(String(voucherId)) || 0);
    }
    else if (balanceMap && typeof balanceMap === "object") {
        balanceCount = Number(balanceMap[String(voucherId)] || 0);
    }
    const total = Math.max(0, physicalCount + balanceCount);
    console.log("[MLM VOUCHER COUNT]", {
        userId,
        voucherId: voucherId.toString(),
        mode: "scoped",
        physicalCount,
        balanceCount,
        total,
    });
    return total;
}
function transferTimeLeftSeconds(expiresAt) {
    if (!expiresAt)
        return 0;
    const ms = new Date(expiresAt).getTime() - Date.now();
    return Math.max(0, Math.floor(ms / 1000));
}
function getTemplateBalanceFromUser(user, voucherId) {
    if (!voucherId) {
        return Number(user?.voucherBalance || 0);
    }
    const balanceMap = user?.voucherBalances;
    if (balanceMap instanceof Map) {
        return Number(balanceMap.get(String(voucherId)) || 0);
    }
    if (balanceMap && typeof balanceMap === "object") {
        return Number(balanceMap[String(voucherId)] || 0);
    }
    return 0;
}
async function refundExpiredPendingTransfer(transfer, currentVoucherCount, now) {
    const timedOutTransfer = await MlmTransfer_1.default.findOneAndUpdate({ _id: transfer._id, status: "pending_unlock" }, {
        status: "returned_timeout",
        currentVoucherCount,
        returnedAt: now,
        returnReason: getVoucherTimeoutReason(),
    }, { new: true });
    if (!timedOutTransfer)
        return;
    const sourceSlotId = transfer.sourceSlotId;
    const recipientId = transfer.receiverId?.toString();
    const senderId = transfer.senderId?.toString();
    const transferAmount = Number(transfer.amount || 0);
    const recipientSlotCount = Math.max(1, Number(transfer.slotCount || 0) || 5);
    if (sourceSlotId) {
        await SpecialCredit_1.default.findByIdAndUpdate(sourceSlotId, {
            status: "available",
            recipientId: null,
            recipientName: null,
            recipientPhone: null,
            sentAt: null,
            expiresAt: null,
        });
    }
    if (recipientId) {
        const recipient = await User_1.default.findById(recipientId)
            .select("specialCredits")
            .lean();
        if (recipient?.specialCredits) {
            await User_1.default.findByIdAndUpdate(recipientId, {
                "specialCredits.balance": Math.max(0, Number(recipient.specialCredits.balance || 0) - transferAmount),
                "specialCredits.totalReceived": Math.max(0, Number(recipient.specialCredits.totalReceived || 0) -
                    transferAmount),
                "specialCredits.availableSlots": Math.max(0, Number(recipient.specialCredits.availableSlots || 0) -
                    recipientSlotCount),
            });
        }
        if (transferAmount > 0) {
            try {
                await (0, walletService_1.subtractCredits)(recipientId, transferAmount);
            }
            catch (_) {
                // Wallet rollback is best-effort.
            }
        }
        if (sourceSlotId) {
            await SpecialCredit_1.default.deleteMany({
                ownerId: recipientId,
                sourceSlotId,
            });
        }
        await SpecialCredit_1.default.updateMany({ ownerId: recipientId, transferId: transfer._id }, {
            $set: {
                isLocked: false,
                lockReason: "returned_timeout",
            },
        });
    }
    if (senderId && transferAmount > 0) {
        const sender = await User_1.default.findById(senderId).select("specialCredits").lean();
        if (sender?.specialCredits) {
            await User_1.default.findByIdAndUpdate(senderId, {
                "specialCredits.totalSent": Math.max(0, Number(sender.specialCredits.totalSent || 0) - transferAmount),
                "specialCredits.usedSlots": Math.max(0, Number(sender.specialCredits.usedSlots || 0) - 1),
            });
        }
    }
}
async function reconcileTransferUnlocksForUser(userId, voucherId) {
    const now = new Date();
    const query = {
        receiverId: new mongoose_1.Types.ObjectId(userId),
        status: "pending_unlock",
    };
    if (voucherId)
        query.voucherId = voucherId;
    const transfers = await MlmTransfer_1.default.find(query).lean();
    for (const transfer of transfers) {
        const currentVoucherCount = await getVoucherCountForTemplate(userId, transfer.voucherId || voucherId || undefined);
        if (transfer?.expiresAt &&
            new Date(transfer.expiresAt).getTime() <= now.getTime()) {
            await refundExpiredPendingTransfer(transfer, currentVoucherCount, now);
            continue;
        }
        if (currentVoucherCount < (transfer.requiredVoucherCount || 0)) {
            await MlmTransfer_1.default.findByIdAndUpdate(transfer._id, {
                currentVoucherCount,
            });
            continue;
        }
        await MlmTransfer_1.default.findByIdAndUpdate(transfer._id, {
            status: "unlocked",
            currentVoucherCount,
            unlockedSlots: transfer.slotCount || 5,
            unlockedAt: now,
        });
        await SpecialCredit_1.default.updateMany({
            ownerId: transfer.receiverId,
            transferId: transfer._id,
            isLocked: true,
        }, {
            $set: {
                isLocked: false,
                lockReason: null,
                unlockedAt: now,
            },
        });
    }
}
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
            const activeTransfer = await MlmTransfer_1.default.findOne({
                senderId: req.userId,
                receiverId: child._id,
                status: { $in: ["pending_unlock", "unlocked"] },
            })
                .sort({ createdAt: -1 })
                .lean();
            // Calculate time left to connect (2 days from creation)
            const createdAt = new Date(child.createdAt);
            const timeoutAt = new Date(createdAt.getTime() + CONNECTION_TIMEOUT_HOURS * 60 * 60 * 1000);
            const timeLeft = Math.max(0, timeoutAt.getTime() - Date.now());
            const hoursLeft = Math.floor(timeLeft / (1000 * 60 * 60));
            const minutesLeft = Math.floor((timeLeft % (1000 * 60 * 60)) / (1000 * 60));
            return {
                level: 1, // Direct children are level 1
                creditsToTransfer: DISTRIBUTION_CREDIT_AMOUNT, // 146,484,360,000 credits per child
                recipientName: child.name,
                recipientPhone: child.phone,
                recipientId: child._id.toString(),
                vouchersShared,
                isLocked,
                timeLeft: `${hoursLeft}h ${minutesLeft}m`,
                transferId: activeTransfer?._id?.toString() || null,
                slotNumber: 1,
                lockReason: activeTransfer
                    ? activeTransfer.status === "pending_unlock"
                        ? `Need ${activeTransfer.requiredVoucherCount} vouchers for this campaign`
                        : null
                    : null,
                timeLeftSeconds: activeTransfer
                    ? transferTimeLeftSeconds(activeTransfer.expiresAt)
                    : 0,
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
        await reconcileTransferUnlocksForUser(req.userId);
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
        const activeTransfers = await MlmTransfer_1.default.find({
            receiverId: req.userId,
            status: { $in: ["pending_unlock", "unlocked"] },
        })
            .sort({ createdAt: -1 })
            .limit(20)
            .lean();
        res.json({
            success: true,
            creditBalance: wallet.creditBalance,
            ...dashboard,
            activeTransfers: activeTransfers.map((t) => ({
                transferId: t._id.toString(),
                status: t.status,
                requiredVoucherCount: t.requiredVoucherCount || 0,
                currentVoucherCount: t.currentVoucherCount || 0,
                timerStartedAt: t.timerStartedAt,
                expiresAt: t.expiresAt,
                timeLeftSeconds: transferTimeLeftSeconds(t.expiresAt),
                slotCount: t.slotCount || 5,
                slotAmount: t.slotAmount || 0,
                unlockedSlots: t.unlockedSlots || 0,
            })),
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
        // Link receiver to sender if not already linked (V2: transactional via NetworkService)
        try {
            await (0, networkService_1.linkUser)(receiverId, req.userId, req.userId, "credit-transfer");
        }
        catch (linkErr) {
            if (linkErr.message?.includes("already linked to another parent")) {
                return res.status(400).json({
                    success: false,
                    message: "Receiver already linked to another parent",
                });
            }
            // Any other link error (cap exceeded, depth, etc.) is surfaced as 400
            if (linkErr.message?.includes("already linked to this exact parent") ||
                linkErr.message?.includes("already linked to another parent")) {
                // No need to re-throw — just continue below
            }
            else {
                throw linkErr;
            }
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
        // Check if user is voucher admin
        const user = await User_1.default.findById(req.userId).select("isVoucherAdmin level specialCredits voucherBalance voucherBalances");
        const isVoucherAdmin = user?.isVoucherAdmin === true;
        const userVoucherBalance = user?.voucherBalance || 0;
        // If requesting admin vouchers specifically
        if (source === "admin") {
            // Published null-userId (global) templates are already injected into the
            // regular response, so we only return user-specific admin-assigned vouchers
            // here to avoid duplicates on the client.
            const query = {
                source: "admin",
                isPublished: true,
                userId: { $exists: true, $ne: null }, // exclude global templates
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
        // Hide admin-issued copies from the list UI; they are represented through
        // the template entry quantity instead of one row per issued voucher.
        const physicalVouchers = userVouchers.filter((voucher) => voucher.source !== "admin");
        const adminIssuedCopies = userVouchers.filter((voucher) => voucher.source === "admin");
        const adminIssuedCountByTemplate = new Map();
        adminIssuedCopies.forEach((voucher) => {
            const key = String(voucher.templateId || voucher._id);
            adminIssuedCountByTemplate.set(key, (adminIssuedCountByTemplate.get(key) || 0) + 1);
        });
        // Add physical vouchers first, then inject published template cards with quantity.
        const allVouchers = [...physicalVouchers];
        // Load published admin templates from DB (managed via admin panel)
        const publishedTemplates = await Voucher_1.default.find({
            isPublished: true,
            $or: [{ userId: { $exists: false } }, { userId: null }],
        })
            .sort({ publishedAt: -1 })
            .lean();
        const templatesToShow = publishedTemplates.length > 0 ? publishedTemplates : null;
        if (templatesToShow) {
            // Use DB-managed published templates — reversed so first published appears first
            for (const template of [...templatesToShow].reverse()) {
                const sv = {
                    _id: template._id,
                    voucherNumber: template.voucherNumber,
                    companyName: template.companyName,
                    companyLogo: template.companyLogo || null,
                    voucherImage: template.voucherImage || null,
                    phoneNumber: template.phoneNumber,
                    address: template.address,
                    title: template.title ||
                        template.description ||
                        template.companyName,
                    description: template.description,
                    MRP: template.MRP || template.amount,
                    amount: template.amount,
                    discountPercentage: template.discountPercentage,
                    issueDate: template.issueDate || new Date(),
                    expiryDate: template.expiryDate,
                    validity: template.validity,
                    redeemedStatus: "unredeemed",
                    source: "instantlly-special",
                    isSpecialCreditsVoucher: true,
                    minVouchersRequired: template.minVouchersRequired,
                };
                const templateKey = String(template.templateId || template._id);
                const balanceQty = getTemplateBalanceFromUser(user, template._id);
                const issuedQty = adminIssuedCountByTemplate.get(templateKey) || 0;
                const totalQty = Math.max(0, balanceQty + issuedQty);
                sv.quantity = totalQty;
                sv.isBalanceVoucher = totalQty > 0;
                if (isVoucherAdmin && user?.specialCredits?.availableSlots) {
                    sv.vouchersFigure = 122070300;
                    sv.specialCredits = {
                        totalSlots: user.specialCredits.availableSlots,
                        usedSlots: user.specialCredits.usedSlots || 0,
                        creditPerSlot: getSpecialCreditsForLevel(user.level || 0),
                    };
                }
                allVouchers.unshift(sv);
            }
            // No else — if no published templates exist, no special voucher is shown
        }
        res.json({
            success: true,
            vouchers: allVouchers,
            voucherBalance: userVoucherBalance,
        });
    }
    catch (error) {
        console.error("MLM VOUCHERS ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// VOUCHER HISTORY ROUTES (must be before :voucherId)
// ============================================
router.get("/vouchers/history", auth_1.requireAuth, async (req, res) => {
    try {
        const { limit = 50, skip = 0 } = req.query;
        // Get all vouchers where user is involved
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
        const purchased = vouchers.filter((v) => v.originalOwner?._id?.toString() === req.userId &&
            v.source === "purchase");
        const received = vouchers.filter((v) => v.userId?._id?.toString() === req.userId &&
            (v.source === "transfer" || v.source === "admin"));
        // Count sent vouchers by checking transfer history
        let sentCount = 0;
        const allTransfers = new Set();
        vouchers.forEach((v) => {
            if (v.transferHistory && v.transferHistory.length > 0) {
                v.transferHistory.forEach((t) => {
                    if (t.from?.toString() === req.userId ||
                        t.from?._id?.toString() === req.userId) {
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
// Get voucher details (including special Instantlly voucher)
router.get("/vouchers/:voucherId", auth_1.requireAuth, async (req, res) => {
    try {
        const { voucherId } = req.params;
        // Handle special Instantlly voucher (legacy string ID or real DB published template ID)
        const isLegacyId = voucherId === "instantlly-special-credits";
        let publishedTemplate = null;
        if (isLegacyId) {
            // Legacy string ID: find the newest published admin template
            publishedTemplate = await Voucher_1.default.findOne({
                isPublished: true,
                $or: [{ userId: { $exists: false } }, { userId: null }],
            })
                .sort({ publishedAt: -1 })
                .lean();
        }
        else {
            // Try to find a published template by real MongoDB ID
            try {
                publishedTemplate = await Voucher_1.default.findOne({
                    _id: voucherId,
                    isPublished: true,
                    $or: [{ userId: { $exists: false } }, { userId: null }],
                }).lean();
            }
            catch (_) { }
        }
        if (isLegacyId || publishedTemplate) {
            const user = await User_1.default.findById(req.userId).select("name phone level isVoucherAdmin specialCredits");
            const isAdmin = user?.isVoucherAdmin === true;
            const specialVoucher = publishedTemplate
                ? {
                    _id: publishedTemplate._id,
                    voucherNumber: publishedTemplate.voucherNumber,
                    companyName: publishedTemplate.companyName,
                    companyLogo: publishedTemplate.companyLogo || null,
                    voucherImage: publishedTemplate.voucherImage || null,
                    phoneNumber: publishedTemplate.phoneNumber,
                    address: publishedTemplate.address,
                    title: publishedTemplate.title ||
                        publishedTemplate.description ||
                        publishedTemplate.companyName,
                    description: publishedTemplate.description,
                    MRP: publishedTemplate.MRP || publishedTemplate.amount,
                    amount: publishedTemplate.amount,
                    discountPercentage: publishedTemplate.discountPercentage,
                    issueDate: publishedTemplate.issueDate || new Date(),
                    expiryDate: publishedTemplate.expiryDate,
                    validity: publishedTemplate.validity,
                    redeemedStatus: "unredeemed",
                    source: "instantlly-special",
                    isSpecialCreditsVoucher: true,
                    canContinueToDashboard: true,
                    isAdmin,
                }
                : null;
            if (!specialVoucher) {
                return res
                    .status(404)
                    .json({ success: false, message: "Voucher not found" });
            }
            // For admin users, add special credits info
            if (isAdmin && user?.specialCredits?.availableSlots) {
                specialVoucher.vouchersFigure = 122070300; // Show credit amount for admin
                specialVoucher.specialCredits = {
                    totalSlots: user.specialCredits.availableSlots || 0,
                    usedSlots: user.specialCredits.usedSlots || 0,
                    availableSlots: (user.specialCredits.availableSlots || 0) -
                        (user.specialCredits.usedSlots || 0),
                    creditPerSlot: getSpecialCreditsForLevel(user?.level || 0),
                };
            }
            return res.json({ success: true, voucher: specialVoucher });
        }
        // Regular voucher lookup
        const voucher = await Voucher_1.default.findOne({
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
    }
    catch (error) {
        console.error("MLM VOUCHER DETAILS ERROR", error);
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
            usedBy: req.userId,
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
            message: voucher.remainingUses > 0
                ? `Voucher used successfully. ${voucher.remainingUses} use${voucher.remainingUses > 1 ? "s" : ""} remaining.`
                : "Voucher fully redeemed",
        });
    }
    catch (error) {
        console.error("MLM VOUCHER REDEEM ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
router.post("/vouchers/:voucherId/transfer", auth_1.requireAuth, async (req, res) => {
    try {
        const { voucherId } = req.params;
        const { recipientPhone, quantity = 1 } = req.body;
        // Prevent transferring special voucher
        if (voucherId === "instantlly-special-credits") {
            return res.status(400).json({
                success: false,
                message: "Cannot transfer special credits voucher. Use the special credits transfer feature instead.",
            });
        }
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
        // Validate quantity
        const qty = parseInt(quantity, 10);
        if (isNaN(qty) || qty < 1 || qty > 10000) {
            return res.status(400).json({
                success: false,
                message: "Quantity must be between 1 and 10000",
            });
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
        const voucherScope = await resolveVoucherScope(voucherId);
        const scopedVoucherId = voucherScope.canonicalVoucherId || asObjectId(voucherId);
        const sender = await User_1.default.findById(req.userId).select("name phone voucherBalance voucherBalances");
        if (!sender) {
            return res.status(404).json({
                success: false,
                message: "Sender not found",
            });
        }
        const physicalVoucher = await Voucher_1.default.findOne({
            _id: voucherId,
            userId: req.userId,
        });
        // Preferred path for campaign vouchers: transfer count/balance, not a new voucher document.
        const templateBalance = getTemplateBalanceFromUser(sender, scopedVoucherId);
        if (scopedVoucherId && templateBalance >= qty) {
            const senderUpdate = {
                voucherBalance: -qty,
            };
            const recipientUpdate = {
                voucherBalance: qty,
            };
            const scopedKey = `voucherBalances.${scopedVoucherId.toString()}`;
            if (templateBalance > 0) {
                senderUpdate[scopedKey] = -qty;
            }
            recipientUpdate[scopedKey] = qty;
            await Promise.all([
                User_1.default.findByIdAndUpdate(req.userId, { $inc: senderUpdate }),
                User_1.default.findByIdAndUpdate(recipient._id, { $inc: recipientUpdate }),
            ]);
            let perVoucherAmount = 1200;
            const voucherTemplate = await Voucher_1.default.findById(scopedVoucherId)
                .select("amount MRP")
                .lean();
            if (voucherTemplate) {
                perVoucherAmount =
                    voucherTemplate.amount ||
                        voucherTemplate.MRP ||
                        1200;
            }
            await reconcileTransferUnlocksForUser(recipient._id.toString(), scopedVoucherId);
            await VoucherTransferLog_1.default.create({
                senderId: req.userId,
                senderName: sender.name,
                senderPhone: sender.phone,
                recipientId: recipient._id,
                recipientName: recipient.name,
                recipientPhone: recipient.phone,
                quantity: qty,
                amount: perVoucherAmount,
                transferredAt: new Date(),
                voucherId: scopedVoucherId,
            });
            return res.json({
                success: true,
                message: `Voucher balance transferred to ${recipient.name} with quantity ${qty}`,
                transferMode: "balance",
                voucherId: scopedVoucherId.toString(),
                quantity: qty,
            });
        }
        // Legacy physical-voucher path for already-issued voucher documents.
        if (!physicalVoucher) {
            if (scopedVoucherId) {
                return res.status(400).json({
                    success: false,
                    message: "Insufficient voucher quantity for this campaign transfer",
                });
            }
            return res
                .status(404)
                .json({ success: false, message: "Voucher not found" });
        }
        if (physicalVoucher.redeemedStatus !== "unredeemed") {
            return res.status(400).json({
                success: false,
                message: `Cannot transfer ${physicalVoucher.redeemedStatus} voucher`,
            });
        }
        if (physicalVoucher.expiryDate < new Date()) {
            return res
                .status(400)
                .json({ success: false, message: "Cannot transfer expired voucher" });
        }
        const previousOwner = physicalVoucher.userId;
        physicalVoucher.userId = recipient._id;
        physicalVoucher.source = "transfer";
        physicalVoucher.transferredFrom = previousOwner;
        physicalVoucher.transferredAt = new Date();
        physicalVoucher.maxUses = qty;
        physicalVoucher.remainingUses = qty;
        physicalVoucher.transferHistory.push({
            from: previousOwner,
            to: recipient._id,
            transferredAt: new Date(),
        });
        await physicalVoucher.save();
        await reconcileTransferUnlocksForUser(recipient._id.toString(), physicalVoucher.templateId ||
            physicalVoucher._id);
        await physicalVoucher.populate("userId", "name phone");
        res.json({
            success: true,
            message: `Voucher transferred to ${recipient.name} with ${qty} use${qty > 1 ? "s" : ""}`,
            voucher: physicalVoucher,
            transferMode: "physical",
        });
    }
    catch (error) {
        console.error("MLM VOUCHER TRANSFER ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// Admin voucher transfer - transfer count/balance for a voucher campaign
router.post("/vouchers/admin-transfer", auth_1.requireAuth, async (req, res) => {
    try {
        const { recipientPhone, quantity = 1, voucherId } = req.body;
        // Check if user is admin
        const admin = await User_1.default.findById(req.userId).select("isVoucherAdmin level name phone");
        const isAdmin = admin?.isVoucherAdmin === true;
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
        if (isNaN(qty) || qty < 1 || qty > 10000) {
            return res.status(400).json({
                success: false,
                message: "Quantity must be between 1 and 10000",
            });
        }
        // Find recipient by phone
        const recipient = await User_1.default.findOne({ phone: recipientPhone });
        if (!recipient) {
            return res
                .status(404)
                .json({ success: false, message: "Recipient not found" });
        }
        const voucherObjectId = asObjectId(voucherId);
        const voucherBalanceInc = {
            voucherBalance: qty,
        };
        const voucherBalanceDec = {
            voucherBalance: -qty,
        };
        // Keep template-level balances in sync for the transferred campaign.
        if (voucherObjectId) {
            const key = `voucherBalances.${voucherObjectId.toString()}`;
            voucherBalanceInc[key] = qty;
            voucherBalanceDec[key] = -qty;
        }
        let perVoucherAmount = 1200;
        const voucherTemplate = voucherObjectId
            ? await Voucher_1.default.findById(voucherObjectId).lean()
            : null;
        if (voucherTemplate) {
            perVoucherAmount =
                voucherTemplate.amount ||
                    voucherTemplate.MRP ||
                    1200;
        }
        const templateId = voucherTemplate?.templateId ||
            voucherObjectId ||
            null;
        await Promise.all([
            User_1.default.findByIdAndUpdate(recipient._id, { $inc: voucherBalanceInc }),
            User_1.default.findByIdAndUpdate(req.userId, { $inc: voucherBalanceDec }),
        ]);
        await reconcileTransferUnlocksForUser(recipient._id.toString(), templateId || voucherObjectId || undefined);
        // Save transfer log for history tracking
        await VoucherTransferLog_1.default.create({
            senderId: req.userId,
            senderName: admin.name,
            senderPhone: admin.phone,
            recipientId: recipient._id,
            recipientName: recipient.name,
            recipientPhone: recipient.phone,
            quantity: qty,
            amount: perVoucherAmount,
            transferredAt: new Date(),
            ...(voucherId ? { voucherId } : {}),
        });
        res.json({
            success: true,
            message: `Transferred ${qty} voucher${qty > 1 ? "s" : ""} to ${recipient.name}`,
            count: qty,
            transferMode: "balance",
        });
    }
    catch (error) {
        console.error("MLM ADMIN VOUCHER TRANSFER ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// TRANSFER HISTORY (Special Credits + Vouchers)
// ============================================
router.get("/transfer-history", auth_1.requireAuth, async (req, res) => {
    try {
        const { limit = 50, voucherId } = req.query;
        // Build voucher filter for SpecialCredit and VoucherTransferLog
        let vidFilter = {};
        if (voucherId) {
            try {
                vidFilter = { voucherId: new mongoose_1.Types.ObjectId(voucherId) };
            }
            catch (_) { }
        }
        // Get special credits sent by this user (scoped to voucher if provided)
        const specialCreditsSent = await SpecialCredit_1.default.find({
            ownerId: req.userId,
            status: "sent",
            ...vidFilter,
        })
            .populate("recipientId", "name phone")
            .sort({ sentAt: -1 })
            .limit(Number(limit))
            .lean();
        // Get special credits received by this user (scoped to voucher if provided)
        const specialCreditsReceived = await SpecialCredit_1.default.find({
            recipientId: req.userId,
            status: "sent",
            ...vidFilter,
        })
            .populate("ownerId", "name phone")
            .sort({ sentAt: -1 })
            .limit(Number(limit))
            .lean();
        // Get balance-based voucher transfer logs (scoped to voucher if provided)
        const voucherLogsSent = await VoucherTransferLog_1.default.find({
            senderId: req.userId,
            ...(voucherId ? vidFilter : {}),
        })
            .sort({ transferredAt: -1 })
            .limit(Number(limit))
            .lean();
        const voucherLogsReceived = await VoucherTransferLog_1.default.find({
            recipientId: req.userId,
            ...(voucherId ? vidFilter : {}),
        })
            .sort({ transferredAt: -1 })
            .limit(Number(limit))
            .lean();
        // Get vouchers sent (check transferHistory)
        const vouchersSent = await Voucher_1.default.find({
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
        const vouchersReceived = await Voucher_1.default.find({
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
                sent: specialCreditsSent.map((sc) => ({
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
                received: specialCreditsReceived.map((sc) => ({
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
                sent: [],
                received: vouchersReceived.map((v) => ({
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
        vouchersSent.forEach((v) => {
            if (v.transferHistory && v.transferHistory.length > 0) {
                v.transferHistory.forEach((th) => {
                    const fromId = th.from?._id?.toString() || th.from?.toString();
                    if (fromId === req.userId) {
                        const recipientId = th.to?._id?.toString() || th.to?.toString();
                        const transferDate = new Date(th.transferredAt);
                        // Create a key based on recipient and transfer time (rounded to minute)
                        const transferMinute = new Date(transferDate.getFullYear(), transferDate.getMonth(), transferDate.getDate(), transferDate.getHours(), transferDate.getMinutes()).getTime();
                        const key = `${recipientId}-${transferMinute}`;
                        if (vouchersSentMap.has(key)) {
                            // Add to existing grouped transfer
                            const existing = vouchersSentMap.get(key);
                            existing.count++;
                            existing.voucherNumbers.push(v.voucherNumber);
                            existing.totalAmount += v.amount || v.MRP;
                        }
                        else {
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
        const VOUCHER_MRP = 1200; // fallback only — logs now store their own amount
        const logSentEntries = voucherLogsSent.map((log) => ({
            type: "voucher",
            direction: "sent",
            voucherNumber: null,
            voucherNumbers: [],
            companyName: "Instantlly",
            amount: log.amount ?? VOUCHER_MRP,
            totalAmount: (log.amount ?? VOUCHER_MRP) * log.quantity,
            count: log.quantity,
            recipient: {
                id: log.recipientId?.toString(),
                name: log.recipientName,
                phone: log.recipientPhone,
            },
            transferredAt: log.transferredAt,
            source: "admin-balance",
        }));
        const logReceivedEntries = voucherLogsReceived.map((log) => ({
            type: "voucher",
            direction: "received",
            voucherNumber: null,
            companyName: "Instantlly",
            amount: log.amount ?? VOUCHER_MRP,
            totalAmount: (log.amount ?? VOUCHER_MRP) * log.quantity,
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
                    vouchersSent: history.vouchers.sent.reduce((sum, v) => sum + (v.count || 1), 0),
                    vouchersReceived: history.vouchers.received.length,
                    totalTransfers: allTransfers.length,
                },
            },
        });
    }
    catch (error) {
        console.error("MLM TRANSFER HISTORY ERROR", error);
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
        const voucherId = req.query.voucherId;
        const voucherScope = await resolveVoucherScope(voucherId);
        const tree = await (0, treeService_1.buildNetworkTree)(req.userId, depth, perParentLimit, voucherScope.canonicalVoucherId?.toString());
        if (voucherId) {
            const [matchedSlots, matchedTransfers] = await Promise.all([
                voucherScope.canonicalVoucherId
                    ? SpecialCredit_1.default.countDocuments({
                        ownerId: req.userId,
                        voucherId: voucherScope.canonicalVoucherId,
                        status: "sent",
                    })
                    : Promise.resolve(0),
                voucherScope.canonicalVoucherId
                    ? MlmTransfer_1.default.countDocuments({
                        senderId: req.userId,
                        voucherId: voucherScope.canonicalVoucherId,
                    })
                    : Promise.resolve(0),
            ]);
            console.log("[MLM_TREE_SCOPE_DEBUG]", {
                userId: req.userId,
                incomingVoucherId: voucherId,
                detectedVoucherType: voucherScope.detectedType,
                resolvedCanonicalVoucherId: voucherScope.canonicalVoucherId?.toString() || null,
                matchedVoucherDocId: voucherScope.matchedVoucherId?.toString() || null,
                matchedSlotCount: matchedSlots,
                matchedTransferCount: matchedTransfers,
                finalDirectChildrenCount: Array.isArray(tree?.directChildren)
                    ? tree.directChildren.length
                    : 0,
            });
        }
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
        const voucherId = req.query.voucherId;
        const voucherScope = await resolveVoucherScope(voucherId);
        const buyers = await (0, directBuyersService_1.getDirectBuyers)(req.userId, limit, skip, voucherScope.canonicalVoucherId?.toString());
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
        await reconcileTransferUnlocksForUser(req.userId);
        const user = await User_1.default.findById(req.userId).select("name phone level directCount downlineCount parentId createdAt isVoucherAdmin specialCredits voucherBalance");
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
        // Add special credits data for voucher admin
        let specialCreditsData = null;
        const isVoucherAdmin = user.isVoucherAdmin === true;
        if (isVoucherAdmin) {
            const specialCreditSlots = await SpecialCredit_1.default.find({
                ownerId: req.userId,
            })
                .populate("recipientId", "name phone")
                .sort({ slotNumber: 1 })
                .lean();
            const totalSlots = specialCreditSlots.length;
            const creditPerSlot = getSpecialCreditsForLevel(user.level || 0);
            const availableSlots = specialCreditSlots.filter((s) => s.status === "available").length;
            const usedSlots = specialCreditSlots.filter((s) => s.status === "sent").length;
            specialCreditsData = {
                vouchersFigure: user.voucherBalance || 0,
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
                downlineCount: user.downlineCount || 0,
                parentId: user.parentId,
                joinedDate: user.createdAt,
                isVoucherAdmin: isVoucherAdmin,
                specialCredits: user.specialCredits || {
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
                vouchersFigure: user.voucherBalance || 0,
            },
            structuralCreditPool,
            baseMrp: mlm_1.MLM_BASE_MRP,
            specialCredits: specialCreditsData,
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
        // Link recipient to Admin (if not already linked) — V2: transactional via NetworkService
        let isNewLink = false;
        try {
            await (0, networkService_1.linkUser)(recipient._id.toString(), adminUserId, adminUserId, "admin-transfer");
            isNewLink = true;
        }
        catch (linkErr) {
            if (linkErr.message?.includes("already linked to another parent")) {
                return res.status(400).json({
                    success: false,
                    message: `${recipient.name} is already linked to another parent in the network`,
                });
            }
            // Idempotent: user is already linked to this admin — that's fine
            if (!linkErr.message?.includes("already linked to this exact parent")) {
                throw linkErr;
            }
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
// ============================================
// SPECIAL CREDITS - "Sales Target at Special Discount"
// ============================================
// Credit calculation per level (divides by 5 each level)
const SPECIAL_CREDIT_CHAIN = [
    29296872000, // Level 0 (Admin)
    5859372000, // Level 1
    1171872000, // Level 2
    234372000, // Level 3
    46872000, // Level 4
    9372000, // Level 5
    1872000, // Level 6
    372000, // Level 7
    72000, // Level 8
    12000, // Level 9
];
// Calculate credits for a given level
function getSpecialCreditsForLevel(level) {
    if (level < 0 || level >= SPECIAL_CREDIT_CHAIN.length)
        return 0;
    return SPECIAL_CREDIT_CHAIN[level];
}
// Compute how many slots a recipient gets: senderSlotCreditAmount / recipientLevelCreditPerSlot
// Falls back to 5 if the configured level amount does not yield a usable slot count.
// This keeps test/admin custom credit amounts (for example 10,000) distributable.
function computeRecipientSlots(grantedCredits, recipientLevel) {
    const creditPerSlot = getSpecialCreditsForLevel(recipientLevel);
    if (creditPerSlot <= 0)
        return 5;
    const computed = Math.round(grantedCredits / creditPerSlot);
    return computed >= 1 ? computed : 5;
}
// ✅ Initialize Admin's Special Credit Slots
router.post("/special-credits/admin/initialize", async (req, res) => {
    try {
        const adminKey = req.headers["x-admin-key"];
        if (adminKey !== process.env.ADMIN_SECRET_KEY &&
            adminKey !== "your-secure-admin-key-here") {
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
        const admin = await User_1.default.findById(adminUserId);
        if (!admin) {
            return res.status(404).json({
                success: false,
                message: "Admin user not found",
            });
        }
        // Parse slotCount from request (required — no default hardcode)
        const slotCount = parseInt(String(req.body.slotCount));
        if (!slotCount || slotCount < 1) {
            return res.status(400).json({
                success: false,
                message: "slotCount is required and must be >= 1",
            });
        }
        // Mark as voucher admin
        admin.isVoucherAdmin = true;
        admin.level = 0; // Admin is level 0
        admin.parentId = null;
        // Initialize special credits
        if (!admin.specialCredits) {
            admin.specialCredits = {
                balance: 0,
                totalReceived: 0,
                totalSent: 0,
                availableSlots: slotCount,
                usedSlots: 0,
            };
        }
        else {
            admin.specialCredits.availableSlots = slotCount;
        }
        await admin.save();
        // Check if slots already exist
        const existingSlots = await SpecialCredit_1.default.countDocuments({
            ownerId: adminUserId,
        });
        if (existingSlots >= slotCount) {
            return res.json({
                success: true,
                message: `Admin already has ${existingSlots} special credit slots initialized`,
                slots: existingSlots,
            });
        }
        // Create slots from existingSlots+1 up to slotCount
        const slots = [];
        const adminCreditAmount = getSpecialCreditsForLevel(0);
        const startFrom = existingSlots + 1;
        for (let i = startFrom; i <= slotCount; i++) {
            const slot = await SpecialCredit_1.default.create({
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
            message: `Admin special credit slots initialized (${slotCount} total)`,
            admin: {
                id: admin._id,
                name: admin.name,
                phone: admin.phone,
                isVoucherAdmin: true,
            },
            slots: slots.length,
            totalSlots: slotCount,
            creditPerSlot: adminCreditAmount,
        });
    }
    catch (error) {
        console.error("SPECIAL CREDITS ADMIN INIT ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ Get Special Credit Slots for current user
router.get("/special-credits/slots", auth_1.requireAuth, async (req, res) => {
    try {
        const voucherId = req.query.voucherId;
        const voucherScope = await resolveVoucherScope(voucherId);
        const user = await User_1.default.findById(req.userId).select("name phone level isVoucherAdmin specialCredits");
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        // Get user's slots.  Use the raw voucherId provided by client when possible – the
        // original behaviour used `canonicalVoucherId` which collapses instances into
        // their template, leading to shared slot pools.  We want per-voucher isolation
        // so prefer the parsed `voucherId` and only fall back to canonical when none is
        // supplied.
        const slotQuery = { ownerId: req.userId };
        const rawVoucherObject = asObjectId(voucherId);
        if (rawVoucherObject) {
            slotQuery.voucherId = rawVoucherObject;
        }
        else if (voucherScope.canonicalVoucherId) {
            slotQuery.voucherId = voucherScope.canonicalVoucherId;
        }
        const slots = await SpecialCredit_1.default.find(slotQuery)
            .populate("recipientId", "name phone")
            .sort({ slotNumber: 1 })
            .lean();
        // Format slots: map directly from DB records (slots are created by initialize / auto-created on receive)
        const formattedSlots = slots.map((slot) => {
            const lockUntil = slot.lockExpiresAt
                ? new Date(slot.lockExpiresAt).getTime()
                : 0;
            const lockLeftSeconds = Math.max(0, Math.floor((lockUntil - Date.now()) / 1000));
            return {
                slotNumber: slot.slotNumber,
                status: slot.status,
                creditAmount: slot.creditAmount,
                recipientName: slot.recipientName || null,
                recipientPhone: slot.recipientPhone || null,
                recipientId: slot.recipientId?._id?.toString() || null,
                sentAt: slot.sentAt || null,
                isAvailable: slot.status === "available",
                transferId: slot.transferId?.toString() || null,
                isLocked: !!slot.isLocked && lockLeftSeconds > 0,
                lockReason: slot.lockReason || null,
                timeLeftSeconds: lockLeftSeconds,
            };
        });
        const creditPerSlot = getSpecialCreditsForLevel(user.level || 0);
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
                totalSlots: formattedSlots.length,
                availableSlots: formattedSlots.filter((s) => s.isAvailable).length,
                usedSlots: formattedSlots.filter((s) => !s.isAvailable).length,
                creditPerSlot: formattedSlots[0]?.creditAmount ?? creditPerSlot,
            },
        });
    }
    catch (error) {
        console.error("SPECIAL CREDITS SLOTS ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ Send Special Credits to a user by phone number
router.post("/special-credits/send", auth_1.requireAuth, async (req, res) => {
    let sentSlotId = null;
    let senderIdForRollback = null;
    let recipientIdForRollback = null;
    let transferIdForRollback = null;
    let recipientSourceSlotIdForRollback = null;
    let senderCreditAmountForRollback = 0;
    let recipientSlotCountForRollback = 0;
    let recipientCreditAmountForRollback = 0;
    try {
        const { recipientPhone, slotNumber, voucherId } = req.body;
        console.log("📤 SPECIAL CREDITS SEND - Request received", {
            userId: req.userId,
            recipientPhone,
            slotNumber,
            voucherId,
        });
        const voucherScope = await resolveVoucherScope(voucherId);
        console.log("📋 SPECIAL CREDITS SEND - Resolved voucher scope", {
            detectedType: voucherScope.detectedType,
            canonicalVoucherId: voucherScope.canonicalVoucherId?.toString(),
        });
        if (voucherId && voucherScope.detectedType === "invalid") {
            return res
                .status(400)
                .json({ success: false, message: "Invalid voucherId" });
        }
        // determine which voucher id should drive slots/send operations.  prefer the
        // exact id passed by the client (parsed to ObjectId) so each voucher has its own
        // pool; fall back to the canonical (template) id only when no id was supplied.
        const parsedVoucher = asObjectId(voucherId);
        const voucherObjectId = parsedVoucher || voucherScope.canonicalVoucherId;
        const requiredVoucherCount = await getVoucherTemplateRequiredCount(voucherObjectId?.toString() || undefined);
        console.log("📌 SPECIAL CREDITS SEND - Parsed voucher", {
            parsedVoucher: parsedVoucher?.toString(),
            voucherObjectId: voucherObjectId?.toString(),
            requiredVoucherCount,
        });
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
        const sender = await User_1.default.findById(req.userId).select("name phone level isVoucherAdmin specialCredits voucherBalance");
        console.log("👤 SPECIAL CREDITS SEND - Sender fetched", {
            senderId: req.userId,
            senderExists: !!sender,
            senderName: sender?.name,
        });
        if (!sender) {
            return res.status(404).json({
                success: false,
                message: "Sender not found",
            });
        }
        const isAdmin = sender.isVoucherAdmin === true;
        // Non-admin users must meet the selected voucher campaign's minimum voucher requirement
        if (!isAdmin) {
            const totalVouchers = await getVoucherCountForTemplate(req.userId, voucherObjectId || undefined);
            if (totalVouchers < requiredVoucherCount) {
                return res.status(403).json({
                    success: false,
                    message: `You need at least ${requiredVoucherCount} vouchers to send special credits for this campaign. Current vouchers: ${totalVouchers}`,
                    requiredVouchers: requiredVoucherCount,
                    currentVouchers: totalVouchers,
                });
            }
        }
        // Find the slot — must be pre-created (by initialize for admin, or auto-created when credits were received)
        console.log("🔍 SPECIAL CREDITS SEND - Looking for slot", {
            ownerId: req.userId,
            slotNumber,
            voucherId: voucherObjectId?.toString(),
        });
        let slot = await SpecialCredit_1.default.findOne({
            ownerId: req.userId,
            slotNumber,
            ...(voucherObjectId ? { voucherId: voucherObjectId } : {}),
        });
        console.log("🎯 SPECIAL CREDITS SEND - Slot lookup result", {
            slotFound: !!slot,
            slotStatus: slot?.status,
            slotId: slot?._id?.toString(),
        });
        if (!slot) {
            const totalSlots = await SpecialCredit_1.default.countDocuments({
                ownerId: req.userId,
                ...(voucherObjectId ? { voucherId: voucherObjectId } : {}),
            });
            return res.status(400).json({
                success: false,
                message: `Slot ${slotNumber} not found. You have ${totalSlots} slot(s) initialized.`,
            });
        }
        // Check if slot is currently locked by a timed transfer
        if (slot.isLocked && slot.lockExpiresAt) {
            const lockMs = new Date(slot.lockExpiresAt).getTime() - Date.now();
            if (lockMs > 0) {
                return res.status(403).json({
                    success: false,
                    message: "Slot is locked until voucher unlock condition is met",
                    timeLeftSeconds: Math.floor(lockMs / 1000),
                    transferId: slot.transferId?.toString() || null,
                });
            }
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
        const recipient = await User_1.default.findOne({
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
        const timerStartedAt = new Date();
        const expiresAt = new Date(Date.now() + VOUCHER_PURCHASE_TIMEOUT_MINUTES * 60 * 1000);
        // V2: send slot atomically + write SlotEvent via SlotService
        await (0, slotService_1.sendSlot)(slot._id.toString(), recipient._id.toString(), recipient.name, recipient.phone, req.userId, slot.voucherId?.toString(), expiresAt);
        sentSlotId = slot._id.toString();
        senderIdForRollback = sender._id.toString();
        recipientIdForRollback = recipient._id.toString();
        recipientSourceSlotIdForRollback = slot._id.toString();
        // Re-fetch slot after update so downstream reads see the new state
        slot = (await SpecialCredit_1.default.findById(slot._id));
        // Update sender's special credits stats
        if (!sender.specialCredits) {
            sender.specialCredits = {
                balance: 0,
                totalReceived: 0,
                totalSent: 0,
                availableSlots: 0,
                usedSlots: 0,
            };
        }
        sender.specialCredits.totalSent += slot.creditAmount;
        sender.specialCredits.usedSlots += 1;
        await sender.save();
        senderCreditAmountForRollback = slot.creditAmount;
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
        // Compute how many slots recipient gets: grantedCredits / creditPerSlotAtRecipientLevel
        const numRecipientSlots = computeRecipientSlots(slot.creditAmount, recipient.level || (sender.level || 0) + 1);
        // Add credits to recipient
        recipient.specialCredits.balance += slot.creditAmount;
        recipient.specialCredits.totalReceived += slot.creditAmount;
        recipient.specialCredits.availableSlots += numRecipientSlots;
        recipientCreditAmountForRollback = slot.creditAmount;
        recipientSlotCountForRollback = numRecipientSlots;
        // Set recipient's level if not already set
        if (!recipient.level || recipient.level === 0) {
            recipient.level = (sender.level || 0) + 1;
        }
        // V2: link recipient to sender via NetworkService (transactional)
        try {
            await (0, networkService_1.linkUser)(recipient._id.toString(), req.userId, req.userId, "special-credit-send");
        }
        catch (linkErr) {
            // Idempotent: user already linked to this sender — fine to continue
            if (!linkErr.message?.includes("already linked to this exact parent")) {
                // Already linked to another parent or cap exceeded — surface as error
                throw linkErr;
            }
        }
        // Persist recipient credit counters without overwriting NetworkService linkage fields.
        await User_1.default.findByIdAndUpdate(recipient._id, {
            "specialCredits.balance": recipient.specialCredits.balance,
            "specialCredits.totalReceived": recipient.specialCredits.totalReceived,
            "specialCredits.availableSlots": recipient.specialCredits.availableSlots,
            ...(recipient.level ? { level: recipient.level } : {}),
        });
        // Add credits to recipient's WALLET
        await (0, walletService_1.addCredits)(recipient._id.toString(), slot.creditAmount);
        // Create 5 slots for recipient with EQUAL amounts
        // Each slot gets: (received amount) / 5
        const recipientLevel = recipient.level;
        const recipientSlots = [];
        const slotCreditAmount = Math.max(1, Math.round(slot.creditAmount / numRecipientSlots));
        const currentVoucherCount = await getVoucherCountForTemplate(recipient._id.toString(), slot.voucherId ||
            voucherObjectId ||
            undefined);
        const shouldUnlockImmediately = currentVoucherCount >= requiredVoucherCount;
        const transfer = await MlmTransfer_1.default.create({
            senderId: sender._id,
            receiverId: recipient._id,
            voucherId: slot.voucherId || voucherObjectId,
            sourceSlotId: slot._id,
            amount: slot.creditAmount,
            slotCount: numRecipientSlots,
            slotAmount: slotCreditAmount,
            slots: Array.from({ length: numRecipientSlots }).map((_, idx) => ({
                slotNumber: idx + 1,
                amount: slotCreditAmount,
                isLocked: !shouldUnlockImmediately,
            })),
            requiredVoucherCount,
            baselineVoucherCount: currentVoucherCount,
            currentVoucherCount,
            unlockedSlots: shouldUnlockImmediately ? numRecipientSlots : 0,
            timerStartedAt,
            expiresAt,
            unlockedAt: shouldUnlockImmediately ? new Date() : null,
            status: shouldUnlockImmediately ? "unlocked" : "pending_unlock",
        });
        transferIdForRollback = transfer._id.toString();
        for (let i = 1; i <= numRecipientSlots; i++) {
            const existingSlot = await SpecialCredit_1.default.findOne({
                ownerId: recipient._id,
                slotNumber: i,
                ...(slot.voucherId || voucherObjectId
                    ? { voucherId: slot.voucherId || voucherObjectId }
                    : {}),
            });
            if (!existingSlot) {
                const newSlot = await SpecialCredit_1.default.create({
                    ownerId: recipient._id,
                    voucherId: slot.voucherId || voucherObjectId || undefined,
                    slotNumber: i,
                    creditAmount: slotCreditAmount, // All slots get the same amount
                    status: "available",
                    level: recipientLevel,
                    sourceSlotId: slot._id,
                    transferId: transfer._id,
                    isLocked: !shouldUnlockImmediately,
                    lockReason: !shouldUnlockImmediately
                        ? `Need ${requiredVoucherCount} vouchers for this campaign`
                        : null,
                    lockExpiresAt: !shouldUnlockImmediately ? expiresAt : null,
                    unlockedAt: shouldUnlockImmediately ? new Date() : null,
                });
                recipientSlots.push(newSlot);
            }
        }
        console.log("✅ SPECIAL CREDITS SEND - About to return success", {
            transferId: transfer._id.toString(),
            recipientId: recipient._id.toString(),
            amount: slot.creditAmount,
            status: transfer.status,
        });
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
                transferId: transfer._id.toString(),
                status: transfer.status,
                requiredVoucherCount,
                currentVoucherCount,
                timerStartedAt: transfer.timerStartedAt,
                expiresAt: transfer.expiresAt,
                timeLeftSeconds: transferTimeLeftSeconds(transfer.expiresAt),
                recipientSlotsCreated: recipientSlots.length,
                numRecipientSlots,
                recipientSlotAmount: slotCreditAmount,
                recipientSlots: recipientSlots.map((s) => ({
                    slotNumber: s.slotNumber,
                    creditAmount: s.creditAmount,
                    isLocked: s.isLocked || false,
                })),
            },
        });
    }
    catch (error) {
        if (sentSlotId) {
            try {
                if (transferIdForRollback) {
                    await SpecialCredit_1.default.deleteMany({
                        ownerId: recipientIdForRollback,
                        transferId: transferIdForRollback,
                    });
                    await MlmTransfer_1.default.findByIdAndDelete(transferIdForRollback);
                }
                else if (recipientSourceSlotIdForRollback && recipientIdForRollback) {
                    await SpecialCredit_1.default.deleteMany({
                        ownerId: recipientIdForRollback,
                        sourceSlotId: recipientSourceSlotIdForRollback,
                    });
                }
                if (recipientIdForRollback && recipientCreditAmountForRollback > 0) {
                    const recipientUser = await User_1.default.findById(recipientIdForRollback)
                        .select("specialCredits")
                        .lean();
                    const currentSpecialCredits = recipientUser?.specialCredits || {};
                    await User_1.default.findByIdAndUpdate(recipientIdForRollback, {
                        $set: {
                            "specialCredits.balance": Math.max(0, Number(currentSpecialCredits.balance || 0) -
                                recipientCreditAmountForRollback),
                            "specialCredits.totalReceived": Math.max(0, Number(currentSpecialCredits.totalReceived || 0) -
                                recipientCreditAmountForRollback),
                            "specialCredits.availableSlots": Math.max(0, Number(currentSpecialCredits.availableSlots || 0) -
                                recipientSlotCountForRollback),
                        },
                    });
                    try {
                        await (0, walletService_1.subtractCredits)(recipientIdForRollback, recipientCreditAmountForRollback);
                    }
                    catch (_) {
                        // Wallet rollback is best-effort.
                    }
                }
                if (senderIdForRollback && senderCreditAmountForRollback > 0) {
                    const senderUser = await User_1.default.findById(senderIdForRollback)
                        .select("specialCredits")
                        .lean();
                    const currentSpecialCredits = senderUser?.specialCredits || {};
                    await User_1.default.findByIdAndUpdate(senderIdForRollback, {
                        $set: {
                            "specialCredits.totalSent": Math.max(0, Number(currentSpecialCredits.totalSent || 0) -
                                senderCreditAmountForRollback),
                            "specialCredits.usedSlots": Math.max(0, Number(currentSpecialCredits.usedSlots || 0) - 1),
                        },
                    });
                }
                await (0, slotService_1.refundSlot)(sentSlotId, req.userId, "rollback_after_special_credit_send_failure");
            }
            catch (rollbackError) {
                console.error("❌ SPECIAL CREDITS SEND ROLLBACK ERROR", {
                    sentSlotId,
                    transferIdForRollback,
                    recipientIdForRollback,
                    rollbackError: rollbackError instanceof Error
                        ? rollbackError.message
                        : String(rollbackError),
                });
            }
        }
        console.error("❌ SPECIAL CREDITS SEND ERROR", {
            userId: req.userId,
            body: req.body,
            error: error instanceof Error ? error.message : String(error),
            stack: error instanceof Error ? error.stack : undefined,
        });
        res.status(500).json({
            success: false,
            message: "Server error",
            debug: process.env.NODE_ENV === "development" ? (error instanceof Error ? error.message : String(error)) : undefined,
        });
    }
});
// ✅ Get Special Credits Dashboard
router.get("/special-credits/dashboard", auth_1.requireAuth, async (req, res) => {
    try {
        const { voucherId } = req.query;
        const voucherScope = await resolveVoucherScope(voucherId);
        // for unlock logic we still consult canonical id – unlocking should be
        // evaluated on the campaign/template level when applicable, but slot
        // filtering below is per‑voucher.
        const activeTransferVoucherId = voucherScope.canonicalVoucherId;
        await reconcileTransferUnlocksForUser(req.userId, activeTransferVoucherId || undefined);
        const user = await User_1.default.findById(req.userId).select("name phone level isVoucherAdmin specialCredits parentId voucherBalance voucherBalances");
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        // Filter slots by the explicit voucherId if provided, otherwise fall back to
        // canonical.  this ensures that even if a voucher accidentally has its
        // templateId set to itself we treat it as a standalone pool.
        const slotQuery = { ownerId: req.userId };
        const rawVoucherObject = asObjectId(voucherId);
        if (rawVoucherObject) {
            slotQuery.voucherId = rawVoucherObject;
        }
        else if (activeTransferVoucherId) {
            slotQuery.voucherId = activeTransferVoucherId;
        }
        // Get slots (filtered by voucher if specified)
        const slots = await SpecialCredit_1.default.find(slotQuery)
            .populate("recipientId", "name phone")
            .sort({ slotNumber: 1 })
            .lean();
        // Calculate stats
        const isAdmin = user.isVoucherAdmin === true;
        const totalSlots = slots.length;
        const creditPerSlot = slots[0]?.creditAmount ?? getSpecialCreditsForLevel(user.level || 0);
        const availableSlots = slots.filter((s) => s.status === "available").length;
        const usedSlots = slots.filter((s) => s.status === "sent").length;
        // Calculate total available credits
        const totalAvailableCredits = availableSlots * creditPerSlot;
        // When filtering by voucherId, compute sent credits from filtered slots only (not global total)
        const totalSentCredits = activeTransferVoucherId
            ? usedSlots * creditPerSlot
            : user.specialCredits?.totalSent || 0;
        // Get network users who received special credits from this user
        const recipientIds = slots
            .filter((s) => s.recipientId)
            .map((s) => s.recipientId._id);
        const networkUsers = await User_1.default.find({
            _id: { $in: recipientIds },
        })
            .select("name phone level specialCredits")
            .lean();
        // Provide vouchers figure for admin and users with special credits
        let vouchersFigure = 0;
        if (isAdmin) {
            // Per-voucher balance if voucherId provided, else global voucherBalance
            if (activeTransferVoucherId) {
                const canonicalVoucherId = activeTransferVoucherId.toString();
                const balMap = user.voucherBalances;
                if (balMap instanceof Map) {
                    vouchersFigure = balMap.get(canonicalVoucherId) || 0;
                }
                else if (balMap && typeof balMap === "object") {
                    vouchersFigure = balMap[canonicalVoucherId] || 0;
                }
            }
            else {
                vouchersFigure = user.voucherBalance || 0;
            }
        }
        else if (user.specialCredits?.availableSlots > 0) {
            // Regular users must see voucher counts scoped to the active voucher when provided.
            vouchersFigure = await getVoucherCountForTemplate(req.userId, activeTransferVoucherId || undefined);
        }
        console.log("[MLM SPECIAL DASHBOARD FIGURE]", {
            userId: req.userId,
            requestedVoucherId: voucherId || null,
            resolvedVoucherId: activeTransferVoucherId?.toString() || null,
            isAdmin,
            availableSlots: user.specialCredits?.availableSlots || 0,
            totalSlots,
            usedSlots,
            vouchersFigure,
        });
        const activeTransfers = await MlmTransfer_1.default.find({
            receiverId: req.userId,
            status: { $in: ["pending_unlock", "unlocked"] },
            ...(activeTransferVoucherId
                ? { voucherId: activeTransferVoucherId }
                : {}),
        })
            .sort({ createdAt: -1 })
            .limit(20)
            .lean();
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
            networkUsers: networkUsers.map((nu) => ({
                id: nu._id.toString(),
                name: nu.name,
                phone: nu.phone,
                level: nu.level,
                creditsReceived: nu.specialCredits?.totalReceived || 0,
            })),
            activeTransfers: activeTransfers.map((t) => ({
                transferId: t._id.toString(),
                status: t.status,
                requiredVoucherCount: t.requiredVoucherCount || 0,
                currentVoucherCount: t.currentVoucherCount || 0,
                timerStartedAt: t.timerStartedAt,
                expiresAt: t.expiresAt,
                timeLeftSeconds: transferTimeLeftSeconds(t.expiresAt),
                slotCount: t.slotCount || 5,
                slotAmount: t.slotAmount || 0,
                unlockedSlots: t.unlockedSlots || 0,
            })),
        });
    }
    catch (error) {
        console.error("SPECIAL CREDITS DASHBOARD ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ✅ Get network users with special credits info (for admin view)
router.get("/special-credits/network", auth_1.requireAuth, async (req, res) => {
    try {
        const { voucherId } = req.query;
        const voucherScope = await resolveVoucherScope(voucherId);
        const user = await User_1.default.findById(req.userId).select("name phone level isVoucherAdmin");
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        // Filter by voucherId if provided
        const sentQuery = { ownerId: req.userId, status: "sent" };
        const allQuery = { ownerId: req.userId };
        if (voucherScope.canonicalVoucherId) {
            sentQuery.voucherId = voucherScope.canonicalVoucherId;
            allQuery.voucherId = voucherScope.canonicalVoucherId;
        }
        // Get slots with recipients
        const slots = await SpecialCredit_1.default.find(sentQuery)
            .populate("recipientId", "name phone level specialCredits")
            .sort({ slotNumber: 1 })
            .lean();
        // Get slots info - Check if user has ANY slots at all
        const allSlots = await SpecialCredit_1.default.find(allQuery)
            .sort({ slotNumber: 1 })
            .lean();
        // If user has no slots in database, return empty — no placeholder injection.
        // The admin must explicitly initialize slots via the admin panel before
        // the voucher dashboard shows any network data.
        if (allSlots.length === 0) {
            return res.json({
                success: true,
                networkUsers: [],
                summary: {
                    totalSlots: 0,
                    usedSlots: 0,
                    availableSlots: 0,
                    creditPerSlot: 0,
                },
            });
        }
        const isAdmin = user.isVoucherAdmin === true;
        const totalSlots = allSlots.length;
        // Use the creditAmount stored on the actual slots (consistent for all slots in this voucher)
        // Fall back to computed value only if no slots exist yet
        const creditPerSlot = allSlots[0]?.creditAmount || getSpecialCreditsForLevel(user.level || 0);
        // Format network users
        const networkUsers = slots.map((slot, index) => ({
            slotNumber: slot.slotNumber,
            name: slot.recipientName || "",
            phone: slot.recipientPhone || "",
            credits: slot.creditAmount,
            sentAt: slot.sentAt,
            recipientLevel: slot.recipientId?.level || 0,
        }));
        // Add placeholders for unused slots (only if user actually has slots)
        const placeholders = [];
        for (let i = 1; i <= totalSlots; i++) {
            const existingSlot = allSlots.find((s) => s.slotNumber === i);
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
            networkUsers: [...networkUsers, ...placeholders].sort((a, b) => a.slotNumber - b.slotNumber),
            summary: {
                totalSlots,
                usedSlots: networkUsers.length,
                availableSlots: placeholders.length,
                creditPerSlot,
            },
        });
    }
    catch (error) {
        console.error("SPECIAL CREDITS NETWORK ERROR", error);
        res.status(500).json({ success: false, message: "Server error" });
    }
});
// ============================================
// AUTO-REFUND SCHEDULER
// Checks periodically for expired special-credit slots.
// If recipient hasn't collected required vouchers, credits are refunded.
// ============================================
function startAutoRefundScheduler() {
    const CHECK_INTERVAL_MS = AUTO_REFUND_CHECK_INTERVAL_MS;
    const runCheck = async () => {
        try {
            const now = new Date();
            // Find sent slots whose unlock window has expired
            const expiredSlots = await SpecialCredit_1.default.find({
                status: "sent",
                expiresAt: { $lt: now },
            })
                .populate("ownerId", "name phone specialCredits")
                .populate("recipientId", "name phone specialCredits voucherBalance")
                .lean();
            if (expiredSlots.length === 0)
                return;
            console.log(`⏰ Auto-refund check: ${expiredSlots.length} expired slot(s) to evaluate`);
            for (const slotDoc of expiredSlots) {
                try {
                    const recipient = await User_1.default.findById(slotDoc.recipientId?._id);
                    if (!recipient) {
                        // Recipient deleted — just expire the slot
                        await SpecialCredit_1.default.findByIdAndUpdate(slotDoc._id, {
                            status: "expired",
                        });
                        continue;
                    }
                    const transfer = await MlmTransfer_1.default.findOne({
                        sourceSlotId: slotDoc._id,
                    }).lean();
                    // Count physical unredeemed vouchers + voucherBalance counter
                    const totalVouchers = transfer?.voucherId
                        ? await getVoucherCountForTemplate(recipient._id.toString(), transfer.voucherId)
                        : (await Voucher_1.default.countDocuments({
                            userId: recipient._id,
                            redeemedStatus: { $ne: "redeemed" },
                        })) + (recipient.voucherBalance || 0);
                    // Default minimum vouchers required: 5 (can be overridden per voucher)
                    const minRequired = transfer?.requiredVoucherCount ||
                        (slotDoc.voucherId
                            ? await getVoucherTemplateRequiredCount(slotDoc.voucherId.toString())
                            : 5);
                    if (totalVouchers >= minRequired) {
                        // ✅ User has enough vouchers — extend expiry far out so we don't re-check
                        await SpecialCredit_1.default.findByIdAndUpdate(slotDoc._id, {
                            expiresAt: new Date(Date.now() + 365 * 24 * 60 * 60 * 1000),
                        });
                        if (transfer && transfer.status === "pending_unlock") {
                            await MlmTransfer_1.default.findByIdAndUpdate(transfer._id, {
                                status: "unlocked",
                                currentVoucherCount: totalVouchers,
                                unlockedSlots: transfer.slotCount || 5,
                                unlockedAt: new Date(),
                            });
                            await SpecialCredit_1.default.updateMany({
                                ownerId: recipient._id,
                                transferId: transfer._id,
                                isLocked: true,
                            }, {
                                $set: {
                                    isLocked: false,
                                    lockReason: null,
                                    unlockedAt: new Date(),
                                },
                            });
                        }
                        console.log(`✅ Slot ${slotDoc.slotNumber} for ${recipient.name}: vouchers satisfied (${totalVouchers}/${minRequired})`);
                        continue;
                    }
                    // ❌ Not enough vouchers — revert the slot and refund credits
                    console.log(`⚠️ Auto-refunding slot ${slotDoc.slotNumber}: ${recipient.name} has ${totalVouchers}/${minRequired} vouchers`);
                    if (transfer) {
                        await refundExpiredPendingTransfer(transfer, totalVouchers, now);
                    }
                    else {
                        // Reset the slot to available even if the transfer row is already gone.
                        await SpecialCredit_1.default.findByIdAndUpdate(slotDoc._id, {
                            status: "available",
                            recipientId: null,
                            recipientName: null,
                            recipientPhone: null,
                            sentAt: null,
                            expiresAt: null,
                        });
                    }
                }
                catch (innerErr) {
                    console.error(`Auto-refund failed for slot ${slotDoc._id}:`, innerErr);
                }
            }
        }
        catch (err) {
            console.error("Auto-refund scheduler error:", err);
        }
    };
    // Run shortly after startup, then repeat on the configured interval.
    setTimeout(runCheck, 30000); // first run 30s after startup
    setInterval(runCheck, CHECK_INTERVAL_MS);
    console.log(`✅ MLM Auto-refund scheduler started (${Math.floor(CHECK_INTERVAL_MS / 1000)}s interval, ${VOUCHER_PURCHASE_TIMEOUT_MINUTES}m timeout)`);
}
exports.default = router;
