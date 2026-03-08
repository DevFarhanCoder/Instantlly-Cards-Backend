import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose, { Types } from "mongoose";
import MlmTransfer from "../models/MlmTransfer";
import MlmWallet from "../models/MlmWallet";
import SpecialCredit from "../models/SpecialCredit";
import User from "../models/User";
import Voucher from "../models/Voucher";

const MlmTransferModel: any = MlmTransfer;
const MlmWalletModel: any = MlmWallet;
const SpecialCreditModel: any = SpecialCredit;
const UserModel: any = User;
const VoucherModel: any = Voucher;

const DEFAULT_REQUIRED_VOUCHERS_PER_TEMPLATE = 5;
const VOUCHER_PURCHASE_TIMEOUT_MINUTES = Math.max(
  1,
  Number(process.env.SPECIAL_CREDIT_UNLOCK_TIMEOUT_MINUTES || 5),
);
const SPECIAL_CREDIT_CHAIN = [
  29296872000, 5859372000, 1171872000, 234372000, 46872000, 9372000, 1872000,
  372000, 72000, 12000,
];

type ScriptOptions = {
  apply: boolean;
  syncWallet: boolean;
  senderPhone?: string;
  recipientPhone?: string;
  voucherId?: string;
  slotNumber?: number;
};

function parseArgs(argv: string[]): ScriptOptions {
  const options: ScriptOptions = {
    apply: false,
    syncWallet: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const next = argv[i + 1];

    if (arg === "--apply") {
      options.apply = true;
      continue;
    }

    if (arg === "--sync-wallet") {
      options.syncWallet = true;
      continue;
    }

    if (arg === "--sender-phone" && next) {
      options.senderPhone = next;
      i += 1;
      continue;
    }

    if (arg === "--recipient-phone" && next) {
      options.recipientPhone = next;
      i += 1;
      continue;
    }

    if (arg === "--voucher-id" && next) {
      options.voucherId = next;
      i += 1;
      continue;
    }

    if (arg === "--slot-number" && next) {
      options.slotNumber = Number(next);
      i += 1;
    }
  }

  return options;
}

function getSpecialCreditsForLevel(level: number): number {
  if (level < 0 || level >= SPECIAL_CREDIT_CHAIN.length) return 0;
  return SPECIAL_CREDIT_CHAIN[level];
}

function computeRecipientSlots(
  grantedCredits: number,
  recipientLevel: number,
): number {
  const creditPerSlot = getSpecialCreditsForLevel(recipientLevel);
  if (creditPerSlot <= 0) return 5;
  const computed = Math.round(grantedCredits / creditPerSlot);
  return computed >= 1 ? computed : 5;
}

async function getVoucherTemplateRequiredCount(
  voucherId?: Types.ObjectId | null,
): Promise<number> {
  if (!voucherId) return DEFAULT_REQUIRED_VOUCHERS_PER_TEMPLATE;
  const voucher = await VoucherModel.findById(voucherId)
    .select("minVouchersRequired")
    .lean();
  return Math.max(
    0,
    Number((voucher as any)?.minVouchersRequired) ||
      DEFAULT_REQUIRED_VOUCHERS_PER_TEMPLATE,
  );
}

async function getVoucherCountForTemplate(
  userId: string,
  voucherId?: Types.ObjectId | null,
): Promise<number> {
  if (!voucherId) {
    const [physicalCount, user] = await Promise.all([
      VoucherModel.countDocuments({
        userId: new Types.ObjectId(userId),
        redeemedStatus: { $ne: "redeemed" },
      }),
      UserModel.findById(userId).select("voucherBalance").lean(),
    ]);

    return Math.max(
      0,
      physicalCount + Number((user as any)?.voucherBalance || 0),
    );
  }

  const [physicalCount, user] = await Promise.all([
    VoucherModel.countDocuments({
      userId: new Types.ObjectId(userId),
      redeemedStatus: { $ne: "redeemed" },
      $or: [{ templateId: voucherId }, { _id: voucherId }],
    }),
    UserModel.findById(userId).select("voucherBalances").lean(),
  ]);

  const voucherBalances = (user as any)?.voucherBalances;
  let balanceCount = 0;
  if (voucherBalances instanceof Map) {
    balanceCount = Number(voucherBalances.get(String(voucherId)) || 0);
  } else if (voucherBalances && typeof voucherBalances === "object") {
    balanceCount = Number(voucherBalances[String(voucherId)] || 0);
  }

  return Math.max(0, physicalCount + balanceCount);
}

async function syncUserSpecialCreditSummary(userId: string): Promise<void> {
  const [ownedSlots, incomingTransfers] = await Promise.all([
    SpecialCreditModel.find({ ownerId: userId }).lean(),
    MlmTransferModel.find({
      receiverId: userId,
      status: { $in: ["pending_unlock", "unlocked"] },
    }).lean(),
  ]);

  const availableSlots = ownedSlots.filter(
    (slot: any) => slot.status === "available",
  );
  const usedSlots = ownedSlots.filter((slot: any) => slot.status === "sent");
  const balance = availableSlots.reduce(
    (sum: number, slot: any) => sum + Number(slot.creditAmount || 0),
    0,
  );
  const totalSent = usedSlots.reduce(
    (sum: number, slot: any) => sum + Number(slot.creditAmount || 0),
    0,
  );
  const totalReceived = incomingTransfers.reduce(
    (sum: number, transfer: any) => sum + Number(transfer.amount || 0),
    0,
  );

  await UserModel.findByIdAndUpdate(userId, {
    "specialCredits.balance": balance,
    "specialCredits.totalReceived": totalReceived,
    "specialCredits.totalSent": totalSent,
    "specialCredits.availableSlots": availableSlots.length,
    "specialCredits.usedSlots": usedSlots.length,
  });
}

async function maybeCreditWallet(
  userId: string,
  amount: number,
  apply: boolean,
): Promise<void> {
  const wallet = await MlmWalletModel.findOne({ userId }).lean();
  const current = Number((wallet as any)?.creditBalance || 0);
  console.log(
    `  wallet: current=${current.toLocaleString()} increment=${amount.toLocaleString()}`,
  );
  if (!apply) return;
  await MlmWalletModel.findOneAndUpdate(
    { userId },
    { $inc: { creditBalance: amount } },
    { new: true, upsert: true },
  );
}

async function run(): Promise<void> {
  const options = parseArgs(process.argv.slice(2));
  await mongoose.connect(process.env.MONGODB_URI!);

  const sender: any = options.senderPhone
    ? await UserModel.findOne({ phone: options.senderPhone }).select("_id").lean()
    : null;
  const recipient: any = options.recipientPhone
    ? await UserModel.findOne({ phone: options.recipientPhone }).select("_id").lean()
    : null;

  const slotQuery: any = {
    status: "sent",
    recipientId: { $ne: null },
  };

  if (sender?._id) slotQuery.ownerId = sender._id;
  if (recipient?._id) slotQuery.recipientId = recipient._id;
  if (options.voucherId && mongoose.isValidObjectId(options.voucherId)) {
    slotQuery.voucherId = new Types.ObjectId(options.voucherId);
  }
  if (options.slotNumber) slotQuery.slotNumber = options.slotNumber;

  const sentSlots = await SpecialCreditModel.find(slotQuery)
    .populate("ownerId", "name phone level")
    .populate("recipientId", "name phone level")
    .sort({ sentAt: -1 })
    .lean();

  let candidateCount = 0;
  let repairedCount = 0;

  for (const slot of sentSlots as any[]) {
    const existingTransfer = await MlmTransferModel.findOne({
      sourceSlotId: slot._id,
    }).lean();
    if (existingTransfer) continue;

    const senderDoc: any = slot.ownerId;
    const recipientDoc: any = slot.recipientId;
    if (!senderDoc?._id || !recipientDoc?._id) {
      console.log(`skip slot ${slot._id}: sender/recipient missing`);
      continue;
    }
    candidateCount += 1;

    const recipientLevel =
      Number(recipientDoc.level || 0) || Number(senderDoc.level || 0) + 1;
    const numRecipientSlots = computeRecipientSlots(
      Number(slot.creditAmount || 0),
      recipientLevel,
    );
    const slotCreditAmount = Math.max(
      1,
      Math.round(Number(slot.creditAmount || 0) / Math.max(1, numRecipientSlots)),
    );
    const timerStartedAt = slot.sentAt ? new Date(slot.sentAt) : new Date();
    const expiresAt = slot.expiresAt
      ? new Date(slot.expiresAt)
      : new Date(
          timerStartedAt.getTime() +
            VOUCHER_PURCHASE_TIMEOUT_MINUTES * 60 * 1000,
        );
    const requiredVoucherCount = await getVoucherTemplateRequiredCount(
      slot.voucherId || null,
    );
    const currentVoucherCount = await getVoucherCountForTemplate(
      recipientDoc._id.toString(),
      slot.voucherId || null,
    );
    const isExpired = expiresAt.getTime() <= Date.now();
    const shouldUnlockImmediately =
      currentVoucherCount >= requiredVoucherCount && !isExpired;
    const transferStatus: "pending_unlock" | "unlocked" | "returned_timeout" =
      shouldUnlockImmediately
      ? "unlocked"
      : isExpired
        ? "returned_timeout"
        : "pending_unlock";

    const existingChildSlots = await SpecialCreditModel.find({
      ownerId: recipientDoc._id,
      sourceSlotId: slot._id,
    })
      .sort({ slotNumber: 1 })
      .lean();

    console.log(
      `orphan slot ${slot._id} sender=${senderDoc.phone} recipient=${recipientDoc.phone} voucher=${slot.voucherId || "none"} slotNumber=${slot.slotNumber}`,
    );
    console.log(
      `  transfer: amount=${Number(slot.creditAmount).toLocaleString()} recipientLevel=${recipientLevel} childSlots=${numRecipientSlots} childAmount=${slotCreditAmount.toLocaleString()} status=${transferStatus}`,
    );
    console.log(
      `  vouchers: current=${currentVoucherCount} required=${requiredVoucherCount} expiresAt=${expiresAt.toISOString()}`,
    );
    console.log(`  existing child slots: ${existingChildSlots.length}`);

    if (!options.apply) {
      console.log("  dry-run: no changes applied");
      continue;
    }

    if (transferStatus === "returned_timeout") {
      console.log("  skipped apply: slot already expired");
      continue;
    }

    const transfer = await MlmTransferModel.create({
      senderId: senderDoc._id,
      receiverId: recipientDoc._id,
      voucherId: slot.voucherId || undefined,
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
      status: transferStatus,
    });

    const occupiedNumbers = new Set(
      existingChildSlots.map((child: any) => Number(child.slotNumber)),
    );
    for (let i = 1; i <= numRecipientSlots; i += 1) {
      if (occupiedNumbers.has(i)) {
        await SpecialCreditModel.updateOne(
          { _id: existingChildSlots.find((child: any) => child.slotNumber === i)?._id },
          {
            $set: {
              transferId: transfer._id,
              isLocked: !shouldUnlockImmediately,
              lockReason: !shouldUnlockImmediately
                ? `Need ${requiredVoucherCount} vouchers for this campaign`
                : null,
              lockExpiresAt: !shouldUnlockImmediately ? expiresAt : null,
              unlockedAt: shouldUnlockImmediately ? new Date() : null,
            },
          },
        );
        continue;
      }

      await SpecialCreditModel.create({
        ownerId: recipientDoc._id,
        voucherId: slot.voucherId || undefined,
        slotNumber: i,
        creditAmount: slotCreditAmount,
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
    }

    if (options.syncWallet) {
      await maybeCreditWallet(
        recipientDoc._id.toString(),
        Number(slot.creditAmount || 0),
        true,
      );
    }

    await syncUserSpecialCreditSummary(senderDoc._id.toString());
    await syncUserSpecialCreditSummary(recipientDoc._id.toString());

    repairedCount += 1;
    console.log(`  repaired transfer ${transfer._id}`);
  }

  console.log(
    options.apply
      ? `\nrepair complete: ${repairedCount} orphan transfer(s) repaired`
      : `\ndry-run complete: ${candidateCount} orphan transfer(s) would be repaired with --apply`,
  );

  await mongoose.connection.close();
}

run().catch((error) => {
  console.error("repair-orphan-special-credit-transfers failed", error);
  process.exit(1);
});

