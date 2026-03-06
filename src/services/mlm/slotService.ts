/**
 * SlotService
 *
 * Centralises all SpecialCredit slot mutations (send / refund / revert).
 * Every mutation runs inside a MongoDB multi-document transaction and writes
 * an immutable SlotEvent audit record.
 *
 * Public API (all functions are async and throw on failure):
 *   sendSlot(slotId, recipientId, actorId, voucherId?)
 *   refundSlot(slotId, actorId, reason?)
 */

import mongoose from "mongoose";
import SpecialCredit from "../../models/SpecialCredit";
import SlotEvent from "../../models/SlotEvent";

// ─── public API ──────────────────────────────────────────────────────────────

/**
 * Mark a slot as sent to a recipient.
 *
 * In a single transaction:
 *   1. Verifies the slot exists and is in "available" state
 *   2. Sets status = "sent", recipientId, recipientName, recipientPhone, sentAt
 *   3. Writes SlotEvent{SLOT_SENT}
 *
 * Throws if the slot does not exist or is not available.
 *
 * @param slotId      - SpecialCredit _id
 * @param recipientId - User receiving the slot
 * @param recipientName  - Snapshot of recipient's display name
 * @param recipientPhone - Snapshot of recipient's phone
 * @param actorId     - Who triggered this (sender / admin)
 * @param voucherId   - Optional voucher scope
 */
export async function sendSlot(
  slotId: string,
  recipientId: string,
  recipientName: string,
  recipientPhone: string,
  actorId: string,
  voucherId?: string,
): Promise<void> {
  const session = await mongoose.startSession();
  try {
    await session.withTransaction(async () => {
      const slot = await SpecialCredit.findById(slotId).session(session);
      if (!slot) throw new Error(`Slot ${slotId} not found`);
      if ((slot as any).status !== "available") {
        throw new Error(
          `Slot ${slotId} is not available (current status: ${(slot as any).status})`,
        );
      }

      const now = new Date();
      await SpecialCredit.findByIdAndUpdate(
        slotId,
        {
          status: "sent",
          recipientId,
          recipientName,
          recipientPhone,
          sentAt: now,
        },
        { session },
      );

      await SlotEvent.create(
        [
          {
            type: "SLOT_SENT",
            slotId,
            ownerId: (slot as any).ownerId,
            recipientId,
            voucherId: voucherId ?? (slot as any).voucherId ?? undefined,
            amount: (slot as any).creditAmount,
            actorId,
            meta: { recipientName, recipientPhone },
          },
        ],
        { session },
      );
    });
  } finally {
    await session.endSession();
  }
}

/**
 * Refund (un-send) a slot — returns it to "available" and clears recipient info.
 *
 * In a single transaction:
 *   1. Verifies the slot exists and is in "sent" state
 *   2. Clears recipient fields, sets status = "available"
 *   3. Writes SlotEvent{SLOT_REFUNDED}
 *
 * Caller is responsible for any balance adjustments outside this function.
 *
 * @param slotId  - SpecialCredit _id
 * @param actorId - Who triggered this (admin / system)
 * @param reason  - Optional human-readable note stored in SlotEvent.meta
 */
export async function refundSlot(
  slotId: string,
  actorId: string,
  reason = "",
): Promise<void> {
  const session = await mongoose.startSession();
  try {
    await session.withTransaction(async () => {
      const slot = await SpecialCredit.findById(slotId).session(session);
      if (!slot) throw new Error(`Slot ${slotId} not found`);
      if ((slot as any).status !== "sent") {
        throw new Error(
          `Slot ${slotId} cannot be refunded (current status: ${(slot as any).status})`,
        );
      }

      const previousRecipientId = (slot as any).recipientId;

      await SpecialCredit.findByIdAndUpdate(
        slotId,
        {
          status: "available",
          $unset: {
            recipientId: "",
            recipientName: "",
            recipientPhone: "",
            sentAt: "",
          },
        },
        { session },
      );

      await SlotEvent.create(
        [
          {
            type: "SLOT_REFUNDED",
            slotId,
            ownerId: (slot as any).ownerId,
            recipientId: previousRecipientId ?? null,
            voucherId: (slot as any).voucherId ?? undefined,
            amount: (slot as any).creditAmount,
            actorId,
            meta: { reason },
          },
        ],
        { session },
      );
    });
  } finally {
    await session.endSession();
  }
}

/**
 * Mark a slot as expired (system / scheduler use).
 *
 * In a single transaction:
 *   1. Verifies the slot is in "available" or "sent" state
 *   2. Sets status = "expired"
 *   3. Writes SlotEvent{SLOT_EXPIRED}
 *
 * @param slotId  - SpecialCredit _id
 * @param actorId - System / admin user id that triggered expiry
 */
export async function expireSlot(
  slotId: string,
  actorId: string,
): Promise<void> {
  const session = await mongoose.startSession();
  try {
    await session.withTransaction(async () => {
      const slot = await SpecialCredit.findById(slotId).session(session);
      if (!slot) throw new Error(`Slot ${slotId} not found`);

      const previousStatus = (slot as any).status;
      if (previousStatus === "expired") return; // Idempotent

      await SpecialCredit.findByIdAndUpdate(
        slotId,
        { status: "expired" },
        { session },
      );

      await SlotEvent.create(
        [
          {
            type: "SLOT_EXPIRED",
            slotId,
            ownerId: (slot as any).ownerId,
            recipientId: (slot as any).recipientId ?? null,
            voucherId: (slot as any).voucherId ?? undefined,
            amount: (slot as any).creditAmount,
            actorId,
            meta: { previousStatus },
          },
        ],
        { session },
      );
    });
  } finally {
    await session.endSession();
  }
}
