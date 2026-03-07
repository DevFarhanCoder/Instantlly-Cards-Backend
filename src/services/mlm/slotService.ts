/**
 * SlotService
 *
 * Centralises all SpecialCredit slot mutations (send / refund / revert).
 * Every mutation writes an immutable SlotEvent audit record.
 *
 * Note: Transactions are NOT used here because the MongoDB instance is a
 * standalone server (not a replica set), which does not support multi-document
 * transactions. Operations are performed sequentially; partial failures are
 * surfaced as thrown errors.
 *
 * Public API (all functions are async and throw on failure):
 *   sendSlot(slotId, recipientId, actorId, voucherId?)
 *   refundSlot(slotId, actorId, reason?)
 *   expireSlot(slotId, actorId)
 */

import SpecialCredit from "../../models/SpecialCredit";
import SlotEvent from "../../models/SlotEvent";

// â”€â”€â”€ public API â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

/**
 * Mark a slot as sent to a recipient.
 *
 * 1. Verifies the slot exists and is in "available" state
 * 2. Sets status = "sent", recipientId, recipientName, recipientPhone, sentAt
 * 3. Writes SlotEvent{SLOT_SENT}
 */
export async function sendSlot(
  slotId: string,
  recipientId: string,
  recipientName: string,
  recipientPhone: string,
  actorId: string,
  voucherId?: string,
  expiresAt?: Date,
): Promise<void> {
  const slot = await SpecialCredit.findById(slotId);
  if (!slot) throw new Error(`Slot ${slotId} not found`);
  if ((slot as any).status !== "available") {
    throw new Error(
      `Slot ${slotId} is not available (current status: ${(slot as any).status})`,
    );
  }

  const now = new Date();
  await SpecialCredit.findByIdAndUpdate(slotId, {
    status: "sent",
    recipientId,
    recipientName,
    recipientPhone,
    sentAt: now,
    ...(expiresAt ? { expiresAt } : {}),
  });

  await SlotEvent.create({
    type: "SLOT_SENT",
    slotId,
    ownerId: (slot as any).ownerId,
    recipientId,
    voucherId: voucherId ?? (slot as any).voucherId ?? undefined,
    amount: (slot as any).creditAmount,
    actorId,
    meta: { recipientName, recipientPhone },
  });
}

/**
 * Refund (un-send) a slot â€” returns it to "available" and clears recipient info.
 *
 * 1. Verifies the slot exists and is in "sent" state
 * 2. Clears recipient fields, sets status = "available"
 * 3. Writes SlotEvent{SLOT_REFUNDED}
 */
export async function refundSlot(
  slotId: string,
  actorId: string,
  reason = "",
): Promise<void> {
  const slot = await SpecialCredit.findById(slotId);
  if (!slot) throw new Error(`Slot ${slotId} not found`);
  if ((slot as any).status !== "sent") {
    throw new Error(
      `Slot ${slotId} cannot be refunded (current status: ${(slot as any).status})`,
    );
  }

  const previousRecipientId = (slot as any).recipientId;

  await SpecialCredit.findByIdAndUpdate(slotId, {
    status: "available",
    $unset: {
      recipientId: "",
      recipientName: "",
      recipientPhone: "",
      sentAt: "",
    },
  });

  await SlotEvent.create({
    type: "SLOT_REFUNDED",
    slotId,
    ownerId: (slot as any).ownerId,
    recipientId: previousRecipientId ?? null,
    voucherId: (slot as any).voucherId ?? undefined,
    amount: (slot as any).creditAmount,
    actorId,
    meta: { reason },
  });
}

/**
 * Mark a slot as expired (system / scheduler use).
 *
 * 1. Verifies the slot is in "available" or "sent" state
 * 2. Sets status = "expired"
 * 3. Writes SlotEvent{SLOT_EXPIRED}
 */
export async function expireSlot(
  slotId: string,
  actorId: string,
): Promise<void> {
  const slot = await SpecialCredit.findById(slotId);
  if (!slot) throw new Error(`Slot ${slotId} not found`);

  const previousStatus = (slot as any).status;
  if (previousStatus === "expired") return; // Idempotent

  await SpecialCredit.findByIdAndUpdate(slotId, { status: "expired" });

  await SlotEvent.create({
    type: "SLOT_EXPIRED",
    slotId,
    ownerId: (slot as any).ownerId,
    recipientId: (slot as any).recipientId ?? null,
    voucherId: (slot as any).voucherId ?? undefined,
    amount: (slot as any).creditAmount,
    actorId,
    meta: { previousStatus },
  });
}
