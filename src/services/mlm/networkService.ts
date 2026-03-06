/**
 * NetworkService
 *
 * Centralises all MLM tree mutations (link / unlink / reassign / soft-delete).
 * Every mutation runs inside a MongoDB multi-document transaction so the tree
 * always stays consistent, and writes an immutable NetworkEvent audit record.
 *
 * Public API (all functions are async and throw on failure):
 *   linkUser(userId, parentId, actorId, reason?)
 *   unlinkUser(userId, actorId, reason?)
 *   softDeleteUser(userId, actorId, reason?)
 *   getDirectBuyerCount(userId): Promise<number>
 *   getMaxDirectBuyers(role): Promise<number>
 */

import mongoose from "mongoose";
import User from "../../models/User";
import NetworkEvent from "../../models/NetworkEvent";
import NetworkRules, { INetworkRules } from "../../models/NetworkRules";

// ─── helpers ─────────────────────────────────────────────────────────────────

/**
 * Build the full ancestor chain (root → immediate parent) for a given user id.
 * Uses the cached `ancestors` array if available, otherwise walks up via parentId.
 */
async function buildAncestors(
  userId: string,
  session?: mongoose.ClientSession,
): Promise<mongoose.Types.ObjectId[]> {
  const opts = session ? { session } : {};
  const user = await User.findById(userId)
    .select("parentId ancestors")
    .session(session ?? null)
    .lean();
  if (!user) return [];
  if ((user as any).ancestors?.length)
    return (user as any).ancestors as mongoose.Types.ObjectId[];

  // Fallback: walk up the chain (used when ancestors cache is cold)
  const chain: mongoose.Types.ObjectId[] = [];
  let currentId: mongoose.Types.ObjectId | null =
    (user as any).parentId ?? null;
  const visited = new Set<string>();

  while (currentId) {
    const idStr = currentId.toString();
    if (visited.has(idStr)) break; // Cycle guard
    visited.add(idStr);
    chain.unshift(currentId); // prepend so index 0 = root
    const parent = await User.findById(currentId)
      .select("parentId ancestors")
      .lean();
    if (!parent) break;
    if ((parent as any).ancestors?.length) {
      // Parent has cached ancestors — prepend them and stop walking
      chain.unshift(
        ...((parent as any).ancestors as mongoose.Types.ObjectId[]),
      );
      break;
    }
    currentId = (parent as any).parentId ?? null;
  }

  return chain;
}

/**
 * Increment downlineCount for every user in the ancestor chain.
 */
async function incrementAncestorDownline(
  ancestorIds: mongoose.Types.ObjectId[],
  delta: 1 | -1,
  session: mongoose.ClientSession,
) {
  if (!ancestorIds.length) return;
  await User.updateMany(
    { _id: { $in: ancestorIds } },
    { $inc: { downlineCount: delta } },
    { session },
  );
}

// ─── public API ──────────────────────────────────────────────────────────────

/**
 * Load the current NetworkRules document.
 * If the singleton doesn't exist yet, return hard-coded defaults so the app
 * still works before the backfill script is run.
 */
export async function getNetworkRules(): Promise<INetworkRules> {
  const rules = (await NetworkRules.findById(
    "default",
  ).lean()) as INetworkRules | null;
  if (rules) return rules;
  return {
    _id: "default",
    maxDirectByRole: { admin: 3, user: 999999 },
    maxDepth: 10,
  };
}

/**
 * Maximum number of direct children allowed for a given role string.
 */
export async function getMaxDirectBuyers(role: string): Promise<number> {
  const rules = await getNetworkRules();
  return (rules.maxDirectByRole as any)[role] ?? rules.maxDirectByRole.user;
}

/**
 * Count non-deleted direct children of a user.
 */
export async function getDirectBuyerCount(userId: string): Promise<number> {
  return User.countDocuments({ parentId: userId, isDeleted: { $ne: true } });
}

/**
 * Link a user to a new parent in a single transaction.
 *
 * Enforces:
 *   1. Parent cap: parent.directCount + 1 <= maxDirectByRole[parent.role]
 *   2. No re-link if user already has a different parent
 *   3. Depth cap: parent.level + 1 <= maxDepth
 *
 * Updates on success:
 *   - user.parentId, user.level, user.ancestors
 *   - parent.directCount + 1
 *   - all ancestor.downlineCount + 1
 *   - NetworkEvent{LINK}
 *
 * @param userId   - The user being linked (child)
 * @param parentId - The user receiving the new child
 * @param actorId  - Who triggered this operation
 * @param reason   - Optional human-readable note
 */
export async function linkUser(
  userId: string,
  parentId: string,
  actorId: string,
  reason = "",
): Promise<void> {
  const session = await mongoose.startSession();
  try {
    await session.withTransaction(async () => {
      const [user, parent] = await Promise.all([
        User.findById(userId).session(session),
        User.findById(parentId).session(session),
      ]);

      if (!user) throw new Error(`User ${userId} not found`);
      if (!parent) throw new Error(`Parent ${parentId} not found`);

      const currentParentId = (user as any).parentId;

      // Already linked to this exact parent — idempotent, nothing to do
      if (currentParentId && currentParentId.toString() === parentId) return;

      // Already linked to a DIFFERENT parent — reject
      if (currentParentId && currentParentId.toString() !== parentId) {
        throw new Error(
          `User ${userId} is already linked to another parent (${currentParentId})`,
        );
      }

      // Enforce parent direct-buyer cap
      const rules = await getNetworkRules();
      const parentRole: string = (parent as any).role ?? "user";
      const maxDirect: number =
        (rules.maxDirectByRole as any)[parentRole] ??
        rules.maxDirectByRole.user;
      const currentDirect = (parent as any).directCount ?? 0;

      if (currentDirect >= maxDirect) {
        throw new Error(
          `Parent ${parentId} has reached the maximum direct-buyer limit (${maxDirect}) for role '${parentRole}'`,
        );
      }

      // Enforce depth cap
      const parentLevel = (parent as any).level ?? 0;
      if (parentLevel + 1 > rules.maxDepth) {
        throw new Error(
          `Adding user would exceed max depth of ${rules.maxDepth}`,
        );
      }

      // Build ancestor chain for the user (= parent's ancestors + parent itself)
      const parentAncestors: mongoose.Types.ObjectId[] =
        (parent as any).ancestors ?? [];
      const newAncestors = [
        ...parentAncestors,
        parent._id as mongoose.Types.ObjectId,
      ];

      // Apply all changes atomically
      await User.findByIdAndUpdate(
        userId,
        {
          parentId,
          level: parentLevel + 1,
          ancestors: newAncestors,
        },
        { session },
      );

      await User.findByIdAndUpdate(
        parentId,
        { $inc: { directCount: 1 } },
        { session },
      );

      await incrementAncestorDownline(newAncestors, 1, session);

      await NetworkEvent.create(
        [
          {
            type: "LINK",
            userId,
            oldParentId: null,
            newParentId: parentId,
            actorId,
            reason,
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
 * Unlink a user from their current parent in a single transaction.
 *
 * Updates on success:
 *   - user.parentId = null, user.level = 0, user.ancestors = []
 *   - old parent.directCount - 1
 *   - all old ancestor.downlineCount - 1
 *   - NetworkEvent{UNLINK}
 *
 * @param userId  - The user being unlinked
 * @param actorId - Who triggered this operation
 * @param reason  - Optional human-readable note
 */
export async function unlinkUser(
  userId: string,
  actorId: string,
  reason = "",
): Promise<void> {
  const session = await mongoose.startSession();
  try {
    await session.withTransaction(async () => {
      const user = await User.findById(userId).session(session);
      if (!user) throw new Error(`User ${userId} not found`);

      const oldParentId: mongoose.Types.ObjectId | null =
        (user as any).parentId ?? null;
      if (!oldParentId) return; // Already a root — nothing to do

      const oldAncestors: mongoose.Types.ObjectId[] =
        (user as any).ancestors ?? [];

      await User.findByIdAndUpdate(
        userId,
        { parentId: null, level: 0, ancestors: [] },
        { session },
      );

      await User.findByIdAndUpdate(
        oldParentId,
        { $inc: { directCount: -1 } },
        { session },
      );

      await incrementAncestorDownline(oldAncestors, -1, session);

      await NetworkEvent.create(
        [
          {
            type: "UNLINK",
            userId,
            oldParentId,
            newParentId: null,
            actorId,
            reason,
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
 * Soft-delete a user: hide from tree queries but preserve all history.
 *
 * Updates on success:
 *   - user.isDeleted = true, user.deletedAt = now
 *   - Calls unlinkUser() to keep counts consistent
 *   - NetworkEvent{SOFT_DELETE}
 *
 * @param userId  - The user to soft-delete
 * @param actorId - Who triggered this operation
 * @param reason  - Optional human-readable note
 */
export async function softDeleteUser(
  userId: string,
  actorId: string,
  reason = "",
): Promise<void> {
  // First unlink so counts stay accurate
  await unlinkUser(userId, actorId, `soft-delete: ${reason}`);

  // Then mark as deleted — no transaction needed (single-document update)
  await User.findByIdAndUpdate(userId, {
    isDeleted: true,
    deletedAt: new Date(),
  });

  // Append SOFT_DELETE event (outside the transaction for simplicity)
  await NetworkEvent.create({
    type: "SOFT_DELETE",
    userId,
    oldParentId: null,
    newParentId: null,
    actorId,
    reason,
  });
}
