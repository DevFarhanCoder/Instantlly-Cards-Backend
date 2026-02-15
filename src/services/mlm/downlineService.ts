// ✅ DOWNLINE TRACKING SERVICE
// Updates downlineCount for all ancestors when a new member joins

import User from "../../models/User";

/**
 * Update downline count for all ancestors when a new user joins
 * @param userId - New user who just joined
 */
export async function updateAncestorDownlineCounts(userId: string) {
  const user = await User.findById(userId);
  if (!user || !(user as any).parentId) {
    return; // Root user or no parent
  }

  // Traverse up the tree and increment downlineCount for all ancestors
  let currentParentId = (user as any).parentId;
  const updatedAncestors: string[] = [];

  while (currentParentId) {
    await User.findByIdAndUpdate(currentParentId, {
      $inc: { downlineCount: 1 },
    });

    updatedAncestors.push(currentParentId.toString());

    // Get next parent
    const parent = await User.findById(currentParentId);
    if (!parent || !(parent as any).parentId) {
      break;
    }
    currentParentId = (parent as any).parentId;
  }

  console.log(
    `✅ Updated downlineCount for ${updatedAncestors.length} ancestors`,
  );
  return updatedAncestors;
}

/**
 * Calculate total downline count for a user (recursive count of all descendants)
 * Use this for one-time fixes or validation
 * @param userId - User ID to calculate for
 * @returns Total descendant count
 */
export async function calculateDownlineCount(userId: string): Promise<number> {
  const directChildren = await User.find({ parentId: userId }).lean();

  if (directChildren.length === 0) {
    return 0;
  }

  let total = directChildren.length;

  for (const child of directChildren) {
    const childDownline = await calculateDownlineCount(
      (child._id as any).toString(),
    );
    total += childDownline;
  }

  return total;
}

/**
 * Recalculate and update downlineCount for all users (admin utility)
 * Use this if downlineCount gets out of sync
 */
export async function recalculateAllDownlineCounts() {
  const allUsers = await User.find({}).lean();

  for (const user of allUsers) {
    const downlineCount = await calculateDownlineCount(
      (user._id as any).toString(),
    );
    await User.findByIdAndUpdate(user._id, { downlineCount });
    console.log(
      `✅ Updated ${(user as any).name}: ${downlineCount} downline members`,
    );
  }

  console.log(`✅ Recalculated downlineCount for ${allUsers.length} users`);
}
