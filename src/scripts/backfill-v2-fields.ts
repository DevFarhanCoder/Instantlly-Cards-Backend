/**
 * V2 Backfill Migration Script
 * ─────────────────────────────
 * Run ONCE after deploying the V2 code to populate the new fields on
 * existing documents and seed the NetworkRules singleton.
 *
 * What it does:
 *   1. Seeds the default NetworkRules document (admin cap = 3, depth = 10)
 *   2. Backfills User.ancestors for every user that has a parentId
 *   3. Recalculates User.directCount from actual child documents
 *   4. Recalculates User.downlineCount (all descendants)
 *   5. Sets User.role based on User.isVoucherAdmin
 *
 * Safe to re-run: every step is idempotent.
 *
 * Usage:
 *   npx ts-node src/scripts/backfill-v2-fields.ts
 * or
 *   npx tsx src/scripts/backfill-v2-fields.ts
 */

import mongoose from "mongoose";
import dotenv from "dotenv";

dotenv.config();

// ─── model imports ────────────────────────────────────────────────────────────
// Import models directly (not via re-export) so this script can run standalone.
import User from "../models/User";
import NetworkRules from "../models/NetworkRules";

const MONGODB_URI = process.env.MONGODB_URI;

if (!MONGODB_URI) {
  console.error("❌  MONGODB_URI environment variable is not set.");
  process.exit(1);
}

// ─── helpers ─────────────────────────────────────────────────────────────────

/**
 * Walk up the parentId chain and return the ordered ancestor array
 * [ root, ..., immediateParent ] for the given userId.
 */
async function computeAncestors(
  userId: string,
): Promise<mongoose.Types.ObjectId[]> {
  const chain: mongoose.Types.ObjectId[] = [];
  const visited = new Set<string>();
  let current = await User.findById(userId).select("parentId").lean();
  let parentId: mongoose.Types.ObjectId | null =
    (current as any)?.parentId ?? null;

  while (parentId) {
    const pStr = parentId.toString();
    if (visited.has(pStr)) {
      console.warn(
        `  ⚠  Cycle detected at ${pStr} while computing ancestors for ${userId}`,
      );
      break;
    }
    visited.add(pStr);
    chain.unshift(parentId); // prepend: oldest ancestor first
    const parent = await User.findById(parentId).select("parentId").lean();
    parentId = (parent as any)?.parentId ?? null;
  }

  return chain;
}

/**
 * Count all descendants recursively.
 */
async function countDescendants(userId: string): Promise<number> {
  const children = await User.find({ parentId: userId }).select("_id").lean();
  let total = children.length;
  for (const child of children) {
    total += await countDescendants((child._id as any).toString());
  }
  return total;
}

// ─── migration steps ─────────────────────────────────────────────────────────

async function seedNetworkRules() {
  console.log("\n[1/5] Seeding NetworkRules...");
  const existing = await NetworkRules.findById("default");
  if (existing) {
    console.log("  ✔  NetworkRules 'default' already exists — skipping.");
    return;
  }
  await NetworkRules.create({
    _id: "default",
    maxDirectByRole: { admin: 3, user: 999999 },
    maxDepth: 10,
  });
  console.log("  ✔  Created NetworkRules 'default'.");
}

async function backfillAncestors() {
  console.log("\n[2/5] Backfilling User.ancestors...");
  const usersWithParent = await User.find({
    parentId: { $exists: true, $ne: null },
  })
    .select("_id parentId ancestors")
    .lean();

  console.log(`  Found ${usersWithParent.length} users with a parentId.`);
  let updated = 0;

  for (const user of usersWithParent) {
    // Only update if ancestors array is empty / missing
    if ((user as any).ancestors?.length) continue;

    const ancestors = await computeAncestors((user._id as any).toString());
    await User.findByIdAndUpdate(user._id, { ancestors });
    updated++;

    if (updated % 100 === 0) {
      console.log(`  ... updated ${updated}/${usersWithParent.length}`);
    }
  }

  console.log(`  ✔  Updated ancestors for ${updated} users.`);
}

async function recalcDirectCounts() {
  console.log("\n[3/5] Recalculating User.directCount...");
  const allUsers = await User.find({}).select("_id").lean();
  let updated = 0;

  for (const user of allUsers) {
    const count = await User.countDocuments({
      parentId: user._id,
      isDeleted: { $ne: true },
    });
    await User.findByIdAndUpdate(user._id, { directCount: count });
    updated++;
  }

  console.log(`  ✔  Recalculated directCount for ${updated} users.`);
}

async function recalcDownlineCounts() {
  console.log(
    "\n[4/5] Recalculating User.downlineCount (recursive, may be slow)...",
  );
  const allUsers = await User.find({}).select("_id").lean();
  let updated = 0;

  for (const user of allUsers) {
    const count = await countDescendants((user._id as any).toString());
    await User.findByIdAndUpdate(user._id, { downlineCount: count });
    updated++;

    if (updated % 50 === 0) {
      console.log(`  ... processed ${updated}/${allUsers.length}`);
    }
  }

  console.log(`  ✔  Recalculated downlineCount for ${updated} users.`);
}

async function backfillRoles() {
  console.log("\n[5/5] Backfilling User.role from isVoucherAdmin...");
  const [adminResult, userResult] = await Promise.all([
    User.updateMany(
      { isVoucherAdmin: true, role: { $ne: "admin" } },
      { $set: { role: "admin" } },
    ),
    User.updateMany(
      { isVoucherAdmin: { $ne: true }, role: { $exists: false } },
      { $set: { role: "user" } },
    ),
  ]);
  console.log(`  ✔  Set role="admin" on ${adminResult.modifiedCount} users.`);
  console.log(`  ✔  Set role="user"  on ${userResult.modifiedCount} users.`);
}

// ─── main ─────────────────────────────────────────────────────────────────────

async function main() {
  console.log("=== V2 Backfill Migration ===");
  console.log(`Connecting to MongoDB...`);

  await mongoose.connect(MONGODB_URI as string);
  console.log("Connected.\n");

  try {
    await seedNetworkRules();
    await backfillAncestors();
    await recalcDirectCounts();
    await recalcDownlineCounts();
    await backfillRoles();

    console.log("\n✅  All steps completed successfully.");
  } catch (err) {
    console.error("\n❌  Migration failed:", err);
    process.exit(1);
  } finally {
    await mongoose.disconnect();
    console.log("Disconnected from MongoDB.");
  }
}

main();
