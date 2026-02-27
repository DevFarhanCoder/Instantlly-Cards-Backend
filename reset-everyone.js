/**
 * reset-everyone.js
 * ─────────────────────────────────────────────────────────────────────────────
 * Resets ALL non-admin users' credits and vouchers so you can test from scratch.
 *
 * What it does:
 *  1. Zeros out MlmWallet.creditBalance for every non-admin user
 *  2. Zeros out User.voucherBalance for every non-admin user
 *  3. Deletes all Voucher documents owned by non-admin users
 *  4. Resets all SpecialCredit slots back to "available" (so admin can re-send)
 *  5. Resets admin's usedSlots → 0, availableSlots → 10
 *
 * Admin credits and voucherBalance are NOT touched.
 */

const mongoose = require("mongoose");
require("dotenv").config();

const ADMIN_PHONE = "+919867477227";
const ADMIN_TOTAL_SLOTS = 10;

const User = mongoose.model(
  "User",
  new mongoose.Schema({}, { strict: false, timestamps: true }),
);
const MlmWallet = mongoose.model(
  "MlmWallet",
  new mongoose.Schema({}, { strict: false, timestamps: true }),
);
const Voucher = mongoose.model(
  "Voucher",
  new mongoose.Schema({}, { strict: false, timestamps: true }),
);
const SpecialCredit = mongoose.model(
  "SpecialCredit",
  new mongoose.Schema({}, { strict: false, timestamps: true }),
);

async function resetEveryone() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI);
    console.log("✅ Connected\n");

    // ── Find admin ──────────────────────────────────────────────────────────
    const admin = await User.findOne({ phone: ADMIN_PHONE });
    if (!admin) throw new Error("Admin not found — check ADMIN_PHONE");
    console.log(`👑 Admin: ${admin.name} (${admin._id})`);

    // ── 1. Zero out all non-admin MlmWallets ────────────────────────────────
    const walletResult = await MlmWallet.updateMany(
      { userId: { $ne: admin._id } },
      { $set: { creditBalance: 0 } },
    );
    console.log(
      `✅ Reset ${walletResult.modifiedCount} non-admin wallets → 0 credits`,
    );

    // ── 2. Zero out all non-admin voucherBalance ─────────────────────────────
    const userResult = await User.updateMany(
      { _id: { $ne: admin._id } },
      { $set: { voucherBalance: 0 } },
    );
    console.log(
      `✅ Reset ${userResult.modifiedCount} non-admin users → voucherBalance: 0`,
    );

    // ── 3. Delete all Voucher docs belonging to non-admin users ──────────────
    const nonAdminUsers = await User.find({ _id: { $ne: admin._id } }, "_id");
    const nonAdminIds = nonAdminUsers.map((u) => u._id);

    const voucherResult = await Voucher.deleteMany({
      $or: [
        { ownerId: { $in: nonAdminIds } },
        { userId: { $in: nonAdminIds } },
        { recipientId: { $in: nonAdminIds } },
      ],
    });
    console.log(`✅ Deleted ${voucherResult.deletedCount} voucher documents`);

    // ── 4. Reset all SpecialCredit slots → available ─────────────────────────
    const scResult = await SpecialCredit.updateMany(
      { ownerId: admin._id },
      {
        $set: {
          status: "available",
          recipientId: null,
          recipientName: null,
          recipientPhone: null,
          sentAt: null,
        },
      },
    );
    console.log(
      `✅ Reset ${scResult.modifiedCount} SpecialCredit slots → available`,
    );

    // ── 5. Reset admin slot counters ─────────────────────────────────────────
    await User.updateOne(
      { _id: admin._id },
      {
        $set: {
          "specialCredits.usedSlots": 0,
          "specialCredits.availableSlots": ADMIN_TOTAL_SLOTS,
          "specialCredits.totalSent": 0,
          directCount: 0,
          downlineCount: 0,
        },
      },
    );
    console.log(
      `✅ Admin slot counters reset (usedSlots: 0, availableSlots: ${ADMIN_TOTAL_SLOTS})`,
    );

    // ── Summary ──────────────────────────────────────────────────────────────
    const adminWallet = await MlmWallet.findOne({ userId: admin._id });
    const adminUser = await User.findById(admin._id);
    console.log("\n" + "=".repeat(60));
    console.log("✅ RESET COMPLETE — Admin data preserved");
    console.log("=".repeat(60));
    console.log(
      `Admin Credits:        ${adminWallet?.creditBalance?.toLocaleString()} (untouched)`,
    );
    console.log(
      `Admin Vouchers:       ${adminUser?.voucherBalance?.toLocaleString()} (untouched)`,
    );
    console.log(`Non-admin wallets:    zeroed`);
    console.log(`Non-admin vouchers:   deleted`);
    console.log(`SpecialCredit slots:  all available for re-sending`);
    console.log("=".repeat(60));
    console.log(
      "\nYou can now run admin-setup.js to re-distribute, or test manually.",
    );

    await mongoose.disconnect();
  } catch (err) {
    console.error("❌ Error:", err.message || err);
    process.exit(1);
  }
}

resetEveryone();
