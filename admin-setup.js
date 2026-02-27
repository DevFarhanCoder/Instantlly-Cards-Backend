/**
 * Admin Setup Script
 * - Set admin credits (14648436000 x 10 slots)
 * - Set admin voucherBalance = 122070300 (stored as a number field, NO documents)
 * - Remove all accounts for Jatin, Mohammad Farhan, Muskan
 * - Recreate Farhan + Muskan as fresh users
 * - Send slot 1 credits + 5 vouchers to Farhan
 * - Send slot 2 credits + 5 vouchers to Muskan
 *
 * To change values later, edit only these 3 constants and re-run:
 */
const ADMIN_CREDITS_PER_SLOT = 14648436000;
const ADMIN_TOTAL_SLOTS = 10;
const ADMIN_VOUCHER_BALANCE = 122070300; // Stored as a number field, not as documents

// ─────────────────────────────────────────────────────────────────────────────

const mongoose = require("mongoose");
const bcrypt = require("bcryptjs");
require("dotenv").config();

const ADMIN_PHONE = "+919867477227";
const FARHAN_PHONE = "+919867969445";
const MUSKAN_PHONE = "+918828188930";

const UserSchema = new mongoose.Schema({}, { timestamps: true, strict: false });
const MlmWalletSchema = new mongoose.Schema(
  {},
  { timestamps: true, strict: false },
);
const VoucherSchema = new mongoose.Schema(
  {},
  { timestamps: true, strict: false },
);
const SpecialCreditSchema = new mongoose.Schema(
  {},
  { timestamps: true, strict: false },
);

const User = mongoose.model("User", UserSchema);
const MlmWallet = mongoose.model("MlmWallet", MlmWalletSchema);
const Voucher = mongoose.model("Voucher", VoucherSchema);
const SpecialCredit = mongoose.model("SpecialCredit", SpecialCreditSchema);

async function adminSetup() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI);
    console.log("✅ Connected to MongoDB\n");

    // ── 1. Find Admin ──────────────────────────────────────────────────────
    const admin = await User.findOne({ phone: ADMIN_PHONE });
    if (!admin) throw new Error("Admin (Rajesh Modi) not found");
    console.log(`👑 Admin found: ${admin.name} (${admin._id})`);

    // ── 2. Set Admin Wallet ─────────────────────────────────────────────────
    const totalCredits = ADMIN_CREDITS_PER_SLOT * ADMIN_TOTAL_SLOTS;
    await MlmWallet.updateOne(
      { userId: admin._id },
      { $set: { creditBalance: totalCredits } },
      { upsert: true },
    );
    console.log(
      `✅ Admin wallet: ${totalCredits.toLocaleString()} credits (${ADMIN_CREDITS_PER_SLOT.toLocaleString()} × ${ADMIN_TOTAL_SLOTS})`,
    );

    // ── 3. Set Admin voucherBalance (NO documents created) ─────────────────
    await User.updateOne(
      { _id: admin._id },
      { $set: { voucherBalance: ADMIN_VOUCHER_BALANCE } },
    );
    console.log(
      `✅ Admin voucherBalance: ${ADMIN_VOUCHER_BALANCE.toLocaleString()} (stored as field)`,
    );

    // ── 4. Delete all accounts for Jatin, Farhan, Muskan/Muskaan ──────────────────
    const namesToDelete = [/jatin/i, /farhan/i, /musk(a|aa)n/i];
    const phonesToDelete = [FARHAN_PHONE, MUSKAN_PHONE];
    for (const nameRegex of namesToDelete) {
      const users = await User.find({
        name: nameRegex,
        phone: { $ne: ADMIN_PHONE },
      });
      for (const u of users) {
        await Voucher.deleteMany({ userId: u._id });
        await MlmWallet.deleteMany({ userId: u._id });
        await SpecialCredit.deleteMany({
          $or: [{ ownerId: u._id }, { recipientId: u._id }],
        });
        await User.deleteOne({ _id: u._id });
        console.log(`🗑️  Deleted: ${u.name} (${u.phone})`);
      }
    }
    // Also delete by phone in case name differs
    for (const phone of phonesToDelete) {
      const u = await User.findOne({ phone });
      if (u && u.phone !== ADMIN_PHONE) {
        await Voucher.deleteMany({ userId: u._id });
        await MlmWallet.deleteMany({ userId: u._id });
        await SpecialCredit.deleteMany({
          $or: [{ ownerId: u._id }, { recipientId: u._id }],
        });
        await User.deleteOne({ _id: u._id });
        console.log(`🗑️  Deleted by phone: ${u.name} (${u.phone})`);
      }
    }

    // Clear stale admin slots
    await SpecialCredit.deleteMany({ ownerId: admin._id });
    // Reset admin stats
    await User.updateOne(
      { _id: admin._id },
      {
        $set: {
          "specialCredits.availableSlots": ADMIN_TOTAL_SLOTS,
          "specialCredits.usedSlots": 0,
          "specialCredits.totalSent": 0,
          directCount: 0,
          downlineCount: 0,
        },
      },
    );
    console.log("✅ Cleared admin slots and reset stats");

    const now = new Date();
    const expiryDate = new Date(now.getTime() + 365 * 24 * 60 * 60 * 1000);

    // ── 5. Recreate Farhan ──────────────────────────────────────────────────
    const farhanPassword = await bcrypt.hash("farhan123", 12);
    const farhan = await User.create({
      name: "Mohammad Farhan",
      phone: FARHAN_PHONE,
      password: farhanPassword,
      level: 1,
      parentId: admin._id,
      voucherBalance: 0,
    });
    await MlmWallet.create({ userId: farhan._id, creditBalance: 0 });
    await User.updateOne(
      { _id: admin._id },
      { $inc: { directCount: 1, downlineCount: 1 } },
    );
    console.log(`✅ Created Mohammad Farhan`);

    // ── 6. Recreate Muskan ──────────────────────────────────────────────────
    const muskanPassword = await bcrypt.hash("muskan123", 12);
    const muskan = await User.create({
      name: "Muskan",
      phone: MUSKAN_PHONE,
      password: muskanPassword,
      level: 1,
      parentId: admin._id,
      voucherBalance: 0,
    });
    await MlmWallet.create({ userId: muskan._id, creditBalance: 0 });
    await User.updateOne(
      { _id: admin._id },
      { $inc: { directCount: 1, downlineCount: 1 } },
    );
    console.log(`✅ Created Muskan`);

    // ── 7. Create admin's 10 slots ──────────────────────────────────────────
    for (let slot = 1; slot <= ADMIN_TOTAL_SLOTS; slot++) {
      await SpecialCredit.create({
        ownerId: admin._id,
        slotNumber: slot,
        creditAmount: ADMIN_CREDITS_PER_SLOT,
        status: "available",
      });
    }
    console.log(`✅ Created ${ADMIN_TOTAL_SLOTS} special credit slots`);

    // ── 8. Send Slot 1 → Farhan ─────────────────────────────────────────────
    await SpecialCredit.updateOne(
      { ownerId: admin._id, slotNumber: 1 },
      {
        $set: {
          status: "sent",
          recipientId: farhan._id,
          recipientName: farhan.name,
          recipientPhone: farhan.phone,
          sentAt: now,
        },
      },
    );
    await MlmWallet.updateOne(
      { userId: farhan._id },
      { $set: { creditBalance: ADMIN_CREDITS_PER_SLOT } },
    );
    await Voucher.insertMany(
      Array.from({ length: 5 }).map((_, i) => ({
        userId: farhan._id,
        originalOwner: admin._id,
        voucherNumber: `ADM-F-${Date.now()}-${i}`,
        companyName: "Instantlly",
        MRP: 1200,
        source: "admin",
        issueDate: now,
        expiryDate,
        redeemedStatus: "unredeemed",
        maxUses: 1,
        remainingUses: 1,
        transferredFrom: admin._id,
        transferredAt: now,
        transferHistory: [
          { from: admin._id, to: farhan._id, transferredAt: now },
        ],
      })),
    );
    await User.updateOne(
      { _id: admin._id },
      {
        $inc: { voucherBalance: -5 },
        $set: {
          "specialCredits.usedSlots": 1,
          "specialCredits.availableSlots": ADMIN_TOTAL_SLOTS - 1,
        },
      },
    );
    console.log(
      `✅ Slot 1 → Farhan: ${ADMIN_CREDITS_PER_SLOT.toLocaleString()} credits + 5 vouchers`,
    );

    // ── 9. Send Slot 2 → Muskan ─────────────────────────────────────────────
    await SpecialCredit.updateOne(
      { ownerId: admin._id, slotNumber: 2 },
      {
        $set: {
          status: "sent",
          recipientId: muskan._id,
          recipientName: muskan.name,
          recipientPhone: muskan.phone,
          sentAt: now,
        },
      },
    );
    await MlmWallet.updateOne(
      { userId: muskan._id },
      { $set: { creditBalance: ADMIN_CREDITS_PER_SLOT } },
    );
    await Voucher.insertMany(
      Array.from({ length: 5 }).map((_, i) => ({
        userId: muskan._id,
        originalOwner: admin._id,
        voucherNumber: `ADM-M-${Date.now()}-${i}`,
        companyName: "Instantlly",
        MRP: 1200,
        source: "admin",
        issueDate: now,
        expiryDate,
        redeemedStatus: "unredeemed",
        maxUses: 1,
        remainingUses: 1,
        transferredFrom: admin._id,
        transferredAt: now,
        transferHistory: [
          { from: admin._id, to: muskan._id, transferredAt: now },
        ],
      })),
    );
    await User.updateOne(
      { _id: admin._id },
      {
        $inc: { voucherBalance: -5 },
        $set: {
          "specialCredits.usedSlots": 2,
          "specialCredits.availableSlots": ADMIN_TOTAL_SLOTS - 2,
        },
      },
    );
    console.log(
      `✅ Slot 2 → Muskan: ${ADMIN_CREDITS_PER_SLOT.toLocaleString()} credits + 5 vouchers`,
    );

    // ── Summary ──────────────────────────────────────────────────────────────
    const finalAdmin = await User.findById(admin._id);
    const finalWallet = await MlmWallet.findOne({ userId: admin._id });
    console.log("\n" + "=".repeat(60));
    console.log("✅ ADMIN SETUP COMPLETE");
    console.log("=".repeat(60));
    console.log(
      `Admin Credits:       ${finalWallet?.creditBalance?.toLocaleString()}`,
    );
    console.log(
      `Admin Vouchers Left: ${finalAdmin?.voucherBalance?.toLocaleString()}`,
    );
    console.log(
      `Farhan Credits:      ${ADMIN_CREDITS_PER_SLOT.toLocaleString()} + 5 vouchers`,
    );
    console.log(
      `Muskan Credits:      ${ADMIN_CREDITS_PER_SLOT.toLocaleString()} + 5 vouchers`,
    );
    console.log(`Admin Slots:         8 available, 2 used`);
    console.log("\nLogin Credentials:");
    console.log(`  Farhan: ${FARHAN_PHONE} / farhan123`);
    console.log(`  Muskan: ${MUSKAN_PHONE} / muskan123`);
    console.log("=".repeat(60));

    await mongoose.disconnect();
  } catch (error) {
    console.error("❌ Error:", error.message || error);
    process.exit(1);
  }
}

adminSetup();
