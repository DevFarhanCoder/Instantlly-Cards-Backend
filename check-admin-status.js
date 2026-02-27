/**
 * Check Admin's Special Credits & Vouchers Status
 */

const mongoose = require("mongoose");
require("dotenv").config();

const ADMIN_PHONE = "+919867477227";

const UserSchema = new mongoose.Schema({}, { timestamps: true, strict: false });
const SpecialCreditSchema = new mongoose.Schema(
  {},
  { timestamps: true, strict: false },
);
const VoucherSchema = new mongoose.Schema(
  {},
  { timestamps: true, strict: false },
);
const MlmWalletSchema = new mongoose.Schema(
  {},
  { timestamps: true, strict: false },
);

const User = mongoose.model("User", UserSchema);
const SpecialCredit = mongoose.model("SpecialCredit", SpecialCreditSchema);
const Voucher = mongoose.model("Voucher", VoucherSchema);
const MlmWallet = mongoose.model("MlmWallet", MlmWalletSchema);

async function checkAdminStatus() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI);
    console.log("✅ Connected to MongoDB\n");

    // Find Admin
    const admin = await User.findOne({ phone: ADMIN_PHONE });
    if (!admin) {
      console.error("❌ Admin not found");
      process.exit(1);
    }

    console.log("👑 ADMIN: Rajesh Modi");
    console.log("=".repeat(60));
    console.log({
      id: admin._id,
      name: admin.name,
      phone: admin.phone,
      level: admin.level,
      isVoucherAdmin: admin.isVoucherAdmin,
      specialCredits: admin.specialCredits,
    });

    // Check wallet
    const wallet = await MlmWallet.findOne({ userId: admin._id });
    console.log("\n💰 WALLET:");
    console.log("=".repeat(60));
    if (wallet) {
      console.log(
        `Credit Balance: ${wallet.creditBalance?.toLocaleString() || 0}`,
      );
    } else {
      console.log("❌ No wallet found");
    }

    // Check special credits slots
    const slots = await SpecialCredit.find({ ownerId: admin._id }).sort({
      slotNumber: 1,
    });
    console.log("\n🎯 SPECIAL CREDITS SLOTS:");
    console.log("=".repeat(60));
    console.log(`Total Slots: ${slots.length}`);

    if (slots.length > 0) {
      console.log("\n📋 Slot Details:");
      for (const slot of slots) {
        const recipient = slot.recipientId
          ? await User.findById(slot.recipientId).select("name phone")
          : null;
        console.log(`\nSlot ${slot.slotNumber}:`);
        console.log(`  - Credits: ${slot.creditAmount?.toLocaleString()}`);
        console.log(`  - Status: ${slot.status}`);
        if (recipient) {
          console.log(`  - Sent to: ${recipient.name} (${recipient.phone})`);
          console.log(`  - Sent at: ${slot.sentAt}`);
        } else {
          console.log(`  - Recipient: None (Available)`);
        }
      }
    }

    // Check vouchers
    const vouchers = await Voucher.find({ userId: admin._id });
    const unredeemed = vouchers.filter((v) => v.redeemedStatus !== "redeemed");

    console.log("\n🎴 VOUCHERS:");
    console.log("=".repeat(60));
    console.log(`Total Vouchers: ${vouchers.length}`);
    console.log(`Unredeemed: ${unredeemed.length}`);
    console.log(`Redeemed: ${vouchers.length - unredeemed.length}`);

    if (vouchers.length > 0) {
      console.log("\n📋 Voucher List:");
      vouchers.forEach((v, i) => {
        console.log(
          `${i + 1}. ${v.voucherNumber} - ${v.companyName} - ₹${v.MRP?.toLocaleString()} - ${v.redeemedStatus}`,
        );
      });
    }

    console.log("\n" + "=".repeat(60));
    console.log("✅ Status check complete\n");

    await mongoose.disconnect();
  } catch (error) {
    console.error("❌ Error:", error);
    process.exit(1);
  }
}

checkAdminStatus();
