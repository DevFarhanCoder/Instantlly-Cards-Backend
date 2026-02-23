/**
 * Fix Admin Voucher Transfer History
 *
 * This script updates all admin-created vouchers to include proper transferHistory
 * so they appear in the transfer history page.
 */

const mongoose = require("mongoose");
const User = require("./src/models/User").default;
const Voucher = require("./src/models/Voucher").default;

const MONGODB_URI =
  process.env.MONGODB_URI || "mongodb://localhost:27017/instantlly-cards";

async function fixAdminVoucherHistory() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(MONGODB_URI);
    console.log("✅ Connected to MongoDB");

    // Find all admin-source vouchers that don't have transferHistory
    const adminVouchers = await Voucher.find({
      source: "admin",
      $or: [
        { transferHistory: { $exists: false } },
        { transferHistory: { $size: 0 } },
      ],
    }).populate("userId", "name phone");

    console.log(
      `\n📊 Found ${adminVouchers.length} admin vouchers without transfer history\n`,
    );

    if (adminVouchers.length === 0) {
      console.log("✅ All admin vouchers already have transfer history!");
      return;
    }

    // Find the voucher administrator (Rajesh Modi)
    const admin = await User.findOne({
      $or: [{ isVoucherAdmin: true }, { phone: "+919867477227" }],
    });

    if (!admin) {
      console.error("❌ Could not find admin user");
      return;
    }

    console.log(`👤 Admin: ${admin.name} (${admin.phone})\n`);

    // Update each voucher
    let updated = 0;
    for (const voucher of adminVouchers) {
      const recipient = voucher.userId;

      // Update the voucher with proper transfer tracking
      await Voucher.updateOne(
        { _id: voucher._id },
        {
          $set: {
            originalOwner: admin._id,
            transferredFrom: admin._id,
            transferredAt: voucher.createdAt || new Date(),
            transferHistory: [
              {
                from: admin._id,
                to: recipient._id,
                transferredAt: voucher.createdAt || new Date(),
              },
            ],
          },
        },
      );

      console.log(
        `✅ Updated voucher ${voucher.voucherNumber} → ${recipient.name} (${recipient.phone})`,
      );
      updated++;
    }

    console.log(`\n✨ Successfully updated ${updated} vouchers!`);
    console.log("\n📝 Summary:");
    console.log(`   - Admin: ${admin.name}`);
    console.log(`   - Vouchers fixed: ${updated}`);
    console.log(`   - These will now appear in transfer history\n`);
  } catch (error) {
    console.error("❌ Error:", error);
  } finally {
    await mongoose.disconnect();
    console.log("👋 Disconnected from MongoDB");
  }
}

// Run the fix
fixAdminVoucherHistory();
