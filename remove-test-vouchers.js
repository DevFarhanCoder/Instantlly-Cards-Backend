/**
 * Remove TEST Vouchers Script
 *
 * This script removes all vouchers with "TEST" in their voucher number or company name
 *
 * Usage: node remove-test-vouchers.js
 */

require("dotenv").config();
const mongoose = require("mongoose");

const MONGO_URI =
  process.env.MONGODB_URI ||
  process.env.MONGO_URI ||
  "mongodb://localhost:27017/instantlly";

// Define Voucher Schema inline
const VoucherSchema = new mongoose.Schema(
  {
    voucherNumber: String,
    companyName: String,
    userId: mongoose.Schema.Types.ObjectId,
    MRP: Number,
    issueDate: Date,
    expiryDate: Date,
    redeemedStatus: String,
    source: String,
    voucherImages: [String],
    companyLogo: String,
    phoneNumber: String,
    address: String,
    amount: Number,
    discountPercentage: Number,
    validity: String,
    voucherImage: String,
    description: String,
    isPublished: Boolean,
  },
  { timestamps: true },
);

const Voucher = mongoose.model("Voucher", VoucherSchema);

async function removeTestVouchers() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    console.log("   URI:", MONGO_URI.replace(/:[^:@]+@/, ":****@"));

    await mongoose.connect(MONGO_URI);
    console.log("✅ Connected to MongoDB\n");

    console.log("🔍 Finding TEST vouchers...\n");

    // Find all vouchers with TEST in voucher number or company name
    const testVouchers = await Voucher.find({
      $or: [{ voucherNumber: /TEST/i }, { companyName: /TEST/i }],
    });

    console.log(`📊 Found ${testVouchers.length} TEST vouchers\n`);

    if (testVouchers.length === 0) {
      console.log("✅ No TEST vouchers found. Database is clean!");
      process.exit(0);
    }

    // Display what will be deleted
    console.log("📋 Vouchers to be deleted:");
    testVouchers.forEach((voucher, index) => {
      console.log(
        `   ${index + 1}. ${voucher.voucherNumber} - ${voucher.companyName || "No company name"}`,
      );
    });

    console.log("\n🗑️  Deleting TEST vouchers...\n");

    // Delete all TEST vouchers
    const result = await Voucher.deleteMany({
      $or: [{ voucherNumber: /TEST/i }, { companyName: /TEST/i }],
    });

    console.log(
      `✅ Successfully deleted ${result.deletedCount} TEST vouchers\n`,
    );

    console.log(
      "🎉 Done! All TEST vouchers have been removed from the database.\n",
    );

    process.exit(0);
  } catch (error) {
    console.error("❌ Error:", error.message);
    console.error(error);
    process.exit(1);
  }
}

// Run the script
removeTestVouchers();
