/**
 * Insert Test Data Script
 *
 * This script populates test data for account: 9867969445
 * - Updates downlineCount to unlock discount levels
 * - Creates test vouchers
 * - Creates test credits
 *
 * Usage: node insert-test-data.js
 */

const mongoose = require("mongoose");
require("dotenv").config();

const TEST_PHONE = "+919867969445";

// Define schemas directly in this file
const UserSchema = new mongoose.Schema(
  {
    name: String,
    phone: String,
    referralCode: String,
    downlineCount: { type: Number, default: 0 },
  },
  { timestamps: true, strict: false },
);

const VoucherSchema = new mongoose.Schema(
  {
    userId: mongoose.Schema.Types.ObjectId,
    originalOwner: mongoose.Schema.Types.ObjectId,
    voucherNumber: String,
    MRP: Number,
    source: String,
    expiryDate: Date,
    redeemedStatus: String,
    transferHistory: Array,
  },
  { timestamps: true, strict: false },
);

const MlmCreditSchema = new mongoose.Schema(
  {
    senderId: mongoose.Schema.Types.ObjectId,
    receiverId: mongoose.Schema.Types.ObjectId,
    senderName: String,
    receiverName: String,
    receiverPhone: String,
    amount: Number,
    paymentStatus: String,
    paymentConfirmedByReceiver: Boolean,
    paymentConfirmedAt: Date,
    adminApprovedBy: String,
    adminApprovedAt: Date,
    status: String,
  },
  { timestamps: true, strict: false },
);

const MlmWalletSchema = new mongoose.Schema(
  {
    userId: mongoose.Schema.Types.ObjectId,
    creditBalance: Number,
  },
  { timestamps: true, strict: false },
);

const User = mongoose.model("User", UserSchema);
const Voucher = mongoose.model("Voucher", VoucherSchema);
const MlmCredit = mongoose.model("MlmCredit", MlmCreditSchema);
const MlmWallet = mongoose.model("MlmWallet", MlmWalletSchema);

async function insertTestData() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI);
    console.log("✅ Connected to MongoDB\n");

    // Find the user
    const user = await User.findOne({ phone: TEST_PHONE });
    if (!user) {
      console.error("❌ User not found with phone:", TEST_PHONE);
      process.exit(1);
    }

    console.log("👤 Found user:", {
      id: user._id,
      name: user.name,
      phone: user.phone,
      referralCode: user.referralCode,
      currentDownlineCount: user.downlineCount || 0,
    });

    // Ask what data to insert
    console.log("\n📊 INSERTING TEST DATA...\n");

    // 1. UPDATE DOWNLINE COUNT (to unlock Level 3 = 62.5% discount)
    const newDownlineCount = 25; // Level 3 requires 25 downlines
    user.downlineCount = newDownlineCount;
    await user.save();
    console.log(`✅ Updated downlineCount: ${newDownlineCount}`);
    console.log(`   → Unlocked Level 3: 62.5% discount`);
    console.log(`   → Payable Amount: ₹2,250 (was ₹3,600)`);
    console.log(`   → Virtual Savings: ₹1,350\n`);

    // 2. ENSURE MLM WALLET EXISTS
    let wallet = await MlmWallet.findOne({ userId: user._id });
    if (!wallet) {
      wallet = await MlmWallet.create({
        userId: user._id,
        creditBalance: 10,
      });
      console.log("✅ Created MLM wallet with 10 credits");
    } else {
      wallet.creditBalance += 10;
      await wallet.save();
      console.log(
        `✅ Added 10 credits to wallet (Total: ${wallet.creditBalance})`,
      );
    }

    // 3. CREATE TEST VOUCHERS
    const vouchersToCreate = 5;
    const now = new Date();
    const expiryDate = new Date(now.getTime() + 365 * 24 * 60 * 60 * 1000); // 1 year

    // First, delete any existing test vouchers for this user
    await Voucher.deleteMany({
      userId: user._id,
      voucherNumber: /^TEST-/,
    });
    console.log("\n🗑️  Removed old test vouchers");

    const vouchers = [];
    for (let i = 0; i < vouchersToCreate; i++) {
      const voucherNumber = `TEST-${Date.now()}-${i}`;
      vouchers.push({
        userId: user._id,
        originalOwner: user._id,
        voucherNumber: voucherNumber,
        MRP: 1200,
        source: "purchase",
        issueDate: now,
        expiryDate: expiryDate,
        redeemedStatus: "unredeemed",
        transferHistory: [],
        voucherImages: [],
        productVideoLink: "",
      });
    }

    const createdVouchers = await Voucher.insertMany(vouchers);
    console.log(
      `✅ Created ${createdVouchers.length} test vouchers (₹1,200 each)`,
    );
    console.log("   Voucher Numbers:");
    createdVouchers.forEach((v, idx) => {
      console.log(`   ${idx + 1}. ${v.voucherNumber}`);
    });

    // 4. CREATE SAMPLE CREDIT TRANSFER RECORDS
    const sampleCredit = await MlmCredit.create({
      senderId: user._id,
      receiverId: user._id,
      senderName: user.name,
      receiverName: "Test Receiver",
      receiverPhone: "+919999999999",
      amount: 5,
      paymentStatus: "approved",
      paymentConfirmedByReceiver: true,
      paymentConfirmedAt: now,
      adminApprovedBy: "admin",
      adminApprovedAt: now,
      status: "approved",
      createdAt: now,
    });
    console.log(
      `\n✅ Created sample credit transfer (ID: ${sampleCredit._id})`,
    );

    // 5. SUMMARY
    console.log("\n" + "=".repeat(60));
    console.log("📊 TEST DATA INSERTION COMPLETE");
    console.log("=".repeat(60));
    console.log("\n📱 Account:", TEST_PHONE);
    console.log("👤 User ID:", user._id);
    console.log("🎯 Discount Level: 3 (62.5% off)");
    console.log("💰 Downline Count:", newDownlineCount);
    console.log("🎴 Vouchers Created:", createdVouchers.length);
    console.log("💳 Wallet Balance:", wallet.creditBalance, "credits");
    console.log("✅ Status: Ready for testing!\n");

    console.log("🧪 TEST NOW:");
    console.log("1. Open app with phone:", TEST_PHONE.replace("+91", ""));
    console.log("2. Go to Network tab");
    console.log("3. Check discount dashboard shows Level 3");
    console.log("4. Verify payable amount is ₹2,250");
    console.log('5. See 5 vouchers in "Your Vouchers" section');
    console.log("6. Try transferring a voucher using phone number\n");

    // 6. VERIFICATION - Query what the API will actually return
    console.log("🔍 VERIFYING API RESPONSES...\n");

    // Test vouchers endpoint
    const apiVouchers = await Voucher.find({ userId: user._id })
      .sort({ issueDate: -1 })
      .limit(20)
      .lean();
    console.log(
      `📡 GET /api/mlm/vouchers → ${apiVouchers.length} vouchers found`,
    );
    if (apiVouchers.length > 0) {
      console.log(
        `   First voucher: ${apiVouchers[0].voucherNumber}, MRP: ₹${apiVouchers[0].MRP}`,
      );
    }

    await mongoose.disconnect();
    console.log("\n🔌 Disconnected from MongoDB");
    process.exit(0);
  } catch (error) {
    console.error("❌ Error inserting test data:", error);
    await mongoose.disconnect();
    process.exit(1);
  }
}

// Run the script
insertTestData();
