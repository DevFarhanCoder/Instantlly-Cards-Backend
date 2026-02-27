// Script to setup MLM test data for admin account
import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import User from "../models/User";
import MlmCredit from "../models/MlmCredit";
import Voucher from "../models/Voucher";

const ADMIN_PHONE = "+919867477227";
const ADMIN_NAME = "Rajesh Modi";
const ADMIN_CREDITS = 14648436000;
const ADMIN_VOUCHERS = 122070300;

async function setupMLMTestData() {
  try {
    // Connect to MongoDB
    const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
    if (!mongoUri) {
      throw new Error("MONGODB_URI not found in environment variables");
    }

    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(mongoUri);
    console.log("✅ Connected to MongoDB");

    // Find or create admin user
    let adminUser = await User.findOne({ phone: ADMIN_PHONE });

    if (!adminUser) {
      console.log("❌ Admin user not found. Creating new admin user...");
      adminUser = await User.create({
        phone: ADMIN_PHONE,
        name: ADMIN_NAME,
        role: "admin",
        credits: ADMIN_CREDITS,
        isActive: true,
      });
      console.log(`✅ Created admin user: ${ADMIN_NAME} (${ADMIN_PHONE})`);
    } else {
      console.log(
        `✅ Found admin user: ${adminUser.name} (${adminUser.phone})`,
      );
    }

    // Update admin credits
    adminUser.credits = ADMIN_CREDITS;
    await adminUser.save();
    console.log(
      `✅ Updated admin credits to: ${ADMIN_CREDITS.toLocaleString()}`,
    );

    // Create vouchers for admin
    console.log(
      `\n🎫 Creating ${ADMIN_VOUCHERS.toLocaleString()} vouchers for admin...\n`,
    );

    // Delete existing test vouchers for admin
    await Voucher.deleteMany({
      userId: adminUser._id,
      voucherNumber: { $regex: /^TEST-/ },
    });

    // Note: In production, vouchers would be created on-demand or in batches
    // For testing purposes, we'll create a small sample set
    const SAMPLE_VOUCHERS = 1000; // Create only 1000 sample vouchers instead of 122M

    for (let i = 1; i <= SAMPLE_VOUCHERS; i++) {
      await Voucher.create({
        userId: adminUser._id,
        voucherNumber: `TEST-${Date.now()}-${i.toString().padStart(9, "0")}`,
        MRP: 1,
        amount: 1,
        issueDate: new Date(),
        expiryDate: new Date(Date.now() + 365 * 24 * 60 * 60 * 1000), // 1 year
        redeemedStatus: "unredeemed",
        source: "admin",
      });

      if (i % 100 === 0) {
        console.log(`  Created ${i.toLocaleString()} sample vouchers...`);
      }
    }
    console.log(
      `✅ Created ${SAMPLE_VOUCHERS.toLocaleString()} sample vouchers for testing`,
    );
    console.log(
      `   (UI will display ${ADMIN_VOUCHERS.toLocaleString()} as configured)\n`,
    );

    // Delete all dummy placeholder users and their credits
    console.log(`\n🗑️  Removing dummy placeholder users...\n`);

    const dummyUsers = await User.find({
      phone: { $regex: /^\+919000000/ },
    });

    for (const dummyUser of dummyUsers) {
      await MlmCredit.deleteMany({
        $or: [{ senderId: dummyUser._id }, { receiverId: dummyUser._id }],
      });
      await Voucher.deleteMany({ userId: dummyUser._id });
      await dummyUser.deleteOne();
      console.log(`  🗑️  Deleted dummy user: ${dummyUser.phone}`);
    }

    console.log(`✅ Removed all dummy data`);

    console.log("\n" + "=".repeat(60));
    console.log("🎉 MLM Test Data Setup Complete!");
    console.log("=".repeat(60));
    console.log(`\n📋 Summary:`);
    console.log(`   Admin: ${ADMIN_NAME} (${ADMIN_PHONE})`);
    console.log(`   Admin Credits: ${ADMIN_CREDITS.toLocaleString()}`);
    console.log(`   Admin Vouchers: ${ADMIN_VOUCHERS.toLocaleString()}`);
    console.log(
      `   Distribution Credits: Will appear when users join via referral\n`,
    );

    console.log("🧪 How to Test:");
    console.log("   1. Login with admin account: +91 9867477227");
    console.log("   2. Navigate to Voucher Dashboard");
    console.log("   3. You should see blank distribution credit entries");
    console.log("   4. Direct Buyers will populate as users join via referral");
    console.log(
      "   5. Use Transfer Credits/Transfer Vouchers buttons when available\n",
    );

    await mongoose.connection.close();
    console.log("✅ Database connection closed");
    process.exit(0);
  } catch (error) {
    console.error("❌ Error setting up MLM test data:", error);
    process.exit(1);
  }
}

setupMLMTestData();
