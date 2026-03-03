"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// Script to cleanup all dummy data from Rajesh Modi's account
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
dotenv_1.default.config({ path: path_1.default.join(__dirname, "../../.env") });
const mongoose_1 = __importDefault(require("mongoose"));
const User_1 = __importDefault(require("../models/User"));
const MlmCredit_1 = __importDefault(require("../models/MlmCredit"));
const Voucher_1 = __importDefault(require("../models/Voucher"));
const ADMIN_PHONE = "+919867477227";
async function cleanupDummyData() {
    try {
        // Connect to MongoDB
        const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
        if (!mongoUri) {
            throw new Error("MONGODB_URI not found in environment variables");
        }
        console.log("🔌 Connecting to MongoDB...");
        await mongoose_1.default.connect(mongoUri);
        console.log("✅ Connected to MongoDB\n");
        // Find admin user
        const adminUser = await User_1.default.findOne({ phone: ADMIN_PHONE });
        if (!adminUser) {
            console.log("❌ Admin user not found!");
            await mongoose_1.default.connection.close();
            process.exit(1);
        }
        console.log(`✅ Found admin user: ${adminUser.name} (${adminUser.phone})\n`);
        console.log("🗑️  Starting cleanup...\n");
        // 1. Delete all test vouchers for admin
        console.log("📝 Deleting test vouchers...");
        const deletedVouchers = await Voucher_1.default.deleteMany({
            userId: adminUser._id,
            voucherNumber: { $regex: /^TEST-/ },
        });
        console.log(`   ✅ Deleted ${deletedVouchers.deletedCount} test vouchers\n`);
        // 2. Delete all dummy placeholder users
        console.log("👥 Deleting dummy placeholder users...");
        const dummyUsers = await User_1.default.find({
            phone: { $regex: /^\+919000000/ },
        });
        for (const dummyUser of dummyUsers) {
            // Delete MlmCredit entries
            await MlmCredit_1.default.deleteMany({
                $or: [{ senderId: dummyUser._id }, { receiverId: dummyUser._id }],
            });
            // Delete vouchers
            await Voucher_1.default.deleteMany({ userId: dummyUser._id });
            // Delete user
            await dummyUser.deleteOne();
            console.log(`   🗑️  Deleted: ${dummyUser.phone}`);
        }
        console.log(`   ✅ Deleted ${dummyUsers.length} dummy users\n`);
        // 3. Delete all MlmCredit entries where admin is sender or receiver (cleanup orphaned credits)
        console.log("💰 Cleaning up orphaned MLM credits...");
        const orphanedCredits = await MlmCredit_1.default.deleteMany({
            $or: [{ senderId: adminUser._id }, { receiverId: adminUser._id }],
        });
        console.log(`   ✅ Deleted ${orphanedCredits.deletedCount} MLM credit entries\n`);
        // 4. Reset admin credits to 0 (optional - comment out if you want to keep credits)
        console.log("🔢 Resetting admin credits...");
        const previousCredits = adminUser.credits;
        adminUser.credits = 0;
        await adminUser.save();
        console.log(`   ✅ Reset credits from ${previousCredits?.toLocaleString() || 0} to 0\n`);
        console.log("\n" + "=".repeat(60));
        console.log("🎉 Cleanup Complete!");
        console.log("=".repeat(60));
        console.log(`\n📋 Summary:`);
        console.log(`   Admin: ${adminUser.name} (${adminUser.phone})`);
        console.log(`   Test Vouchers Deleted: ${deletedVouchers.deletedCount}`);
        console.log(`   Dummy Users Deleted: ${dummyUsers.length}`);
        console.log(`   MLM Credits Deleted: ${orphanedCredits.deletedCount}`);
        console.log(`   Current Credits: ${adminUser.credits}\n`);
        await mongoose_1.default.connection.close();
        console.log("✅ Database connection closed");
        process.exit(0);
    }
    catch (error) {
        console.error("❌ Error during cleanup:", error);
        process.exit(1);
    }
}
cleanupDummyData();
