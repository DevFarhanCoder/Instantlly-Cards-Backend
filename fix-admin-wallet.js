/**
 * Fix Admin Wallet - Set large credit balance
 */

const mongoose = require("mongoose");
require("dotenv").config();

const ADMIN_PHONE = "+919867477227";
const ADMIN_CREDITS = 1000000000000; // 1 Trillion credits for admin

const UserSchema = new mongoose.Schema({}, { timestamps: true, strict: false });
const MlmWalletSchema = new mongoose.Schema(
  {},
  { timestamps: true, strict: false },
);

const User = mongoose.model("User", UserSchema);
const MlmWallet = mongoose.model("MlmWallet", MlmWalletSchema);

async function fixAdminWallet() {
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

    console.log("👑 Admin:", {
      id: admin._id,
      name: admin.name,
      phone: admin.phone,
      isVoucherAdmin: admin.isVoucherAdmin,
      level: admin.level,
    });

    // Find or create wallet
    let wallet = await MlmWallet.findOne({ userId: admin._id });

    if (!wallet) {
      wallet = await MlmWallet.create({
        userId: admin._id,
        creditBalance: ADMIN_CREDITS,
      });
      console.log(`\n✅ Created admin wallet with ${ADMIN_CREDITS} credits`);
    } else {
      const oldBalance = wallet.creditBalance;
      wallet.creditBalance = ADMIN_CREDITS;
      await wallet.save();
      console.log(`\n✅ Updated admin wallet:`);
      console.log(`   Old balance: ${oldBalance}`);
      console.log(`   New balance: ${ADMIN_CREDITS}`);
    }

    console.log("\n✅ COMPLETE!");
    console.log(`Admin now has ${ADMIN_CREDITS.toLocaleString()} credits\n`);

    await mongoose.disconnect();
  } catch (error) {
    console.error("❌ Error:", error);
    process.exit(1);
  }
}

fixAdminWallet();
