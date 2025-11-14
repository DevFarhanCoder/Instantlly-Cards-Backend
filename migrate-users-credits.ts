// Script to migrate existing users with credits system
// Run this once to give all existing users their initial 500,000 credits and unique referral codes

import dotenv from "dotenv";
dotenv.config();

import mongoose from "mongoose";
import User from "./src/models/User";
import Transaction from "./src/models/Transaction";

// Helper function to generate unique referral code
function generateReferralCode(): string {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let code = '';
  for (let i = 0; i < 8; i++) {
    code += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return code;
}

async function migrateExistingUsers() {
  try {
    console.log("🚀 Starting credits migration for existing users...");

    // Connect to MongoDB
    if (!process.env.MONGODB_URI) {
      throw new Error("MONGODB_URI not found in environment");
    }

    await mongoose.connect(process.env.MONGODB_URI);
    console.log("✅ Connected to MongoDB");

    // Find all users without credits
    const usersWithoutCredits = await User.find({
      $or: [
        { credits: { $exists: false } },
        { credits: null },
        { credits: 0 }
      ]
    });

    console.log(`📊 Found ${usersWithoutCredits.length} users without credits`);

    // Find all users without referral codes
    const usersWithoutReferralCode = await User.find({
      $or: [
        { referralCode: { $exists: false } },
        { referralCode: null },
        { referralCode: '' }
      ]
    });

    console.log(`📊 Found ${usersWithoutReferralCode.length} users without referral codes`);

    let creditsUpdated = 0;
    let referralCodesAdded = 0;
    let transactionsCreated = 0;

    // Update users without credits
    for (const user of usersWithoutCredits) {
      try {
        user.set({ credits: 500000 });
        await user.save();
        creditsUpdated++;

        // Create transaction record
        await Transaction.create({
          type: 'signup_bonus',
          toUser: user._id,
          amount: 500000,
          description: 'Retroactive signup bonus - 5 lac credits',
          balanceBefore: 0,
          balanceAfter: 500000,
          status: 'completed'
        });
        transactionsCreated++;

        console.log(`✅ Updated ${user.name} (${user.phone}) with 500,000 credits`);
      } catch (error) {
        console.error(`❌ Error updating ${user.name}:`, error);
      }
    }

    // Update users without referral codes
    for (const user of usersWithoutReferralCode) {
      try {
        // Generate unique referral code
        let newReferralCode = generateReferralCode();
        let codeExists = await User.findOne({ referralCode: newReferralCode });
        
        while (codeExists) {
          newReferralCode = generateReferralCode();
          codeExists = await User.findOne({ referralCode: newReferralCode });
        }

        user.set({ referralCode: newReferralCode });
        await user.save();
        referralCodesAdded++;

        console.log(`✅ Added referral code ${newReferralCode} to ${user.name}`);
      } catch (error) {
        console.error(`❌ Error adding referral code to ${user.name}:`, error);
      }
    }

    console.log("\n🎉 Migration completed!");
    console.log(`📊 Summary:`);
    console.log(`   - Credits updated: ${creditsUpdated} users`);
    console.log(`   - Referral codes added: ${referralCodesAdded} users`);
    console.log(`   - Transactions created: ${transactionsCreated}`);

    // Close connection
    await mongoose.connection.close();
    console.log("✅ Database connection closed");

  } catch (error) {
    console.error("❌ Migration failed:", error);
    process.exit(1);
  }
}

// Run the migration
migrateExistingUsers();
