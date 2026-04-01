/**
 * Script to restore credits for users who were affected by the fixed expiry date bug
 * 
 * This script:
 * 1. Finds users with 0 credits but whose expiry date hasn't passed yet
 * 2. Calculates their correct balance from transactions
 * 3. Restores their credits to the correct amount
 * 
 * Run: node restore-credits-from-transactions.js
 */

const mongoose = require("mongoose");
const User = require("./src/models/User").default;
const Transaction = require("./src/models/Transaction").default;
require("dotenv").config();

async function restoreCredits() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(process.env.MONGODB_URI);
    console.log("✅ Connected to MongoDB\n");

    const now = new Date();
    console.log(`📅 Current date: ${now.toISOString()}\n`);

    // Find users with 0 credits but expiry date is still in the future
    const affectedUsers = await User.find({
      credits: 0,
      creditsExpiryDate: { $gt: now }
    }).select('name phone credits creditsExpiryDate');

    console.log(`📊 Found ${affectedUsers.length} users with 0 credits but unexpired expiry dates\n`);

    if (affectedUsers.length === 0) {
      console.log("✅ No users need credit restoration");
      process.exit(0);
    }

    let restoredCount = 0;
    let errorCount = 0;

    for (const user of affectedUsers) {
      try {
        // Calculate correct balance from transactions
        const transactions = await Transaction.find({
          $or: [
            { toUser: user._id },
            { fromUser: user._id }
          ],
          status: "completed"
        }).sort({ createdAt: 1 });

        let balance = 0;
        for (const txn of transactions) {
          if (txn.toUser && txn.toUser.toString() === user._id.toString()) {
            balance += txn.amount;
          }
          if (txn.fromUser && txn.fromUser.toString() === user._id.toString()) {
            balance -= txn.amount;
          }
        }

        if (balance > 0) {
          user.credits = balance;
          await user.save();
          
          console.log(`✅ Restored ${balance} credits for ${user.name} (${user.phone})`);
          console.log(`   Expiry: ${user.creditsExpiryDate.toISOString()}`);
          console.log(`   Based on ${transactions.length} transactions\n`);
          
          restoredCount++;
        } else {
          console.log(`⚠️  User ${user.name} (${user.phone}) has 0 balance from transactions - no restoration needed\n`);
        }
      } catch (error) {
        console.error(`❌ Error restoring credits for ${user.name}:`, error.message);
        errorCount++;
      }
    }

    console.log("\n" + "=".repeat(70));
    console.log("📊 Credit Restoration Summary:");
    console.log("=".repeat(70));
    console.log(`✅ Successfully restored: ${restoredCount} users`);
    console.log(`❌ Errors: ${errorCount}`);
    console.log(`⏭️  Skipped (0 balance): ${affectedUsers.length - restoredCount - errorCount}`);
    console.log("=".repeat(70));

    process.exit(0);
  } catch (error) {
    console.error("❌ Fatal error:", error);
    process.exit(1);
  }
}

restoreCredits();
