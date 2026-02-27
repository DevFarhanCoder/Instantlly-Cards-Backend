/**
 * Reset ALL user credits to 0
 * 
 * Usage: node reset-all-credits.js
 */

const mongoose = require("mongoose");
require("dotenv").config();

const MONGODB_URI = process.env.MONGODB_URI;

async function resetAllCredits() {
  try {
    console.log("🔌 Connecting to MongoDB...");
    await mongoose.connect(MONGODB_URI);
    console.log("✅ Connected to MongoDB");

    const db = mongoose.connection.db;
    const usersCollection = db.collection("users");

    // Count users before update
    const totalUsers = await usersCollection.countDocuments();
    const usersWithCredits = await usersCollection.countDocuments({ credits: { $gt: 0 } });
    console.log(`📊 Total users: ${totalUsers}`);
    console.log(`💰 Users with credits > 0: ${usersWithCredits}`);

    // Reset all credits to 0
    const result = await usersCollection.updateMany(
      {},
      { $set: { credits: 0 } }
    );

    console.log(`\n✅ Done! Modified ${result.modifiedCount} out of ${result.matchedCount} users.`);
    console.log("All user credits are now 0.");
  } catch (error) {
    console.error("❌ Error:", error.message);
  } finally {
    await mongoose.disconnect();
    console.log("🔌 Disconnected from MongoDB");
    process.exit(0);
  }
}

resetAllCredits();
