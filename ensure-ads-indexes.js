/**
 * Ensure Ads Collection Indexes Exist
 * Fixes: 502 Bad Gateway on GET /api/ads (4-5 min query timeout)
 * 
 * Run this script to create missing indexes in production MongoDB
 */

const mongoose = require("mongoose");
require("dotenv").config();

async function ensureIndexes() {
  try {
    console.log("🔄 Connecting to MongoDB...");
    
    const uri = process.env.MONGO_URI || process.env.MONGODB_URI;
    if (!uri) {
      throw new Error("❌ MONGO_URI not found in environment variables");
    }
    
    await mongoose.connect(uri, {
      retryWrites: true,
      retryReads: true,
    });
    
    console.log("✅ Connected to MongoDB");
    
    // Get the ads collection
    const db = mongoose.connection.db;
    const adsCollection = db.collection("ads");
    
    console.log("\n📊 Current Indexes:");
    const existingIndexes = await adsCollection.indexes();
    existingIndexes.forEach(idx => {
      console.log(`   - ${idx.name}:`, JSON.stringify(idx.key));
    });
    
    console.log("\n🔨 Creating missing indexes...");
    
    // Critical index for GET /api/ads query: Ad.find({}).sort({ createdAt: -1 })
    console.log("   Creating index: { createdAt: -1 }");
    await adsCollection.createIndex({ createdAt: -1 }, { 
      name: "createdAt_-1",
      background: true 
    });
    
    // Index for active ads date range queries
    console.log("   Creating index: { startDate: 1, endDate: 1 }");
    await adsCollection.createIndex({ startDate: 1, endDate: 1 }, {
      name: "startDate_1_endDate_1",
      background: true
    });
    
    // Index for priority-based sorting
    console.log("   Creating index: { priority: -1, createdAt: -1 }");
    await adsCollection.createIndex({ priority: -1, createdAt: -1 }, {
      name: "priority_-1_createdAt_-1",
      background: true
    });
    
    // Compound index for active ads with priority
    console.log("   Creating index: { startDate: 1, endDate: 1, priority: -1, createdAt: -1 }");
    await adsCollection.createIndex({ startDate: 1, endDate: 1, priority: -1, createdAt: -1 }, {
      name: "active_ads_compound",
      background: true
    });
    
    console.log("\n✅ All indexes created successfully!");
    
    console.log("\n📊 Final Indexes:");
    const finalIndexes = await adsCollection.indexes();
    finalIndexes.forEach(idx => {
      console.log(`   - ${idx.name}:`, JSON.stringify(idx.key));
    });
    
    // Test query performance
    console.log("\n⏱️  Testing query performance...");
    const startTime = Date.now();
    const count = await adsCollection.countDocuments({});
    const queryTime = Date.now() - startTime;
    
    console.log(`   Total ads: ${count}`);
    console.log(`   Count query time: ${queryTime}ms`);
    
    // Test the actual query from GET /api/ads
    console.log("\n⏱️  Testing actual GET /api/ads query...");
    const queryStartTime = Date.now();
    const ads = await adsCollection.find({})
      .sort({ createdAt: -1 })
      .limit(1000)
      .toArray();
    const actualQueryTime = Date.now() - queryStartTime;
    
    console.log(`   Fetched ${ads.length} ads`);
    console.log(`   Query time: ${actualQueryTime}ms`);
    
    if (actualQueryTime < 1000) {
      console.log("   ✅ Query performance is GOOD (<1s)");
    } else if (actualQueryTime < 5000) {
      console.log("   ⚠️  Query performance is OK (1-5s)");
    } else {
      console.log("   ❌ Query performance is SLOW (>5s) - indexes may need time to build");
    }
    
    console.log("\n✅ Index creation complete!");
    console.log("💡 If query is still slow, indexes are building in background.");
    console.log("💡 Wait 1-2 minutes and test again.");
    
    await mongoose.disconnect();
    process.exit(0);
    
  } catch (error) {
    console.error("\n❌ Error:", error.message);
    console.error(error);
    process.exit(1);
  }
}

ensureIndexes();
