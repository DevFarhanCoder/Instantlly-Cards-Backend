/**
 * Fix GET /api/ads by using projection to exclude large image fields
 * This will drastically reduce query time and response size
 */

const mongoose = require("mongoose");
require("dotenv").config();

async function quickTest() {
  try {
    console.log("🔄 Testing MongoDB connection stability...\n");
    
    const uri = process.env.MONGO_URI || process.env.MONGODB_URI;
    
    // Test 1: Simple connection test
    console.log("Test 1: Basic connection (10s timeout)");
    const conn = await mongoose.connect(uri, {
      retryWrites: true,
      retryReads: true,
      serverSelectionTimeoutMS: 10000,
    });
    
    console.log("✅ Connected!\n");
    
    // Test 2: Simple ping
    console.log("Test 2: Database ping");
    const start = Date.now();
    await mongoose.connection.db.admin().ping();
    console.log(`✅ Ping successful (${Date.now() - start}ms)\n`);
    
    // Test 3: Count documents with timeout
    console.log("Test 3: Count ads (5s timeout)");
    const countStart = Date.now();
    const count = await mongoose.connection.db.collection("ads").countDocuments({}, {
      maxTimeMS: 5000
    });
    console.log(`✅ Found ${count} ads (${Date.now() - countStart}ms)\n`);
    
    // Test 4: Find one document
    console.log("Test 4: Find one ad (5s timeout)");
    const findStart = Date.now();
    const ad = await mongoose.connection.db.collection("ads").findOne({}, {
      maxTimeMS: 5000
    });
    console.log(`✅ Found ad (${Date.now() - findStart}ms)`);
    console.log(`   Document size: ${(JSON.stringify(ad).length / 1024).toFixed(2)}KB\n`);
    
    // Test 5: Find all WITHOUT images (projection)
    console.log("Test 5: Find all ads WITHOUT images (10s timeout)");
    const projectionStart = Date.now();
    const adsWithoutImages = await mongoose.connection.db.collection("ads")
      .find({})
      .project({ bottomImage: 0, fullscreenImage: 0 })
      .sort({ createdAt: -1 })
      .limit(1000)
      .maxTimeMS(10000)
      .toArray();
    console.log(`✅ Fetched ${adsWithoutImages.length} ads (${Date.now() - projectionStart}ms)`);
    const avgSize = adsWithoutImages.reduce((sum, a) => sum + JSON.stringify(a).length, 0) / adsWithoutImages.length;
    console.log(`   Avg size: ${(avgSize / 1024).toFixed(2)}KB\n`);
    
    // Test 6: Find all WITH images
    console.log("Test 6: Find all ads WITH images (30s timeout)");
    const fullQueryStart = Date.now();
    try {
      const allAds = await mongoose.connection.db.collection("ads")
        .find({})
        .sort({ createdAt: -1 })
        .limit(1000)
        .maxTimeMS(30000)
        .toArray();
      console.log(`✅ Fetched ${allAds.length} ads (${Date.now() - fullQueryStart}ms)`);
      const fullAvgSize = allAds.reduce((sum, a) => sum + JSON.stringify(a).length, 0) / allAds.length;
      console.log(`   Avg size: ${(fullAvgSize / 1024).toFixed(2)}KB\n`);
    } catch (err) {
      console.log(`❌ TIMEOUT after ${Date.now() - fullQueryStart}ms`);
      console.log(`   Error: ${err.message}\n`);
    }
    
    console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    console.log("✅ All tests complete!");
    console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    
    await mongoose.disconnect();
    process.exit(0);
    
  } catch (error) {
    console.error("\n❌ Connection failed:", error.message);
    console.error("\n🔴 This confirms MongoDB Atlas is blocking connections");
    console.error("🔧 FIX: Add 0.0.0.0/0 to MongoDB Atlas Network Access");
    process.exit(1);
  }
}

quickTest();
