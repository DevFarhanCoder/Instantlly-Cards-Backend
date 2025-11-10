/**
 * Test GET /api/ads query performance
 * Diagnose why query takes 4-5 minutes despite having indexes
 */

const mongoose = require("mongoose");
require("dotenv").config();

async function testQuery() {
  try {
    console.log("🔄 Connecting to MongoDB...");
    
    const uri = process.env.MONGO_URI || process.env.MONGODB_URI;
    await mongoose.connect(uri, {
      retryWrites: true,
      retryReads: true,
    });
    
    console.log("✅ Connected to MongoDB\n");
    
    const db = mongoose.connection.db;
    const adsCollection = db.collection("ads");
    
    // 1. Count total ads
    console.log("📊 Step 1: Counting ads...");
    const startCount = Date.now();
    const totalAds = await adsCollection.countDocuments({});
    console.log(`   Total ads: ${totalAds}`);
    console.log(`   Time: ${Date.now() - startCount}ms\n`);
    
    // 2. Test the exact query from GET /api/ads
    console.log("⏱️  Step 2: Testing actual query (limit 1000)...");
    const startQuery = Date.now();
    
    const ads = await adsCollection
      .find({})
      .sort({ createdAt: -1 })
      .limit(1000)
      .toArray();
    
    const queryTime = Date.now() - startQuery;
    console.log(`   Fetched: ${ads.length} ads`);
    console.log(`   Time: ${queryTime}ms\n`);
    
    // 3. Check if ads have large embedded images
    console.log("🔍 Step 3: Checking document sizes...");
    let totalSize = 0;
    let largeDocsCount = 0;
    let maxSize = 0;
    let sampleLargeDoc = null;
    
    ads.slice(0, 100).forEach(ad => {
      const docSize = JSON.stringify(ad).length;
      totalSize += docSize;
      
      if (docSize > maxSize) {
        maxSize = docSize;
        sampleLargeDoc = ad;
      }
      
      if (docSize > 100000) { // >100KB
        largeDocsCount++;
      }
    });
    
    const avgSize = Math.round(totalSize / Math.min(100, ads.length));
    console.log(`   Average doc size (first 100): ${(avgSize / 1024).toFixed(2)}KB`);
    console.log(`   Largest doc size: ${(maxSize / 1024).toFixed(2)}KB`);
    console.log(`   Docs >100KB: ${largeDocsCount}`);
    
    if (sampleLargeDoc) {
      const hasBottomImage = sampleLargeDoc.bottomImage?.length > 100;
      const hasFullscreenImage = sampleLargeDoc.fullscreenImage?.length > 100;
      console.log(`   Sample large doc has:`);
      console.log(`     - bottomImage: ${hasBottomImage ? (sampleLargeDoc.bottomImage.length / 1024).toFixed(2) + 'KB' : 'GridFS ref'}`);
      console.log(`     - fullscreenImage: ${hasFullscreenImage ? (sampleLargeDoc.fullscreenImage.length / 1024).toFixed(2) + 'KB' : 'GridFS ref'}`);
    }
    
    // 4. Check for GridFS migration status
    console.log("\n🔍 Step 4: Checking GridFS migration status...");
    const base64Count = await adsCollection.countDocuments({
      $or: [
        { bottomImage: { $regex: /^data:image/ } },
        { fullscreenImage: { $regex: /^data:image/ } }
      ]
    });
    const gridfsCount = await adsCollection.countDocuments({
      $or: [
        { bottomImageGridFS: { $exists: true, $ne: null } },
        { fullscreenImageGridFS: { $exists: true, $ne: null } }
      ]
    });
    
    console.log(`   Ads with base64 images: ${base64Count}`);
    console.log(`   Ads with GridFS images: ${gridfsCount}`);
    
    // 5. Test query with projection (exclude images)
    console.log("\n⏱️  Step 5: Testing query WITHOUT images (projection)...");
    const startProjection = Date.now();
    
    const adsWithoutImages = await adsCollection
      .find({})
      .project({ 
        bottomImage: 0, 
        fullscreenImage: 0 
      })
      .sort({ createdAt: -1 })
      .limit(1000)
      .toArray();
    
    const projectionTime = Date.now() - startProjection;
    console.log(`   Fetched: ${adsWithoutImages.length} ads`);
    console.log(`   Time: ${projectionTime}ms`);
    console.log(`   Speed improvement: ${(queryTime / projectionTime).toFixed(2)}x faster\n`);
    
    // Summary
    console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    console.log("📊 DIAGNOSIS:\n");
    
    if (queryTime > 5000) {
      console.log("❌ Query is SLOW (>5 seconds)");
      
      if (base64Count > 0) {
        console.log("🔴 ROOT CAUSE: Ads contain embedded base64 images");
        console.log("   Solution: Migrate images to GridFS");
        console.log("   Command: node migrate-images-to-gridfs.js");
      } else if (projectionTime < queryTime / 2) {
        console.log("🔴 ROOT CAUSE: Large document transfer time");
        console.log("   Solution: Use projection to exclude images");
      } else {
        console.log("🔴 ROOT CAUSE: Unknown - check MongoDB Atlas metrics");
      }
    } else if (queryTime > 1000) {
      console.log("⚠️  Query is OK but slow (1-5 seconds)");
      console.log("💡 Consider using projection or pagination");
    } else {
      console.log("✅ Query is FAST (<1 second)");
    }
    
    console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    
    await mongoose.disconnect();
    process.exit(0);
    
  } catch (error) {
    console.error("\n❌ Error:", error.message);
    process.exit(1);
  }
}

testQuery();
