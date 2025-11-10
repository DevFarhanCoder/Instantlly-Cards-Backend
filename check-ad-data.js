/**
 * Check what data exists for a specific ad
 */

const mongoose = require("mongoose");
require("dotenv").config();

async function checkAd() {
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
    
    // Check the specific ad from your logs
    const adId = "691189b19f95e43d2aa03db9";
    
    console.log(`🔍 Checking ad: ${adId}\n`);
    
    const ad = await adsCollection.findOne({ _id: new mongoose.Types.ObjectId(adId) });
    
    if (!ad) {
      console.log("❌ Ad not found!");
      process.exit(1);
    }
    
    console.log("📋 Ad Details:");
    console.log("   Title:", ad.title);
    console.log("   Phone:", ad.phoneNumber);
    console.log("");
    
    console.log("🖼️  Image Storage:");
    console.log("   bottomImage type:", typeof ad.bottomImage);
    console.log("   bottomImage length:", ad.bottomImage?.length || 0);
    console.log("   bottomImage is base64:", ad.bottomImage?.startsWith('data:image') || false);
    console.log("   bottomImageGridFS:", ad.bottomImageGridFS || 'NOT SET');
    console.log("");
    console.log("   fullscreenImage type:", typeof ad.fullscreenImage);
    console.log("   fullscreenImage length:", ad.fullscreenImage?.length || 0);
    console.log("   fullscreenImage is base64:", ad.fullscreenImage?.startsWith('data:image') || false);
    console.log("   fullscreenImageGridFS:", ad.fullscreenImageGridFS || 'NOT SET');
    console.log("");
    
    // Check if GridFS files exist
    const filesCollection = db.collection("adImages.files");
    
    if (ad.bottomImageGridFS) {
      const bottomFile = await filesCollection.findOne({ _id: ad.bottomImageGridFS });
      console.log("🔍 Bottom Image GridFS file:");
      if (bottomFile) {
        console.log("   ✅ EXISTS");
        console.log("   Size:", (bottomFile.length / 1024).toFixed(2), "KB");
        console.log("   Upload date:", bottomFile.uploadDate);
      } else {
        console.log("   ❌ NOT FOUND - GridFS reference is broken!");
      }
      console.log("");
    }
    
    if (ad.fullscreenImageGridFS) {
      const fullscreenFile = await filesCollection.findOne({ _id: ad.fullscreenImageGridFS });
      console.log("🔍 Fullscreen Image GridFS file:");
      if (fullscreenFile) {
        console.log("   ✅ EXISTS");
        console.log("   Size:", (fullscreenFile.length / 1024).toFixed(2), "KB");
        console.log("   Upload date:", fullscreenFile.uploadDate);
      } else {
        console.log("   ❌ NOT FOUND - GridFS reference is broken!");
      }
      console.log("");
    }
    
    console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    console.log("📊 DIAGNOSIS:\n");
    
    const hasBase64Bottom = ad.bottomImage && ad.bottomImage.startsWith('data:image');
    const hasBase64Fullscreen = ad.fullscreenImage && ad.fullscreenImage.startsWith('data:image');
    const hasGridFSBottom = ad.bottomImageGridFS;
    const hasGridFSFullscreen = ad.fullscreenImageGridFS;
    
    if (hasBase64Bottom || hasBase64Fullscreen) {
      console.log("✅ Ad has base64 images - edit form WILL show images");
    } else if (hasGridFSBottom || hasGridFSFullscreen) {
      console.log("⚠️  Ad ONLY has GridFS references - edit form WON'T show images");
      console.log("   Solution: Fetch from /api/ads/image/:id/:type endpoint");
    } else {
      console.log("❌ Ad has NO images at all!");
    }
    
    console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    
    await mongoose.disconnect();
    process.exit(0);
    
  } catch (error) {
    console.error("\n❌ Error:", error.message);
    process.exit(1);
  }
}

checkAd();
