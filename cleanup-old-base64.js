/**
 * CLEANUP SCRIPT: Remove old base64 data from ads that have GridFS references
 * 
 * This script finds all ads that have BOTH base64 data AND GridFS references,
 * and clears the base64 data to:
 * 1. Reduce database size
 * 2. Ensure images update correctly
 * 3. Fix the "images not updating" issue
 */

const mongoose = require('mongoose');
require('dotenv').config();

const adSchema = new mongoose.Schema({
  title: String,
  phoneNumber: String,
  bottomImage: String,
  fullscreenImage: String,
  bottomImageGridFS: mongoose.Schema.Types.ObjectId,
  fullscreenImageGridFS: mongoose.Schema.Types.ObjectId,
  startDate: Date,
  endDate: Date,
  priority: Number,
  status: { type: String, enum: ['active', 'paused'], default: 'active' }
}, { timestamps: true, collection: 'ads' });

const Ad = mongoose.model('Ad', adSchema);

async function cleanupOldBase64() {
  try {
    console.log('🧹 Starting base64 cleanup...\n');
    
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');
    
    // Find all ads
    const allAds = await Ad.find({});
    console.log(`📋 Found ${allAds.length} total ads\n`);
    
    let cleanedCount = 0;
    let skippedCount = 0;
    
    for (const ad of allAds) {
      let needsUpdate = false;
      const updateData = {};
      
      // Check bottom image
      if (ad.bottomImageGridFS && ad.bottomImage && ad.bottomImage.length > 100) {
        console.log(`🧹 [${ad.title}] Clearing old bottom image base64 (${ad.bottomImage.length} bytes)`);
        updateData.bottomImage = '';
        needsUpdate = true;
      }
      
      // Check fullscreen image
      if (ad.fullscreenImageGridFS && ad.fullscreenImage && ad.fullscreenImage.length > 100) {
        console.log(`🧹 [${ad.title}] Clearing old fullscreen image base64 (${ad.fullscreenImage.length} bytes)`);
        updateData.fullscreenImage = '';
        needsUpdate = true;
      }
      
      if (needsUpdate) {
        await Ad.findByIdAndUpdate(ad._id, updateData);
        cleanedCount++;
        console.log(`   ✅ Cleaned!\n`);
      } else {
        skippedCount++;
      }
    }
    
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📊 CLEANUP SUMMARY');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log(`✅ Ads cleaned: ${cleanedCount}`);
    console.log(`⏭️  Ads skipped (already clean): ${skippedCount}`);
    console.log(`📋 Total ads: ${allAds.length}`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    
    console.log('✅ Cleanup complete!');
    console.log('💡 Now refresh your dashboard and images should update correctly!\n');
    
    await mongoose.connection.close();
    process.exit(0);
    
  } catch (error) {
    console.error('❌ Cleanup failed:', error);
    process.exit(1);
  }
}

cleanupOldBase64();
