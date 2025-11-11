const mongoose = require('mongoose');
require('dotenv').config();

// Ad schema
const adSchema = new mongoose.Schema({}, { strict: false, collection: 'ads' });
const Ad = mongoose.model('Ad', adSchema);

async function cleanupBase64Data() {
  try {
    console.log('🧹 CLEANUP SCRIPT: Removing old base64 image data');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    // Connect to MongoDB
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Find all ads
    const ads = await Ad.find({}).lean();
    console.log(`📋 Found ${ads.length} total ads\n`);

    let cleanedCount = 0;
    let skippedCount = 0;
    let totalSizeSaved = 0;

    for (const ad of ads) {
      const updates = {};
      let adSizeSaved = 0;
      let needsUpdate = false;

      // Check bottom image
      if (ad.bottomImageGridFS && ad.bottomImage && ad.bottomImage.length > 100) {
        console.log(`🧹 Cleaning ${ad.title}:`);
        console.log(`   - bottomImage: ${ad.bottomImage.length} bytes → CLEARING`);
        updates.bottomImage = '';
        adSizeSaved += ad.bottomImage.length;
        needsUpdate = true;
      }

      // Check fullscreen image
      if (ad.fullscreenImageGridFS && ad.fullscreenImage && ad.fullscreenImage.length > 100) {
        if (!needsUpdate) {
          console.log(`🧹 Cleaning ${ad.title}:`);
        }
        console.log(`   - fullscreenImage: ${ad.fullscreenImage.length} bytes → CLEARING`);
        updates.fullscreenImage = '';
        adSizeSaved += ad.fullscreenImage.length;
        needsUpdate = true;
      }

      if (needsUpdate) {
        await Ad.findByIdAndUpdate(ad._id, updates);
        cleanedCount++;
        totalSizeSaved += adSizeSaved;
        console.log(`   ✅ Cleaned! Saved ${(adSizeSaved / 1024 / 1024).toFixed(2)} MB\n`);
      } else {
        skippedCount++;
      }
    }

    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📊 CLEANUP SUMMARY:');
    console.log(`   ✅ Cleaned: ${cleanedCount} ads`);
    console.log(`   ⏭️  Skipped: ${skippedCount} ads (already clean)`);
    console.log(`   💾 Total size saved: ${(totalSizeSaved / 1024 / 1024).toFixed(2)} MB`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    await mongoose.connection.close();
    console.log('✅ Cleanup complete! MongoDB connection closed.\n');
    
  } catch (error) {
    console.error('❌ Cleanup failed:', error);
    process.exit(1);
  }
}

cleanupBase64Data();
