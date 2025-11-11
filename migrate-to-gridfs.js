const mongoose = require('mongoose');
const { GridFSBucket } = require('mongodb');
require('dotenv').config();

// Ad schema
const adSchema = new mongoose.Schema({}, { strict: false, collection: 'ads' });
const Ad = mongoose.model('Ad', adSchema);

let gridFSBucket;

// Upload base64 to GridFS
async function uploadBase64ToGridFS(base64String, filename, metadata = {}) {
  if (!base64String || !base64String.startsWith('data:image')) {
    throw new Error('Invalid base64 image data');
  }

  // Extract base64 data
  const base64Data = base64String.split(',')[1];
  const buffer = Buffer.from(base64Data, 'base64');

  return new Promise((resolve, reject) => {
    const uploadStream = gridFSBucket.openUploadStream(filename, {
      metadata: {
        ...metadata,
        uploadDate: new Date(),
        migrated: true // Mark as migrated from base64
      }
    });

    uploadStream.on('finish', (file) => {
      console.log(`   ✅ Uploaded ${filename} (${(buffer.length / 1024 / 1024).toFixed(2)} MB) → GridFS ID: ${file._id}`);
      resolve(file._id);
    });

    uploadStream.on('error', (error) => {
      console.error(`   ❌ Upload failed for ${filename}:`, error.message);
      reject(error);
    });

    uploadStream.end(buffer);
  });
}

async function migrateAdsToGridFS() {
  try {
    console.log('\n🔄 MIGRATION: Converting all base64 ads to GridFS');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    // Connect to MongoDB
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Initialize GridFS bucket
    const db = mongoose.connection.db;
    gridFSBucket = new GridFSBucket(db, { bucketName: 'adImages' });
    console.log('✅ GridFS bucket initialized\n');

    // Find all ads with base64 images (no GridFS reference)
    const adsToMigrate = await Ad.find({
      $or: [
        { 
          bottomImage: { $exists: true, $ne: '', $regex: /^data:image/ },
          bottomImageGridFS: { $exists: false }
        },
        { 
          fullscreenImage: { $exists: true, $ne: '', $regex: /^data:image/ },
          fullscreenImageGridFS: { $exists: false }
        }
      ]
    }).lean();

    console.log(`📋 Found ${adsToMigrate.length} ads to migrate\n`);

    if (adsToMigrate.length === 0) {
      console.log('✅ No ads need migration - all already using GridFS!\n');
      await mongoose.connection.close();
      return;
    }

    let migratedCount = 0;
    let errorCount = 0;
    let totalSizeSaved = 0;

    for (const ad of adsToMigrate) {
      console.log(`\n🔄 Migrating: ${ad.title} (ID: ${ad._id})`);
      
      const updates = {};
      let adSizeSaved = 0;

      try {
        // Migrate bottom image
        if (ad.bottomImage && ad.bottomImage.startsWith('data:image') && !ad.bottomImageGridFS) {
          console.log('   📤 Migrating bottom image...');
          const bottomImageId = await uploadBase64ToGridFS(
            ad.bottomImage,
            `${Date.now()}_bottom_migrated.jpg`,
            { adId: ad._id.toString(), title: ad.title, type: 'bottom' }
          );
          updates.bottomImageGridFS = bottomImageId;
          updates.bottomImage = ''; // Clear base64
          adSizeSaved += ad.bottomImage.length;
        }

        // Migrate fullscreen image
        if (ad.fullscreenImage && ad.fullscreenImage.startsWith('data:image') && !ad.fullscreenImageGridFS) {
          console.log('   📤 Migrating fullscreen image...');
          const fullscreenImageId = await uploadBase64ToGridFS(
            ad.fullscreenImage,
            `${Date.now()}_fullscreen_migrated.jpg`,
            { adId: ad._id.toString(), title: ad.title, type: 'fullscreen' }
          );
          updates.fullscreenImageGridFS = fullscreenImageId;
          updates.fullscreenImage = ''; // Clear base64
          adSizeSaved += ad.fullscreenImage.length;
        }

        // Update the ad
        if (Object.keys(updates).length > 0) {
          await Ad.findByIdAndUpdate(ad._id, updates);
          migratedCount++;
          totalSizeSaved += adSizeSaved;
          console.log(`   ✅ Migrated! Freed ${(adSizeSaved / 1024 / 1024).toFixed(2)} MB`);
        }
      } catch (error) {
        errorCount++;
        console.error(`   ❌ Migration failed:`, error.message);
      }
    }

    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📊 MIGRATION SUMMARY:');
    console.log(`   ✅ Successfully migrated: ${migratedCount} ads`);
    console.log(`   ❌ Failed: ${errorCount} ads`);
    console.log(`   💾 Total space freed: ${(totalSizeSaved / 1024 / 1024).toFixed(2)} MB`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    await mongoose.connection.close();
    console.log('✅ Migration complete! MongoDB connection closed.\n');
    
  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  }
}

migrateAdsToGridFS();
