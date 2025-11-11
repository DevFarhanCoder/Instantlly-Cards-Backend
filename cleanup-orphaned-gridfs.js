const mongoose = require('mongoose');
require('dotenv').config();

const adSchema = new mongoose.Schema({}, { strict: false, collection: 'ads' });
const Ad = mongoose.model('Ad', adSchema);

async function cleanupOrphanedFiles() {
  try {
    console.log('🗑️  CLEANUP: Removing orphaned GridFS files\n');

    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    const db = mongoose.connection.db;
    const filesCollection = db.collection('adImages.files');
    const chunksCollection = db.collection('adImages.chunks');

    // Get all referenced image IDs from ads
    const ads = await Ad.find({}).select('bottomImageGridFS fullscreenImageGridFS').lean();
    const referencedIds = new Set();
    
    ads.forEach(ad => {
      if (ad.bottomImageGridFS) referencedIds.add(ad.bottomImageGridFS.toString());
      if (ad.fullscreenImageGridFS) referencedIds.add(ad.fullscreenImageGridFS.toString());
    });

    console.log(`📋 Found ${referencedIds.size} images currently in use by ${ads.length} ads\n`);

    // Find all files in GridFS
    const allFiles = await filesCollection.find({}).toArray();
    console.log(`📁 Found ${allFiles.length} total files in GridFS\n`);

    // Identify orphaned files
    const orphanedFiles = allFiles.filter(file => !referencedIds.has(file._id.toString()));
    
    if (orphanedFiles.length === 0) {
      console.log('✅ No orphaned files found! Storage is clean.\n');
      await mongoose.connection.close();
      return;
    }

    console.log(`🗑️  Found ${orphanedFiles.length} orphaned files to delete:\n`);

    let totalSizeDeleted = 0;
    let filesDeleted = 0;
    let chunksDeleted = 0;

    for (const file of orphanedFiles) {
      const fileId = file._id;
      const fileName = file.filename;
      const fileSize = file.length;

      // Delete chunks
      const chunksResult = await chunksCollection.deleteMany({ files_id: fileId });
      
      // Delete file metadata
      await filesCollection.deleteOne({ _id: fileId });

      filesDeleted++;
      chunksDeleted += chunksResult.deletedCount;
      totalSizeDeleted += fileSize;

      console.log(`   ✅ Deleted: ${fileName} (${(fileSize / 1024).toFixed(2)} KB, ${chunksResult.deletedCount} chunks)`);
    }

    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📊 CLEANUP SUMMARY:');
    console.log(`   🗑️  Files deleted: ${filesDeleted}`);
    console.log(`   📦 Chunks deleted: ${chunksDeleted}`);
    console.log(`   💾 Space freed: ${(totalSizeDeleted / 1024 / 1024).toFixed(2)} MB`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    // Verify cleanup
    const remainingFiles = await filesCollection.countDocuments();
    const remainingChunks = await chunksCollection.countDocuments();
    
    console.log('✅ VERIFICATION:');
    console.log(`   Remaining files: ${remainingFiles} (should be ~${referencedIds.size})`);
    console.log(`   Remaining chunks: ${remainingChunks}`);
    console.log('\n✅ Cleanup complete!\n');

    await mongoose.connection.close();
  } catch (error) {
    console.error('❌ Cleanup failed:', error);
    process.exit(1);
  }
}

cleanupOrphanedFiles();
