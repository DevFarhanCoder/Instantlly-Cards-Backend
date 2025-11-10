const mongoose = require('mongoose');
require('dotenv').config();

async function checkImageSizes() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    const db = mongoose.connection.db;
    const bucket = new mongoose.mongo.GridFSBucket(db, {
      bucketName: 'adImages'
    });

    console.log('\n📊 Analyzing ad image sizes...\n');

    const files = await db.collection('adImages.files').find({}).toArray();
    
    if (files.length === 0) {
      console.log('No images found in GridFS');
      process.exit(0);
    }

    let totalSize = 0;
    let maxSize = 0;
    let minSize = Infinity;
    
    const sizeCategories = {
      'tiny (0-100KB)': 0,
      'small (100KB-500KB)': 0,
      'medium (500KB-1MB)': 0,
      'large (1MB-2MB)': 0,
      'xlarge (2MB-5MB)': 0,
      'huge (5MB+)': 0
    };

    console.log('Images found:');
    console.log('━'.repeat(80));
    
    files.forEach(file => {
      const sizeMB = (file.length / 1024 / 1024).toFixed(2);
      const sizeKB = (file.length / 1024).toFixed(0);
      console.log(`${file.filename}: ${sizeMB}MB (${sizeKB}KB)`);
      
      totalSize += file.length;
      maxSize = Math.max(maxSize, file.length);
      minSize = Math.min(minSize, file.length);
      
      // Categorize
      const sizeInKB = file.length / 1024;
      const sizeInMB = file.length / 1024 / 1024;
      
      if (sizeInKB < 100) sizeCategories['tiny (0-100KB)']++;
      else if (sizeInKB < 500) sizeCategories['small (100KB-500KB)']++;
      else if (sizeInMB < 1) sizeCategories['medium (500KB-1MB)']++;
      else if (sizeInMB < 2) sizeCategories['large (1MB-2MB)']++;
      else if (sizeInMB < 5) sizeCategories['xlarge (2MB-5MB)']++;
      else sizeCategories['huge (5MB+)']++;
    });

    console.log('\n━'.repeat(80));
    console.log('\n📈 Statistics:');
    console.log(`Total images: ${files.length}`);
    console.log(`Total size: ${(totalSize / 1024 / 1024).toFixed(2)}MB`);
    console.log(`Average size: ${(totalSize / files.length / 1024).toFixed(0)}KB`);
    console.log(`Largest: ${(maxSize / 1024 / 1024).toFixed(2)}MB`);
    console.log(`Smallest: ${(minSize / 1024).toFixed(0)}KB`);
    
    console.log('\n📊 Size Distribution:');
    Object.entries(sizeCategories).forEach(([category, count]) => {
      if (count > 0) {
        console.log(`  ${category}: ${count} images`);
      }
    });

    console.log('\n💡 Recommendations:');
    if (maxSize > 2 * 1024 * 1024) {
      console.log('⚠️  You have images larger than 2MB - these will load slowly on mobile!');
      console.log('   Consider compressing images to under 500KB for faster loading');
    }
    if (totalSize / files.length > 1024 * 1024) {
      console.log('⚠️  Average image size is over 1MB - users will experience slow loading');
      console.log('   Recommended: Compress images to 300-500KB for optimal performance');
    }

    await mongoose.disconnect();
    console.log('\n✅ Analysis complete');
    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

checkImageSizes();
