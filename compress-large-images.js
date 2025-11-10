const mongoose = require('mongoose');
const sharp = require('sharp');
const stream = require('stream');
require('dotenv').config();

const LARGE_IMAGE_THRESHOLD = 2 * 1024 * 1024; // 2MB
const TARGET_SIZE = 800 * 1024; // 800KB target

async function compressLargeImages() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    const db = mongoose.connection.db;
    const bucket = new mongoose.mongo.GridFSBucket(db, {
      bucketName: 'adImages'
    });

    console.log('\n🔍 Finding images larger than 2MB...\n');

    const files = await db.collection('adImages.files').find({
      length: { $gt: LARGE_IMAGE_THRESHOLD }
    }).toArray();

    if (files.length === 0) {
      console.log('✅ No images larger than 2MB found!');
      await mongoose.disconnect();
      process.exit(0);
    }

    console.log(`Found ${files.length} large images:\n`);
    files.forEach(file => {
      const sizeMB = (file.length / 1024 / 1024).toFixed(2);
      console.log(`  - ${file.filename}: ${sizeMB}MB`);
    });

    console.log('\n🔄 Starting compression...\n');

    for (const file of files) {
      const originalSize = file.length;
      const originalSizeMB = (originalSize / 1024 / 1024).toFixed(2);
      
      console.log(`\n📦 Processing: ${file.filename} (${originalSizeMB}MB)`);

      try {
        // Download original image
        const downloadStream = bucket.openDownloadStream(file._id);
        const chunks = [];
        
        for await (const chunk of downloadStream) {
          chunks.push(chunk);
        }
        
        const imageBuffer = Buffer.concat(chunks);
        console.log(`  ✓ Downloaded original (${chunks.length} chunks)`);

        // Compress image
        const compressedBuffer = await sharp(imageBuffer)
          .jpeg({
            quality: 80,
            progressive: true,
            mozjpeg: true
          })
          .toBuffer();

        const compressedSize = compressedBuffer.length;
        const compressedSizeMB = (compressedSize / 1024 / 1024).toFixed(2);
        const savings = ((1 - compressedSize / originalSize) * 100).toFixed(1);

        console.log(`  ✓ Compressed: ${compressedSizeMB}MB (${savings}% smaller)`);

        // Delete old file
        await bucket.delete(file._id);
        console.log(`  ✓ Deleted original`);

        // Upload compressed version with same filename
        const uploadStream = bucket.openUploadStream(file.filename, {
          contentType: 'image/jpeg',
          metadata: file.metadata
        });

        const bufferStream = new stream.PassThrough();
        bufferStream.end(compressedBuffer);
        
        await new Promise((resolve, reject) => {
          bufferStream.pipe(uploadStream)
            .on('finish', resolve)
            .on('error', reject);
        });

        console.log(`  ✅ Uploaded compressed version`);
        const savedMB = ((originalSize - compressedSize) / 1024 / 1024).toFixed(2);
        console.log(`  💾 Saved ${savedMB}MB`);

      } catch (error) {
        console.error(`  ❌ Error processing ${file.filename}:`, error.message);
      }
    }

    console.log('\n✅ Compression complete!');
    await mongoose.disconnect();
    process.exit(0);

  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

compressLargeImages();
