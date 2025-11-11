/*
Fast cleanup script - dry-run by default.
Usage:
  # Dry-run (shows how many docs and approx bytes will be cleared)
  node cleanup-base64-fast.js

  # Perform the updates (actually clear fields)
  node cleanup-base64-fast.js --run

This script clears legacy base64 image fields (that start with "data:")
only when a GridFS reference exists for that image. It uses aggregation
to compute total bytes and a single updateMany per field so it's fast.
*/

const mongoose = require('mongoose');
require('dotenv').config();

const MONGO_URI = process.env.MONGODB_URI || process.env.MONGODB_URI || process.env.MONGODB_URI;
if (!MONGO_URI) {
  console.error('❌ MONGO URI not found in environment. Set MONGODB_URI and retry.');
  process.exit(1);
}

async function connect() {
  await mongoose.connect(MONGO_URI);
}

async function getFieldStats(db, match, fieldName) {
  // Compute count and total length (using $strLenCP)
  const pipeline = [
    { $match: match },
    { $project: { len: { $strLenCP: `$${fieldName}` } } },
    { $group: { _id: null, totalLen: { $sum: '$len' }, count: { $sum: 1 } } }
  ];

  const result = await db.collection('ads').aggregate(pipeline).toArray();
  if (result && result.length > 0) return result[0];
  return { totalLen: 0, count: 0 };
}

async function run(dryRun = true) {
  await connect();
  const db = mongoose.connection.db;

  console.log('\n🧾 Fast Cleanup (dryRun=' + dryRun + ')');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

  // Match base64 legacy strings that start with data:
  const bottomMatch = { bottomImageGridFS: { $exists: true, $ne: null }, bottomImage: { $type: 'string', $regex: '^data:' } };
  const fullMatch = { fullscreenImageGridFS: { $exists: true, $ne: null }, fullscreenImage: { $type: 'string', $regex: '^data:' } };

  // Stats
  const bottomStats = await getFieldStats(db, bottomMatch, 'bottomImage');
  const fullStats = await getFieldStats(db, fullMatch, 'fullscreenImage');

  console.log(`📋 Bottom images matched: ${bottomStats.count} docs, total bytes: ${bottomStats.totalLen || 0}`);
  console.log(`📋 Fullscreen images matched: ${fullStats.count} docs, total bytes: ${fullStats.totalLen || 0}`);

  const totalBytes = (bottomStats.totalLen || 0) + (fullStats.totalLen || 0);
  console.log(`💾 Total bytes that would be cleared: ${(totalBytes/1024/1024).toFixed(2)} MB`);

  if (dryRun) {
    console.log('\n⚠️ Dry run complete. To actually clear the fields run:');
    console.log('   node cleanup-base64-fast.js --run');
    await mongoose.connection.close();
    return;
  }

  console.log('\n🚀 Running updates...');

  // Do updateMany operations - single DB ops per field
  const bottomRes = await db.collection('ads').updateMany(bottomMatch, { $set: { bottomImage: '' } });
  console.log(`   ✅ bottom update matched ${bottomRes.matchedCount}, modified ${bottomRes.modifiedCount}`);

  const fullRes = await db.collection('ads').updateMany(fullMatch, { $set: { fullscreenImage: '' } });
  console.log(`   ✅ fullscreen update matched ${fullRes.matchedCount}, modified ${fullRes.modifiedCount}`);

  console.log('\n🔁 Recomputing post-update stats...');
  const b2 = await getFieldStats(db, bottomMatch, 'bottomImage');
  const f2 = await getFieldStats(db, fullMatch, 'fullscreenImage');
  const remainingBytes = (b2.totalLen || 0) + (f2.totalLen || 0);

  console.log(`
📊 Summary:`);
  console.log(`   - Bottom cleared: ${bottomRes.modifiedCount} docs`);
  console.log(`   - Fullscreen cleared: ${fullRes.modifiedCount} docs`);
  console.log(`   - Approx bytes freed: ${(totalBytes - remainingBytes)/1024/1024 .toFixed(2)} MB`);

  await mongoose.connection.close();
  console.log('\n✅ Done. Closed DB connection.');
}

const runFlag = process.argv.includes('--run');
run(!runFlag).catch(err => {
  console.error('❌ Fatal error:', err);
  process.exit(1);
});
