// Fix missing indexes on SharedCard collection
require('dotenv').config();
const mongoose = require('mongoose');

async function fixIndexes() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    const db = mongoose.connection.db;
    const collection = db.collection('sharedcards');

    console.log('📋 Current indexes on SharedCard collection:');
    const existingIndexes = await collection.indexes();
    existingIndexes.forEach(idx => {
      console.log(`  - ${idx.name}: ${JSON.stringify(idx.key)}`);
    });

    console.log('\n🔧 Creating optimized indexes...\n');

    // Drop existing indexes (except _id) to rebuild fresh
    console.log('🗑️  Dropping old indexes...');
    const indexNames = existingIndexes.map(idx => idx.name).filter(name => name !== '_id_');
    for (const indexName of indexNames) {
      try {
        await collection.dropIndex(indexName);
        console.log(`   ✓ Dropped index: ${indexName}`);
      } catch (err) {
        console.log(`   ⚠️  Could not drop ${indexName}:`, err.message);
      }
    }

    console.log('\n✨ Creating new optimized indexes...\n');

    // Index 1: For sent cards query (senderId + sentAt desc)
    // This is the MOST IMPORTANT index for /cards/sent endpoint
    console.log('Creating index: { senderId: 1, sentAt: -1 }');
    await collection.createIndex(
      { senderId: 1, sentAt: -1 },
      { 
        name: 'senderId_1_sentAt_-1',
        background: true 
      }
    );
    console.log('✅ Created senderId + sentAt index');

    // Index 2: For received cards query (recipientId + sentAt desc)
    // This is the MOST IMPORTANT index for /cards/received endpoint
    console.log('\nCreating index: { recipientId: 1, sentAt: -1 }');
    await collection.createIndex(
      { recipientId: 1, sentAt: -1 },
      { 
        name: 'recipientId_1_sentAt_-1',
        background: true 
      }
    );
    console.log('✅ Created recipientId + sentAt index');

    // Index 3: For card lookup
    console.log('\nCreating index: { cardId: 1 }');
    await collection.createIndex(
      { cardId: 1 },
      { 
        name: 'cardId_1',
        background: true 
      }
    );
    console.log('✅ Created cardId index');

    // Index 4: For status filtering (if needed)
    console.log('\nCreating index: { status: 1 }');
    await collection.createIndex(
      { status: 1 },
      { 
        name: 'status_1',
        background: true 
      }
    );
    console.log('✅ Created status index');

    // Index 5: Compound index for unviewed cards per recipient
    console.log('\nCreating index: { recipientId: 1, status: 1, sentAt: -1 }');
    await collection.createIndex(
      { recipientId: 1, status: 1, sentAt: -1 },
      { 
        name: 'recipientId_1_status_1_sentAt_-1',
        background: true 
      }
    );
    console.log('✅ Created recipientId + status + sentAt compound index');

    console.log('\n\n📋 Final indexes on SharedCard collection:');
    const finalIndexes = await collection.indexes();
    finalIndexes.forEach(idx => {
      console.log(`  - ${idx.name}: ${JSON.stringify(idx.key)}`);
    });

    // Get collection stats
    console.log('\n\n📊 Collection Statistics:');
    const stats = await collection.stats();
    console.log(`Total documents: ${stats.count.toLocaleString()}`);
    console.log(`Average document size: ${(stats.avgObjSize / 1024).toFixed(2)} KB`);
    console.log(`Total collection size: ${(stats.size / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Total index size: ${(stats.totalIndexSize / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Number of indexes: ${stats.nindexes}`);

    console.log('\n\n✅ Index optimization complete!');
    console.log('\n📌 These indexes will significantly improve query performance for:');
    console.log('   - GET /api/cards/sent (senderId + sentAt index)');
    console.log('   - GET /api/cards/received (recipientId + sentAt index)');
    console.log('   - Filtering by status (status index)');
    console.log('   - Unviewed cards queries (compound index)');
    
  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n👋 Disconnected from MongoDB');
  }
}

fixIndexes();
