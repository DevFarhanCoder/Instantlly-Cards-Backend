// Script to update the joinCode index to be sparse
const mongoose = require('mongoose');
require('dotenv').config();

// MongoDB connection string from environment
const MONGODB_URI = process.env.MONGODB_URI;

async function updateIndex() {
  try {
    await mongoose.connect(MONGODB_URI);
    console.log('Connected to MongoDB');

    const db = mongoose.connection.db;
    const groupsCollection = db.collection('groups');

    console.log('Checking existing indexes...');
    const indexes = await groupsCollection.indexes();
    console.log('Current indexes:', indexes.map(idx => ({ name: idx.name, key: idx.key, sparse: idx.sparse })));

    // Check if there's an existing joinCode index
    const joinCodeIndex = indexes.find(idx => 
      idx.key && idx.key.joinCode !== undefined
    );

    if (joinCodeIndex) {
      console.log('Found existing joinCode index:', joinCodeIndex.name);
      
      // Drop the existing index
      await groupsCollection.dropIndex(joinCodeIndex.name);
      console.log('Dropped existing joinCode index');
    }

    // Create new sparse unique index
    await groupsCollection.createIndex(
      { joinCode: 1 }, 
      { 
        unique: true, 
        sparse: true,  // This will ignore null/undefined values
        name: 'joinCode_1_sparse'
      }
    );
    console.log('Created new sparse unique index for joinCode');

    // Verify the new index
    const newIndexes = await groupsCollection.indexes();
    const newJoinCodeIndex = newIndexes.find(idx => 
      idx.key && idx.key.joinCode !== undefined
    );
    console.log('New joinCode index:', newJoinCodeIndex);

    console.log('Index update completed successfully');

  } catch (error) {
    console.error('Index update failed:', error);
  } finally {
    await mongoose.disconnect();
    console.log('Disconnected from MongoDB');
  }
}

// Run the index update
updateIndex()
  .then(() => {
    console.log('Script completed');
    process.exit(0);
  })
  .catch((error) => {
    console.error('Script failed:', error);
    process.exit(1);
  });