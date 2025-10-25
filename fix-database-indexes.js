const mongoose = require('mongoose');
require('dotenv').config();

const fixDatabase = async () => {
  try {
    console.log('🔌 Connecting to database...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to database');

    const db = mongoose.connection.db;
    const collection = db.collection('users');

    // List current indexes
    console.log('\n📋 Current indexes:');
    const indexes = await collection.indexes();
    console.log(JSON.stringify(indexes, null, 2));

    // Check if email_1 index exists and is unique
    const emailIndex = indexes.find(idx => idx.name === 'email_1');
    if (emailIndex) {
      console.log('\n🔍 Found email_1 index:', emailIndex);
      
      // Drop the email index since we don't want email to be unique anymore
      console.log('\n🗑️ Dropping email_1 index...');
      await collection.dropIndex('email_1');
      console.log('✅ Dropped email_1 index');
    } else {
      console.log('\n✅ No email_1 index found');
    }

    // List indexes after dropping
    console.log('\n📋 Indexes after cleanup:');
    const indexesAfter = await collection.indexes();
    console.log(JSON.stringify(indexesAfter, null, 2));

  } catch (error) {
    console.error('❌ Database fix error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n🔌 Disconnected from database');
  }
};

fixDatabase();