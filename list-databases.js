/**
 * List all databases and collections to find the right one
 */

const mongoose = require('mongoose');

const MONGO_URI = "mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0";

async function listDatabases() {
  try {
    await mongoose.connect(MONGO_URI);
    console.log('✅ Connected to MongoDB\n');

    const admin = mongoose.connection.db.admin();
    const databases = await admin.listDatabases();
    
    console.log('📊 Available databases:\n');
    
    for (const db of databases.databases) {
      console.log(`📁 ${db.name} (${(db.sizeOnDisk / 1024 / 1024).toFixed(2)} MB)`);
      
      // Connect to each database and list collections
      const dbConn = mongoose.connection.client.db(db.name);
      const collections = await dbConn.listCollections().toArray();
      
      if (collections.length > 0) {
        console.log(`   Collections: ${collections.map(c => c.name).join(', ')}`);
        
        // Check if users collection exists
        const hasUsers = collections.some(c => c.name === 'users');
        if (hasUsers) {
          const usersCount = await dbConn.collection('users').countDocuments();
          console.log(`   👥 ${usersCount} users found!`);
        }
      }
      console.log('');
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    console.log('🔌 Connection closed');
    process.exit(0);
  }
}

listDatabases();
