// test-documentdb-connection.js
// Quick test script to verify DocumentDB connection
require('dotenv').config();

const mongoose = require('mongoose');
const fs = require('fs');
const path = require('path');

async function testDocumentDBConnection() {
  console.log('🧪 Testing DocumentDB Connection...');
  console.log('====================================');
  
  // Check environment variables
  console.log('\n📋 Environment Check:');
  console.log(`DOCUMENTDB_URI: ${process.env.DOCUMENTDB_URI ? '✅ Set' : '❌ Not set'}`);
  console.log(`MONGODB_URI: ${process.env.MONGODB_URI ? '✅ Set' : '❌ Not set'}`);
  
  // Check SSL certificate
  const sslCertPath = path.join(__dirname, 'global-bundle.pem');
  const certExists = fs.existsSync(sslCertPath);
  console.log(`SSL Certificate: ${certExists ? '✅ Found' : '❌ Missing'}`);
  
  if (!certExists) {
    console.log('❌ SSL certificate missing. Run: curl -o global-bundle.pem https://truststore.pki.rds.amazonaws.com/global/global-bundle.pem');
    return;
  }
  
  if (!process.env.DOCUMENTDB_URI) {
    console.log('❌ DOCUMENTDB_URI not set. Using MongoDB Atlas fallback...');
    
    if (!process.env.MONGODB_URI) {
      console.log('❌ Neither DOCUMENTDB_URI nor MONGODB_URI is set!');
      return;
    }
    
    // Test MongoDB Atlas connection
    console.log('\n🔄 Testing MongoDB Atlas connection...');
    try {
      await mongoose.connect(process.env.MONGODB_URI, {
        serverSelectionTimeoutMS: 10000,
        socketTimeoutMS: 15000,
        connectTimeoutMS: 10000,
      });
      
      console.log('✅ MongoDB Atlas connection successful!');
      console.log(`📊 Database: ${mongoose.connection.name}`);
      console.log(`🌐 Host: ${mongoose.connection.host}`);
      
      await mongoose.disconnect();
      console.log('✅ Test completed successfully');
      
    } catch (error) {
      console.error('❌ MongoDB Atlas connection failed:', error.message);
    }
    
    return;
  }
  
  // Test DocumentDB connection
  console.log('\n🔄 Testing DocumentDB connection...');
  
  try {
    const options = {
      tls: true,
      tlsCAFile: sslCertPath,
      serverSelectionTimeoutMS: 30000,
      socketTimeoutMS: 45000,
      connectTimeoutMS: 30000,
      maxPoolSize: 10,
      minPoolSize: 2,
      retryWrites: false,
      retryReads: true,
    };
    
    console.log('🔗 Attempting DocumentDB connection...');
    await mongoose.connect(process.env.DOCUMENTDB_URI, options);
    
    console.log('✅ DocumentDB connection successful!');
    console.log(`📊 Database: ${mongoose.connection.name}`);
    console.log(`🌐 Host: ${mongoose.connection.host}`);
    console.log(`🔒 SSL: Enabled`);
    
    // Test database operations
    console.log('\n🧪 Testing database operations...');
    
    // Ping the database
    await mongoose.connection.db.admin().ping();
    console.log('✅ Database ping successful');
    
    // List collections (if any)
    const collections = await mongoose.connection.db.listCollections().toArray();
    console.log(`📋 Collections found: ${collections.length}`);
    
    if (collections.length > 0) {
      console.log('📄 Collection names:');
      collections.forEach(col => console.log(`   - ${col.name}`));
    }
    
    await mongoose.disconnect();
    console.log('✅ DocumentDB test completed successfully');
    console.log('\n🎉 Your DocumentDB setup is working correctly!');
    
  } catch (error) {
    console.error('❌ DocumentDB connection failed:', error.message);
    console.log('\n🔄 Attempting fallback to MongoDB Atlas...');
    
    if (process.env.MONGODB_URI) {
      try {
        await mongoose.connect(process.env.MONGODB_URI, {
          serverSelectionTimeoutMS: 10000,
        });
        
        console.log('✅ Fallback to MongoDB Atlas successful!');
        await mongoose.disconnect();
        
      } catch (fallbackError) {
        console.error('❌ Both DocumentDB and MongoDB Atlas failed:', fallbackError.message);
      }
    }
  }
}

// Run the test
testDocumentDBConnection()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('❌ Test failed:', error);
    process.exit(1);
  });
