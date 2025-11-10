const mongoose = require('mongoose');
require('dotenv').config();

async function testConnection() {
  console.log('\n🔍 Testing MongoDB Connection...\n');
  console.log('━'.repeat(60));
  
  // Get connection string from .env
  const connectionString = process.env.MONGODB_URI;
  
  // Parse and show details (hide password)
  const urlMatch = connectionString.match(/mongodb\+srv:\/\/([^:]+):([^@]+)@([^\/]+)\/([^?]+)/);
  
  if (urlMatch) {
    console.log('📋 Connection Details:');
    console.log(`   Username: ${urlMatch[1]}`);
    console.log(`   Password: ${urlMatch[2]}`);
    console.log(`   Cluster:  ${urlMatch[3]}`);
    console.log(`   Database: ${urlMatch[4]}`);
  }
  
  console.log('━'.repeat(60));
  console.log('\n⏳ Attempting connection...\n');

  try {
    await mongoose.connect(connectionString, {
      serverSelectionTimeoutMS: 10000,
      socketTimeoutMS: 10000,
    });
    
    console.log('✅ CONNECTION SUCCESSFUL!\n');
    console.log('MongoDB Details:');
    console.log(`   Host: ${mongoose.connection.host}`);
    console.log(`   Port: ${mongoose.connection.port}`);
    console.log(`   Database: ${mongoose.connection.name}`);
    console.log(`   State: ${mongoose.connection.readyState === 1 ? 'Connected' : 'Not Connected'}`);
    
    // Test a simple query
    console.log('\n🔍 Testing query...');
    const collections = await mongoose.connection.db.listCollections().toArray();
    console.log(`✅ Found ${collections.length} collections`);
    collections.forEach(col => {
      console.log(`   - ${col.name}`);
    });
    
    await mongoose.disconnect();
    console.log('\n✅ Test completed successfully!\n');
    process.exit(0);
    
  } catch (error) {
    console.log('❌ CONNECTION FAILED!\n');
    console.error('Error Type:', error.name);
    console.error('Error Message:', error.message);
    
    if (error.message.includes('Authentication failed')) {
      console.log('\n🔐 AUTHENTICATION ERROR DETECTED!\n');
      console.log('This means:');
      console.log('  ❌ Username or password is incorrect');
      console.log('  ❌ User might not exist in MongoDB Atlas');
      console.log('  ❌ User might not have access to this database\n');
      console.log('✅ Solutions:');
      console.log('  1. Go to MongoDB Atlas → Database Access');
      console.log('  2. Check if user "9326702120" exists');
      console.log('  3. Reset password to "123456"');
      console.log('  4. Ensure user has "Read and Write" permissions');
      console.log('  5. Click "Edit" → "Edit Password" → Enter "123456" → "Update User"\n');
    } else if (error.message.includes('timed out')) {
      console.log('\n⏱️ TIMEOUT ERROR DETECTED!\n');
      console.log('This means:');
      console.log('  ❌ Cannot reach MongoDB server');
      console.log('  ❌ Network/firewall blocking connection\n');
      console.log('✅ Solutions:');
      console.log('  1. Go to MongoDB Atlas → Network Access');
      console.log('  2. Add IP: 0.0.0.0/0 (allow all)');
      console.log('  3. Or add your current IP address\n');
    }
    
    process.exit(1);
  }
}

testConnection();
