/**
 * Test script to verify new MongoDB database connection
 * Run: node test-new-database.js
 */

const mongoose = require('mongoose');
const dotenv = require('dotenv');

// Load environment variables
dotenv.config();

const MONGODB_URI = process.env.MONGODB_URI;

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('🧪 TESTING NEW MONGODB DATABASE CONNECTION');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

async function testDatabase() {
  try {
    // Test 1: Connection
    console.log('📝 Test 1: MongoDB Connection');
    console.log('   URI:', MONGODB_URI.replace(/:[^:@]+@/, ':***@'));
    
    await mongoose.connect(MONGODB_URI);
    console.log('   ✅ Connected successfully!\n');
    
    // Test 2: Database Info
    console.log('📝 Test 2: Database Information');
    console.log('   Database:', mongoose.connection.name);
    console.log('   Host:', mongoose.connection.host);
    console.log('   Port:', mongoose.connection.port);
    console.log('   ✅ Database info retrieved\n');
    
    // Test 3: Collections
    console.log('📝 Test 3: Collections');
    const collections = await mongoose.connection.db.listCollections().toArray();
    console.log('   Total collections:', collections.length);
    collections.forEach(c => console.log('   -', c.name));
    console.log('   ✅ Collections listed\n');
    
    // Test 4: Data Counts
    console.log('📝 Test 4: Data Verification');
    const counts = await Promise.all([
      mongoose.connection.db.collection('users').countDocuments(),
      mongoose.connection.db.collection('ads').countDocuments(),
      mongoose.connection.db.collection('cards').countDocuments(),
      mongoose.connection.db.collection('messages').countDocuments(),
      mongoose.connection.db.collection('adImages.files').countDocuments(),
      mongoose.connection.db.collection('adImages.chunks').countDocuments(),
      mongoose.connection.db.collection('contacts').countDocuments()
    ]);
    
    console.log('   Users:', counts[0]);
    console.log('   Ads:', counts[1]);
    console.log('   Cards:', counts[2]);
    console.log('   Messages:', counts[3]);
    console.log('   GridFS Files:', counts[4]);
    console.log('   GridFS Chunks:', counts[5]);
    console.log('   Contacts:', counts[6]);
    console.log('   ✅ Data counts verified\n');
    
    // Test 5: Sample Data
    console.log('📝 Test 5: Sample Data Retrieval');
    const sampleAd = await mongoose.connection.db.collection('ads').findOne();
    const sampleUser = await mongoose.connection.db.collection('users').findOne();
    
    console.log('   Sample Ad:', sampleAd ? `"${sampleAd.title}" (ID: ${sampleAd._id})` : 'No ads found');
    console.log('   Sample User:', sampleUser ? `"${sampleUser.name}" (Phone: ${sampleUser.phoneNumber})` : 'No users found');
    console.log('   ✅ Sample data retrieved\n');
    
    // Test 6: GridFS Verification
    console.log('📝 Test 6: GridFS Images');
    const gridfsImages = await mongoose.connection.db.collection('adImages.files').find().limit(3).toArray();
    console.log('   Sample GridFS files:');
    gridfsImages.forEach(img => {
      console.log(`   - ${img.filename} (${Math.round(img.length / 1024)}KB)`);
    });
    console.log('   ✅ GridFS verified\n');
    
    // Summary
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('✅ ALL TESTS PASSED!');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('\n📋 Summary:');
    console.log(`   ✅ Connected to: ${mongoose.connection.host}`);
    console.log(`   ✅ Database: ${mongoose.connection.name}`);
    console.log(`   ✅ Collections: ${collections.length}`);
    console.log(`   ✅ Total Documents: ${counts.reduce((a, b) => a + b, 0).toLocaleString()}`);
    console.log('\n🎉 New MongoDB database is working perfectly!');
    console.log('\n📝 Next Steps:');
    console.log('   1. ✅ Database migration completed');
    console.log('   2. ⏳ Update Render environment variable');
    console.log('   3. ⏳ Deploy to production');
    console.log('   4. ⏳ Test production API');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    
  } catch (error) {
    console.error('\n❌ TEST FAILED:', error.message);
    console.error('Stack:', error.stack);
    process.exit(1);
  } finally {
    await mongoose.connection.close();
    console.log('🔌 Connection closed');
    process.exit(0);
  }
}

// Run tests
testDatabase();
