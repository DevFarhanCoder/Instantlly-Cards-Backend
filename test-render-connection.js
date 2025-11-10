const mongoose = require('mongoose');

// Test the EXACT connection string from Render
const RENDER_CONNECTION_STRING = 'mongodb+srv://rajeshmodi:Newpass1234@cluster0.9yfi96i.mongodb.net/instantlly?retryWrites=true&w=majority&appName=Cluster0';

async function testRenderConnection() {
  console.log('\n🔍 Testing EXACT Render Connection String...\n');
  console.log('━'.repeat(60));
  console.log('Connection String:', RENDER_CONNECTION_STRING.replace(/Newpass1234/, '***'));
  console.log('━'.repeat(60));

  try {
    console.log('\n⏳ Connecting (timeout: 30s)...\n');
    
    const startTime = Date.now();
    
    await mongoose.connect(RENDER_CONNECTION_STRING, {
      serverSelectionTimeoutMS: 30000,
      connectTimeoutMS: 30000,
      socketTimeoutMS: 30000,
    });
    
    const elapsed = Date.now() - startTime;
    
    console.log(`✅ CONNECTION SUCCESSFUL! (${elapsed}ms)\n`);
    console.log('Connection Details:');
    console.log(`   Host: ${mongoose.connection.host}`);
    console.log(`   Database: ${mongoose.connection.name}`);
    console.log(`   Ready State: ${mongoose.connection.readyState}`);
    
    // Test a query that's timing out on Render
    console.log('\n🔍 Testing Card Query (like Render does)...\n');
    
    const queryStart = Date.now();
    const card = await mongoose.connection.db.collection('cards').findOne({});
    const queryElapsed = Date.now() - queryStart;
    
    if (card) {
      console.log(`✅ Query successful! (${queryElapsed}ms)`);
      console.log(`   Found card: ${card._id}`);
    } else {
      console.log('⚠️  No cards found in database');
    }
    
    // Test SharedCard query
    console.log('\n🔍 Testing SharedCard Query...\n');
    
    const sharedStart = Date.now();
    const sharedCard = await mongoose.connection.db.collection('sharedcards').findOne({});
    const sharedElapsed = Date.now() - sharedStart;
    
    if (sharedCard) {
      console.log(`✅ Query successful! (${sharedElapsed}ms)`);
      console.log(`   Found shared card: ${sharedCard._id}`);
    } else {
      console.log('⚠️  No shared cards found');
    }
    
    // Test with timeout like production
    console.log('\n🔍 Testing with 5s timeout (like production)...\n');
    
    try {
      const timeoutStart = Date.now();
      const result = await Promise.race([
        mongoose.connection.db.collection('sharedcards').findOne({}),
        new Promise((_, reject) => 
          setTimeout(() => reject(new Error('5s timeout')), 5000)
        )
      ]);
      const timeoutElapsed = Date.now() - timeoutStart;
      console.log(`✅ Completed within 5s! (${timeoutElapsed}ms)`);
    } catch (error) {
      console.log('❌ TIMEOUT! Query took longer than 5 seconds');
      console.log('   This is why Render is failing!');
    }
    
    await mongoose.disconnect();
    console.log('\n✅ Test completed!\n');
    process.exit(0);
    
  } catch (error) {
    console.log('\n❌ CONNECTION FAILED!\n');
    console.error('Error:', error.message);
    console.error('\nFull Error:', error);
    
    if (error.message.includes('Authentication failed')) {
      console.log('\n🔐 Authentication failed - password is wrong!');
    } else if (error.message.includes('ENOTFOUND') || error.message.includes('timed out')) {
      console.log('\n🌐 Network issue - cannot reach MongoDB Atlas');
      console.log('\nPossible causes:');
      console.log('  1. Render IP not whitelisted in MongoDB Atlas');
      console.log('  2. Network connectivity issue');
      console.log('  3. MongoDB Atlas cluster is down');
    }
    
    process.exit(1);
  }
}

testRenderConnection();
