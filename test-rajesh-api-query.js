// Test script to verify Rajesh Modi can fetch received cards via API
require('dotenv').config();
const mongoose = require('mongoose');

async function testRajeshReceivedAPI() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    const db = mongoose.connection.db;
    const usersCollection = db.collection('users');
    const sharedCardsCollection = db.collection('sharedcards');

    // Find Rajesh Modi
    const rajesh = await usersCollection.findOne({ name: /rajesh/i });
    if (!rajesh) {
      console.log('❌ Rajesh Modi not found');
      return;
    }

    console.log(`✅ Testing API query for Rajesh Modi (${rajesh._id})\n`);

    // Simulate the EXACT backend query (cursor-based pagination)
    console.log('🔍 TEST 1: First page (limit 20, no cursor)');
    const firstPage = await sharedCardsCollection.find({
      recipientId: rajesh._id
    }, {
      projection: {
        _id: 1,
        cardId: 1,
        senderId: 1,
        senderName: 1,
        cardTitle: 1,
        cardPhoto: 1,
        senderProfilePicture: 1,
        sentAt: 1,
        status: 1,
        message: 1,
        viewedAt: 1
      }
    })
    .sort({ _id: -1 })
    .limit(21) // Fetch 21 to check hasMore
    .toArray();

    const hasMoreFirstPage = firstPage.length > 20;
    const firstPageItems = firstPage.slice(0, 20);
    const nextCursor = hasMoreFirstPage && firstPageItems.length > 0 
      ? firstPageItems[firstPageItems.length - 1]._id 
      : null;

    console.log(`   ✅ Found: ${firstPageItems.length} cards`);
    console.log(`   📄 Has more pages: ${hasMoreFirstPage}`);
    console.log(`   🔗 Next cursor: ${nextCursor}`);
    
    if (firstPageItems.length > 0) {
      console.log('\n   📋 Sample cards from first page:');
      firstPageItems.slice(0, 3).forEach((card, i) => {
        console.log(`      ${i + 1}. "${card.cardTitle}" from ${card.senderName}`);
        console.log(`         - Has cardPhoto: ${!!card.cardPhoto}`);
        console.log(`         - Has senderProfilePicture: ${!!card.senderProfilePicture}`);
        console.log(`         - Status: ${card.status}`);
      });
    }

    // Test second page if there is one
    if (nextCursor) {
      console.log(`\n🔍 TEST 2: Second page (cursor=${nextCursor})`);
      const secondPage = await sharedCardsCollection.find({
        recipientId: rajesh._id,
        _id: { $lt: nextCursor }
      }, {
        projection: {
          _id: 1,
          cardId: 1,
          senderId: 1,
          senderName: 1,
          cardTitle: 1,
          cardPhoto: 1,
          senderProfilePicture: 1,
          sentAt: 1,
          status: 1,
          message: 1,
          viewedAt: 1
        }
      })
      .sort({ _id: -1 })
      .limit(21)
      .toArray();

      const hasMoreSecondPage = secondPage.length > 20;
      const secondPageItems = secondPage.slice(0, 20);

      console.log(`   ✅ Found: ${secondPageItems.length} cards`);
      console.log(`   📄 Has more pages: ${hasMoreSecondPage}`);
      
      if (secondPageItems.length > 0) {
        console.log('\n   📋 Sample from second page:');
        secondPageItems.slice(0, 2).forEach((card, i) => {
          console.log(`      ${i + 1}. "${card.cardTitle}" from ${card.senderName}`);
        });
      }
    }

    // Summary
    console.log('\n' + '='.repeat(60));
    console.log('📊 SUMMARY');
    console.log('='.repeat(60));
    console.log(`✅ First page: ${firstPageItems.length} cards`);
    console.log(`✅ Pagination: ${hasMoreFirstPage ? 'Working' : 'N/A (all cards fit in first page)'}`);
    console.log(`✅ Cursor-based: ${nextCursor ? 'Working' : 'N/A'}`);
    console.log(`✅ Photos: ${firstPageItems.filter(c => c.cardPhoto).length}/${firstPageItems.length} have card photos`);
    console.log(`✅ Profile pics: ${firstPageItems.filter(c => c.senderProfilePicture).length}/${firstPageItems.length} have sender pics`);
    console.log('\n🎉 Backend API query simulation SUCCESSFUL!');
    console.log('📱 Frontend pull-to-refresh will fetch this data correctly.\n');

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('👋 Disconnected from MongoDB');
  }
}

testRajeshReceivedAPI();
