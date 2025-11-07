// Check sent/received cards for phone +919920067878
require('dotenv').config();
const mongoose = require('mongoose');

async function checkUserCards() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    const db = mongoose.connection.db;
    const usersCollection = db.collection('users');
    const sharedCardsCollection = db.collection('sharedcards');

    // Find user by phone
    const phone = '+919920067978';
    console.log('🔍 Searching for user with phone:', phone);
    
    const user = await usersCollection.findOne({ 
      $or: [
        { phone: phone },
        { phone: phone.replace('+91', '') },
        { phone: '9920067978' },
        { phoneNumber: phone },
        { phoneNumber: phone.replace('+91', '') }
      ]
    });

    if (!user) {
      console.log('❌ User not found with phone:', phone);
      
      // Try to find similar numbers
      const similar = await usersCollection.find({ 
        $or: [
          { phone: /9920067878/ },
          { phoneNumber: /9920067878/ }
        ]
      }).limit(5).toArray();
      
      console.log('\n📋 Similar numbers found:', similar.length);
      similar.forEach(u => {
        console.log(`   - ${u.name}: ${u.phone || u.phoneNumber} (ID: ${u._id})`);
      });
      return;
    }

    console.log('✅ Found user:');
    console.log(`   Name: ${user.name}`);
    console.log(`   Phone: ${user.phone || user.phoneNumber}`);
    console.log(`   ID: ${user._id}`);
    console.log(`   String ID: ${user._id.toString()}`);

    const userId = user._id;

    // Check SENT cards
    console.log('\n📤 SENT CARDS:');
    console.log('='.repeat(60));
    
    const sentCards = await sharedCardsCollection.find({ 
      senderId: userId 
    })
    .sort({ _id: -1 })
    .toArray();
    
    console.log(`Total sent cards: ${sentCards.length}\n`);
    
    if (sentCards.length > 0) {
      console.log('All sent cards (newest first):');
      sentCards.forEach((card, i) => {
        const date = new Date(card.sentAt || card.createdAt);
        console.log(`${i + 1}. To: ${card.recipientName}`);
        console.log(`   Card: ${card.cardTitle}`);
        console.log(`   Sent: ${date.toLocaleString()}`);
        console.log(`   _id: ${card._id}`);
        console.log(`   Status: ${card.status}`);
        console.log('');
      });
      
      // Check what first page would return (cursor pagination)
      console.log('\n📄 FIRST PAGE (limit 20):');
      console.log('-'.repeat(60));
      const firstPage = await sharedCardsCollection.find({ 
        senderId: userId 
      })
      .sort({ _id: -1 })
      .limit(21) // Fetch 21 to check hasMore
      .toArray();
      
      console.log(`Returned: ${firstPage.length} cards`);
      console.log(`Has more pages: ${firstPage.length > 20}`);
      if (firstPage.length > 0) {
        console.log(`First card: "${firstPage[0].cardTitle}" to ${firstPage[0].recipientName}`);
        console.log(`Last card: "${firstPage[firstPage.length-1].cardTitle}" to ${firstPage[firstPage.length-1].recipientName}`);
      }
    } else {
      console.log('❌ No sent cards found');
    }

    // Check RECEIVED cards
    console.log('\n\n📥 RECEIVED CARDS:');
    console.log('='.repeat(60));
    
    const receivedCards = await sharedCardsCollection.find({ 
      recipientId: userId 
    })
    .sort({ _id: -1 })
    .toArray();
    
    console.log(`Total received cards: ${receivedCards.length}\n`);
    
    if (receivedCards.length > 0) {
      console.log('All received cards (newest first):');
      receivedCards.forEach((card, i) => {
        const date = new Date(card.sentAt || card.createdAt);
        console.log(`${i + 1}. From: ${card.senderName}`);
        console.log(`   Card: ${card.cardTitle}`);
        console.log(`   Received: ${date.toLocaleString()}`);
        console.log(`   _id: ${card._id}`);
        console.log(`   Viewed: ${card.status === 'viewed'}`);
        console.log('');
      });
      
      // Check what first page would return
      console.log('\n📄 FIRST PAGE (limit 20):');
      console.log('-'.repeat(60));
      const firstPage = await sharedCardsCollection.find({ 
        recipientId: userId 
      })
      .sort({ _id: -1 })
      .limit(21)
      .toArray();
      
      console.log(`Returned: ${firstPage.length} cards`);
      console.log(`Has more pages: ${firstPage.length > 20}`);
      if (firstPage.length > 0) {
        console.log(`First card: "${firstPage[0].cardTitle}" from ${firstPage[0].senderName}`);
        console.log(`Last card: "${firstPage[firstPage.length-1].cardTitle}" from ${firstPage[firstPage.length-1].recipientName}`);
      }
    } else {
      console.log('❌ No received cards found');
    }

    // Check data types consistency
    console.log('\n\n🔍 DATA TYPE ANALYSIS:');
    console.log('='.repeat(60));
    if (sentCards.length > 0 || receivedCards.length > 0) {
      const sampleCard = sentCards[0] || receivedCards[0];
      console.log('Sample card field types:');
      console.log(`   senderId: ${typeof sampleCard.senderId} (${sampleCard.senderId})`);
      console.log(`   recipientId: ${typeof sampleCard.recipientId} (${sampleCard.recipientId})`);
      console.log(`   _id: ${typeof sampleCard._id} (${sampleCard._id})`);
      console.log(`   cardPhoto: ${sampleCard.cardPhoto ? 'present' : 'missing'}`);
      console.log(`   senderProfilePicture: ${sampleCard.senderProfilePicture ? 'present' : 'missing'}`);
      console.log(`   recipientProfilePicture: ${sampleCard.recipientProfilePicture ? 'present' : 'missing'}`);
    }

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n👋 Disconnected from MongoDB');
  }
}

checkUserCards();
