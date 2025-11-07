// Debug script to check Rajesh Modi's received cards
require('dotenv').config();
const mongoose = require('mongoose');

async function debugRajeshCards() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    const db = mongoose.connection.db;
    const usersCollection = db.collection('users');
    const sharedCardsCollection = db.collection('sharedcards');

    // Find Rajesh Modi
    console.log('🔍 Searching for Rajesh Modi...');
    const rajesh = await usersCollection.findOne({ 
      $or: [
        { name: /rajesh/i },
        { phoneNumber: /rajesh/i }
      ]
    });

    if (!rajesh) {
      console.log('❌ Rajesh Modi not found in users collection');
      
      // List all users to find the right one
      const allUsers = await usersCollection.find({}).limit(10).toArray();
      console.log('\n📋 Available users (first 10):');
      allUsers.forEach(u => {
        console.log(`   - ${u.name} (${u.phoneNumber}) - ID: ${u._id}`);
      });
      return;
    }

    console.log(`✅ Found Rajesh Modi:`);
    console.log(`   Name: ${rajesh.name}`);
    console.log(`   Phone: ${rajesh.phoneNumber}`);
    console.log(`   ID: ${rajesh._id}`);
    console.log(`   String ID: ${rajesh._id.toString()}`);

    // Check received cards with recipientId as ObjectId
    console.log(`\n🔍 Checking received cards (recipientId as ObjectId)...`);
    const receivedAsObjectId = await sharedCardsCollection.find({ 
      recipientId: rajesh._id 
    }).toArray();
    console.log(`   Found: ${receivedAsObjectId.length} cards`);

    // Check received cards with recipientId as String
    console.log(`\n🔍 Checking received cards (recipientId as String)...`);
    const receivedAsString = await sharedCardsCollection.find({ 
      recipientId: rajesh._id.toString() 
    }).toArray();
    console.log(`   Found: ${receivedAsString.length} cards`);

    // Check sent cards with senderId as ObjectId
    console.log(`\n🔍 Checking sent cards (senderId as ObjectId)...`);
    const sentAsObjectId = await sharedCardsCollection.find({ 
      senderId: rajesh._id 
    }).toArray();
    console.log(`   Found: ${sentAsObjectId.length} cards`);

    // Check sent cards with senderId as String
    console.log(`\n🔍 Checking sent cards (senderId as String)...`);
    const sentAsString = await sharedCardsCollection.find({ 
      senderId: rajesh._id.toString() 
    }).toArray();
    console.log(`   Found: ${sentAsString.length} cards`);

    // Sample some shared cards to check data types
    console.log(`\n🔍 Analyzing shared cards data types...`);
    const sampleCards = await sharedCardsCollection.find({}).limit(5).toArray();
    console.log(`\nSample shared cards (first 5):`);
    sampleCards.forEach((card, i) => {
      console.log(`\n${i + 1}. Card ${card._id}:`);
      console.log(`   senderId type: ${typeof card.senderId} - value: ${card.senderId}`);
      console.log(`   recipientId type: ${typeof card.recipientId} - value: ${card.recipientId}`);
      console.log(`   senderName: ${card.senderName}`);
      console.log(`   recipientName: ${card.recipientName}`);
      console.log(`   cardTitle: ${card.cardTitle}`);
      console.log(`   Has cardPhoto: ${!!card.cardPhoto}`);
      console.log(`   Has senderProfilePicture: ${!!card.senderProfilePicture}`);
      console.log(`   Has recipientProfilePicture: ${!!card.recipientProfilePicture}`);
    });

    // Check if there are any cards at all
    const totalSharedCards = await sharedCardsCollection.countDocuments();
    console.log(`\n📊 Total shared cards in database: ${totalSharedCards}`);

    // Find all unique recipientIds
    console.log(`\n🔍 Finding all unique recipient IDs...`);
    const allRecipientIds = await sharedCardsCollection.distinct('recipientId');
    console.log(`   Total unique recipients: ${allRecipientIds.length}`);
    console.log(`   Sample recipient IDs (first 5):`);
    allRecipientIds.slice(0, 5).forEach(id => {
      console.log(`      - Type: ${typeof id}, Value: ${id}`);
    });

    // Check if Rajesh's ID matches any pattern
    const rajeshIdString = rajesh._id.toString();
    const matchesAsString = allRecipientIds.filter(id => id === rajeshIdString).length;
    const matchesAsObjectId = allRecipientIds.filter(id => id.toString() === rajeshIdString).length;
    
    console.log(`\n🔍 Matching analysis:`);
    console.log(`   Rajesh ID in recipients (as string): ${matchesAsString > 0 ? 'YES' : 'NO'}`);
    console.log(`   Rajesh ID in recipients (as ObjectId): ${matchesAsObjectId > 0 ? 'YES' : 'NO'}`);

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n👋 Disconnected from MongoDB');
  }
}

debugRajeshCards();
