const mongoose = require('mongoose');
require('dotenv').config();

async function debugRawQuery() {
  // Connect to MongoDB
  await mongoose.connect(process.env.MONGODB_URI || 'mongodb://localhost:27017/instantlly-cards');
  console.log('✅ Connected to MongoDB');
  try {
    console.log('\n🔍 DEBUGGING RAW DATABASE QUERIES\n');
    
    const rajeshId = '68edfc0739b50dcdcacd3c5b';
    console.log(`🎯 Testing for Rajesh Modi ID: ${rajeshId}`);
    
    // Get the raw collection without mongoose models
    const db = mongoose.connection.db;
    const sharedCardsCollection = db.collection('sharedcards');
    
    console.log('\n1️⃣ RAW SENT CARDS COUNT:');
    const sentCount = await sharedCardsCollection.countDocuments({ 
      senderId: new mongoose.Types.ObjectId(rajeshId) 
    });
    console.log(`   Found ${sentCount} sent cards`);
    
    console.log('\n2️⃣ RAW RECEIVED CARDS COUNT:');
    const receivedCount = await sharedCardsCollection.countDocuments({ 
      recipientId: new mongoose.Types.ObjectId(rajeshId) 
    });
    console.log(`   Found ${receivedCount} received cards`);
    
    console.log('\n3️⃣ SAMPLE SENT CARDS (first 3):');
    const sampleSent = await sharedCardsCollection.find({ 
      senderId: new mongoose.Types.ObjectId(rajeshId) 
    }).limit(3).toArray();
    
    sampleSent.forEach((card, i) => {
      console.log(`   ${i+1}. To: ${card.recipientId}`);
      console.log(`      Card: ${card.cardId}`);  
      console.log(`      Status: ${card.status}`);
      console.log(`      Date: ${card.sentAt}`);
    });
    
    console.log('\n4️⃣ SAMPLE RECEIVED CARDS (first 3):');
    const sampleReceived = await sharedCardsCollection.find({ 
      recipientId: new mongoose.Types.ObjectId(rajeshId) 
    }).limit(3).toArray();
    
    sampleReceived.forEach((card, i) => {
      console.log(`   ${i+1}. From: ${card.senderId}`);
      console.log(`      Card: ${card.cardId}`);
      console.log(`      Status: ${card.status}`); 
      console.log(`      Date: ${card.sentAt}`);
    });
    
    console.log('\n5️⃣ CHECKING COLLECTION NAMES:');
    const collections = await db.listCollections().toArray();
    console.log('   All collections:');
    collections.forEach(c => console.log(`   - ${c.name}`));
    
    // Also check if there's an issue with recent dates
    console.log('\n6️⃣ RECENT SENT CARDS (last 7 days):');
    const sevenDaysAgo = new Date();
    sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);
    
    const recentSent = await sharedCardsCollection.countDocuments({
      senderId: new mongoose.Types.ObjectId(rajeshId),
      sentAt: { $gte: sevenDaysAgo }
    });
    console.log(`   Recent sent cards: ${recentSent}`);
    
    const recentReceived = await sharedCardsCollection.countDocuments({
      recipientId: new mongoose.Types.ObjectId(rajeshId),
      sentAt: { $gte: sevenDaysAgo }
    });
    console.log(`   Recent received cards: ${recentReceived}`);
    
  } catch (error) {
    console.error('❌ Error debugging raw query:', error);
  } finally {
    mongoose.connection.close();
  }
}

debugRawQuery();