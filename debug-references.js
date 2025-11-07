const mongoose = require('mongoose');
require('dotenv').config();

async function debugReferences() {
  await mongoose.connect(process.env.MONGODB_URI || 'mongodb://localhost:27017/instantlly-cards');
  console.log('✅ Connected to MongoDB');

  try {
    console.log('\n🔍 DEBUGGING REFERENCE INTEGRITY\n');
    
    const db = mongoose.connection.db;
    const sharedCardsCollection = db.collection('sharedcards');
    const usersCollection = db.collection('users');
    const cardsCollection = db.collection('cards');
    
    const rajeshId = new mongoose.Types.ObjectId('68edfc0739b50dcdcacd3c5b');
    
    // Get sent cards
    console.log('📤 CHECKING SENT CARDS REFERENCES:');
    const sentCards = await sharedCardsCollection.find({ senderId: rajeshId })
      .sort({ sentAt: -1 }).limit(5).toArray();
    
    console.log(`   Testing first 5 of ${sentCards.length} sent cards:`);
    
    for (let i = 0; i < sentCards.length; i++) {
      const card = sentCards[i];
      console.log(`\n   ${i+1}. SharedCard ID: ${card._id}`);
      console.log(`      Sent At: ${card.sentAt}`);
      
      // Check if recipient exists
      const recipient = await usersCollection.findOne({ _id: card.recipientId });
      console.log(`      Recipient (${card.recipientId}): ${recipient ? '✓ EXISTS' : '❌ MISSING'}`);
      if (recipient) console.log(`        Name: ${recipient.name}`);
      
      // Check if card exists  
      const cardDoc = await cardsCollection.findOne({ _id: card.cardId });
      console.log(`      Card (${card.cardId}): ${cardDoc ? '✓ EXISTS' : '❌ MISSING'}`);
      if (cardDoc) console.log(`        Name: ${cardDoc.name || cardDoc.companyName}`);
      
      const isValid = recipient && cardDoc;
      console.log(`      WOULD BE FILTERED: ${isValid ? 'NO ✓' : 'YES ❌'}`);
    }
    
    console.log('\n📥 CHECKING RECEIVED CARDS REFERENCES:');
    const receivedCards = await sharedCardsCollection.find({ recipientId: rajeshId })
      .sort({ sentAt: -1 }).limit(5).toArray();
    
    console.log(`   Testing first 5 of ${receivedCards.length} received cards:`);
    
    for (let i = 0; i < receivedCards.length; i++) {
      const card = receivedCards[i];
      console.log(`\n   ${i+1}. SharedCard ID: ${card._id}`);
      console.log(`      Sent At: ${card.sentAt}`);
      
      // Check if sender exists
      const sender = await usersCollection.findOne({ _id: card.senderId });
      console.log(`      Sender (${card.senderId}): ${sender ? '✓ EXISTS' : '❌ MISSING'}`);
      if (sender) console.log(`        Name: ${sender.name}`);
      
      // Check if card exists
      const cardDoc = await cardsCollection.findOne({ _id: card.cardId });
      console.log(`      Card (${card.cardId}): ${cardDoc ? '✓ EXISTS' : '❌ MISSING'}`);
      if (cardDoc) console.log(`        Name: ${cardDoc.name || cardDoc.companyName}`);
      
      const isValid = sender && cardDoc;
      console.log(`      WOULD BE FILTERED: ${isValid ? 'NO ✓' : 'YES ❌'}`);
    }
    
    // Count valid vs invalid
    console.log('\n📊 SUMMARY OF REFERENCE INTEGRITY:');
    
    let validSentCount = 0;
    let invalidSentCount = 0;
    
    const allSentCards = await sharedCardsCollection.find({ senderId: rajeshId }).toArray();
    
    for (const card of allSentCards) {
      const recipient = await usersCollection.findOne({ _id: card.recipientId });
      const cardDoc = await cardsCollection.findOne({ _id: card.cardId });
      
      if (recipient && cardDoc) {
        validSentCount++;
      } else {
        invalidSentCount++;
      }
    }
    
    console.log(`   Sent Cards - Valid: ${validSentCount}, Invalid: ${invalidSentCount}`);
    
    let validReceivedCount = 0;
    let invalidReceivedCount = 0;
    
    const allReceivedCards = await sharedCardsCollection.find({ recipientId: rajeshId }).toArray();
    
    for (const card of allReceivedCards) {
      const sender = await usersCollection.findOne({ _id: card.senderId });
      const cardDoc = await cardsCollection.findOne({ _id: card.cardId });
      
      if (sender && cardDoc) {
        validReceivedCount++;
      } else {
        invalidReceivedCount++;
      }
    }
    
    console.log(`   Received Cards - Valid: ${validReceivedCount}, Invalid: ${invalidReceivedCount}`);
    
  } catch (error) {
    console.error('❌ Error debugging references:', error);
  } finally {
    mongoose.connection.close();
  }
}

debugReferences();