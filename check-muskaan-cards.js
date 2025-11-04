require('dotenv').config();
const mongoose = require('mongoose');

const SharedCard = mongoose.model('SharedCard', new mongoose.Schema({}, { strict: false, collection: 'sharedcards' }));
const GroupSharedCard = mongoose.model('GroupSharedCard', new mongoose.Schema({}, { strict: false, collection: 'groupsharedcards' }));
const GroupSession = mongoose.model('GroupSession', new mongoose.Schema({}, { strict: false, collection: 'groupsessions' }));
const CardShare = mongoose.model('CardShare', new mongoose.Schema({}, { strict: false, collection: 'cardshares' }));

async function checkMuskaanCards() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');
    
    const muskaanId = '69060586fb80202fced57791';
    const jatinId = '690487b54cb9574681fdac64';
    
    console.log('🔍 Checking cards shared WITH Muskaan (recipientId)...');
    const receivedByMuskaan = await SharedCard.find({ recipientId: muskaanId })
      .populate('cardId', 'name companyName')
      .populate('senderId', 'name')
      .sort({ sentAt: -1 })
      .limit(10);
    
    console.log(`\n📬 Cards received by Muskaan: ${receivedByMuskaan.length}`);
    receivedByMuskaan.forEach((card, i) => {
      console.log(`\n${i + 1}. From: ${card.senderId?.name || 'Unknown'}`);
      console.log(`   Card: ${card.cardId?.name || card.cardTitle}`);
      console.log(`   Company: ${card.cardId?.companyName || 'N/A'}`);
      console.log(`   Status: ${card.status}`);
      console.log(`   Sent At: ${card.sentAt}`);
      console.log(`   Card ID: ${card.cardId?._id}`);
      console.log(`   Share ID: ${card._id}`);
    });
    
    console.log('\n\n🔍 Checking cards shared BY Muskaan (senderId)...');
    const sentByMuskaan = await SharedCard.find({ senderId: muskaanId })
      .populate('cardId', 'name companyName')
      .populate('recipientId', 'name')
      .sort({ sentAt: -1 })
      .limit(10);
    
    console.log(`\n📤 Cards sent by Muskaan: ${sentByMuskaan.length}`);
    sentByMuskaan.forEach((card, i) => {
      console.log(`\n${i + 1}. To: ${card.recipientId?.name || 'Unknown'}`);
      console.log(`   Card: ${card.cardId?.name || card.cardTitle}`);
      console.log(`   Company: ${card.cardId?.companyName || 'N/A'}`);
      console.log(`   Status: ${card.status}`);
      console.log(`   Sent At: ${card.sentAt}`);
    });
    
    console.log('\n\n🔍 Checking cards shared WITH Jatin (recipientId)...');
    const receivedByJatin = await SharedCard.find({ recipientId: jatinId })
      .populate('cardId', 'name companyName')
      .populate('senderId', 'name')
      .sort({ sentAt: -1 })
      .limit(10);
    
    console.log(`\n📬 Cards received by Jatin: ${receivedByJatin.length}`);
    receivedByJatin.forEach((card, i) => {
      console.log(`\n${i + 1}. From: ${card.senderId?.name || 'Unknown'}`);
      console.log(`   Card: ${card.cardId?.name || card.cardTitle}`);
      console.log(`   Company: ${card.cardId?.companyName || 'N/A'}`);
      console.log(`   Status: ${card.status}`);
      console.log(`   Sent At: ${card.sentAt}`);
    });
    
    // Check GroupSharedCard collection
    console.log('\n\n🔍 Checking GROUP shared cards...');
    const groupShares = await GroupSharedCard.find({})
      .sort({ sharedAt: -1 })
      .limit(20);
    
    console.log(`\n📦 Total group shared cards: ${groupShares.length}`);
    groupShares.forEach((card, i) => {
      console.log(`\n${i + 1}. From: ${card.senderId} → To: ${card.recipientId}`);
      console.log(`   Card ID: ${card.card || card.cardId}`);
      console.log(`   Session: ${card.sessionId}`);
      console.log(`   Shared At: ${card.sharedAt}`);
      console.log(`   Full record:`, JSON.stringify(card, null, 2));
    });
    
    // Check most recent group session
    console.log('\n\n🔍 Checking most recent group session...');
    const recentSession = await GroupSession.findOne({ code: '3736' }).sort({ createdAt: -1 });
    
    if (recentSession) {
      console.log('\n📋 Session details:');
      console.log('   Code:', recentSession.code);
      console.log('   Admin:', recentSession.adminId);
      console.log('   Status:', recentSession.status);
      console.log('   Active:', recentSession.isActive);
      console.log('   Participants:', recentSession.participants?.length);
      console.log('   Created:', recentSession.createdAt);
      console.log('   Full session:', JSON.stringify(recentSession, null, 2));
    } else {
      console.log('❌ No session found with code 3736');
    }
    
    // Check CardShare collection (duplicate tracking)
    console.log('\n\n🔍 Checking CardShare records (duplicate tracking)...');
    const cardShares = await CardShare.find({}).sort({ createdAt: -1 }).limit(10);
    console.log(`\n📝 Total CardShare records: ${cardShares.length}`);
    cardShares.forEach((share, i) => {
      console.log(`\n${i + 1}. From: ${share.fromUserId} → To: ${share.toUserId}`);
      console.log(`   Card: ${share.cardId}`);
      console.log(`   Session: ${share.sessionId}`);
      console.log(`   Created: ${share.createdAt}`);
    });
    
    await mongoose.disconnect();
    console.log('\n✅ Disconnected from MongoDB');
    
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

checkMuskaanCards();
