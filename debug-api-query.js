const mongoose = require('mongoose');
require('dotenv').config();

// Connect to MongoDB
mongoose.connect(process.env.MONGODB_URI || 'mongodb://localhost:27017/instantlly-cards')
  .then(() => console.log('✅ Connected to MongoDB'))
  .catch(err => console.error('❌ MongoDB connection error:', err));

// Define schema first
const SharedCardSchema = new mongoose.Schema({
  senderId: { type: mongoose.Schema.Types.ObjectId, ref: 'User', required: true },
  recipientId: { type: mongoose.Schema.Types.ObjectId, ref: 'User', required: true },
  cardId: { type: mongoose.Schema.Types.ObjectId, ref: 'Card', required: true },
  senderName: String,
  recipientName: String,
  cardTitle: String,
  message: String,
  status: { type: String, enum: ['sent', 'viewed'], default: 'sent' },
  sentAt: { type: Date, default: Date.now },
  viewedAt: Date
});

const SharedCard = mongoose.model('SharedCard', SharedCardSchema);

async function debugAPIQuery() {
  try {
    console.log('\n🔍 DEBUGGING API QUERY MISMATCH\n');
    
    const rajeshId = '68edfc0739b50dcdcacd3c5b';
    console.log(`🎯 Testing query for Rajesh Modi ID: ${rajeshId}`);
    
    console.log('\n1️⃣ TESTING SENT CARDS QUERY (exact API replica):');
    const sentCardsQuery = SharedCard.find({ senderId: new mongoose.Types.ObjectId(rajeshId) })
      .populate('cardId', 'companyName name companyPhoto')
      .populate('recipientId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .lean();
      
    const sentCards = await sentCardsQuery.exec();
    console.log(`   Raw query result: ${sentCards.length} cards`);
    
    // Test filtering
    const filteredSent = sentCards.filter((share) => share.recipientId && share.cardId);
    console.log(`   After filtering: ${filteredSent.length} cards`);
    console.log(`   Filtered out: ${sentCards.length - filteredSent.length} cards`);
    
    // Show first few results
    console.log('\n   First 3 sent cards:');
    sentCards.slice(0, 3).forEach((card, i) => {
      console.log(`   ${i+1}. Recipient: ${card.recipientId ? '✓' : '❌'} Card: ${card.cardId ? '✓' : '❌'}`);
      console.log(`      RecipientId: ${card.recipientId?._id || 'MISSING'}`);
      console.log(`      CardId: ${card.cardId?._id || 'MISSING'}`);
    });
    
    console.log('\n2️⃣ TESTING RECEIVED CARDS QUERY (exact API replica):');
    const receivedCardsQuery = SharedCard.find({ recipientId: new mongoose.Types.ObjectId(rajeshId) })
      .populate('cardId', 'companyName name companyPhoto')
      .populate('senderId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .lean();
      
    const receivedCards = await receivedCardsQuery.exec();
    console.log(`   Raw query result: ${receivedCards.length} cards`);
    
    // Test filtering
    const filteredReceived = receivedCards.filter((share) => share.senderId && share.cardId);
    console.log(`   After filtering: ${filteredReceived.length} cards`);
    console.log(`   Filtered out: ${receivedCards.length - filteredReceived.length} cards`);
    
    // Show first few results  
    console.log('\n   First 3 received cards:');
    receivedCards.slice(0, 3).forEach((card, i) => {
      console.log(`   ${i+1}. Sender: ${card.senderId ? '✓' : '❌'} Card: ${card.cardId ? '✓' : '❌'}`);
      console.log(`      SenderId: ${card.senderId?._id || 'MISSING'}`);
      console.log(`      CardId: ${card.cardId?._id || 'MISSING'}`);
    });
    
    console.log('\n3️⃣ CHECKING COLLECTION NAMES:');
    const collections = await mongoose.connection.db.listCollections().toArray();
    const sharedCardCollections = collections.filter(c => c.name.toLowerCase().includes('shared'));
    console.log('   Collections with "shared" in name:');
    sharedCardCollections.forEach(c => console.log(`   - ${c.name}`));
    
    console.log('\n4️⃣ RAW COLLECTION COUNT:');
    const rawCount = await SharedCard.countDocuments({ senderId: new mongoose.Types.ObjectId(rajeshId) });
    console.log(`   Direct count of sent cards: ${rawCount}`);
    
    const rawReceivedCount = await SharedCard.countDocuments({ recipientId: new mongoose.Types.ObjectId(rajeshId) });
    console.log(`   Direct count of received cards: ${rawReceivedCount}`);
    
  } catch (error) {
    console.error('❌ Error debugging API query:', error);
  } finally {
    mongoose.connection.close();
  }
}

debugAPIQuery();