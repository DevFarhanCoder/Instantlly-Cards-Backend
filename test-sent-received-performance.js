// Test script to analyze sent/received cards API performance
require('dotenv').config();
const mongoose = require('mongoose');

// Card Schema
const cardSchema = new mongoose.Schema({
  userId: { type: String, required: true },
  name: String,
  designation: String,
  contact: String,
  email: String,
  companyName: String,
  companyPhoto: String,
});
const Card = mongoose.model('Card', cardSchema);

// User Schema
const userSchema = new mongoose.Schema({
  phoneNumber: String,
  name: String,
  profilePicture: String,
});
const User = mongoose.model('User', userSchema);

// SharedCard Schema
const sharedCardSchema = new mongoose.Schema({
  cardId: { type: mongoose.Schema.Types.ObjectId, ref: 'Card', required: true },
  senderId: { type: String, required: true },
  recipientId: { type: String, required: true },
  message: { type: String, maxlength: 500, default: "" },
  status: { type: String, enum: ['sent', 'delivered', 'viewed'], default: 'sent' },
  sentAt: { type: Date, default: Date.now },
  viewedAt: { type: Date },
  cardTitle: { type: String, required: true },
  senderName: { type: String, required: true },
  recipientName: { type: String, required: true },
}, {
  timestamps: true
});

sharedCardSchema.index({ senderId: 1, sentAt: -1 });
sharedCardSchema.index({ recipientId: 1, sentAt: -1 });
sharedCardSchema.index({ cardId: 1 });
sharedCardSchema.index({ status: 1 });

const SharedCard = mongoose.model('SharedCard', sharedCardSchema);

async function testPerformance() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Get some sample user IDs
    const sampleShares = await SharedCard.find().limit(5).lean();
    if (sampleShares.length === 0) {
      console.log('❌ No shared cards found in database');
      process.exit(0);
    }

    console.log(`Found ${sampleShares.length} sample shared cards\n`);
    
    // Test with the first sender
    const testSenderId = sampleShares[0].senderId;
    const testRecipientId = sampleShares[0].recipientId;

    console.log(`\n📤 TESTING SENT CARDS QUERY FOR USER: ${testSenderId}`);
    console.log('='.repeat(60));
    
    // Test 1: Count total sent cards
    const countStart = Date.now();
    const totalSent = await SharedCard.countDocuments({ senderId: testSenderId });
    const countElapsed = Date.now() - countStart;
    console.log(`Total sent cards: ${totalSent} (took ${countElapsed}ms)`);

    // Test 2: Query with populate (like production)
    const queryStart = Date.now();
    const sentCards = await SharedCard.find({ senderId: testSenderId })
      .populate('cardId', 'companyName name companyPhoto')
      .populate('recipientId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .limit(100)
      .lean()
      .exec();
    const queryElapsed = Date.now() - queryStart;
    console.log(`Query with populate: ${sentCards.length} cards (took ${queryElapsed}ms)`);

    // Test 3: Query without populate (baseline)
    const baselineStart = Date.now();
    const sentCardsBaseline = await SharedCard.find({ senderId: testSenderId })
      .sort({ sentAt: -1 })
      .limit(100)
      .lean()
      .exec();
    const baselineElapsed = Date.now() - baselineStart;
    console.log(`Query without populate: ${sentCardsBaseline.length} cards (took ${baselineElapsed}ms)`);
    
    console.log(`\nPopulate overhead: ${queryElapsed - baselineElapsed}ms (${((queryElapsed - baselineElapsed) / queryElapsed * 100).toFixed(1)}%)`);

    // Analyze the results
    const validCards = sentCards.filter(c => c.recipientId && c.cardId).length;
    const invalidCards = sentCards.length - validCards;
    console.log(`Valid cards: ${validCards}, Invalid (deleted): ${invalidCards}`);

    console.log(`\n\n📥 TESTING RECEIVED CARDS QUERY FOR USER: ${testRecipientId}`);
    console.log('='.repeat(60));
    
    // Test 1: Count total received cards
    const countStart2 = Date.now();
    const totalReceived = await SharedCard.countDocuments({ recipientId: testRecipientId });
    const countElapsed2 = Date.now() - countStart2;
    console.log(`Total received cards: ${totalReceived} (took ${countElapsed2}ms)`);

    // Test 2: Query with populate (like production)
    const queryStart2 = Date.now();
    const receivedCards = await SharedCard.find({ recipientId: testRecipientId })
      .populate('cardId', 'companyName name companyPhoto')
      .populate('senderId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .limit(100)
      .lean()
      .exec();
    const queryElapsed2 = Date.now() - queryStart2;
    console.log(`Query with populate: ${receivedCards.length} cards (took ${queryElapsed2}ms)`);

    // Test 3: Query without populate (baseline)
    const baselineStart2 = Date.now();
    const receivedCardsBaseline = await SharedCard.find({ recipientId: testRecipientId })
      .sort({ sentAt: -1 })
      .limit(100)
      .lean()
      .exec();
    const baselineElapsed2 = Date.now() - baselineStart2;
    console.log(`Query without populate: ${receivedCardsBaseline.length} cards (took ${baselineElapsed2}ms)`);
    
    console.log(`\nPopulate overhead: ${queryElapsed2 - baselineElapsed2}ms (${((queryElapsed2 - baselineElapsed2) / queryElapsed2 * 100).toFixed(1)}%)`);

    // Analyze the results
    const validCards2 = receivedCards.filter(c => c.senderId && c.cardId).length;
    const invalidCards2 = receivedCards.length - validCards2;
    console.log(`Valid cards: ${validCards2}, Invalid (deleted): ${invalidCards2}`);

    // Check if indexes are being used
    console.log(`\n\n🔍 CHECKING INDEX USAGE`);
    console.log('='.repeat(60));
    
    const explainSent = await SharedCard.find({ senderId: testSenderId })
      .sort({ sentAt: -1 })
      .limit(100)
      .explain('executionStats');
    
    console.log('\nSent Cards Query Execution Stats:');
    console.log(`  - Execution time: ${explainSent.executionStats.executionTimeMillis}ms`);
    console.log(`  - Documents examined: ${explainSent.executionStats.totalDocsExamined}`);
    console.log(`  - Documents returned: ${explainSent.executionStats.nReturned}`);
    console.log(`  - Index used: ${explainSent.executionStats.executionStages.indexName || 'NONE (FULL SCAN!)'}`);
    
    const explainReceived = await SharedCard.find({ recipientId: testRecipientId })
      .sort({ sentAt: -1 })
      .limit(100)
      .explain('executionStats');
    
    console.log('\nReceived Cards Query Execution Stats:');
    console.log(`  - Execution time: ${explainReceived.executionStats.executionTimeMillis}ms`);
    console.log(`  - Documents examined: ${explainReceived.executionStats.totalDocsExamined}`);
    console.log(`  - Documents returned: ${explainReceived.executionStats.nReturned}`);
    console.log(`  - Index used: ${explainReceived.executionStats.executionStages.indexName || 'NONE (FULL SCAN!)'}`);

    // Get database stats
    console.log(`\n\n📊 DATABASE STATISTICS`);
    console.log('='.repeat(60));
    
    const stats = await SharedCard.collection.stats();
    console.log(`Total documents: ${stats.count}`);
    console.log(`Average document size: ${(stats.avgObjSize / 1024).toFixed(2)} KB`);
    console.log(`Total collection size: ${(stats.size / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Total index size: ${(stats.totalIndexSize / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Number of indexes: ${stats.nindexes}`);
    
    console.log('\n✅ Performance test complete\n');

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('👋 Disconnected from MongoDB');
  }
}

testPerformance();
