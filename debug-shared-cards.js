const mongoose = require('mongoose');
require('dotenv').config();

// Connect to MongoDB
mongoose.connect(process.env.MONGODB_URI || 'mongodb://localhost:27017/instantlly-cards')
  .then(() => console.log('✅ Connected to MongoDB'))
  .catch(err => console.error('❌ MongoDB connection error:', err));

// Define models (simplified)
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

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  email: String
});

const CardSchema = new mongoose.Schema({
  userId: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
  name: String,
  companyName: String
});

const SharedCard = mongoose.model('SharedCard', SharedCardSchema);
const User = mongoose.model('User', UserSchema);
const Card = mongoose.model('Card', CardSchema);

async function debugSharedCards() {
  try {
    console.log('\n🔍 DEBUGGING SHARED CARDS DATA\n');
    
    // Find Rajesh Modi's user ID
    const rajeshPhone = '+919867477227';
    const rajesh = await User.findOne({ 
      $or: [
        { phone: rajeshPhone },
        { phone: '919867477227' },
        { name: /rajesh.*modi/i }
      ]
    });
    
    if (!rajesh) {
      console.log('❌ Rajesh Modi not found in database');
      return;
    }
    
    console.log('👤 Found Rajesh Modi:');
    console.log(`   ID: ${rajesh._id}`);
    console.log(`   Name: ${rajesh.name}`);
    console.log(`   Phone: ${rajesh.phone}`);
    
    // Check sent cards
    console.log('\n📤 SENT CARDS (by Rajesh):');
    const sentCards = await SharedCard.find({ senderId: rajesh._id })
      .populate('recipientId', 'name phone')
      .populate('cardId', 'name companyName')
      .sort({ sentAt: -1 });
    
    console.log(`   Found ${sentCards.length} sent cards:`);
    sentCards.forEach((card, i) => {
      console.log(`   ${i+1}. To: ${card.recipientName || card.recipientId?.name || 'Unknown'}`);
      console.log(`      Card: ${card.cardTitle || card.cardId?.name || 'Unknown'}`);
      console.log(`      Status: ${card.status}`);
      console.log(`      Sent: ${card.sentAt}`);
      console.log(`      Recipient ID: ${card.recipientId?._id || 'Missing'}`);
      console.log(`      Card ID: ${card.cardId?._id || 'Missing'}`);
    });
    
    // Check received cards
    console.log('\n📥 RECEIVED CARDS (by Rajesh):');
    const receivedCards = await SharedCard.find({ recipientId: rajesh._id })
      .populate('senderId', 'name phone')
      .populate('cardId', 'name companyName')
      .sort({ sentAt: -1 });
    
    console.log(`   Found ${receivedCards.length} received cards:`);
    receivedCards.forEach((card, i) => {
      console.log(`   ${i+1}. From: ${card.senderName || card.senderId?.name || 'Unknown'}`);
      console.log(`      Card: ${card.cardTitle || card.cardId?.name || 'Unknown'}`);
      console.log(`      Status: ${card.status}`);
      console.log(`      Sent: ${card.sentAt}`);
      console.log(`      Sender ID: ${card.senderId?._id || 'Missing'}`);
      console.log(`      Card ID: ${card.cardId?._id || 'Missing'}`);
    });
    
    // Check all shared cards in database
    console.log('\n📊 ALL SHARED CARDS IN DATABASE:');
    const allSharedCards = await SharedCard.find({}).sort({ sentAt: -1 }).limit(10);
    console.log(`   Total shared cards: ${allSharedCards.length}`);
    
    allSharedCards.forEach((card, i) => {
      console.log(`   ${i+1}. Sender: ${card.senderId} → Recipient: ${card.recipientId}`);
      console.log(`      Status: ${card.status}, Sent: ${card.sentAt}`);
    });
    
    // Check if there are any cards with deleted references
    console.log('\n🔍 CHECKING FOR BROKEN REFERENCES:');
    const brokenCards = await SharedCard.find({})
      .populate('senderId', '_id name')
      .populate('recipientId', '_id name') 
      .populate('cardId', '_id name');
      
    const broken = brokenCards.filter(card => 
      !card.senderId || !card.recipientId || !card.cardId
    );
    
    console.log(`   Found ${broken.length} cards with broken references:`);
    broken.forEach((card, i) => {
      console.log(`   ${i+1}. ID: ${card._id}`);
      console.log(`      Missing sender: ${!card.senderId}`);
      console.log(`      Missing recipient: ${!card.recipientId}`);  
      console.log(`      Missing card: ${!card.cardId}`);
    });
    
  } catch (error) {
    console.error('❌ Error debugging shared cards:', error);
  } finally {
    mongoose.connection.close();
  }
}

debugSharedCards();