// Check group sharing status
const mongoose = require('mongoose');
require('dotenv').config();

const SharedCard = require('./dist/models/SharedCard').default;
const GroupSession = require('./dist/models/GroupSession').default;
const CardShare = require('./dist/models/CardShare').default;
const User = require('./dist/models/User').default;

async function checkGroupSharing() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    // Find recent group sessions
    console.log('\n📊 RECENT GROUP SESSIONS:');
    const sessions = await GroupSession.find()
      .sort({ createdAt: -1 })
      .limit(5)
      .lean();

    for (const session of sessions) {
      console.log(`\nSession ID: ${session._id}`);
      console.log(`Code: ${session.code}`);
      console.log(`Admin: ${session.adminName} (${session.adminPhone})`);
      console.log(`Status: ${session.status}`);
      console.log(`Participants: ${session.participants.length}`);
      
      session.participants.forEach((p, i) => {
        console.log(`  ${i + 1}. ${p.userName} (${p.userPhone})`);
        console.log(`     Cards to share: ${p.cardsToShare.length}`);
        console.log(`     Default card: ${p.defaultCardId || 'None'}`);
      });

      // Check CardShare records for this session
      const cardShares = await CardShare.find({ sessionId: session._id }).lean();
      console.log(`CardShare records: ${cardShares.length}`);

      // Check SharedCard records from this session's participants
      const participantIds = session.participants.map(p => p.userId);
      const sharedCards = await SharedCard.find({
        senderId: { $in: participantIds },
        recipientId: { $in: participantIds },
        message: { $regex: session.code }
      }).lean();
      
      console.log(`SharedCard records (matching session code in message): ${sharedCards.length}`);
    }

    // Find Vibhuti Jain (Admin)
    console.log('\n\n📱 CHECKING VIBHUTI JAIN (ADMIN):');
    const vibhuti = await User.findOne({ name: /vibhuti/i }).lean();
    if (vibhuti) {
      console.log(`Found: ${vibhuti.name} (${vibhuti.phone})`);
      console.log(`User ID: ${vibhuti._id}`);

      // Cards sent by Vibhuti
      const sentCards = await SharedCard.find({ senderId: vibhuti._id })
        .sort({ sentAt: -1 })
        .limit(10)
        .lean();
      console.log(`\nCards sent: ${sentCards.length}`);
      sentCards.forEach((card, i) => {
        console.log(`  ${i + 1}. To: ${card.recipientName}`);
        console.log(`     Card: ${card.cardTitle}`);
        console.log(`     Message: ${card.message.substring(0, 50)}...`);
        console.log(`     Sent at: ${card.sentAt}`);
      });

      // Cards received by Vibhuti
      const receivedCards = await SharedCard.find({ recipientId: vibhuti._id })
        .sort({ sentAt: -1 })
        .limit(10)
        .lean();
      console.log(`\nCards received: ${receivedCards.length}`);
      receivedCards.forEach((card, i) => {
        console.log(`  ${i + 1}. From: ${card.senderName}`);
        console.log(`     Card: ${card.cardTitle}`);
        console.log(`     Message: ${card.message.substring(0, 50)}...`);
        console.log(`     Received at: ${card.sentAt}`);
      });
    } else {
      console.log('Vibhuti Jain not found');
    }

    // Check other participants
    console.log('\n\n👥 CHECKING OTHER PARTICIPANTS:');
    const names = ['sonu', 'pummy', 'dinky', 'lakhan'];
    
    for (const name of names) {
      const user = await User.findOne({ name: new RegExp(name, 'i') }).lean();
      if (user) {
        const sent = await SharedCard.countDocuments({ senderId: user._id });
        const received = await SharedCard.countDocuments({ recipientId: user._id });
        console.log(`${user.name}: Sent ${sent}, Received ${received}`);
      } else {
        console.log(`${name}: Not found`);
      }
    }

    await mongoose.connection.close();
    console.log('\n✅ Done');

  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

checkGroupSharing();
