const mongoose = require('mongoose');
require('dotenv').config();

async function main() {
  await mongoose.connect(process.env.MONGODB_URI);
  console.log('Connected to MongoDB');
  
  const Transaction = require('./src/models/Transaction').default;
  
  const txns = await Transaction.find({ 
    type: { $in: ['transfer_sent', 'transfer_received', 'transfer'] } 
  })
  .populate('fromUser', 'name phone')
  .populate('toUser', 'name phone')
  .sort({ createdAt: -1 })
  .limit(20)
  .lean();
  
  console.log('\n=== TRANSFER TRANSACTIONS ===\n');
  
  txns.forEach((txn, i) => {
    console.log(`${i + 1}. Type: ${txn.type}`);
    console.log(`   From: ${txn.fromUser?.name} (${txn.fromUser?._id})`);
    console.log(`   To: ${txn.toUser?.name} (${txn.toUser?._id})`);
    console.log(`   Amount: ${txn.amount}`);
    console.log(`   Desc: ${txn.description}`);
    console.log(`   Created: ${txn.createdAt}`);
    console.log('');
  });
  
  await mongoose.disconnect();
}

main().catch(console.error);
