import mongoose from 'mongoose';
import User from '../models/User';
import dotenv from 'dotenv';

dotenv.config();

async function checkCredits() {
  await mongoose.connect(process.env.MONGODB_URI!);
  console.log(' Connected to MongoDB\n');
  
  // Get credit distribution
  const distribution = await User.aggregate([
    { $group: { _id: '$credits', count: { $sum: 1 } } },
    { $sort: { count: -1 } }
  ]);
  
  console.log(' Credit Distribution:');
  distribution.forEach(d => {
    console.log(`   ${d._id} credits: ${d.count} users`);
  });
  
  // Show sample users with high credits
  console.log('\n Users with credits > 10000:');
  const highCredit = await User.find({ credits: { $gt: 10000 } }).limit(5);
  highCredit.forEach(u => {
    console.log(`   ${u.name} (${u.phone}): ${(u as any).credits} credits`);
  });
  
  await mongoose.disconnect();
}

checkCredits();
