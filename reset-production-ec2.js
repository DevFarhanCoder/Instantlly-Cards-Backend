/**
 * Reset credits on ACTUAL PRODUCTION MongoDB (AWS EC2)
 */

const mongoose = require('mongoose');

// PRODUCTION MongoDB on AWS EC2
const PROD_MONGO_URI = "mongodb://rajeshmodi:Newpass1234@3.90.205.216:27017/instantlly?authSource=admin";

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: Number,
  referredBy: String,
  referralCode: String
}, { collection: 'users', strict: false });

const User = mongoose.model('User', userSchema);

async function resetProductionCredits() {
  try {
    console.log('🔄 Connecting to PRODUCTION MongoDB (AWS EC2)...');
    await mongoose.connect(PROD_MONGO_URI);
    console.log('✅ Connected to:', mongoose.connection.db.databaseName);
    console.log('📍 Host: 3.90.205.216\n');

    // Find all users
    const totalUsers = await User.countDocuments();
    console.log(`📊 Total users: ${totalUsers}`);

    // Check for users with high credits
    const highCreditUsers = await User.find({ credits: { $gt: 1000 } });
    console.log(`⚠️  Users with > 1000 credits: ${highCreditUsers.length}\n`);

    if (highCreditUsers.length > 0) {
      console.log('Users with high credits:');
      highCreditUsers.forEach(u => {
        console.log(`  ${u.name || u.phone}: ${u.credits} credits (Referral: ${u.referralCode || 'N/A'})`);
      });

      console.log('\n⏳ Resetting ALL users to 200 credits in 3 seconds...');
      await new Promise(resolve => setTimeout(resolve, 3000));

      // Reset introducers (with referredBy) to 300, others to 200
      const introducerResult = await User.updateMany(
        { referredBy: { $exists: true, $ne: null, $ne: '' } },
        { $set: { credits: 300 } }
      );

      const regularResult = await User.updateMany(
        { $or: [{ referredBy: { $exists: false } }, { referredBy: null }, { referredBy: '' }] },
        { $set: { credits: 200 } }
      );

      console.log('\n✅ RESET COMPLETE!');
      console.log(`   Introducers (300 credits): ${introducerResult.modifiedCount}`);
      console.log(`   Regular users (200 credits): ${regularResult.modifiedCount}`);
    } else {
      console.log('⚠️  No users with high credits found. Resetting all to 200 anyway...\n');
      
      const result = await User.updateMany({}, { $set: { credits: 200 } });
      console.log(`✅ Reset ${result.modifiedCount} users to 200 credits!`);
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    console.log('\n🔌 Database connection closed');
    process.exit(0);
  }
}

resetProductionCredits();
