/**
 * Find ALL users with 500000 credits
 */

require('dotenv').config();
const mongoose = require('mongoose');

const MONGO_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  email: String,
  credits: Number,
  referralCode: String,
  referredBy: String
}, { collection: 'users', strict: false });

const User = mongoose.model('User', userSchema);

async function findAllWithHighCredits() {
  try {
    await mongoose.connect(MONGO_URI);
    console.log('✅ Connected to MongoDB\n');
    console.log('🔍 Searching for users with 500000 credits...\n');

    const usersWithHighCredits = await User.find({ credits: 500000 });
    
    console.log(`📊 Found ${usersWithHighCredits.length} users with 500000 credits\n`);

    if (usersWithHighCredits.length === 0) {
      // Check if there are users with other high credit values
      console.log('🔍 Checking for users with credits > 10000...');
      const highCredits = await User.find({ credits: { $gt: 10000 } });
      
      if (highCredits.length > 0) {
        console.log(`\n⚠️  Found ${highCredits.length} users with high credits:\n`);
        highCredits.forEach(user => {
          console.log(`👤 ${user.name || user.phone}`);
          console.log(`   Credits: ${user.credits}`);
          console.log(`   Referral Code: ${user.referralCode || 'N/A'}`);
          console.log(`   Phone: ${user.phone}\n`);
        });
      } else {
        console.log('✅ No users with high credits found!');
      }
    } else {
      usersWithHighCredits.forEach((user, index) => {
        console.log(`${index + 1}. ${user.name || user.phone}`);
        console.log(`   Credits: ${user.credits}`);
        console.log(`   Referral Code: ${user.referralCode || 'N/A'}`);
        console.log(`   Phone: ${user.phone}`);
        console.log(`   Referred By: ${user.referredBy || 'None'}\n`);
      });

      console.log('🔄 Fixing all these users...\n');
      
      const result = await User.updateMany(
        { credits: 500000 },
        { $set: { credits: 200 } }
      );

      console.log(`✅ Fixed ${result.modifiedCount} users!`);
    }

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.connection.close();
    console.log('\n🔌 Database connection closed');
    process.exit(0);
  }
}

findAllWithHighCredits();
