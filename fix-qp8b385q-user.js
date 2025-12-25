/**
 * Find and fix user with referral code QP8B385Q
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
}, { collection: 'users' });

const User = mongoose.model('User', userSchema);

async function findAndFixUser() {
  try {
    await mongoose.connect(MONGO_URI);
    console.log('✅ Connected to MongoDB\n');

    // Find user with referral code QP8B385Q
    const user = await User.findOne({ referralCode: 'QP8B385Q' });
    
    if (!user) {
      console.log('❌ User with referral code QP8B385Q not found!');
      return;
    }

    console.log('👤 Found user:');
    console.log(`   Name: ${user.name}`);
    console.log(`   Phone: ${user.phone}`);
    console.log(`   Email: ${user.email || 'N/A'}`);
    console.log(`   Credits: ${user.credits}`);
    console.log(`   Referral Code: ${user.referralCode}`);
    console.log(`   Referred By: ${user.referredBy || 'None'}`);

    if (user.credits === 500000) {
      console.log('\n⚠️  User still has 500000 credits!');
      console.log('🔄 Resetting to 200...');
      
      user.credits = 200;
      await user.save();
      
      console.log('✅ User credits reset to 200!');
    } else {
      console.log('\n✅ User already has correct credits:', user.credits);
    }

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.connection.close();
    console.log('\n🔌 Database connection closed');
    process.exit(0);
  }
}

findAndFixUser();
