/**
 * Find the user with ID 693d15e60a4316be169dd757 and check their credits
 */

require('dotenv').config();
const mongoose = require('mongoose');

const MONGO_URI = process.env.MONGODB_URI; // Should be test database

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  email: String,
  credits: Number,
  referralCode: String,
  referredBy: String
}, { collection: 'users', strict: false });

const User = mongoose.model('User', userSchema);

async function checkSpecificUser() {
  try {
    await mongoose.connect(MONGO_URI);
    console.log('✅ Connected to MongoDB');
    console.log(`📍 Database: ${mongoose.connection.db.databaseName}\n`);

    const userId = '693d15e60a4316be169dd757';
    const user = await User.findById(userId);
    
    if (!user) {
      console.log(`❌ User with ID ${userId} not found!`);
      return;
    }

    console.log('👤 FOUND YOUR USER:');
    console.log(`   Name: ${user.name}`);
    console.log(`   Phone: ${user.phone}`);
    console.log(`   Email: ${user.email || 'N/A'}`);
    console.log(`   Credits: ${user.credits} ⚠️`);
    console.log(`   Referral Code: ${user.referralCode}`);
    console.log(`   Referred By: ${user.referredBy || 'None'}`);

    if (user.credits !== 200) {
      console.log(`\n🔧 FIXING: Resetting from ${user.credits} to 200...`);
      user.credits = 200;
      await user.save();
      console.log('✅ Credits reset to 200!');
      
      // Verify the update
      const updated = await User.findById(userId);
      console.log(`\n✅ VERIFIED: Credits are now ${updated.credits}`);
    } else {
      console.log('\n✅ Credits are already correct!');
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    console.log('\n🔌 Database connection closed');
    process.exit(0);
  }
}

checkSpecificUser();
