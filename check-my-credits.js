/**
 * Check and fix credits for specific user (logged in user)
 */

require('dotenv').config();
const mongoose = require('mongoose');

const MONGO_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: Number,
  referredBy: String
}, { collection: 'users' });

const User = mongoose.model('User', userSchema);

async function checkAndFixCredits() {
  try {
    await mongoose.connect(MONGO_URI);
    console.log('✅ Connected to MongoDB\n');

    // Find all users with 500000 credits
    const usersWithHighCredits = await User.find({ credits: 500000 });
    
    if (usersWithHighCredits.length === 0) {
      console.log('✅ No users found with 500000 credits - all reset successfully!');
    } else {
      console.log(`⚠️  Found ${usersWithHighCredits.length} users still with 500000 credits:\n`);
      
      usersWithHighCredits.forEach((user, index) => {
        console.log(`${index + 1}. ${user.name || 'No name'} (${user.phone})`);
        console.log(`   Credits: ${user.credits}`);
        console.log(`   Referred by: ${user.referredBy || 'None'}\n`);
      });

      console.log('🔄 Resetting these users to correct credits...\n');

      for (const user of usersWithHighCredits) {
        const correctCredits = user.referredBy ? 300 : 200;
        user.credits = correctCredits;
        await user.save();
        console.log(`✅ Reset ${user.name || user.phone} to ${correctCredits} credits`);
      }

      console.log(`\n✅ Fixed ${usersWithHighCredits.length} user(s)!`);
    }

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.connection.close();
    console.log('\n🔌 Database connection closed');
    process.exit(0);
  }
}

checkAndFixCredits();
