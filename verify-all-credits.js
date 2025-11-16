/**
 * Verify all users have 500,000 credits (5 lac)
 * Check for any users still with old credit amounts
 */

const mongoose = require('mongoose');
require('dotenv').config();

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: Number,
  referralCode: String,
  createdAt: Date
}, { timestamps: true });

const User = mongoose.model('User', UserSchema);

async function verifyCredits() {
  try {
    console.log('🔍 Verifying all users have 500,000 credits...\n');

    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Get all users
    const allUsers = await User.find({}).sort({ createdAt: 1 });
    console.log(`📊 Total users: ${allUsers.length}\n`);

    // Check for users with less than 500,000 credits
    const usersWithLowCredits = allUsers.filter(u => (u.credits || 0) < 500000);
    
    if (usersWithLowCredits.length > 0) {
      console.log(`⚠️  Found ${usersWithLowCredits.length} users with less than 500,000 credits:\n`);
      
      for (const user of usersWithLowCredits) {
        console.log(`   ${user.name} (${user.phone})`);
        console.log(`   Credits: ${(user.credits || 0).toLocaleString()}`);
        console.log(`   ID: ${user._id}`);
        console.log(`   Created: ${user.createdAt}\n`);
      }
      
      // Ask to fix them
      console.log('💡 Updating these users to 500,000 credits...\n');
      
      for (const user of usersWithLowCredits) {
        const oldCredits = user.credits || 0;
        user.credits = 500000;
        await user.save();
        console.log(`✅ Updated ${user.name}: ${oldCredits.toLocaleString()} → 500,000`);
      }
      
      console.log(`\n✅ Fixed ${usersWithLowCredits.length} users!`);
    } else {
      console.log('✅ All users have 500,000 or more credits!');
    }

    // Statistics
    console.log('\n📈 Credit Statistics:');
    const totalCredits = allUsers.reduce((sum, u) => sum + (u.credits || 0), 0);
    const avgCredits = totalCredits / allUsers.length;
    const minCredits = Math.min(...allUsers.map(u => u.credits || 0));
    const maxCredits = Math.max(...allUsers.map(u => u.credits || 0));
    
    console.log(`   Total Credits: ${totalCredits.toLocaleString()}`);
    console.log(`   Average: ${Math.round(avgCredits).toLocaleString()}`);
    console.log(`   Min: ${minCredits.toLocaleString()}`);
    console.log(`   Max: ${maxCredits.toLocaleString()}`);

    // Check users without referral codes
    const usersWithoutCodes = allUsers.filter(u => !u.referralCode);
    if (usersWithoutCodes.length > 0) {
      console.log(`\n⚠️  ${usersWithoutCodes.length} users without referral codes`);
    } else {
      console.log('\n✅ All users have referral codes');
    }

    // Show sample users
    console.log('\n📋 Sample Users:');
    const sampleUsers = allUsers.slice(0, 5);
    for (const user of sampleUsers) {
      console.log(`   ${user.name} (${user.phone})`);
      console.log(`   Credits: ${(user.credits || 0).toLocaleString()}`);
      console.log(`   Referral Code: ${user.referralCode || 'NONE'}\n`);
    }

  } catch (error) {
    console.error('💥 Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('👋 Disconnected from MongoDB');
  }
}

verifyCredits()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
