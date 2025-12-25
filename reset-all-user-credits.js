/**
 * Reset All User Credits Script
 * 
 * Sets correct credit values:
 * - 200 credits for existing users (normal signup)
 * - 300 credits for introducers (users who signed up with referral code)
 */

require('dotenv').config();
const mongoose = require('mongoose');

// MongoDB Connection
const MONGO_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

if (!MONGO_URI) {
  console.error('❌ MONGODB_URI not found in environment variables');
  process.exit(1);
}

// User Schema (simplified)
const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: { type: Number, default: 0 },
  referredBy: String,
  creditsExpiryDate: Date
}, { collection: 'users' });

const User = mongoose.model('User', userSchema);

async function resetAllUserCredits() {
  try {
    console.log('🔄 Connecting to MongoDB...');
    await mongoose.connect(MONGO_URI);
    console.log('✅ Connected to MongoDB');

    // Count total users
    const totalUsers = await User.countDocuments();
    console.log(`\n📊 Total users in database: ${totalUsers}`);

    // Find users with referral codes (introducers)
    const introducers = await User.find({ referredBy: { $exists: true, $ne: null } });
    const introducerCount = introducers.length;
    console.log(`👥 Users with referral codes (introducers): ${introducerCount}`);

    // Find regular users (no referral)
    const regularUsers = await User.find({ 
      $or: [
        { referredBy: { $exists: false } },
        { referredBy: null },
        { referredBy: '' }
      ]
    });
    const regularCount = regularUsers.length;
    console.log(`👤 Regular users (no referral): ${regularCount}`);

    console.log('\n⚠️  IMPORTANT: This will reset credits for ALL users!');
    console.log(`   - ${introducerCount} users will get 300 credits (introducers)`);
    console.log(`   - ${regularCount} users will get 200 credits (regular users)`);
    console.log('\n⏳ Starting in 5 seconds... (Press Ctrl+C to cancel)');
    
    await new Promise(resolve => setTimeout(resolve, 5000));

    console.log('\n🚀 Starting credit reset...\n');

    // Update introducers to 300 credits
    console.log('💰 Resetting introducers to 300 credits...');
    const introducerResult = await User.updateMany(
      { referredBy: { $exists: true, $ne: null, $ne: '' } },
      { $set: { credits: 300 } }
    );
    console.log(`✅ Updated ${introducerResult.modifiedCount} introducers to 300 credits`);

    // Update regular users to 200 credits
    console.log('💰 Resetting regular users to 200 credits...');
    const regularResult = await User.updateMany(
      { 
        $or: [
          { referredBy: { $exists: false } },
          { referredBy: null },
          { referredBy: '' }
        ]
      },
      { $set: { credits: 200 } }
    );
    console.log(`✅ Updated ${regularResult.modifiedCount} regular users to 200 credits`);

    // Show summary
    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('✅ CREDIT RESET COMPLETE!');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log(`📊 Total users processed: ${totalUsers}`);
    console.log(`👥 Introducers (300 credits): ${introducerResult.modifiedCount}`);
    console.log(`👤 Regular users (200 credits): ${regularResult.modifiedCount}`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    // Show sample users after reset
    console.log('📋 Sample users after reset:');
    const sampleIntroducers = await User.find({ referredBy: { $exists: true, $ne: null } }).limit(3);
    const sampleRegular = await User.find({ referredBy: { $exists: false } }).limit(3);

    console.log('\n👥 Sample Introducers:');
    sampleIntroducers.forEach(user => {
      console.log(`   ${user.name || 'No name'} (${user.phone}): ${user.credits} credits (referred by ${user.referredBy})`);
    });

    console.log('\n👤 Sample Regular Users:');
    sampleRegular.forEach(user => {
      console.log(`   ${user.name || 'No name'} (${user.phone}): ${user.credits} credits`);
    });

    console.log('\n✅ Script completed successfully!');

  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  } finally {
    await mongoose.connection.close();
    console.log('🔌 Database connection closed');
    process.exit(0);
  }
}

// Run the script
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('🔄 USER CREDITS RESET SCRIPT');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
resetAllUserCredits();
