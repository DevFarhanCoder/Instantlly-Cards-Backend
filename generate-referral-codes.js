/**
 * Generate referral codes for all users who don't have one
 */

const mongoose = require('mongoose');
require('dotenv').config();

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: Number,
  referralCode: String,
  referredBy: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
  createdAt: Date,
  updatedAt: Date
}, { timestamps: true });

const User = mongoose.model('User', UserSchema);

function generateReferralCode() {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let code = '';
  for (let i = 0; i < 8; i++) {
    code += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return code;
}

async function generateReferralCodes() {
  try {
    console.log('🚀 Generating referral codes for users...\n');

    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Find users without referral codes
    const usersWithoutCodes = await User.find({ 
      $or: [
        { referralCode: { $exists: false } },
        { referralCode: null },
        { referralCode: '' }
      ]
    });

    console.log(`📊 Found ${usersWithoutCodes.length} users without referral codes\n`);

    let updated = 0;
    let errors = 0;

    for (const user of usersWithoutCodes) {
      try {
        // Generate unique code
        let newCode = generateReferralCode();
        let existingCode = await User.findOne({ referralCode: newCode });
        
        // Keep generating until we get a unique code
        while (existingCode) {
          newCode = generateReferralCode();
          existingCode = await User.findOne({ referralCode: newCode });
        }

        // Update user
        user.referralCode = newCode;
        await user.save();

        console.log(`✅ ${user.name} (${user.phone}) → ${newCode}`);
        updated++;

      } catch (error) {
        console.error(`❌ Error updating ${user.name}:`, error.message);
        errors++;
      }
    }

    console.log('\n📈 Summary:');
    console.log(`   ✅ Updated: ${updated} users`);
    console.log(`   ❌ Errors: ${errors} users`);
    console.log(`   📊 Total: ${usersWithoutCodes.length} users`);

  } catch (error) {
    console.error('💥 Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n👋 Disconnected from MongoDB');
  }
}

generateReferralCodes()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
