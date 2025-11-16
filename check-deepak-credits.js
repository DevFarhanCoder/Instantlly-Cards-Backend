/**
 * Check Deepak's current credits and update if needed
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

async function checkDeepak() {
  try {
    console.log('🔍 Searching for Deepak Bhanushali...\n');

    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Try different phone number formats
    const phonePatterns = [
      '9768676666',
      '+919768676666',
      '919768676666',
      '+9768676666'
    ];

    console.log('📱 Trying phone patterns:', phonePatterns);

    for (const pattern of phonePatterns) {
      const user = await User.findOne({ phone: pattern });
      if (user) {
        console.log('\n✅ Found Deepak!');
        console.log('📋 User Details:');
        console.log('   ID:', user._id);
        console.log('   Name:', user.name);
        console.log('   Phone:', user.phone);
        console.log('   Credits:', user.credits?.toLocaleString() || 0);
        console.log('   Referral Code:', user.referralCode);
        console.log('   Created:', user.createdAt);
        console.log('   Updated:', user.updatedAt);
        
        // If credits are not 500,000, update them
        if (user.credits < 500000) {
          console.log('\n⚠️  Credits are less than 500,000!');
          console.log(`💰 Updating from ${user.credits?.toLocaleString() || 0} to 500,000...`);
          
          user.credits = 500000;
          await user.save();
          
          console.log('✅ Credits updated successfully!');
        } else {
          console.log('\n✅ Credits already at 500,000 or more');
        }
        
        break;
      }
    }

    // Also search by name
    const userByName = await User.findOne({ name: /Deepak.*Bhanushali/i });
    if (userByName) {
      console.log('\n✅ Also found by name:');
      console.log('   ID:', userByName._id);
      console.log('   Name:', userByName.name);
      console.log('   Phone:', userByName.phone);
      console.log('   Credits:', userByName.credits?.toLocaleString() || 0);
      
      if (userByName.credits < 500000) {
        console.log('\n💰 Updating credits to 500,000...');
        userByName.credits = 500000;
        await userByName.save();
        console.log('✅ Updated!');
      }
    }

    // List all users with similar names
    console.log('\n🔍 Searching for all Deepak users...');
    const allDeepaks = await User.find({ name: /deepak/i }).select('name phone credits referralCode');
    
    if (allDeepaks.length > 0) {
      console.log(`\n📋 Found ${allDeepaks.length} Deepak user(s):\n`);
      for (const user of allDeepaks) {
        console.log(`   ${user.name} (${user.phone})`);
        console.log(`   Credits: ${user.credits?.toLocaleString() || 0}`);
        console.log(`   Referral Code: ${user.referralCode}`);
        console.log(`   ID: ${user._id}\n`);
      }
    }

  } catch (error) {
    console.error('💥 Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n👋 Disconnected from MongoDB');
  }
}

checkDeepak()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
