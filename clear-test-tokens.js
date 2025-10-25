/**
 * Clear test tokens and force re-registration
 */

const mongoose = require('mongoose');
require('dotenv').config();

async function clearTestTokens() {
  console.log('\n🔧 CLEARING TEST TOKENS FROM DATABASE');
  console.log('='.repeat(70));

  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to database');

    const User = mongoose.model('User', new mongoose.Schema({}, { strict: false }));

    // Find users with TEST tokens
    const result = await User.updateMany(
      {
        pushToken: { $regex: /^ExponentPushToken\[TEST-/ }
      },
      {
        $unset: {
          pushToken: "",
          platform: "",
          deviceInfo: "",
          pushTokenUpdatedAt: ""
        }
      }
    );

    console.log(`\n✅ Cleared ${result.modifiedCount} test token(s)`);
    console.log('\n📋 NEXT STEPS:');
    console.log('1. Logout from BOTH phones');
    console.log('2. Force close the apps');
    console.log('3. Reopen and login again');
    console.log('4. Wait 30 seconds');
    console.log('5. Run: node test-push-token-registration.js');
    console.log('\n💡 The apps will now register REAL Expo push tokens!\n');

  } catch (error) {
    console.error('\n❌ Error:', error);
  } finally {
    await mongoose.disconnect();
  }
}

clearTestTokens();
