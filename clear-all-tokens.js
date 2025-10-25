const mongoose = require('mongoose');
require('dotenv').config();

async function clearAllPushTokens() {
  try {
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('🧹 CLEARING ALL PUSH TOKENS FROM DATABASE');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    const User = mongoose.model('User', new mongoose.Schema({
      name: String,
      phone: String,
      pushToken: String,
      platform: String,
      deviceInfo: String,
      pushTokenUpdatedAt: Date,
    }));

    // Clear all push token fields
    const result = await User.updateMany(
      {}, // All users
      {
        $unset: {
          pushToken: "",
          platform: "",
          deviceInfo: "",
          pushTokenUpdatedAt: ""
        }
      }
    );

    console.log(`✅ Cleared push tokens from ${result.modifiedCount} user(s)\n`);

    // Verify all tokens are cleared
    const usersWithTokens = await User.countDocuments({ pushToken: { $exists: true } });
    
    if (usersWithTokens === 0) {
      console.log('✅ VERIFICATION: All push tokens successfully cleared!\n');
    } else {
      console.log(`⚠️  WARNING: ${usersWithTokens} users still have tokens!\n`);
    }

    await mongoose.connection.close();
    console.log('👋 Disconnected from MongoDB\n');

    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('🎯 DATABASE IS NOW CLEAN!');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('\n📋 NEXT STEPS:');
    console.log('1. Build the NEW APK with the fix:');
    console.log('   cd "C:\\Users\\user3\\Documents\\App\\InstantllyCards"');
    console.log('   eas build --profile production --platform android');
    console.log('');
    console.log('2. Install NEW APK on BOTH phones');
    console.log('');
    console.log('3. Login on both phones');
    console.log('');
    console.log('4. Wait 10 seconds');
    console.log('');
    console.log('5. Check Render logs for:');
    console.log('   📱 [TOKEN-REGISTER] New push token registration request');
    console.log('');
    console.log('6. Verify tokens saved:');
    console.log('   node check-all-tokens.js');
    console.log('');
    console.log('7. Test notifications by sending messages!');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  } catch (error) {
    console.error('❌ Error:', error.message);
  }
}

clearAllPushTokens();
