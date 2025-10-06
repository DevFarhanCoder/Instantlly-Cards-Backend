// Force reset push token for a user (removes expo-go-local-mode)
const mongoose = require('mongoose');
require('dotenv').config();

async function resetPushToken() {
  try {
    const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
    await mongoose.connect(mongoUri);
    console.log('✅ Connected to MongoDB\n');
    
    const User = mongoose.model('User', new mongoose.Schema({
      name: String,
      email: String,
      phone: String,
      pushToken: String,
      platform: String,
    }));

    const phone = process.argv[2];
    
    if (!phone) {
      console.error('❌ Please provide a phone number:');
      console.error('   node force-reset-token.js +919867969445');
      process.exit(1);
    }

    console.log('🔍 Finding user with phone:', phone);
    const user = await User.findOne({ phone });
    
    if (!user) {
      console.error(`❌ User not found with phone: ${phone}`);
      process.exit(1);
    }

    console.log('✅ Found user:', user.name);
    console.log('📱 Current push token:', user.pushToken || 'NOT SET');
    console.log('');

    if (user.pushToken === 'expo-go-local-mode') {
      console.log('⚠️  Detected expo-go-local-mode token');
      console.log('🔄 Removing old token...');
      
      await User.findByIdAndUpdate(user._id, {
        $unset: { pushToken: 1 }, // Remove the field entirely
        platform: null,
        pushTokenUpdatedAt: null
      });
      
      console.log('✅ Push token removed successfully!');
      console.log('');
      console.log('📋 Next Steps:');
      console.log('   1. Open your app (APK v1.0.12)');
      console.log('   2. Log out completely');
      console.log('   3. Close the app (swipe from recent apps)');
      console.log('   4. Open the app and log in again');
      console.log('   5. Grant notification permissions when asked');
      console.log('   6. Wait for "Push token registered" message');
      console.log('');
      console.log('💡 The app will now get a fresh Firebase push token!');
      
    } else if (!user.pushToken) {
      console.log('ℹ️  User has no push token');
      console.log('');
      console.log('📋 Next Steps:');
      console.log('   1. Make sure you have APK v1.0.12 installed');
      console.log('   2. Open the app and log in');
      console.log('   3. Grant notification permissions');
      console.log('   4. Token will be registered automatically');
      
    } else if (user.pushToken.startsWith('ExponentPushToken[')) {
      console.log('✅ User already has a valid production token!');
      console.log('');
      console.log('📋 You can test notifications now:');
      console.log(`   node test-manual-notification.js ${phone}`);
      
    } else {
      console.log('⚠️  Unknown token format:', user.pushToken);
      console.log('🔄 Removing token...');
      
      await User.findByIdAndUpdate(user._id, {
        $unset: { pushToken: 1 },
        platform: null,
        pushTokenUpdatedAt: null
      });
      
      console.log('✅ Token removed! Log in again to get a fresh token.');
    }
    
  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    process.exit(0);
  }
}

console.log('');
console.log('╔════════════════════════════════════════════╗');
console.log('║  🔄 Force Reset Push Token Tool           ║');
console.log('║     InstantllyCards Backend               ║');
console.log('╚════════════════════════════════════════════╝');
console.log('');

resetPushToken();
