// test-push-token-registration.js
// Run this to test push token registration manually

const mongoose = require('mongoose');
require('dotenv').config();

// Define User schema (simplified version for this test)
const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  pushToken: String,
  platform: String,
  pushTokenUpdatedAt: Date,
}, { collection: 'users' });

async function testTokenRegistration() {
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('🧪 Testing Push Token Registration');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  try {
    // Connect to MongoDB
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Get or create User model
    const User = mongoose.models.User || mongoose.model('User', userSchema);

    // Test users (from your check-all-tokens output)
    const users = [
      { phone: '+919867969445', name: 'Mohammad Farhan' },
      { phone: '+919326664680', name: 'Dinky' }
    ];

    console.log('Step 1: Check if users have tokens stored...\n');
    
    let foundAny = false;
    
    for (const userInfo of users) {
      try {
        const user = await User.findOne({ phone: userInfo.phone });
        
        if (user) {
          foundAny = true;
          console.log(`✅ Found user: ${user.name || userInfo.name}`);
          console.log(`   Phone: ${user.phone}`);
          
          if (user.pushToken) {
            console.log(`   ✅ Push Token: ${user.pushToken.substring(0, 30)}...`);
            console.log(`   ✅ Platform: ${user.platform || 'not set'}`);
            console.log(`   ✅ Token Updated: ${user.pushTokenUpdatedAt || 'not set'}`);
          } else {
            console.log(`   ❌ Push Token: NOT SET`);
            console.log(`   ❌ Platform: ${user.platform || 'not set'}`);
          }
          console.log('');
        } else {
          console.log(`⚠️  User not found: ${userInfo.name} (${userInfo.phone})`);
          console.log('');
        }
      } catch (error) {
        console.error(`❌ Error checking user ${userInfo.name}:`, error.message);
      }
    }

    if (!foundAny) {
      console.log('❌ No users found in database!\n');
    }

    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📋 Diagnosis:');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    
    console.log('If tokens are NOT SET, possible reasons:');
    console.log('');
    console.log('1. ❌ Using OLD APK build (before token registration fix)');
    console.log('   Solution: Build NEW APK with latest code');
    console.log('   Command: eas build --profile production --platform android\n');
    
    console.log('2. ❌ App never called registerForPushNotifications()');
    console.log('   Check: Is app._layout.tsx calling registerForPushNotifications()?');
    console.log('   Check: Are you in Expo Go or production APK?\n');
    
    console.log('3. ❌ Token registration failed silently');
    console.log('   Check: Device logs for errors');
    console.log('   Command (Android): npx react-native log-android\n');
    
    console.log('4. ❌ Not on physical device');
    console.log('   Note: Push tokens only work on physical devices, not emulators\n');
    
    console.log('5. ❌ Permissions not granted');
    console.log('   Check: Did you grant notification permissions?\n');
    
    console.log('6. ❌ Expo project ID not configured');
    console.log('   Check: app.json has extra.eas.projectId set\n');

    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('🔧 Recommended Actions:');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
    
    console.log('1. Download NEW APK (build finished at 2:57 PM today!)');
    console.log('   URL: https://expo.dev/artifacts/eas/gMhyiCtiNaBqKtxLKpb9sC.apk');
    console.log('2. Install NEW APK on physical device');
    console.log('3. Grant notification permissions when prompted');
    console.log('4. Login to app');
    console.log('5. Check device logs: npx react-native log-android');
    console.log('6. Look for these log messages:');
    console.log('   - "🚀 Initializing app systems..."');
    console.log('   - "📱 [REGISTER] Starting push notification registration..."');
    console.log('   - "🎉 [REGISTER] Push token obtained successfully"');
    console.log('   - "✅ [BACKEND] Token registered successfully"');
    console.log('7. Run this script again to verify tokens are saved\n');
    
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
  
  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    // Close MongoDB connection
    await mongoose.connection.close();
    console.log('👋 Disconnected from MongoDB');
  }
}

testTokenRegistration();
