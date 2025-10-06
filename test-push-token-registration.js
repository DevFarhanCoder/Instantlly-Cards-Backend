// test-push-token-registration.js
// Run this to test push token registration manually

const axios = require('axios');

const API_BASE = 'https://instantlly-cards-backend.onrender.com/api';

async function testTokenRegistration() {
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('🧪 Testing Push Token Registration');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  // Test users (from your check-all-tokens output)
  const users = [
    { phone: '+919867969445', name: 'Mohammad Farhan' },
    { phone: '+919326664680', name: 'Dinky' }
  ];

  console.log('Step 1: Check if users have tokens stored...\n');
  
  for (const user of users) {
    try {
      const response = await axios.get(`${API_BASE}/admin/users`, {
        headers: { 'x-admin-key': 'your-secure-admin-key-here' },
        params: { search: user.phone }
      });
      
      if (response.data.users && response.data.users.length > 0) {
        const userData = response.data.users[0];
        console.log(`✅ Found user: ${user.name}`);
        console.log(`   Phone: ${userData.phone}`);
        console.log(`   Push Token: ${userData.pushToken || 'NOT SET'}`);
        console.log(`   Platform: ${userData.platform || 'not set'}`);
        console.log(`   Token Updated: ${userData.pushTokenUpdatedAt || 'never'}`);
        console.log('');
      }
    } catch (error) {
      console.error(`❌ Error checking user ${user.name}:`, error.message);
    }
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
  
  console.log('1. Wait for NEW APK build to complete');
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
}

testTokenRegistration().catch(console.error);
