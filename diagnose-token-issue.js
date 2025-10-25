/**
 * DIAGNOSTIC SCRIPT - Figure out why tokens aren't registering
 * This will help us understand what's happening
 */

const mongoose = require('mongoose');
require('dotenv').config();

async function diagnose() {
  console.log('\n🔍 DIAGNOSTIC ANALYSIS - Push Token Registration Issue');
  console.log('='.repeat(70));

  try {
    // Connect to database
    console.log('\n📡 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to database');

    const User = mongoose.model('User', new mongoose.Schema({}, { strict: false }));

    // Get all users
    const users = await User.find({}).lean();
    console.log(`\n📊 Total users in database: ${users.length}`);

    // Check each user
    for (const user of users) {
      console.log('\n' + '─'.repeat(70));
      console.log(`👤 User: ${user.name || 'No name'} (${user.phone || 'No phone'})`);
      console.log('─'.repeat(70));
      
      console.log('📋 User Data:');
      console.log(`  - ID: ${user._id}`);
      console.log(`  - Email: ${user.email || 'NOT SET'}`);
      console.log(`  - Phone: ${user.phone || 'NOT SET'}`);
      console.log(`  - Created: ${user.createdAt || 'Unknown'}`);
      console.log(`  - Last Login: ${user.lastLogin || 'Unknown'}`);
      
      console.log('\n📱 Push Token Status:');
      console.log(`  - Has pushToken field: ${user.pushToken ? 'YES' : 'NO'}`);
      console.log(`  - Push Token: ${user.pushToken || 'NOT SET'}`);
      console.log(`  - Platform: ${user.platform || 'NOT SET'}`);
      console.log(`  - Token Updated: ${user.pushTokenUpdatedAt || 'NEVER'}`);
      
      if (user.deviceInfo) {
        console.log('\n📱 Device Info:');
        console.log(`  - Brand: ${user.deviceInfo.brand || 'Unknown'}`);
        console.log(`  - Model: ${user.deviceInfo.modelName || 'Unknown'}`);
        console.log(`  - OS: ${user.deviceInfo.osName || 'Unknown'} ${user.deviceInfo.osVersion || ''}`);
      } else {
        console.log('\n📱 Device Info: NOT SET');
      }
      
      // Check user schema
      console.log('\n🔧 Available Fields in User Document:');
      const fields = Object.keys(user).filter(key => !key.startsWith('_'));
      fields.forEach(field => {
        console.log(`  - ${field}: ${typeof user[field]}`);
      });
    }

    console.log('\n' + '═'.repeat(70));
    console.log('🎯 DIAGNOSIS SUMMARY');
    console.log('═'.repeat(70));

    const usersWithTokens = users.filter(u => u.pushToken && u.pushToken !== 'NOT SET');
    const usersWithoutTokens = users.filter(u => !u.pushToken || u.pushToken === 'NOT SET');

    console.log(`\n✅ Users WITH push tokens: ${usersWithTokens.length}`);
    console.log(`❌ Users WITHOUT push tokens: ${usersWithoutTokens.length}`);

    if (usersWithoutTokens.length > 0) {
      console.log('\n❌ PROBLEM DETECTED:');
      console.log(`   ${usersWithoutTokens.length} user(s) don't have push tokens registered`);
      console.log('\n🔍 POSSIBLE CAUSES:');
      console.log('   1. App is not calling registerForPushNotifications()');
      console.log('   2. App is not calling registerPendingPushToken() after login');
      console.log('   3. Notification permissions are denied on the device');
      console.log('   4. Backend /api/notifications/register-token endpoint is failing');
      console.log('   5. App is in Expo Go mode instead of production APK');
      console.log('   6. Network error preventing token from reaching backend');
      
      console.log('\n💡 NEXT STEPS:');
      console.log('   1. Check if you see console logs on phone screen');
      console.log('   2. Enable "Remote JS Debugging" in Expo Dev Menu');
      console.log('   3. Check Chrome DevTools console for app logs');
      console.log('   4. Look for "[REGISTER]" and "[BACKEND]" log messages');
    } else {
      console.log('\n✅ ALL USERS HAVE TOKENS REGISTERED!');
      console.log('   Push notifications should be working.');
    }

    console.log('\n' + '═'.repeat(70));
    console.log('📞 TROUBLESHOOTING CHECKLIST:');
    console.log('═'.repeat(70));
    console.log('\n[ ] 1. Using production APK (not Expo Go)?');
    console.log('[ ] 2. Notification permissions allowed?');
    console.log('[ ] 3. Logged in successfully?');
    console.log('[ ] 4. Waited 10 seconds after login?');
    console.log('[ ] 5. Backend receiving requests? (check Render logs)');
    console.log('[ ] 6. Check app logs in Chrome DevTools (enable remote debugging)');

  } catch (error) {
    console.error('\n❌ Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n✅ Disconnected from database\n');
  }
}

diagnose();
