// Test notification script - Place in Instantlly-Cards-Backend folder
// Run with: node test-push-notification.js

const { Expo } = require('expo-server-sdk');
const mongoose = require('mongoose');
require('dotenv').config();

const expo = new Expo({
  accessToken: process.env.EXPO_ACCESS_TOKEN
});

async function testPushNotification() {
  try {
    // Connect to database
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    // Get a user with push token
    const User = mongoose.model('User', new mongoose.Schema({
      name: String,
      email: String,
      pushToken: String,
      platform: String,
    }));

    // Find a user with push token
    const user = await User.findOne({ pushToken: { $exists: true, $ne: null } });
    
    if (!user) {
      console.error('❌ No users with push tokens found!');
      console.log('💡 Make sure at least one user has logged in to register their push token');
      process.exit(1);
    }

    console.log('\n📱 Found user:', {
      name: user.name,
      email: user.email,
      pushToken: user.pushToken.substring(0, 30) + '...',
      platform: user.platform
    });

    // Verify token format
    if (!Expo.isExpoPushToken(user.pushToken)) {
      console.error('❌ Invalid push token format!');
      console.log('Token:', user.pushToken);
      process.exit(1);
    }

    console.log('✅ Token format is valid');

    // Send test notification
    console.log('\n📤 Sending test notification...');
    
    const message = {
      to: user.pushToken,
      sound: 'default',
      title: '🧪 Test Notification',
      body: 'If you can see this, push notifications are working!',
      data: {
        type: 'test',
        timestamp: new Date().toISOString(),
      },
      badge: 1,
    };

    const tickets = await expo.sendPushNotificationsAsync([message]);
    
    console.log('\n✅ Notification sent!');
    console.log('Response:', JSON.stringify(tickets, null, 2));

    // Check for errors
    if (tickets[0].status === 'error') {
      console.error('\n❌ Error sending notification:');
      console.error('Message:', tickets[0].message);
      console.error('Details:', tickets[0].details);
    } else {
      console.log('\n🎉 SUCCESS! Check your device for the notification!');
      console.log('\nIf you see the notification:');
      console.log('  ✅ Firebase is configured correctly');
      console.log('  ✅ Backend can send notifications');
      console.log('  ✅ Your device can receive notifications');
      console.log('\nIf you DON\'T see the notification:');
      console.log('  1. Make sure app is installed from EAS build (not Expo Go)');
      console.log('  2. Check notification permissions on device');
      console.log('  3. Make sure device has internet connection');
      console.log('  4. Wait a few seconds (notifications can be delayed)');
    }

    await mongoose.disconnect();
    process.exit(0);

  } catch (error) {
    console.error('\n❌ Error:', error.message);
    console.error('\nFull error:', error);
    process.exit(1);
  }
}

// Run the test
testPushNotification();
