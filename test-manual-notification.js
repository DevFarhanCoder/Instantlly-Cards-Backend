const { Expo } = require('expo-server-sdk');
const mongoose = require('mongoose');
require('dotenv').config();

const expo = new Expo({
  accessToken: process.env.EXPO_ACCESS_TOKEN,
  useFcmV1: true
});

async function testPushNotification() {
  try {
    // Use MONGODB_URI (backend's variable name) or MONGO_URI as fallback
    const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
    
    if (!mongoUri) {
      console.error('❌ MongoDB URI not found in environment variables!');
      console.error('\n� You need to create a .env file with your MongoDB URI:');
      console.error('   1. Create a file named ".env" in the Instantlly-Cards-Backend folder');
      console.error('   2. Add this line (replace with your actual MongoDB URI):');
      console.error('      MONGODB_URI=mongodb+srv://username:password@cluster.mongodb.net/instantllycards');
      console.error('\n💡 Or get the URI from Render:');
      console.error('   1. Go to https://dashboard.render.com/');
      console.error('   2. Click your backend service');
      console.error('   3. Go to "Environment" tab');
      console.error('   4. Copy the MONGODB_URI value');
      process.exit(1);
    }
    
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(mongoUri);
    console.log('✅ Connected to MongoDB\n');
    
    // Define User schema
    const User = mongoose.model('User', new mongoose.Schema({
      name: String,
      email: String,
      phone: String,
      pushToken: String,
      platform: String,
    }));

    // Get phone number or email from command line argument
    const testIdentifier = process.argv[2];
    
    if (!testIdentifier) {
      console.error('❌ Please provide a phone number or email address:');
      console.error('   node test-manual-notification.js +919867969445');
      console.error('   node test-manual-notification.js your@email.com');
      process.exit(1);
    }

    // Determine if it's a phone number or email
    const isPhone = testIdentifier.startsWith('+') || /^\d+$/.test(testIdentifier);
    const searchField = isPhone ? 'phone' : 'email';
    const searchValue = testIdentifier;

    console.log(`🔍 Searching for user with ${searchField}:`, searchValue);
    const user = await User.findOne({ [searchField]: searchValue });
    
    if (!user) {
      console.error(`❌ User not found with ${searchField}:`, searchValue);
      console.error('\n💡 Try checking your MongoDB:');
      console.error('   db.users.find({}, { name: 1, email: 1, phone: 1 }).limit(5)');
      console.error('\n💡 Make sure to include the country code for phone numbers:');
      console.error('   Example: +919867969445');
      process.exit(1);
    }

    console.log('✅ Found user:', user.name);
    console.log('📧 Email:', user.email || 'not set');
    console.log('📱 Phone:', user.phone || 'not set');
    console.log('📱 Platform:', user.platform || 'not set');
    
    if (!user.pushToken) {
      console.error('\n❌ User has no push token registered!');
      console.error('\n💡 Solutions:');
      console.error('   1. Make sure user has logged in to the app');
      console.error('   2. Check if notification permissions are granted');
      console.error('   3. Verify APK was built with google-services.json');
      console.error('   4. Try logging out and logging in again');
      process.exit(1);
    }

    if (user.pushToken === 'expo-go-local-mode') {
      console.error('\n⚠️  User is using Expo Go in local mode');
      console.error('   Push notifications won\'t work in this mode');
      console.error('   Please use the production APK build');
      process.exit(1);
    }

    console.log('📱 Push Token:', user.pushToken.substring(0, 50) + '...');
    
    // Validate token format
    if (!Expo.isExpoPushToken(user.pushToken)) {
      console.error('\n❌ Invalid Expo push token format!');
      console.error('   Token should start with: ExponentPushToken[');
      console.error('   Actual token:', user.pushToken);
      process.exit(1);
    }

    console.log('✅ Push token format is valid\n');

    // Create test message
    const message = {
      to: user.pushToken,
      sound: 'default',
      title: '🧪 Manual Test Notification',
      body: 'If you see this, your notification system is working perfectly! 🎉',
      data: { 
        type: 'manual_test',
        timestamp: new Date().toISOString(),
        test: true 
      },
      badge: 1,
      priority: 'high',
      channelId: 'messages',
    };

    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📤 Sending test notification...');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('To:', user.name);
    console.log('Title:', message.title);
    console.log('Body:', message.body);
    console.log('Channel:', message.channelId);
    console.log('Priority:', message.priority);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    const chunks = expo.chunkPushNotifications([message]);
    const tickets = await expo.sendPushNotificationsAsync(chunks[0]);
    
    console.log('📬 Response from Expo:');
    console.log(JSON.stringify(tickets, null, 2));
    console.log('');
    
    const ticket = tickets[0];
    
    if (ticket.status === 'ok') {
      console.log('✅ SUCCESS! Push notification sent successfully! 🎊');
      console.log('✅ Ticket ID:', ticket.id);
      console.log('\n📱 CHECK YOUR DEVICE NOW!');
      console.log('   You should see a notification in your tray');
      console.log('   Title: "🧪 Manual Test Notification"');
      console.log('   Body: "If you see this, your notification system is working perfectly! 🎉"');
      console.log('\n💡 Make sure:');
      console.log('   1. Your app is completely closed (swiped from recent apps)');
      console.log('   2. Notification permissions are granted');
      console.log('   3. Device has internet connection');
    } else if (ticket.status === 'error') {
      console.error('❌ FAILED to send notification!');
      console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.error('Error:', ticket.message);
      console.error('Details:', ticket.details);
      console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      
      // Common error diagnostics
      if (ticket.message && ticket.message.includes('DeviceNotRegistered')) {
        console.error('\n💡 The push token is no longer valid!');
        console.error('   Solutions:');
        console.error('   1. User needs to log out and log in again');
        console.error('   2. Reinstall the app');
        console.error('   3. Clear app data and log in fresh');
      } else if (ticket.message && ticket.message.includes('InvalidCredentials')) {
        console.error('\n💡 Firebase credentials issue!');
        console.error('   Solutions:');
        console.error('   1. Check google-services.json is correct');
        console.error('   2. Verify Firebase Cloud Messaging API (V1) is enabled');
        console.error('   3. Check EXPO_ACCESS_TOKEN environment variable');
      } else if (ticket.message && ticket.message.includes('MessageTooBig')) {
        console.error('\n💡 Notification payload is too large!');
        console.error('   The message data should be less than 4KB');
      } else {
        console.error('\n💡 Unknown error. Check:');
        console.error('   1. Firebase configuration');
        console.error('   2. Network connectivity');
        console.error('   3. Expo status: https://status.expo.dev/');
      }
    }
    
  } catch (error) {
    console.error('\n❌ Error during test:');
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.error(error.message);
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.error('\nStack trace:');
    console.error(error.stack);
  } finally {
    await mongoose.connection.close();
    console.log('\n🔌 MongoDB connection closed');
    process.exit(0);
  }
}

// ASCII Banner
console.log('');
console.log('╔════════════════════════════════════════════╗');
console.log('║  🧪 Manual Push Notification Tester        ║');
console.log('║     InstantllyCards Backend               ║');
console.log('╚════════════════════════════════════════════╝');
console.log('');

testPushNotification();
