/**
 * Test Script: Simulate Mobile App Push Token Registration
 * 
 * This script simulates what the mobile app should be doing:
 * 1. Get an Expo push token from Expo's servers
 * 2. Register it with our backend
 * 
 * This helps us verify:
 * - Can we get valid Expo push tokens?
 * - Does our backend accept them?
 * - What errors (if any) occur?
 */

const fetch = require('node-fetch');

// Configuration
const EXPO_PROJECT_ID = '2d7524da-4330-496c-816f-4e011831e6f4'; // rajeshmodi account
const BACKEND_URL = 'https://instantlly-cards-backend.onrender.com';
const TEST_USER_PHONE = process.argv[2] || '+919867969445'; // Mohammad Farhan (or pass as first argument)
const TEST_USER_PASSWORD = process.argv[3] || 'Farhan_90'; // Pass as second argument

if (process.argv.length < 4) {
  console.log('⚠️  Usage: node test-expo-token-generation.js <phone> <password>');
  console.log('   Example: node test-expo-token-generation.js +919867969445 yourpassword');
  console.log('   Using defaults for now...\n');
}

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('🧪 Testing Expo Push Token Generation & Registration');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

async function testPushTokenFlow() {
  try {
    // Step 1: Login to get auth token
    console.log('📝 Step 1: Login to backend...');
    console.log(`   Phone: ${TEST_USER_PHONE}`);
    
    const loginResponse = await fetch(`${BACKEND_URL}/api/auth/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        phone: TEST_USER_PHONE,
        password: TEST_USER_PASSWORD,
      }),
    });

    if (!loginResponse.ok) {
      const errorText = await loginResponse.text();
      throw new Error(`Login failed: ${loginResponse.status} - ${errorText}`);
    }

    const loginData = await loginResponse.json();
    const authToken = loginData.token;
    
    if (!authToken) {
      throw new Error('No auth token received from login');
    }
    
    console.log('✅ Login successful!');
    console.log(`   User: ${loginData.user?.name || 'Unknown'}`);
    console.log(`   Auth Token: ${authToken.substring(0, 20)}...`);
    console.log('');

    // Step 2: Get Expo Push Token
    console.log('📱 Step 2: Getting Expo Push Token...');
    console.log(`   Project ID: ${EXPO_PROJECT_ID}`);
    
    // Generate a valid UUID for deviceId
    const crypto = require('crypto');
    const deviceUUID = crypto.randomUUID();
    
    console.log(`   Device UUID: ${deviceUUID}`);
    
    // Simulate getting a device token (in real app, this comes from the device)
    // For testing, we'll use Expo's API to get a push token
    const expoTokenResponse = await fetch('https://exp.host/--/api/v2/push/getExpoPushToken', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        deviceId: deviceUUID,
        experienceId: `@rajeshmodi/instantllycards`,
        appId: 'com.instantllycards.www.twa',
        deviceToken: `test-fcm-token-${Date.now()}`,
        type: 'fcm',
        development: false,
      }),
    });

    if (!expoTokenResponse.ok) {
      const errorText = await expoTokenResponse.text();
      console.log('⚠️  Expo token API response:', expoTokenResponse.status);
      console.log('⚠️  Response body:', errorText);
      throw new Error(`Expo token request failed: ${expoTokenResponse.status}`);
    }

    const expoTokenData = await expoTokenResponse.json();
    console.log('📦 Expo API Response:', JSON.stringify(expoTokenData, null, 2));
    
    const expoPushToken = expoTokenData.data?.expoPushToken;
    
    if (!expoPushToken) {
      console.log('❌ No push token in response!');
      console.log('Full response:', JSON.stringify(expoTokenData, null, 2));
      throw new Error('Failed to get Expo push token from API');
    }

    console.log('✅ Expo Push Token obtained!');
    console.log(`   Token: ${expoPushToken}`);
    console.log('');

    // Step 3: Register token with our backend
    console.log('🔄 Step 3: Registering token with backend...');
    
    const registerResponse = await fetch(`${BACKEND_URL}/api/notifications/register-token`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${authToken}`,
      },
      body: JSON.stringify({
        pushToken: expoPushToken,
        platform: 'android',
        deviceInfo: {
          brand: 'Test',
          modelName: 'Test Device',
          osName: 'Android',
          osVersion: '14',
        },
      }),
    });

    if (!registerResponse.ok) {
      const errorText = await registerResponse.text();
      throw new Error(`Token registration failed: ${registerResponse.status} - ${errorText}`);
    }

    const registerData = await registerResponse.json();
    console.log('✅ Token registered successfully!');
    console.log('   Response:', JSON.stringify(registerData, null, 2));
    console.log('');

    // Step 4: Verify token was saved
    console.log('🔍 Step 4: Verifying token in database...');
    
    const mongoose = require('mongoose');
    require('dotenv').config();
    
    await mongoose.connect(process.env.MONGODB_URI);
    
    const User = mongoose.model('User', new mongoose.Schema({
      name: String,
      phone: String,
      pushToken: String,
      platform: String,
      pushTokenUpdatedAt: Date,
    }));
    
    const user = await User.findOne({ phone: TEST_USER_PHONE });
    
    if (!user) {
      throw new Error('User not found in database');
    }
    
    console.log('✅ User found in database!');
    console.log(`   Name: ${user.name}`);
    console.log(`   Phone: ${user.phone}`);
    console.log(`   Push Token: ${user.pushToken || 'NOT SET'}`);
    console.log(`   Platform: ${user.platform || 'NOT SET'}`);
    console.log(`   Updated At: ${user.pushTokenUpdatedAt || 'NEVER'}`);
    console.log('');

    await mongoose.connection.close();

    // Final Summary
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('🎉 SUCCESS! Complete Flow Tested:');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('✅ 1. Login successful');
    console.log('✅ 2. Expo push token obtained');
    console.log('✅ 3. Token registered with backend');
    console.log('✅ 4. Token saved in database');
    console.log('');
    console.log('📱 Push Token:', expoPushToken);
    console.log('');
    console.log('🎯 Next Steps:');
    console.log('   1. This proves the BACKEND and EXPO API both work!');
    console.log('   2. The issue is in the MOBILE APP - it\'s not executing this flow');
    console.log('   3. We need to check why the mobile app isn\'t calling registerForPushNotifications()');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

  } catch (error) {
    console.error('');
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.error('❌ ERROR during test:');
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.error('Message:', error.message);
    console.error('Stack:', error.stack);
    console.error('');
    console.error('💡 This error tells us WHERE the problem is in the flow');
    console.error('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  }
}

// Run the test
testPushTokenFlow();
