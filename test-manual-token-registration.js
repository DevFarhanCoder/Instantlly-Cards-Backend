/**
 * TEST: Manually register a push token to see if backend works
 */

const axios = require('axios');

const BACKEND_URL = 'https://instantlly-cards-backend.onrender.com';

async function testManualRegistration() {
  console.log('\n🔍 TESTING MANUAL TOKEN REGISTRATION');
  console.log('='.repeat(70));

  try {
    // Step 1: Login to get auth token
    console.log('\n📱 Step 1: Logging in to get auth token...');
    const loginResponse = await axios.post(`${BACKEND_URL}/api/auth/login`, {
      phone: '+919867969445',
      password: 'Farhan_90' // REPLACE WITH ACTUAL PASSWORD
    }, {
      timeout: 120000
    });

    const authToken = loginResponse.data.token;
    console.log('✅ Login successful!');
    console.log('🔑 Auth token:', authToken.substring(0, 20) + '...');

    // Step 2: Try to register a test push token
    console.log('\n📱 Step 2: Registering test push token...');
    
    const testPushToken = 'ExponentPushToken[TEST-' + Date.now() + ']';
    console.log('📲 Test token:', testPushToken);

    const registerResponse = await axios.post(
      `${BACKEND_URL}/api/notifications/register-token`,
      {
        pushToken: testPushToken,
        platform: 'android',
        deviceInfo: {
          brand: 'TestBrand',
          modelName: 'TestModel',
          osName: 'Android',
          osVersion: '14'
        }
      },
      {
        headers: {
          'Authorization': `Bearer ${authToken}`,
          'Content-Type': 'application/json'
        },
        timeout: 30000
      }
    );

    console.log('\n✅ TOKEN REGISTRATION SUCCESSFUL!');
    console.log('📋 Response:', registerResponse.data);
    console.log('\n🎉 Backend is working correctly!');
    console.log('   The issue is that the mobile app is NOT calling this endpoint.');

  } catch (error) {
    console.error('\n❌ ERROR:', error.response?.data || error.message);
    
    if (error.response?.status === 401) {
      console.log('\n⚠️  AUTHENTICATION FAILED');
      console.log('   Please update the password in this script (line 18)');
    } else if (error.response?.status === 400) {
      console.log('\n⚠️  BAD REQUEST');
      console.log('   Backend rejected the token format');
      console.log('   Error:', error.response.data);
    } else {
      console.log('\n⚠️  NETWORK OR SERVER ERROR');
      console.log('   Backend might be sleeping or unreachable');
    }
  }
}

console.log('\n⚠️  IMPORTANT: Update the password on line 18 before running!');
console.log('   Change "your_password_here" to Mohammad Farhan\'s actual password\n');

testManualRegistration();
