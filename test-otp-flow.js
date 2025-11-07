// Test OTP sending with detailed debugging
require('dotenv').config();
const axios = require('axios');

const BACKEND_URL = process.env.BACKEND_URL || 'https://instantlly-cards-backend-6ki0.onrender.com';
const FAST2SMS_API_KEY = process.env.FAST2SMS_API_KEY || 'tH2an11rgORVwQE5FT8sHLqOYbn6AexAVGe3Y47JH9BszQM79JsISCg7aqGy';

async function testOTPFlow() {
  console.log('🧪 Testing OTP Flow\n');
  console.log('='.repeat(60));
  
  const testPhone = '+919876543210'; // Test phone number
  
  try {
    // Test 1: Check backend health
    console.log('\n📡 TEST 1: Backend Health Check');
    console.log('-'.repeat(60));
    try {
      const health = await axios.get(`${BACKEND_URL}/api/health`);
      console.log('✅ Backend is online');
      console.log('   Status:', health.data.dbStatus);
      console.log('   DB Ping:', health.data.dbPing);
      console.log('   Version:', health.data.version);
    } catch (error) {
      console.log('❌ Backend health check failed:', error.message);
      console.log('   URL:', `${BACKEND_URL}/api/health`);
      return;
    }

    // Test 2: Fast2SMS API credentials
    console.log('\n🔑 TEST 2: Fast2SMS API Key Check');
    console.log('-'.repeat(60));
    console.log('   API Key configured:', !!FAST2SMS_API_KEY);
    console.log('   API Key length:', FAST2SMS_API_KEY.length);
    console.log('   API Key (first 10 chars):', FAST2SMS_API_KEY.substring(0, 10) + '...');

    // Test 3: Send OTP to test number
    console.log('\n📱 TEST 3: Sending OTP to Test Number');
    console.log('-'.repeat(60));
    console.log('   Test Phone:', testPhone);
    console.log('   Clean Phone:', testPhone.replace(/\D/g, ''));
    
    try {
      const response = await axios.post(`${BACKEND_URL}/api/auth/send-otp`, {
        phone: testPhone
      });
      
      console.log('✅ OTP API Response:');
      console.log('   Success:', response.data.success);
      console.log('   Message:', response.data.message);
      console.log('   TTL:', response.data.ttl, 'seconds');
      
      if (response.data.devOTP) {
        console.log('   🔐 [DEV] OTP Code:', response.data.devOTP);
        console.log('   ℹ️  Use this code to verify (development mode)');
      }
    } catch (error) {
      console.log('❌ Send OTP failed:');
      console.log('   Error:', error.response?.data || error.message);
      console.log('   Status:', error.response?.status);
    }

    // Test 4: Direct Fast2SMS API test
    console.log('\n🌐 TEST 4: Direct Fast2SMS API Test');
    console.log('-'.repeat(60));
    
    const testMessage = 'Test message from InstantllyCards - OTP: 123456';
    const cleanPhone = testPhone.replace(/\D/g, '');
    
    console.log('   Message:', testMessage);
    console.log('   Phone:', cleanPhone);
    console.log('   API Key:', FAST2SMS_API_KEY.substring(0, 10) + '...');
    
    try {
      const fast2smsResponse = await axios.get('https://www.fast2sms.com/dev/bulkV2', {
        params: {
          authorization: FAST2SMS_API_KEY,
          message: testMessage,
          language: 'english',
          route: 'q',
          numbers: cleanPhone
        }
      });
      
      console.log('✅ Fast2SMS Direct Response:');
      console.log('   Return:', fast2smsResponse.data.return);
      console.log('   Message:', fast2smsResponse.data.message);
      console.log('   Full Response:', JSON.stringify(fast2smsResponse.data, null, 2));
      
      if (fast2smsResponse.data.return === true) {
        console.log('\n✅ SMS sent successfully via Fast2SMS!');
      } else {
        console.log('\n❌ Fast2SMS returned error:');
        console.log('   Response:', fast2smsResponse.data);
      }
    } catch (error) {
      console.log('❌ Fast2SMS Direct API failed:');
      if (error.response) {
        console.log('   Status:', error.response.status);
        console.log('   Data:', JSON.stringify(error.response.data, null, 2));
      } else {
        console.log('   Error:', error.message);
      }
    }

    // Test 5: Check OTP in backend logs
    console.log('\n📋 TEST 5: OTP Storage Check');
    console.log('-'.repeat(60));
    console.log('   ℹ️  OTP is stored in-memory on backend');
    console.log('   ℹ️  Check backend logs for: "🔐 [DEV] OTP for"');
    console.log('   ℹ️  Valid for 5 minutes (300 seconds)');

  } catch (error) {
    console.error('\n❌ Test failed:', error.message);
    if (error.response) {
      console.log('   Response:', error.response.data);
    }
  }
  
  console.log('\n' + '='.repeat(60));
  console.log('🏁 OTP Flow Test Complete\n');
  
  // Summary
  console.log('📊 TROUBLESHOOTING GUIDE:');
  console.log('-'.repeat(60));
  console.log('1. If backend health fails:');
  console.log('   → Check BACKEND_URL is correct');
  console.log('   → Verify backend is deployed and running');
  console.log('');
  console.log('2. If OTP sends but not received on phone:');
  console.log('   → Check Fast2SMS account balance');
  console.log('   → Verify phone number is correct format');
  console.log('   → Check sender ID is approved');
  console.log('   → Check Fast2SMS dashboard for delivery status');
  console.log('');
  console.log('3. If Fast2SMS API fails:');
  console.log('   → Verify API key is valid');
  console.log('   → Check Fast2SMS account is active');
  console.log('   → Ensure sufficient credits');
  console.log('   → Check route "q" is enabled');
  console.log('');
  console.log('4. Use devOTP for testing:');
  console.log('   → Backend returns OTP in development mode');
  console.log('   → Use the code shown in backend logs');
  console.log('   → No SMS needed for testing');
  console.log('');
}

testOTPFlow();
