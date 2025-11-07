// Quick test to verify Fast2SMS API key and check account status
require('dotenv').config();
const axios = require('axios');

const FAST2SMS_API_KEY = process.env.FAST2SMS_API_KEY || 'tH2an11rgORVwQE5FT8sHLqOYbn6AexAVGe3Y47JH9BszQM79JsISCg7aqGy';

async function checkFast2SMSAccount() {
  console.log('🔍 Checking Fast2SMS Account Status\n');
  console.log('='.repeat(60));
  
  console.log('\n🔑 API Key Check:');
  console.log('   Length:', FAST2SMS_API_KEY.length);
  console.log('   First 15 chars:', FAST2SMS_API_KEY.substring(0, 15) + '...');
  console.log('   Last 10 chars: ...' + FAST2SMS_API_KEY.substring(FAST2SMS_API_KEY.length - 10));
  
  try {
    // Test 1: Check account balance and credits
    console.log('\n💰 TEST 1: Check Account Balance');
    console.log('-'.repeat(60));
    
    try {
      const balanceResponse = await axios.get('https://www.fast2sms.com/dev/wallet', {
        headers: {
          'authorization': FAST2SMS_API_KEY
        }
      });
      
      console.log('✅ Account Status:');
      console.log('   Response:', JSON.stringify(balanceResponse.data, null, 2));
    } catch (error) {
      console.log('❌ Failed to fetch balance:', error.response?.data || error.message);
    }
    
    // Test 2: Send test SMS with different routes
    console.log('\n📱 TEST 2: Test SMS with Route v3 (Transactional)');
    console.log('-'.repeat(60));
    
    const testPhone = '9892254636'; // The number from your logs
    const testMessage = 'Test OTP from InstantllyCards: 123456';
    
    console.log('   Phone:', testPhone);
    console.log('   Message:', testMessage);
    
    // Try route v3
    try {
      const v3Response = await axios.get('https://www.fast2sms.com/dev/bulkV2', {
        params: {
          authorization: FAST2SMS_API_KEY,
          message: testMessage,
          language: 'english',
          route: 'v3',
          numbers: testPhone,
          sender_id: 'TXTIND'
        },
        timeout: 10000
      });
      
      console.log('✅ Route v3 Response:');
      console.log(JSON.stringify(v3Response.data, null, 2));
      
      if (v3Response.data.return === true) {
        console.log('\n✅ SUCCESS! SMS sent via route v3');
      } else {
        console.log('\n❌ Failed with route v3');
        console.log('   Error:', v3Response.data.message);
        console.log('   Status Code:', v3Response.data.status_code);
      }
    } catch (error) {
      console.log('❌ Route v3 failed:');
      console.log('   Status:', error.response?.status);
      console.log('   Data:', JSON.stringify(error.response?.data, null, 2));
    }
    
    // Test 3: Try OTP route (dlt)
    console.log('\n📱 TEST 3: Test SMS with Route dlt (OTP)');
    console.log('-'.repeat(60));
    
    try {
      const dltResponse = await axios.get('https://www.fast2sms.com/dev/bulkV2', {
        params: {
          authorization: FAST2SMS_API_KEY,
          variables_values: '123456',
          route: 'dlt',
          numbers: testPhone
        },
        timeout: 10000
      });
      
      console.log('✅ Route dlt Response:');
      console.log(JSON.stringify(dltResponse.data, null, 2));
    } catch (error) {
      console.log('❌ Route dlt failed (may need template registration):');
      console.log('   Status:', error.response?.status);
      console.log('   Data:', JSON.stringify(error.response?.data, null, 2));
    }
    
    // Test 4: Try quick (q) route
    console.log('\n📱 TEST 4: Test SMS with Route q (Quick/Promotional)');
    console.log('-'.repeat(60));
    
    try {
      const qResponse = await axios.get('https://www.fast2sms.com/dev/bulkV2', {
        params: {
          authorization: FAST2SMS_API_KEY,
          message: testMessage,
          language: 'english',
          route: 'q',
          numbers: testPhone
        },
        timeout: 10000
      });
      
      console.log('✅ Route q Response:');
      console.log(JSON.stringify(qResponse.data, null, 2));
      
      if (qResponse.data.return === true) {
        console.log('\n✅ SUCCESS! SMS sent via route q');
      } else {
        console.log('\n❌ Failed with route q');
        console.log('   Error:', qResponse.data.message);
      }
    } catch (error) {
      console.log('❌ Route q failed:');
      console.log('   Status:', error.response?.status);
      console.log('   Data:', JSON.stringify(error.response?.data, null, 2));
    }
    
  } catch (error) {
    console.error('\n❌ Test failed:', error.message);
  }
  
  console.log('\n' + '='.repeat(60));
  console.log('🏁 Fast2SMS Account Check Complete\n');
  
  console.log('📋 NEXT STEPS:');
  console.log('-'.repeat(60));
  console.log('1. If balance check failed:');
  console.log('   → API key might be invalid');
  console.log('   → Check Fast2SMS dashboard for correct key');
  console.log('');
  console.log('2. If all routes failed:');
  console.log('   → Check account has sufficient credits');
  console.log('   → Login to Fast2SMS dashboard');
  console.log('   → Recharge account if needed');
  console.log('');
  console.log('3. If route v3 shows "route not enabled":');
  console.log('   → Contact Fast2SMS support to enable transactional route');
  console.log('   → Or use route that works from the tests above');
  console.log('');
  console.log('4. If DND error persists:');
  console.log('   → Use the devOTP from backend logs for testing');
  console.log('   → Consider alternative: Email OTP or WhatsApp OTP');
  console.log('');
}

checkFast2SMSAccount();
