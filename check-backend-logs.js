/**
 * CHECK BACKEND LOGS
 * This script helps you see if the app is trying to register tokens with the backend
 */

const axios = require('axios');

const BACKEND_URL = 'https://instantlly-cards-backend-6ki0.onrender.com';

async function checkBackendLogs() {
  console.log('\n🔍 CHECKING BACKEND LOGS FOR TOKEN REGISTRATION');
  console.log('='.repeat(60));

  try {
    // First, wake up the backend
    console.log('\n⏳ Waking up backend (this may take 60-90 seconds)...');
    const healthCheck = await axios.get(`${BACKEND_URL}/api/health`, {
      timeout: 120000
    });
    console.log('✅ Backend is awake:', healthCheck.data);

    console.log('\n📋 INSTRUCTIONS:');
    console.log('1. Now, on your phone, open the Instantlly app');
    console.log('2. Logout if you\'re logged in');
    console.log('3. Force close the app (swipe from recents)');
    console.log('4. Reopen the app');
    console.log('5. Login again');
    console.log('6. Wait 10 seconds');
    console.log('7. Then run: node test-push-token-registration.js');
    
    console.log('\n💡 WHAT TO LOOK FOR:');
    console.log('- If tokens appear, registration is working! ✅');
    console.log('- If tokens still "NOT SET", the app isn\'t calling the backend ❌');
    
  } catch (error) {
    console.error('\n❌ Error:', error.message);
  }
}

checkBackendLogs();
