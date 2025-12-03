const fetch = require('node-fetch');

async function testLiveAPI() {
  try {
    console.log('🌐 TESTING LIVE API ENDPOINTS\n');
    
    // This is likely Rajesh's token from the logs
    const baseURL = 'https://api.instantllycards.com/api';
    
    // Test without auth first to see basic connectivity
    console.log('1️⃣ Testing basic connectivity...');
    try {
      const response = await fetch(`${baseURL}/ads/active`);
      console.log(`   Ads endpoint: ${response.status} ${response.statusText}`);
    } catch (err) {
      console.log(`   Ads endpoint error: ${err.message}`);
    }
    
    console.log('\n2️⃣ Testing cards endpoints (need auth token)...');
    console.log('   Note: These will likely return 401 without proper auth token');
    
    try {
      const response = await fetch(`${baseURL}/cards/sent`);
      console.log(`   Sent cards: ${response.status} ${response.statusText}`);
      if (response.status === 401) {
        console.log('   (Expected - needs authentication)');
      }
    } catch (err) {
      console.log(`   Sent cards error: ${err.message}`);
    }
    
    try {
      const response = await fetch(`${baseURL}/cards/received`);
      console.log(`   Received cards: ${response.status} ${response.statusText}`);
      if (response.status === 401) {
        console.log('   (Expected - needs authentication)');
      }
    } catch (err) {
      console.log(`   Received cards error: ${err.message}`);
    }
    
    console.log('\n3️⃣ Checking deployment timestamp...');
    try {
      const response = await fetch(`${baseURL}/health`);
      if (response.ok) {
        const data = await response.json();
        console.log('   Health check:', data);
      } else {
        console.log(`   No health endpoint: ${response.status}`);
      }
    } catch (err) {
      console.log(`   Health check error: ${err.message}`);
    }
    
    console.log('\n📊 ANALYSIS:');
    console.log('   If ads endpoint works but cards return 401, deployment is live');
    console.log('   The issue might be:');
    console.log('   1. Authentication token validation failing');
    console.log('   2. Database connection issue in production');
    console.log('   3. Caching layer interfering');
    console.log('   4. Memory/timeout issues on Render');
    
  } catch (error) {
    console.error('❌ Error testing live API:', error);
  }
}

testLiveAPI();