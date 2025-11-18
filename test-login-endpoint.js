// test-login-endpoint.js
// Quick test script to verify the login endpoint is working
// Usage: node test-login-endpoint.js

const BASE_URL = process.env.API_URL || 'https://instantlly-cards-backend-6ki0.onrender.com';

async function testLogin(phone, password) {
  console.log(`\n🧪 Testing login with phone: ${phone}`);
  console.log(`📡 API URL: ${BASE_URL}/api/auth/login`);
  
  try {
    const response = await fetch(`${BASE_URL}/api/auth/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'User-Agent': 'LoginTestScript/1.0'
      },
      body: JSON.stringify({ phone, password })
    });

    console.log(`📊 Response Status: ${response.status} ${response.statusText}`);
    console.log(`📋 Response Headers:`, Object.fromEntries(response.headers.entries()));
    
    const text = await response.text();
    console.log(`📥 Response Body (raw):`, text.substring(0, 500));
    
    let data;
    try {
      data = JSON.parse(text);
      console.log(`✅ Parsed JSON response:`, JSON.stringify(data, null, 2));
    } catch (e) {
      console.log(`❌ Failed to parse JSON:`, e.message);
      return;
    }

    if (response.ok) {
      console.log(`✅ Login successful!`);
      console.log(`🎫 Token received: ${data.token ? 'Yes' : 'No'}`);
      console.log(`👤 User data:`, data.user);
    } else {
      console.log(`❌ Login failed:`, data.message || data);
      if (data.debug) {
        console.log(`🔍 Debug info:`, data.debug);
      }
    }
  } catch (error) {
    console.log(`💥 Request failed:`, error.message);
    console.log(`🔍 Error details:`, error);
  }
}

async function testEmptyBody() {
  console.log(`\n🧪 Testing with empty body (should show debug info)`);
  console.log(`📡 API URL: ${BASE_URL}/api/auth/login`);
  
  try {
    const response = await fetch(`${BASE_URL}/api/auth/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({})
    });

    const data = await response.json();
    console.log(`📊 Response Status: ${response.status}`);
    console.log(`📥 Response:`, JSON.stringify(data, null, 2));
    
    if (data.debug) {
      console.log(`✅ Debug info is present - logging is working!`);
    }
  } catch (error) {
    console.log(`💥 Request failed:`, error.message);
  }
}

async function testMissingPhone() {
  console.log(`\n🧪 Testing with missing phone (should show debug info)`);
  console.log(`📡 API URL: ${BASE_URL}/api/auth/login`);
  
  try {
    const response = await fetch(`${BASE_URL}/api/auth/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ password: 'test123' })
    });

    const data = await response.json();
    console.log(`📊 Response Status: ${response.status}`);
    console.log(`📥 Response:`, JSON.stringify(data, null, 2));
    
    if (data.debug) {
      console.log(`✅ Debug info shows:`, data.debug);
    }
  } catch (error) {
    console.log(`💥 Request failed:`, error.message);
  }
}

async function testWrongContentType() {
  console.log(`\n🧪 Testing with wrong Content-Type`);
  console.log(`📡 API URL: ${BASE_URL}/api/auth/login`);
  
  try {
    const response = await fetch(`${BASE_URL}/api/auth/login`, {
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain', // Wrong content type
      },
      body: JSON.stringify({ phone: '+1234567890', password: 'test123' })
    });

    const data = await response.json();
    console.log(`📊 Response Status: ${response.status}`);
    console.log(`📥 Response:`, JSON.stringify(data, null, 2));
  } catch (error) {
    console.log(`💥 Request failed:`, error.message);
  }
}

// Run tests
async function runAllTests() {
  console.log(`\n${'='.repeat(60)}`);
  console.log(`🚀 Login Endpoint Test Suite`);
  console.log(`${'='.repeat(60)}`);

  await testEmptyBody();
  await testMissingPhone();
  await testWrongContentType();
  
  // Test with actual credentials if provided
  if (process.argv[2] && process.argv[3]) {
    await testLogin(process.argv[2], process.argv[3]);
  } else {
    console.log(`\n💡 To test with real credentials, run:`);
    console.log(`   node test-login-endpoint.js "+1234567890" "password"`);
  }

  console.log(`\n${'='.repeat(60)}`);
  console.log(`✅ Tests completed!`);
  console.log(`${'='.repeat(60)}\n`);
}

runAllTests();
