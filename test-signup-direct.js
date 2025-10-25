// test-signup-direct.js
// Test signup with direct API call to debug the issue

const https = require('https');

const data = JSON.stringify({
  name: "Test User",
  phone: "+916209011721",
  password: "TestPass123"
});

const options = {
  hostname: 'instantlly-cards-backend.onrender.com',
  port: 443,
  path: '/api/auth/signup',
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Content-Length': data.length
  }
};

console.log('🚀 Testing signup with:', JSON.parse(data));
console.log('📡 Making request to:', `https://${options.hostname}${options.path}`);

const req = https.request(options, (res) => {
  console.log(`📊 Status: ${res.statusCode}`);
  console.log(`📋 Headers:`, res.headers);
  
  let responseData = '';
  res.on('data', (chunk) => {
    responseData += chunk;
  });
  
  res.on('end', () => {
    console.log('📥 Response:', responseData);
    try {
      const parsed = JSON.parse(responseData);
      console.log('📝 Parsed response:', JSON.stringify(parsed, null, 2));
    } catch (e) {
      console.log('❌ Could not parse response as JSON');
    }
  });
});

req.on('error', (error) => {
  console.error('❌ Request error:', error);
});

req.write(data);
req.end();