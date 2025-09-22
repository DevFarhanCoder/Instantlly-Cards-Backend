const https = require('https');

const data = JSON.stringify({
  name: "TestUser", 
  phone: "+919876543210",
  password: "Test123"
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

console.log('🚀 Testing simple signup with only 3 fields...');
console.log('📤 Sending:', JSON.parse(data));

const req = https.request(options, (res) => {
  console.log('📥 Status:', res.statusCode);
  
  let responseData = '';
  res.on('data', (chunk) => {
    responseData += chunk;
  });
  
  res.on('end', () => {
    try {
      const parsed = JSON.parse(responseData);
      console.log('📥 Response:', parsed);
    } catch {
      console.log('📥 Raw Response:', responseData);
    }
  });
});

req.on('error', (e) => {
  console.error('❌ Error:', e.message);
});

req.write(data);
req.end();