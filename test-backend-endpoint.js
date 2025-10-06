// Test if the backend token registration endpoint is working
const https = require('https');

async function testBackendEndpoint() {
  console.log('🧪 Testing Backend Token Registration Endpoint\n');
  
  // First, we need to get an auth token by logging in
  console.log('Step 1: Getting authentication token...');
  
  const loginData = JSON.stringify({
    phone: '+919867969445',
    password: 'your_password_here' // REPLACE THIS WITH YOUR ACTUAL PASSWORD
  });
  
  const loginOptions = {
    hostname: 'instantlly-cards-backend.onrender.com',
    port: 443,
    path: '/api/auth/login',
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': loginData.length
    }
  };
  
  return new Promise((resolve, reject) => {
    const req = https.request(loginOptions, (res) => {
      let data = '';
      
      res.on('data', (chunk) => {
        data += chunk;
      });
      
      res.on('end', () => {
        try {
          const response = JSON.parse(data);
          
          if (response.token) {
            console.log('✅ Login successful!');
            console.log('✅ Got auth token:', response.token.substring(0, 20) + '...\n');
            
            // Now test token registration
            testTokenRegistration(response.token);
          } else {
            console.error('❌ Login failed:', response);
            console.error('\n⚠️  Please update your password in the script!');
            process.exit(1);
          }
        } catch (error) {
          console.error('❌ Failed to parse login response:', error);
          console.error('Response:', data);
          process.exit(1);
        }
      });
    });
    
    req.on('error', (error) => {
      console.error('❌ Login request failed:', error.message);
      process.exit(1);
    });
    
    req.write(loginData);
    req.end();
  });
}

function testTokenRegistration(authToken) {
  console.log('Step 2: Testing token registration endpoint...');
  
  const testToken = 'ExponentPushToken[TEST-TOKEN-FROM-SCRIPT]';
  
  const registerData = JSON.stringify({
    pushToken: testToken,
    platform: 'android',
    deviceInfo: {
      brand: 'Test',
      modelName: 'Script Test',
      osName: 'Node.js',
      osVersion: process.version
    }
  });
  
  const registerOptions = {
    hostname: 'instantlly-cards-backend.onrender.com',
    port: 443,
    path: '/api/notifications/register-token',
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': registerData.length,
      'Authorization': `Bearer ${authToken}`
    }
  };
  
  const req = https.request(registerOptions, (res) => {
    let data = '';
    
    res.on('data', (chunk) => {
      data += chunk;
    });
    
    res.on('end', () => {
      console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log('Response Status:', res.statusCode);
      console.log('Response Body:', data);
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
      
      if (res.statusCode === 200) {
        console.log('✅ SUCCESS! Backend token registration endpoint is working!');
        console.log('✅ This means the backend can receive and process token registrations.');
        console.log('\n💡 If your app still can\'t register tokens, the issue is on the app side:');
        console.log('   - App might not be getting Firebase token');
        console.log('   - App might not be reaching the backend');
        console.log('   - App might have network issues');
      } else {
        console.log('❌ FAILED! Backend returned error status:', res.statusCode);
        try {
          const response = JSON.parse(data);
          console.log('Error details:', response);
        } catch (e) {
          console.log('Raw response:', data);
        }
      }
      
      process.exit(0);
    });
  });
  
  req.on('error', (error) => {
    console.error('❌ Token registration request failed:', error.message);
    console.error('\n⚠️  Backend might be down or not accessible!');
    process.exit(1);
  });
  
  req.write(registerData);
  req.end();
}

console.log('');
console.log('╔════════════════════════════════════════════════════════════╗');
console.log('║  🧪 Backend Token Registration Endpoint Test              ║');
console.log('║     This tests if the backend can accept tokens           ║');
console.log('╚════════════════════════════════════════════════════════════╝');
console.log('');
console.log('⚠️  IMPORTANT: Edit this file and add your password first!');
console.log('   Line 13: password: \'your_password_here\'');
console.log('');

// Check if password was updated
const fs = require('fs');
const scriptContent = fs.readFileSync(__filename, 'utf8');
if (scriptContent.includes('your_password_here')) {
  console.error('❌ Please update your password in the script before running!');
  console.error('   Open: test-backend-endpoint.js');
  console.error('   Find: password: \'your_password_here\'');
  console.error('   Replace with your actual password');
  process.exit(1);
}

testBackendEndpoint();
