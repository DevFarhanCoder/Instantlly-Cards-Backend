const https = require('https');

async function testSignup() {
  const data = JSON.stringify({
    name: "Test User Debug",
    phone: "+999999999999", // Using a unique phone number
    password: "test123"
    // Note: NOT sending email field at all
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

  return new Promise((resolve, reject) => {
    const req = https.request(options, (res) => {
      let responseData = '';
      
      res.on('data', (chunk) => {
        responseData += chunk;
      });
      
      res.on('end', () => {
        console.log('Status:', res.statusCode);
        console.log('Response:', responseData);
        resolve();
      });
    });

    req.on('error', (error) => {
      console.error('Error:', error);
      reject(error);
    });

    req.write(data);
    req.end();
  });
}

console.log('🚀 Testing signup with no email field...');
testSignup().catch(console.error);