const fetch = require('node-fetch');

async function testLogin() {
  const API_BASE_URL = 'https://instantlly-cards-backend-6ki0.onrender.com/api';
  
  console.log('\n🧪 Testing CORRECT Backend URL...\n');
  console.log(`API URL: ${API_BASE_URL}/admin-auth/login\n`);
  
  const testPasswords = ['Admin@123', 'admin123', 'admin'];

  for (const password of testPasswords) {
    console.log(`🔐 Testing: admin / ${password}`);
    
    try {
      const response = await fetch(`${API_BASE_URL}/admin-auth/login`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username: 'admin', password })
      });

      const data = await response.json();
      console.log(`   Status: ${response.status}`);
      console.log(`   Success: ${data.success ? '✅' : '❌'}`);
      
      if (data.success) {
        console.log('\n✅ LOGIN SUCCESSFUL!');
        console.log(`Username: admin`);
        console.log(`Password: ${password}\n`);
        return;
      } else {
        console.log(`   Message: ${data.message}\n`);
      }
      
    } catch (error) {
      console.log(`   ❌ Error: ${error.message}\n`);
    }
  }
}

testLogin();
