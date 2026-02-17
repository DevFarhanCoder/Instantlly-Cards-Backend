// Test Admin Login API
// This script tests if the admin login endpoint is working
// Usage: node test-admin-login.js

const fetch = require('node-fetch');

async function testLogin() {
  const API_BASE_URL = 'https://api.channel-partner.instantllycards.com/api';
  
  console.log('\n🧪 Testing Admin Login API...\n');
  console.log(`API URL: ${API_BASE_URL}/admin-auth/login\n`);
  console.log('━'.repeat(70));
  
  // Test 1: Login with username "admin" and various passwords
  const testPasswords = [
    'Admin@123',
    'admin123', 
    'admin',
    'password'
  ];

  for (const password of testPasswords) {
    console.log(`\n🔐 Testing with username: "admin" and password: "${password}"`);
    
    try {
      const response = await fetch(`${API_BASE_URL}/admin-auth/login`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ 
          username: 'admin', 
          password: password 
        })
      });

      const data = await response.json();
      
      console.log(`   Status: ${response.status} ${response.statusText}`);
      console.log(`   Response:`, JSON.stringify(data, null, 2));
      
      if (data.success) {
        console.log('\n✅ SUCCESS! Login worked with these credentials:');
        console.log(`   Username: admin`);
        console.log(`   Password: ${password}`);
        console.log('\n━'.repeat(70));
        return;
      }
      
    } catch (error) {
      console.log(`   ❌ Error: ${error.message}`);
    }
  }
  
  console.log('\n━'.repeat(70));
  console.log('\n⚠️  None of the test passwords worked.');
  console.log('\n💡 Solutions:');
  console.log('   1. Reset the password using: node reset-admin-password.js admin YourNewPassword');
  console.log('   2. Check if the backend server is running');
  console.log('   3. Verify MONGODB_URI is configured correctly in the backend\n');
}

testLogin().catch(console.error);
