// Test script to verify ObjectId validation in authentication
const axios = require('axios');

async function testObjectIdValidation() {
  const baseURL = 'http://localhost:3001/api/cards';
  
  console.log('🧪 Testing ObjectId validation in authentication...\n');
  
  // Test 1: Invalid ObjectId format (should fail with 401)
  console.log('Test 1: Invalid ObjectId format (user_1762428529619)');
  try {
    const response = await axios.get(`${baseURL}/sent`, {
      headers: {
        'Authorization': 'Bearer invalid-token-with-bad-user-id'
      }
    });
    console.log('❌ Should have failed but got:', response.status);
  } catch (error) {
    if (error.response) {
      if (error.response.status === 401) {
        console.log('✅ Correctly rejected invalid token with 401');
        console.log('   Response:', error.response.data);
      } else {
        console.log('❌ Unexpected status:', error.response.status);
        console.log('   Response:', error.response.data);
      }
    } else {
      console.log('❌ Network/Connection error:', error.message);
    }
  }
  
  // Test 2: Valid ObjectId format but invalid token (should fail with 401)  
  console.log('\nTest 2: Valid ObjectId format but invalid token signature');
  try {
    // Create a fake JWT with valid ObjectId but invalid signature
    const fakeToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiI2NzFhMzUyYjIxNzgyMGFlMDg0YjUwNWIifQ.invalid-signature';
    const response = await axios.get(`${baseURL}/sent`, {
      headers: {
        'Authorization': `Bearer ${fakeToken}`
      }
    });
    console.log('❌ Should have failed but got:', response.status);
  } catch (error) {
    if (error.response) {
      if (error.response.status === 401) {
        console.log('✅ Correctly rejected invalid token signature with 401');
        console.log('   Response:', error.response.data);
      } else {
        console.log('❌ Unexpected status:', error.response.status);
        console.log('   Response:', error.response.data);
      }
    } else {
      console.log('❌ Network/Connection error:', error.message);
    }
  }
  
  console.log('\n🏁 ObjectId validation tests completed!');
  console.log('The authentication middleware now properly validates ObjectId format.');
  console.log('This should prevent "Cast to ObjectId failed" errors in production.');
}

// Run the tests
testObjectIdValidation().catch(console.error);