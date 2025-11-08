// Test the /api/ads endpoint to see what's causing the 500 error
const axios = require('axios');
require('dotenv').config();

async function testAdsEndpoint() {
  try {
    // First, login as admin to get a valid token
    console.log('🔐 Logging in as admin...');
    const loginResponse = await axios.post(
      'https://instantlly-cards-backend-6ki0.onrender.com/api/admin-auth/login',
      {
        username: 'admin',
        password: process.env.ADMIN_PASSWORD || 'Admin@123'
      }
    );

    const token = loginResponse.data.token;
    console.log('✅ Login successful, token received');
    console.log('Token preview:', token.substring(0, 50) + '...');

    // Now test the /api/ads endpoint
    console.log('\n📊 Testing GET /api/ads...');
    const adsResponse = await axios.get(
      'https://instantlly-cards-backend-6ki0.onrender.com/api/ads',
      {
        headers: {
          'Authorization': `Bearer ${token}`
        }
      }
    );

    console.log('✅ Success! Received', adsResponse.data.data?.length, 'ads');
    if (adsResponse.data.data && adsResponse.data.data.length > 0) {
      console.log('\n📸 First ad sample:');
      const firstAd = adsResponse.data.data[0];
      console.log({
        _id: firstAd._id,
        title: firstAd.title,
        bottomImage: firstAd.bottomImage?.substring(0, 100) + '...',
        hasBottomImageGridFS: !!firstAd.bottomImageGridFS,
        hasFullscreenImageGridFS: !!firstAd.fullscreenImageGridFS
      });
    }

  } catch (error) {
    console.error('\n❌ Error:', error.response?.data || error.message);
    console.error('Status:', error.response?.status);
    console.error('Headers:', error.response?.headers);
  }
}

testAdsEndpoint();
