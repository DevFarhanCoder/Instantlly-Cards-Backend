// Script to create an admin user
const axios = require('axios');

// AWS Cloud (Primary)
const API_BASE = process.env.API_BASE || 'https://api.instantllycards.com';
const BACKUP_API = 'https://instantlly-cards-backend-6ki0.onrender.com';

async function createAdmin() {
  try {
    console.log('Creating admin user...');
    
    const response = await axios.post(`${API_BASE}/api/admin-auth/create-first-admin`, {
      username: 'Farhan',
      email: 'farhan@instantllycards.com',
      password: 'Farhan_90'
    });

    if (response.data.success) {
      console.log('✅ Admin user created successfully!');
      console.log('Username: Farhan');
      console.log('Password: Farhan_90');
      console.log('\n✅ Admin credentials are set!');
    } else {
      console.log('❌ Failed to create admin:', response.data.message);
    }
  } catch (error) {
    if (error.response) {
      console.log('❌ Error:', error.response.data.message);
    } else {
      console.error('❌ Error creating admin:', error.message);
    }
  }
}

createAdmin();
