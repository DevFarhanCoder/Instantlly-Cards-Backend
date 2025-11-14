// Script to create an admin user
const axios = require('axios');

const API_BASE = process.env.API_BASE || 'http://localhost:5000';

async function createAdmin() {
  try {
    console.log('Creating admin user...');
    
    const response = await axios.post(`${API_BASE}/api/admin-auth/create-first-admin`, {
      username: 'admin',
      email: 'admin@instantllycards.com',
      password: 'admin123' // Change this to a secure password
    });

    if (response.data.success) {
      console.log('✅ Admin user created successfully!');
      console.log('Username: admin');
      console.log('Password: admin123');
      console.log('\n⚠️  IMPORTANT: Change the password after first login!');
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
