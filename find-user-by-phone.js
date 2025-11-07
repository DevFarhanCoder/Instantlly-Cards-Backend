// Find user by partial phone match
require('dotenv').config();
const mongoose = require('mongoose');

async function findUser() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    const db = mongoose.connection.db;
    const usersCollection = db.collection('users');

    console.log('🔍 Searching for users with phone containing "9920067878"...\n');
    
    const users = await usersCollection.find({
      $or: [
        { phone: { $regex: '9920067878' } },
        { phoneNumber: { $regex: '9920067878' } }
      ]
    }).toArray();

    console.log(`Found ${users.length} users:\n`);
    
    users.forEach((u, i) => {
      console.log(`${i + 1}. ${u.name}`);
      console.log(`   Phone: ${u.phone || u.phoneNumber}`);
      console.log(`   ID: ${u._id}`);
      console.log(`   Created: ${u.createdAt}`);
      console.log('');
    });

    if (users.length === 0) {
      console.log('No users found. Let me show some recent users:');
      const recent = await usersCollection.find({}).sort({ createdAt: -1 }).limit(10).toArray();
      recent.forEach(u => {
        console.log(`- ${u.name}: ${u.phone || u.phoneNumber}`);
      });
    }

    await mongoose.disconnect();
  } catch (error) {
    console.error('Error:', error);
  }
}

findUser();
