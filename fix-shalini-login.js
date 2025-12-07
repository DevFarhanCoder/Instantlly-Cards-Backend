// Fix Shalini's login - find user and update password
require('dotenv').config();
const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');

async function fixShaliniLogin() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');
    
    const db = mongoose.connection.db;
    const usersCollection = db.collection('users');

    // Search for Shalini by name first
    console.log('🔍 Searching for user "Shalini"...\n');
    
    let user = await usersCollection.findOne({ 
      name: { $regex: /shalini/i } 
    });

    if (user) {
      console.log('✅ Found user:');
      console.log(`   Name: ${user.name}`);
      console.log(`   Current Phone: ${user.phone}`);
      console.log(`   ID: ${user._id}`);
      console.log('');

      // Hash the password "123456"
      const hashedPassword = await bcrypt.hash('123456', 10);

      // Update phone and password
      const result = await usersCollection.updateOne(
        { _id: user._id },
        { 
          $set: { 
            phone: '+918070366363',
            password: hashedPassword
          } 
        }
      );

      console.log('✅ User updated successfully!');
      console.log(`   New Phone: +918070366363`);
      console.log(`   New Password: 123456`);
      console.log('');
      console.log('🎉 You can now login with:');
      console.log('   Phone: +918070366363');
      console.log('   Password: 123456');
    } else {
      console.log('❌ User "Shalini" not found');
      console.log('\n📋 Showing recent users:');
      
      const recentUsers = await usersCollection.find({})
        .sort({ createdAt: -1 })
        .limit(10)
        .toArray();
      
      recentUsers.forEach((u, i) => {
        console.log(`${i + 1}. ${u.name} - ${u.phone}`);
      });
    }

    await mongoose.disconnect();
    console.log('\n✅ Disconnected from MongoDB');
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

fixShaliniLogin();
