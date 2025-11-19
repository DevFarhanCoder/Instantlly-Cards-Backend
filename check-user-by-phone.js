// Quick script to check if user exists with phone variations
require('dotenv').config();
const mongoose = require('mongoose');
const User = require('./src/models/User').default;

async function checkUser() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to database');

    const phoneVariations = [
      '+919876543210',
      '919876543210',
      '9876543210',
      '+91 9876543210',
    ];

    console.log('\n🔍 Searching for user with phone variations:');
    
    for (const phone of phoneVariations) {
      const user = await User.findOne({ phone });
      if (user) {
        console.log(`\n✅ FOUND USER with phone: ${phone}`);
        console.log('User details:', {
          _id: user._id,
          name: user.name,
          phone: user.phone,
          hasPassword: !!user.password,
          createdAt: user.createdAt
        });
        break;
      } else {
        console.log(`❌ Not found: ${phone}`);
      }
    }

    // Also search for similar phone numbers
    console.log('\n🔍 Searching for similar phone numbers starting with 9876...');
    const similarUsers = await User.find({ 
      phone: { $regex: '^.*9876.*' } 
    }).select('name phone createdAt').limit(5);

    if (similarUsers.length > 0) {
      console.log('\n📋 Found similar phone numbers:');
      similarUsers.forEach(user => {
        console.log(`  - ${user.phone} (${user.name})`);
      });
    } else {
      console.log('❌ No similar phone numbers found');
    }

    await mongoose.disconnect();
    console.log('\n✅ Done');
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

checkUser();
