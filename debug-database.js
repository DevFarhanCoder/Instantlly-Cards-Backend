const mongoose = require('mongoose');
require('dotenv').config();

// User schema (copy from model)
const userSchema = new mongoose.Schema({
  name: { type: String, required: true },
  phone: { type: String, required: true, unique: true },
  email: { type: String },
  password: { type: String, required: true },
  profilePicture: { type: String, default: "" },
  about: { type: String, default: "Available" },
  createdAt: { type: Date, default: Date.now },
  updatedAt: { type: Date, default: Date.now }
});

const User = mongoose.model('User', userSchema);

const debugDatabase = async () => {
  try {
    console.log('🔌 Connecting to database...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to database');

    // Find all users
    console.log('\n📊 All users in database:');
    const allUsers = await User.find({}).select('name phone email createdAt');
    console.log('Total users found:', allUsers.length);
    
    allUsers.forEach((user, index) => {
      console.log(`\nUser ${index + 1}:`);
      console.log('  ID:', user._id);
      console.log('  Name:', user.name);
      console.log('  Phone:', user.phone);
      console.log('  Email:', user.email || 'none');
      console.log('  Created:', user.createdAt);
    });

    // Check specifically for the phone number we're trying to register
    console.log('\n🔍 Checking for phone +919326664680:');
    const specificUser = await User.findOne({ phone: '+919326664680' });
    if (specificUser) {
      console.log('❌ Found user with this phone:', {
        id: specificUser._id,
        name: specificUser.name,
        phone: specificUser.phone,
        email: specificUser.email
      });
    } else {
      console.log('✅ No user found with phone +919326664680');
    }

    // Check for variations of the phone number
    console.log('\n🔍 Checking for phone number variations:');
    const variations = [
      '+919326664680',
      '919326664680',
      '9326664680'
    ];
    
    for (const variation of variations) {
      const found = await User.findOne({ phone: variation });
      if (found) {
        console.log(`❌ Found user with phone "${variation}":`, found.name);
      } else {
        console.log(`✅ No user with phone "${variation}"`);
      }
    }

    // Check indexes
    console.log('\n📋 Database indexes:');
    const indexes = await User.collection.getIndexes();
    console.log(JSON.stringify(indexes, null, 2));

  } catch (error) {
    console.error('❌ Database debug error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n🔌 Disconnected from database');
  }
};

debugDatabase();