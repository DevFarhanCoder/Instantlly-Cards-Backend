const mongoose = require('mongoose');
require('dotenv').config();

// Define User schema directly
const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  password: String,
  email: String,
  profilePicture: String,
  about: String,
  pushToken: String,
  platform: String,
  deviceInfo: String,
  pushTokenUpdatedAt: Date
}, { timestamps: true });

const User = mongoose.model('User', userSchema);

async function checkMohammadToken() {
  try {
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('🔍 DETAILED TOKEN CHECK FOR MOHAMMAD FARHAN');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    // Connect to MongoDB
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Find Mohammad Farhan with ALL fields
    const user = await User.findOne({ phone: '+919867969445' }).lean();
    
    if (!user) {
      console.log('❌ User not found!');
      return;
    }

    console.log('📋 FULL USER DOCUMENT:');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log(JSON.stringify(user, null, 2));
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    console.log('🔑 SPECIFIC FIELDS:');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log(`Name: ${user.name}`);
    console.log(`Phone: ${user.phone}`);
    console.log(`Push Token: ${user.pushToken || 'NOT SET'}`);
    console.log(`Platform: ${user.platform || 'NOT SET'}`);
    console.log(`Device Info: ${user.deviceInfo || 'NOT SET'}`);
    console.log(`Token Updated At: ${user.pushTokenUpdatedAt || 'NEVER'}`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

    // Check if field exists in schema
    const schema = User.schema.paths;
    console.log('📝 SCHEMA FIELDS RELATED TO PUSH:');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('pushToken exists in schema:', 'pushToken' in schema ? '✅ YES' : '❌ NO');
    console.log('platform exists in schema:', 'platform' in schema ? '✅ YES' : '❌ NO');
    console.log('deviceInfo exists in schema:', 'deviceInfo' in schema ? '✅ YES' : '❌ NO');
    console.log('pushTokenUpdatedAt exists in schema:', 'pushTokenUpdatedAt' in schema ? '✅ YES' : '❌ NO');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    console.log('👋 Disconnected from MongoDB');
  }
}

checkMohammadToken();
