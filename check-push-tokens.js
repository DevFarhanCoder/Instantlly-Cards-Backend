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
  pushToken: { type: String },
  platform: { type: String },
  pushTokenUpdatedAt: { type: Date },
  createdAt: { type: Date, default: Date.now },
  updatedAt: { type: Date, default: Date.now }
});

const User = mongoose.model('User', userSchema);

const checkPushTokens = async () => {
  try {
    console.log('🔌 Connecting to database...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to database');

    // Find all users
    console.log('\n📊 Checking push tokens for all users:');
    const allUsers = await User.find({}).select('name phone pushToken platform pushTokenUpdatedAt createdAt');
    console.log(`Total users found: ${allUsers.length}`);
    
    allUsers.forEach((user, index) => {
      console.log(`\n👤 User ${index + 1}:`);
      console.log('  Name:', user.name);
      console.log('  Phone:', user.phone);
      console.log('  Push Token:', user.pushToken || 'None');
      console.log('  Platform:', user.platform || 'None');
      console.log('  Token Updated:', user.pushTokenUpdatedAt || 'Never');
      console.log('  Created:', user.createdAt);
    });

    // Check push token statistics
    const usersWithTokens = allUsers.filter(user => user.pushToken);
    const usersWithExpoGo = allUsers.filter(user => user.pushToken === 'expo-go-local-mode');
    const usersWithRealTokens = allUsers.filter(user => user.pushToken && user.pushToken !== 'expo-go-local-mode');

    console.log('\n📈 Push Token Statistics:');
    console.log(`  Total users: ${allUsers.length}`);
    console.log(`  Users with push tokens: ${usersWithTokens.length}`);
    console.log(`  Users with Expo Go tokens: ${usersWithExpoGo.length}`);
    console.log(`  Users with real push tokens: ${usersWithRealTokens.length}`);

  } catch (error) {
    console.error('❌ Error checking push tokens:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n🔌 Disconnected from database');
  }
};

checkPushTokens();