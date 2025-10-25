// Quick script to check all users' push tokens
const mongoose = require('mongoose');
require('dotenv').config();

async function checkTokens() {
  try {
    const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
    await mongoose.connect(mongoUri);
    console.log('✅ Connected to MongoDB\n');
    
    const User = mongoose.model('User', new mongoose.Schema({
      name: String,
      email: String,
      phone: String,
      pushToken: String,
      platform: String,
    }));

    const users = await User.find({}, { name: 1, phone: 1, pushToken: 1, platform: 1 }).limit(10);
    
    console.log('📊 Users and their push tokens:\n');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    users.forEach((user, index) => {
      console.log(`${index + 1}. ${user.name || 'Unnamed'}`);
      console.log(`   Phone: ${user.phone || 'not set'}`);
      console.log(`   Platform: ${user.platform || 'not set'}`);
      console.log(`   Push Token: ${user.pushToken || 'NOT SET'}`);
      
      if (user.pushToken) {
        if (user.pushToken === 'expo-go-local-mode') {
          console.log(`   ⚠️  Status: Using Expo Go (won't receive push notifications)`);
        } else if (user.pushToken.startsWith('ExponentPushToken[')) {
          console.log(`   ✅ Status: Valid production token`);
        } else {
          console.log(`   ❌ Status: Invalid token format`);
        }
      } else {
        console.log(`   ❌ Status: No token registered`);
      }
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    });
    
    const totalUsers = await User.countDocuments();
    const usersWithValidTokens = await User.countDocuments({ 
      pushToken: { $regex: /^ExponentPushToken\[/ }
    });
    const usersWithExpoGo = await User.countDocuments({ pushToken: 'expo-go-local-mode' });
    const usersWithNoToken = await User.countDocuments({ 
      $or: [
        { pushToken: { $exists: false } },
        { pushToken: null },
        { pushToken: '' }
      ]
    });
    
    console.log('\n📈 Summary:');
    console.log(`   Total users: ${totalUsers}`);
    console.log(`   ✅ With valid production tokens: ${usersWithValidTokens}`);
    console.log(`   ⚠️  Using Expo Go: ${usersWithExpoGo}`);
    console.log(`   ❌ No token: ${usersWithNoToken}`);
    
  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    process.exit(0);
  }
}

console.log('🔍 Checking all user push tokens...\n');
checkTokens();
