const mongoose = require('mongoose');
require('dotenv').config();

// Define User schema inline
const UserSchema = new mongoose.Schema(
  {
    name: { type: String, required: true },
    phone: { type: String, required: true },
    password: { type: String, required: true },
    email: { type: String },
    profilePicture: { type: String, default: "" },
    about: { type: String, default: "Available" },
    pushToken: { type: String },
    platform: { type: String },
    pushTokenUpdatedAt: { type: Date },
    deviceInfo: { type: mongoose.Schema.Types.Mixed },
  },
  { timestamps: true }
);

async function testDirectUpdate() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    const User = mongoose.model('User', UserSchema);

    // Find Mohammad Farhan
    const user = await User.findOne({ phone: '+919867969445' });
    if (!user) {
      console.log('❌ User not found!');
      process.exit(1);
    }

    console.log('\n📋 BEFORE UPDATE:');
    console.log('Name:', user.name);
    console.log('Phone:', user.phone);
    console.log('Push Token:', user.pushToken || 'NOT SET');
    console.log('Platform:', user.platform || 'NOT SET');
    console.log('Updated At:', user.pushTokenUpdatedAt || 'NOT SET');

    // Try direct update
    console.log('\n🔄 Attempting direct update...');
    const testToken = 'ExponentPushToken[TEST_DIRECT_UPDATE_' + Date.now() + ']';
    
    user.pushToken = testToken;
    user.platform = 'android';
    user.pushTokenUpdatedAt = new Date();
    
    await user.save();
    console.log('✅ Save called');

    // Verify immediately
    const verifyUser = await User.findById(user._id);
    console.log('\n📋 AFTER UPDATE (immediate verify):');
    console.log('Push Token:', verifyUser.pushToken || 'NOT SET');
    console.log('Platform:', verifyUser.platform || 'NOT SET');
    console.log('Updated At:', verifyUser.pushTokenUpdatedAt || 'NOT SET');

    // Wait a second and verify again
    await new Promise(resolve => setTimeout(resolve, 1000));
    const verifyUser2 = await User.findById(user._id);
    console.log('\n📋 AFTER UPDATE (1 second later):');
    console.log('Push Token:', verifyUser2.pushToken || 'NOT SET');
    console.log('Platform:', verifyUser2.platform || 'NOT SET');
    console.log('Updated At:', verifyUser2.pushTokenUpdatedAt || 'NOT SET');

    if (verifyUser2.pushToken === testToken) {
      console.log('\n✅ SUCCESS! Database update is working!');
      console.log('This means the issue is somewhere else in the registration flow.');
    } else {
      console.log('\n❌ FAILED! Database did NOT save the token!');
      console.log('This is a database or schema issue.');
    }

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.disconnect();
  }
}

testDirectUpdate();
