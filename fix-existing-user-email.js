// fix-existing-user-email.js
// Remove null email from existing user

const mongoose = require('mongoose');
require('dotenv').config();

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  password: String,
  email: String,
  profilePicture: String,
  about: String,
  pushToken: String,
  platform: String,
  pushTokenUpdatedAt: Date,
}, { timestamps: true });

const User = mongoose.model('User', UserSchema);

async function fixExistingUserEmail() {
  try {
    console.log('🔗 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    // Remove email field from users who have null emails
    const result = await User.updateMany(
      { email: null },
      { $unset: { email: "" } }
    );

    console.log(`✅ Removed email field from ${result.modifiedCount} users`);

    // Verify the fix
    const usersAfterFix = await User.find({});
    console.log('📊 Users after fix:');
    usersAfterFix.forEach(user => {
      console.log(`  - ${user.name} | ${user.phone} | email: ${user.email !== undefined ? '"' + user.email + '"' : 'field not present'}`);
    });

    process.exit(0);

  } catch (error) {
    console.error('❌ Error during fix:', error);
    process.exit(1);
  }
}

fixExistingUserEmail();