// check-null-emails.js
// Check for users with null emails that might be causing conflicts

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

async function checkNullEmails() {
  try {
    console.log('🔗 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    // Check for users with null emails
    const nullEmails = await User.find({ email: null });
    console.log(`📧 Users with null emails: ${nullEmails.length}`);
    nullEmails.forEach(user => {
      console.log(`  - ${user.name} | ${user.phone} | email: ${user.email}`);
    });

    // Check for users with undefined emails
    const undefinedEmails = await User.find({ email: undefined });
    console.log(`📧 Users with undefined emails: ${undefinedEmails.length}`);
    undefinedEmails.forEach(user => {
      console.log(`  - ${user.name} | ${user.phone} | email: ${user.email}`);
    });

    // Check for users without email field at all
    const noEmailField = await User.find({ email: { $exists: false } });
    console.log(`📧 Users without email field: ${noEmailField.length}`);
    noEmailField.forEach(user => {
      console.log(`  - ${user.name} | ${user.phone} | email field exists: ${user.email !== undefined}`);
    });

    // Check all users and their email values
    const allUsers = await User.find({});
    console.log(`📊 All users (${allUsers.length}):`);
    allUsers.forEach(user => {
      console.log(`  - ${user.name} | ${user.phone} | email: "${user.email}" (type: ${typeof user.email})`);
    });

    process.exit(0);

  } catch (error) {
    console.error('❌ Error during check:', error);
    process.exit(1);
  }
}

checkNullEmails();