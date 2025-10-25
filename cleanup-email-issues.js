// cleanup-email-issues.js
// Script to fix email field issues in existing database

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

async function cleanupEmailIssues() {
  try {
    console.log('🔗 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    console.log('🔍 Finding users with email issues...');
    
    // Find users with undefined or empty email
    const usersWithUndefinedEmail = await User.find({
      $or: [
        { email: undefined },
        { email: "" },
        { email: null }
      ]
    });

    console.log(`📊 Found ${usersWithUndefinedEmail.length} users with email issues`);

    if (usersWithUndefinedEmail.length > 0) {
      console.log('🧹 Cleaning up email fields...');
      
      // Update all undefined/empty emails to null
      const result = await User.updateMany(
        {
          $or: [
            { email: undefined },
            { email: "" }
          ]
        },
        { $set: { email: null } }
      );

      console.log(`✅ Updated ${result.modifiedCount} user records`);
    }

    // Check for duplicate phone numbers (the real issue)
    console.log('🔍 Checking for duplicate phone numbers...');
    const phoneStats = await User.aggregate([
      { $group: { _id: "$phone", count: { $sum: 1 }, users: { $push: { _id: "$_id", name: "$name" } } } },
      { $match: { count: { $gt: 1 } } }
    ]);

    if (phoneStats.length > 0) {
      console.log('⚠️ Found duplicate phone numbers:');
      phoneStats.forEach(stat => {
        console.log(`  Phone: ${stat._id}, Count: ${stat.count}`);
        stat.users.forEach(user => {
          console.log(`    - ${user.name} (${user._id})`);
        });
      });
    } else {
      console.log('✅ No duplicate phone numbers found');
    }

    console.log('🎉 Cleanup completed!');
    process.exit(0);

  } catch (error) {
    console.error('❌ Error during cleanup:', error);
    process.exit(1);
  }
}

cleanupEmailIssues();