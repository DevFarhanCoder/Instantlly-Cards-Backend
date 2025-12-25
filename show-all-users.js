/**
 * Find ALL users and show their credits
 */

require('dotenv').config();
const mongoose = require('mongoose');

const MONGO_URI = process.env.MONGODB_URI;

const userSchema = new mongoose.Schema({}, { collection: 'users', strict: false });
const User = mongoose.model('User', userSchema);

async function showAllUsers() {
  try {
    await mongoose.connect(MONGO_URI);
    console.log('✅ Connected to:', mongoose.connection.db.databaseName, '\n');

    const users = await User.find({}).limit(20);
    console.log(`📊 Showing first ${users.length} users:\n`);

    users.forEach((user, i) => {
      console.log(`${i + 1}. ${user.name || 'No name'} (${user.phone})`);
      console.log(`   ID: ${user._id}`);
      console.log(`   Credits: ${user.credits}`);
      console.log(`   Referral Code: ${user.referralCode || 'N/A'}\n`);
    });

    // Check for users with high credits
    const highCreditUsers = await User.find({ credits: { $gt: 1000 } });
    if (highCreditUsers.length > 0) {
      console.log(`\n⚠️  ${highCreditUsers.length} users with > 1000 credits:`);
      highCreditUsers.forEach(u => {
        console.log(`   ${u.name}: ${u.credits} (ID: ${u._id})`);
      });

      console.log('\n🔧 Fixing these users...');
      const result = await User.updateMany(
        { credits: { $gt: 1000 } },
        { $set: { credits: 200 } }
      );
      console.log(`✅ Fixed ${result.modifiedCount} users!`);
    } else {
      console.log('✅ No users with high credits found!');
    }

  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    process.exit(0);
  }
}

showAllUsers();
