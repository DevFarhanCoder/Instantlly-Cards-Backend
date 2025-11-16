/**
 * Set credits expiry date for all existing users
 * Credits expire 1 month from their signup date (createdAt)
 */

const mongoose = require('mongoose');
require('dotenv').config();

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: Number,
  creditsExpiryDate: Date,
  createdAt: Date,
  updatedAt: Date
}, { timestamps: true });

const User = mongoose.model('User', UserSchema);

async function setCreditsExpiry() {
  try {
    console.log('🚀 Setting credits expiry dates for all users...\n');

    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Get all users
    const users = await User.find({});
    console.log(`📊 Found ${users.length} users\n`);

    let updated = 0;
    let skipped = 0;

    for (const user of users) {
      try {
        // Calculate expiry date = 1 month from signup date
        const signupDate = new Date(user.createdAt);
        const expiryDate = new Date(signupDate);
        expiryDate.setMonth(expiryDate.getMonth() + 1);

        // Check if already expired
        const now = new Date();
        const isExpired = now > expiryDate;
        const daysRemaining = Math.ceil((expiryDate.getTime() - now.getTime()) / (1000 * 60 * 60 * 24));

        // Update user
        user.creditsExpiryDate = expiryDate;
        
        // If expired, set credits to 0
        if (isExpired && user.credits > 0) {
          user.credits = 0;
          console.log(`⏰ ${user.name} (${user.phone})`);
          console.log(`   Joined: ${signupDate.toLocaleDateString()}`);
          console.log(`   Expired: ${expiryDate.toLocaleDateString()}`);
          console.log(`   Credits: ${user.credits} → 0 (EXPIRED)\n`);
        } else {
          console.log(`✅ ${user.name} (${user.phone})`);
          console.log(`   Joined: ${signupDate.toLocaleDateString()}`);
          console.log(`   Expires: ${expiryDate.toLocaleDateString()}`);
          console.log(`   Days Remaining: ${Math.max(0, daysRemaining)} days`);
          console.log(`   Credits: ${user.credits?.toLocaleString() || 0}\n`);
        }

        await user.save();
        updated++;

      } catch (error) {
        console.error(`❌ Error updating ${user.name}:`, error.message);
        skipped++;
      }
    }

    console.log('\n📈 Summary:');
    console.log(`   ✅ Updated: ${updated} users`);
    console.log(`   ❌ Skipped: ${skipped} users`);
    console.log(`   📊 Total: ${users.length} users`);

    // Show expired users count
    const expiredUsers = await User.find({ 
      creditsExpiryDate: { $lt: new Date() },
      credits: 0
    });
    
    console.log(`\n⏰ Expired Users: ${expiredUsers.length}`);
    
    // Show active users count
    const activeUsers = await User.find({
      creditsExpiryDate: { $gte: new Date() },
      credits: { $gt: 0 }
    });
    
    console.log(`✅ Active Users: ${activeUsers.length}`);

  } catch (error) {
    console.error('💥 Error:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\n👋 Disconnected from MongoDB');
  }
}

setCreditsExpiry()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
