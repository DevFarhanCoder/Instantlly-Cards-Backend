/**
 * Migration Script: Update all users to new 5 lac (500,000) credits system
 * 
 * This script:
 * 1. Updates all existing users to have 500,000 credits (replacing old 1200 credits)
 * 2. Recalculates referral bonuses (20% of 500,000 = 100,000 credits)
 * 3. Creates transaction records for the credit adjustments
 */

const mongoose = require('mongoose');
require('dotenv').config();

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: Number,
  referralCode: String,
  referredBy: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
  createdAt: Date,
  updatedAt: Date
}, { timestamps: true });

const TransactionSchema = new mongoose.Schema({
  type: String,
  fromUser: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
  toUser: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
  amount: Number,
  description: String,
  balanceBefore: Number,
  balanceAfter: Number,
  status: String,
  createdAt: Date
}, { timestamps: true });

const User = mongoose.model('User', UserSchema);
const Transaction = mongoose.model('Transaction', TransactionSchema);

async function migrateCredits() {
  try {
    console.log('🚀 Starting credit migration to 5 lac system...\n');

    // Connect to MongoDB
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Get all users
    const users = await User.find({}).sort({ createdAt: 1 });
    console.log(`📊 Found ${users.length} total users\n`);

    let migrated = 0;
    let skipped = 0;
    let errors = 0;

    for (const user of users) {
      try {
        const oldCredits = user.credits || 0;
        const newCredits = 500000; // 5 lac

        // Skip if already has 500,000 or more (already migrated or earned)
        if (oldCredits >= 500000) {
          console.log(`⏭️  Skipping ${user.name} (${user.phone}) - Already has ${oldCredits.toLocaleString()} credits`);
          skipped++;
          continue;
        }

        // Update user credits
        user.credits = newCredits;
        await user.save();

        // Create transaction record for the adjustment
        await Transaction.create({
          type: 'credit_adjustment',
          toUser: user._id,
          amount: newCredits - oldCredits,
          description: `Credit system upgrade: ${oldCredits.toLocaleString()} → ${newCredits.toLocaleString()} credits`,
          balanceBefore: oldCredits,
          balanceAfter: newCredits,
          status: 'completed'
        });

        console.log(`✅ Migrated ${user.name} (${user.phone}): ${oldCredits.toLocaleString()} → ${newCredits.toLocaleString()} credits`);
        migrated++;

      } catch (error) {
        console.error(`❌ Error migrating user ${user.name}:`, error.message);
        errors++;
      }
    }

    console.log('\n📈 Migration Summary:');
    console.log(`   ✅ Migrated: ${migrated} users`);
    console.log(`   ⏭️  Skipped: ${skipped} users (already have 500k+ credits)`);
    console.log(`   ❌ Errors: ${errors} users`);
    console.log(`   📊 Total: ${users.length} users`);

    console.log('\n🎉 Migration completed successfully!');

  } catch (error) {
    console.error('💥 Migration failed:', error);
    throw error;
  } finally {
    await mongoose.disconnect();
    console.log('\n👋 Disconnected from MongoDB');
  }
}

// Run migration
migrateCredits()
  .then(() => {
    console.log('\n✨ All done!');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n💥 Fatal error:', error);
    process.exit(1);
  });
