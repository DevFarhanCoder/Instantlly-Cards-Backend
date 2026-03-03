"use strict";
/**
 * One-time Migration Script: Reset All Users' Credits to New System
 *
 * This script migrates all existing users to the new credit system:
 * - Resets credits to 200 (or configured signupBonus)
 * - Sets expiry date to 30 days from now
 * - Preserves referral relationships
 *
 * Run this ONCE after deploying the new credit system
 */
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const User_1 = __importDefault(require("../models/User"));
const CreditConfig_1 = __importDefault(require("../models/CreditConfig"));
const dotenv_1 = __importDefault(require("dotenv"));
dotenv_1.default.config();
async function migrateCredits() {
    try {
        console.log('🚀 Starting credit migration...\n');
        // Connect to MongoDB
        if (!process.env.MONGODB_URI) {
            throw new Error('MONGODB_URI not found in environment');
        }
        await mongoose_1.default.connect(process.env.MONGODB_URI);
        console.log('✅ Connected to MongoDB\n');
        // Get or create credit config
        let creditConfig = await CreditConfig_1.default.findOne();
        if (!creditConfig) {
            creditConfig = await CreditConfig_1.default.create({
                signupBonus: 200,
                referralReward: 300,
                lastUpdatedBy: 'migration-script',
                lastUpdatedAt: new Date()
            });
            console.log('✅ Created CreditConfig with defaults\n');
        }
        const newCreditAmount = creditConfig.signupBonus;
        console.log(`📋 Target credit amount: ${newCreditAmount}\n`);
        // Calculate new expiry date (30 days from now)
        const newExpiryDate = new Date();
        newExpiryDate.setMonth(newExpiryDate.getMonth() + 1);
        // Find all users
        const allUsers = await User_1.default.find({});
        console.log(`👥 Found ${allUsers.length} total users\n`);
        // Categorize users - users with null, undefined, or different credit amounts
        const usersToMigrate = allUsers.filter(user => {
            const userCredits = user.credits;
            // Migrate if credits is null, undefined, or not equal to target
            return userCredits === null || userCredits === undefined || userCredits !== newCreditAmount;
        });
        console.log(`📊 Migration Summary:`);
        console.log(`   - Total users: ${allUsers.length}`);
        console.log(`   - Users to migrate: ${usersToMigrate.length}`);
        console.log(`   - Users already correct: ${allUsers.length - usersToMigrate.length}\n`);
        if (usersToMigrate.length === 0) {
            console.log('✅ All users already have correct credits. No migration needed!');
            await mongoose_1.default.disconnect();
            return;
        }
        // Show sample of users to be migrated
        console.log(`📝 Sample users to migrate (first 5):`);
        usersToMigrate.slice(0, 5).forEach(user => {
            console.log(`   - ${user.name} (${user.phone}): ${user.credits || 0} → ${newCreditAmount}`);
        });
        console.log('');
        // Confirm migration (safety check)
        console.log('⚠️  WARNING: This will reset credits for all users!\n');
        console.log('💡 To proceed, run with --confirm flag\n');
        if (!process.argv.includes('--confirm')) {
            console.log('🛑 Dry run complete. No changes made.');
            console.log('   Run with --confirm to apply changes: npm run migrate:credits -- --confirm\n');
            await mongoose_1.default.disconnect();
            return;
        }
        // Perform migration
        console.log('🔄 Starting migration...\n');
        let migratedCount = 0;
        let errorCount = 0;
        for (const user of usersToMigrate) {
            try {
                const oldCredits = user.credits || 0;
                const oldExpiry = user.creditsExpiryDate;
                user.credits = newCreditAmount;
                user.creditsExpiryDate = newExpiryDate;
                await user.save();
                migratedCount++;
                if (migratedCount % 100 === 0) {
                    console.log(`   ✅ Migrated ${migratedCount}/${usersToMigrate.length} users...`);
                }
                // Log significant changes
                if (oldCredits > 10000 || oldCredits < 0) {
                    console.log(`   ⚠️  Notable: ${user.name} had ${oldCredits} credits (now ${newCreditAmount})`);
                }
            }
            catch (error) {
                errorCount++;
                console.error(`   ❌ Error migrating user ${user._id}:`, error);
            }
        }
        console.log('\n✅ Migration Complete!\n');
        console.log(`📊 Results:`);
        console.log(`   - Successfully migrated: ${migratedCount}`);
        console.log(`   - Errors: ${errorCount}`);
        console.log(`   - Total processed: ${usersToMigrate.length}`);
        console.log(`   - New credit amount: ${newCreditAmount}`);
        console.log(`   - New expiry date: ${newExpiryDate.toISOString()}\n`);
        await mongoose_1.default.disconnect();
        console.log('✅ Disconnected from MongoDB\n');
    }
    catch (error) {
        console.error('❌ Migration failed:', error);
        process.exit(1);
    }
}
// Run migration
migrateCredits();
