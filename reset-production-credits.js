/**
 * Reset credits on PRODUCTION database
 * Make sure to use PRODUCTION MongoDB URI
 */

const mongoose = require('mongoose');

// PRODUCTION MongoDB URI - UPDATE THIS!
const PRODUCTION_MONGO_URI = "mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/instantllycards?retryWrites=true&w=majority&appName=Cluster0";

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  credits: Number,
  referredBy: String
}, { collection: 'users' });

const User = mongoose.model('User', userSchema);

async function resetProductionCredits() {
  try {
    console.log('🔄 Connecting to PRODUCTION MongoDB...');
    console.log(`📍 URI: ${PRODUCTION_MONGO_URI.substring(0, 50)}...`);
    
    await mongoose.connect(PRODUCTION_MONGO_URI);
    console.log('✅ Connected to PRODUCTION MongoDB\n');

    // Find ALL users
    const allUsers = await User.find({});
    console.log(`📊 Total users: ${allUsers.length}\n`);

    // Show users with high credits
    const highCreditUsers = allUsers.filter(u => u.credits > 1000);
    if (highCreditUsers.length > 0) {
      console.log(`⚠️  Users with > 1000 credits:`);
      highCreditUsers.forEach(u => {
        console.log(`   ${u.name || u.phone}: ${u.credits} credits`);
      });
      console.log('');
    }

    console.log('⏳ Resetting ALL users to 200 credits in 3 seconds...');
    await new Promise(resolve => setTimeout(resolve, 3000));

    const result = await User.updateMany({}, { $set: { credits: 200 } });
    
    console.log(`\n✅ Reset ${result.modifiedCount} users to 200 credits!`);

  } catch (error) {
    console.error('❌ Error:', error.message);
  } finally {
    await mongoose.connection.close();
    console.log('🔌 Database connection closed');
    process.exit(0);
  }
}

resetProductionCredits();
