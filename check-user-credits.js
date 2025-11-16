require('dotenv').config();
const mongoose = require('mongoose');

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  email: String,
  credits: Number,
  creditsExpiryDate: Date,
  createdAt: Date
});

const User = mongoose.model('User', userSchema);

async function checkUser() {
  try {
    const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
    if (!mongoUri) {
      console.log('❌ No MongoDB URI found in environment');
      console.log('Available env vars:', Object.keys(process.env).filter(k => k.includes('MONGO')));
      return;
    }
    
    await mongoose.connect(mongoUri);
    console.log('✅ Connected to MongoDB');

    // Check if specific phone number was provided
    const testPhone = process.argv[2];
    
    let user;
    if (testPhone) {
      user = await User.findOne({ phone: testPhone });
      if (!user) {
        console.log(`❌ No user found with phone: ${testPhone}`);
        return;
      }
    } else {
      // Get a random user with credits > 0
      user = await User.findOne({ credits: { $gt: 0 } }).sort({ createdAt: -1 });
    }
    
    if (!user) {
      console.log('❌ No user with credits found');
      return;
    }

    console.log('\n📊 Latest User Details:');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('Name:', user.name);
    console.log('Phone:', user.phone);
    console.log('Email:', user.email);
    console.log('💰 Credits:', user.credits?.toLocaleString() || 0);
    console.log('📅 Expiry Date:', user.creditsExpiryDate ? new Date(user.creditsExpiryDate).toLocaleString() : 'Not set');
    console.log('📅 Joined:', user.createdAt ? new Date(user.createdAt).toLocaleString() : 'Unknown');
    
    if (user.creditsExpiryDate) {
      const now = new Date();
      const expiryDate = new Date(user.creditsExpiryDate);
      const isExpired = now > expiryDate;
      const daysRemaining = Math.ceil((expiryDate.getTime() - now.getTime()) / (1000 * 60 * 60 * 24));
      
      console.log('\n⏰ Status:', isExpired ? '❌ EXPIRED' : '✅ ACTIVE');
      if (!isExpired) {
        console.log('⏳ Days Remaining:', daysRemaining);
      }
    }

    await mongoose.disconnect();
    console.log('\n✅ Database disconnected');
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

checkUser();
