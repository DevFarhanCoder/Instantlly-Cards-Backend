const mongoose = require('mongoose');

const MONGODB_URI = "mongodb+srv://rajeshmodi:Newpass1234@cluster0.9yfi96i.mongodb.net/instantlly?retryWrites=true&w=majority&appName=Cluster0";

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  email: String,
  credits: { type: Number, default: 0 },
  referralCode: String
});

const User = mongoose.model('User', UserSchema);

async function findDeepak() {
  try {
    await mongoose.connect(MONGODB_URI);
    console.log('✅ Connected to MongoDB');
    
    // Search for Deepak by name or phone variations
    const users = await User.find({
      $or: [
        { name: /Deepak/i },
        { phone: /9768676666/ },
        { phone: /\+889768676666/ }
      ]
    });
    
    console.log(`\n📋 Found ${users.length} matching users:`);
    users.forEach(user => {
      console.log('\n👤', user.name);
      console.log('  ID:', user._id);
      console.log('  Phone:', user.phone);
      console.log('  Email:', user.email);
      console.log('  💰 Credits:', user.credits);
    });
    
    await mongoose.disconnect();
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

findDeepak();
