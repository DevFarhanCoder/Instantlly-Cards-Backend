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

async function checkDeepak() {
  try {
    await mongoose.connect(MONGODB_URI);
    console.log('✅ Connected to MongoDB');
    
    const user = await User.findOne({ phone: '9768676666' });
    if (user) {
      console.log('\n👤 Deepak Bhanushali:');
      console.log('ID:', user._id);
      console.log('Name:', user.name);
      console.log('Phone:', user.phone);
      console.log('Email:', user.email);
      console.log('💰 Credits:', user.credits);
      console.log('Referral Code:', user.referralCode);
    } else {
      console.log('❌ No user found with phone 9768676666');
    }
    
    await mongoose.disconnect();
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

checkDeepak();
