const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');
require('dotenv').config();

const MONGODB_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

console.log('🔌 Connecting to MongoDB...');

mongoose.connect(MONGODB_URI)
  .then(() => {
    console.log('✅ Connected to MongoDB');
    createUser();
  })
  .catch(err => {
    console.error('❌ MongoDB connection error:', err);
    process.exit(1);
  });

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  password: String,
  email: String,
  profilePicture: String,
  createdAt: Date,
  updatedAt: Date,
  credits: { type: Number, default: 0 },
  referralCode: String
}, { collection: 'users' });

const User = mongoose.model('User', userSchema);

// Generate unique referral code
function generateReferralCode() {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let code = '';
  for (let i = 0; i < 8; i++) {
    code += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return code;
}

async function createUser() {
  try {
    const phoneNumber = '+918070366363';
    const password = '123456';
    const name = 'Shalini';
    
    console.log('🔍 Checking if user already exists...');
    const existingUser = await User.findOne({ phone: phoneNumber });
    
    if (existingUser) {
      console.log('⚠️ User already exists!');
      console.log('   Name:', existingUser.name);
      console.log('   Phone:', existingUser.phone);
      console.log('   ID:', existingUser._id);
      
      // Update password
      console.log('\n🔐 Updating password to: 123456');
      const salt = await bcrypt.genSalt(10);
      const hashedPassword = await bcrypt.hash(password, salt);
      
      existingUser.password = hashedPassword;
      existingUser.updatedAt = new Date();
      await existingUser.save();
      
      console.log('✅ Password updated!');
      console.log('');
      console.log('═══════════════════════════════════════');
      console.log('📱 Your Login Credentials:');
      console.log('   Name: ' + existingUser.name);
      console.log('   Phone: ' + phoneNumber);
      console.log('   Password: ' + password);
      console.log('═══════════════════════════════════════');
      
      process.exit(0);
      return;
    }
    
    console.log('📝 Creating new user account...');
    
    // Hash password
    const salt = await bcrypt.genSalt(10);
    const hashedPassword = await bcrypt.hash(password, salt);
    
    // Generate referral code
    const referralCode = generateReferralCode();
    
    // Create user
    const newUser = await User.create({
      name: name,
      phone: phoneNumber,
      password: hashedPassword,
      credits: 0,
      referralCode: referralCode,
      createdAt: new Date(),
      updatedAt: new Date()
    });
    
    console.log('✅ User created successfully!');
    console.log('');
    console.log('═══════════════════════════════════════');
    console.log('📱 Your New Account:');
    console.log('   Name: ' + newUser.name);
    console.log('   Phone: ' + phoneNumber);
    console.log('   Password: ' + password);
    console.log('   ID: ' + newUser._id);
    console.log('   Referral Code: ' + newUser.referralCode);
    console.log('═══════════════════════════════════════');
    console.log('');
    console.log('✅ You can now login with these credentials!');
    
    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}
