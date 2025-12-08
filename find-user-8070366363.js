const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');
require('dotenv').config();

const MONGODB_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

console.log('🔌 Connecting to MongoDB...');

mongoose.connect(MONGODB_URI)
  .then(() => {
    console.log('✅ Connected to MongoDB');
    findAndUpdateUser();
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
  updatedAt: Date
}, { collection: 'users' });

const User = mongoose.model('User', userSchema);

async function findAndUpdateUser() {
  try {
    const phoneNumber = '8070366363';
    const newPassword = '123456';
    
    console.log('🔍 Searching for user with phone:', phoneNumber);
    
    // Try multiple variations
    const phoneVariations = [
      phoneNumber,
      '+91' + phoneNumber,
      '91' + phoneNumber,
      '+' + phoneNumber,
      '+91 ' + phoneNumber,
      '+91-' + phoneNumber
    ];
    
    console.log('📋 Trying phone variations:', phoneVariations);
    
    let user = null;
    for (const variation of phoneVariations) {
      user = await User.findOne({ phone: variation });
      if (user) {
        console.log('✅ Found user with phone variation:', variation);
        break;
      }
    }
    
    if (!user) {
      // Try regex search
      console.log('⚠️ Direct search failed, trying regex search...');
      user = await User.findOne({ phone: { $regex: '8070366363' } });
      
      if (!user) {
        console.log('❌ User not found with any variation');
        console.log('📋 Let me search all users...');
        
        const allUsers = await User.find().limit(30);
        console.log(`\nFound ${allUsers.length} users:`);
        allUsers.forEach((u, i) => {
          console.log(`${i + 1}. Name: ${u.name}, Phone: ${u.phone}, ID: ${u._id}`);
        });
        
        process.exit(1);
        return;
      }
    }
    
    console.log('\n👤 User found:');
    console.log('   Name:', user.name);
    console.log('   Phone:', user.phone);
    console.log('   ID:', user._id);
    console.log('   Has Password:', !!user.password);
    
    // Hash the new password
    console.log('\n🔐 Setting password to: 123456');
    const salt = await bcrypt.genSalt(10);
    const hashedPassword = await bcrypt.hash(newPassword, salt);
    
    // Update the password and ensure phone has +91 prefix
    const normalizedPhone = user.phone.replace(/[\s\-\(\)]/g, '');
    const finalPhone = normalizedPhone.startsWith('+') ? normalizedPhone : '+91' + phoneNumber;
    
    console.log('📱 Normalizing phone to:', finalPhone);
    
    user.password = hashedPassword;
    user.phone = finalPhone;
    user.updatedAt = new Date();
    await user.save();
    
    // Verify the password works
    console.log('✅ Password updated successfully!');
    console.log('🔐 Verifying password...');
    const isMatch = await bcrypt.compare(newPassword, user.password);
    
    if (isMatch) {
      console.log('✅ Password verification successful!');
      console.log('');
      console.log('═══════════════════════════════════════');
      console.log('📱 Your Login Credentials:');
      console.log('   Name: ' + user.name);
      console.log('   Phone: ' + finalPhone);
      console.log('   Password: ' + newPassword);
      console.log('═══════════════════════════════════════');
      console.log('');
      console.log('✅ You can now login with these credentials!');
    } else {
      console.log('❌ Password verification failed!');
    }
    
    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}
