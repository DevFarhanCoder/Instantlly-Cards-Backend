const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');
require('dotenv').config();

const MONGODB_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

console.log('🔌 Connecting to MongoDB...');

mongoose.connect(MONGODB_URI)
  .then(() => {
    console.log('✅ Connected to MongoDB');
    updatePassword();
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

async function updatePassword() {
  try {
    const phoneNumber = '8073636369';
    const newPassword = '123456';
    
    console.log('🔍 Searching for user with phone:', phoneNumber);
    
    let user = await User.findOne({ phone: phoneNumber });
    
    if (!user) {
      console.log('❌ User not found');
      process.exit(1);
    }
    
    console.log('👤 User found:');
    console.log('   Name:', user.name);
    console.log('   Phone:', user.phone);
    console.log('   ID:', user._id);
    
    // Hash the new password
    console.log('🔐 Hashing new password...');
    const salt = await bcrypt.genSalt(10);
    const hashedPassword = await bcrypt.hash(newPassword, salt);
    
    // Update the password
    console.log('💾 Updating password in database...');
    user.password = hashedPassword;
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
      console.log('📱 Login Credentials for Shalini Shukla:');
      console.log('   Phone: ' + user.phone);
      console.log('   Password: ' + newPassword);
      console.log('═══════════════════════════════════════');
      console.log('');
      console.log('✅ You can now login with these credentials!');
    } else {
      console.log('❌ Password verification failed!');
    }
    
    process.exit(0);
  } catch (error) {
    console.error('❌ Error updating password:', error);
    process.exit(1);
  }
}
