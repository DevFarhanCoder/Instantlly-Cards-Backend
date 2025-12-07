const mongoose = require('mongoose');
require('dotenv').config();

const MONGODB_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

console.log('🔌 Connecting to MongoDB...');

mongoose.connect(MONGODB_URI)
  .then(() => {
    console.log('✅ Connected to MongoDB');
    updatePhoneFormat();
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

async function updatePhoneFormat() {
  try {
    console.log('🔍 Searching for Shalini...');
    
    const user = await User.findOne({ phone: '8073636369' });
    
    if (!user) {
      console.log('❌ User not found');
      process.exit(1);
    }
    
    console.log('👤 User found:');
    console.log('   Name:', user.name);
    console.log('   Current Phone:', user.phone);
    console.log('   ID:', user._id);
    
    // Update phone to include +91 prefix
    const newPhone = '+918073636369';
    console.log('📱 Updating phone to:', newPhone);
    
    user.phone = newPhone;
    user.updatedAt = new Date();
    await user.save();
    
    console.log('✅ Phone number updated successfully!');
    console.log('');
    console.log('═══════════════════════════════════════');
    console.log('📱 Updated Login Credentials:');
    console.log('   Phone: ' + newPhone);
    console.log('   Password: 123456');
    console.log('═══════════════════════════════════════');
    console.log('');
    console.log('✅ You can now login with +918073636369');
    
    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}
