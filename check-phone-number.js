// check-phone-number.js
// Script to check what's in the database for a specific phone number

const mongoose = require('mongoose');
require('dotenv').config();

const UserSchema = new mongoose.Schema({
  name: String,
  phone: String,
  password: String,
  email: String,
  profilePicture: String,
  about: String,
  pushToken: String,
  platform: String,
  pushTokenUpdatedAt: Date,
}, { timestamps: true });

const User = mongoose.model('User', UserSchema);

async function checkPhoneNumber() {
  try {
    console.log('🔗 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    const phoneToCheck = '+916209011721';
    const normalizedPhone = phoneToCheck.replace(/[\s\-\(\)]/g, '');
    
    console.log(`🔍 Searching for phone number: ${phoneToCheck}`);
    console.log(`🔍 Normalized phone: ${normalizedPhone}`);
    
    // Check for exact match
    const exactMatch = await User.findOne({ phone: phoneToCheck });
    console.log('📱 Exact match:', exactMatch ? {
      id: exactMatch._id,
      name: exactMatch.name,
      phone: exactMatch.phone,
      email: exactMatch.email,
      createdAt: exactMatch.createdAt
    } : 'Not found');

    // Check for normalized match
    const normalizedMatch = await User.findOne({ phone: normalizedPhone });
    console.log('📱 Normalized match:', normalizedMatch ? {
      id: normalizedMatch._id,
      name: normalizedMatch.name,
      phone: normalizedMatch.phone,
      email: normalizedMatch.email,
      createdAt: normalizedMatch.createdAt
    } : 'Not found');

    // Check for any phone that contains these digits
    const partialMatches = await User.find({ 
      phone: { $regex: '6209011721', $options: 'i' } 
    });
    console.log(`📱 Partial matches (${partialMatches.length}):`, partialMatches.map(user => ({
      id: user._id,
      name: user.name,
      phone: user.phone,
      email: user.email,
      createdAt: user.createdAt
    })));

    // List all users to see what's in the database
    const allUsers = await User.find({}).limit(10);
    console.log(`📊 All users in database (showing first 10 of ${await User.countDocuments()}):`);
    allUsers.forEach(user => {
      console.log(`  - ${user.name} | ${user.phone} | ${user.email || 'no email'} | ${user.createdAt}`);
    });

    process.exit(0);

  } catch (error) {
    console.error('❌ Error during check:', error);
    process.exit(1);
  }
}

checkPhoneNumber();