const mongoose = require('mongoose');
require('dotenv').config();

const MONGODB_URI = process.env.MONGODB_URI || process.env.MONGO_URI;

console.log('🔌 Connecting to MongoDB...');

mongoose.connect(MONGODB_URI)
  .then(() => {
    console.log('✅ Connected to MongoDB');
    findUser();
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

async function findUser() {
  try {
    console.log('🔍 Searching for users with "shalini" in name...');
    const usersByName = await User.find({ name: { $regex: 'shalini', $options: 'i' } });
    
    if (usersByName.length > 0) {
      console.log(`\n✅ Found ${usersByName.length} user(s) with "shalini" in name:`);
      usersByName.forEach((u, i) => {
        console.log(`\n${i + 1}. Name: ${u.name}`);
        console.log(`   Phone: ${u.phone}`);
        console.log(`   Email: ${u.email || 'N/A'}`);
        console.log(`   ID: ${u._id}`);
        console.log(`   Has Password: ${u.password ? 'Yes' : 'No'}`);
      });
    } else {
      console.log('❌ No users found with "shalini" in name');
    }
    
    console.log('\n\n🔍 Searching for users with "8070366363" in phone...');
    const usersByPhone = await User.find({ phone: { $regex: '8070366363' } });
    
    if (usersByPhone.length > 0) {
      console.log(`\n✅ Found ${usersByPhone.length} user(s) with "8070366363" in phone:`);
      usersByPhone.forEach((u, i) => {
        console.log(`\n${i + 1}. Name: ${u.name}`);
        console.log(`   Phone: ${u.phone}`);
        console.log(`   Email: ${u.email || 'N/A'}`);
        console.log(`   ID: ${u._id}`);
        console.log(`   Has Password: ${u.password ? 'Yes' : 'No'}`);
      });
    } else {
      console.log('❌ No users found with "8070366363" in phone');
    }
    
    console.log('\n\n🔍 Listing all users (first 20)...');
    const allUsers = await User.find().limit(20);
    console.log(`\n📋 Total users found: ${allUsers.length}`);
    allUsers.forEach((u, i) => {
      console.log(`\n${i + 1}. Name: ${u.name || 'N/A'}`);
      console.log(`   Phone: ${u.phone || 'N/A'}`);
      console.log(`   ID: ${u._id}`);
    });
    
    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}
