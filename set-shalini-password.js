const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');
require('dotenv').config();

const MONGODB_URI = process.env.MONGODB_URI || 'mongodb+srv://farhan:J4Wofe0vyDMxnOFe@cluster0.mongodb.net/instantlly-cards?retryWrites=true&w=majority';

const userSchema = new mongoose.Schema({
  name: String,
  phone: String,
  password: String,
  profilePicture: String,
  about: String,
  createdAt: Date,
  updatedAt: Date,
}, { collection: 'users' });

const User = mongoose.model('User', userSchema);

async function setPassword() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    const phoneNumber = '+918070366363';
    const newPassword = '123456';

    console.log(`\n🔍 Searching for user with phone: ${phoneNumber}`);
    
    const user = await User.findOne({ phone: phoneNumber });
    
    if (!user) {
      console.log('❌ User not found with that phone number');
      console.log('\n🔍 Searching for variations...');
      
      const variations = [
        '918070366363',
        '8070366363',
        '+91 8070366363',
        '+91-8070366363'
      ];
      
      for (const variation of variations) {
        const foundUser = await User.findOne({ phone: variation });
        if (foundUser) {
          console.log(`✅ Found user with phone: ${variation}`);
          console.log(`   Name: ${foundUser.name}`);
          console.log(`   ID: ${foundUser._id}`);
          
          const hashedPassword = await bcrypt.hash(newPassword, 10);
          foundUser.password = hashedPassword;
          await foundUser.save();
          
          console.log('\n✅ Password updated successfully!');
          console.log(`📱 Phone: ${foundUser.phone}`);
          console.log(`👤 Name: ${foundUser.name}`);
          console.log(`🔑 New Password: ${newPassword}`);
          
          await mongoose.connection.close();
          return;
        }
      }
      
      console.log('❌ User not found with any variation');
      await mongoose.connection.close();
      return;
    }

    console.log(`✅ Found user: ${user.name}`);
    console.log(`   Phone: ${user.phone}`);
    console.log(`   ID: ${user._id}`);

    const hashedPassword = await bcrypt.hash(newPassword, 10);
    user.password = hashedPassword;
    await user.save();

    console.log('\n✅ Password updated successfully!');
    console.log(`📱 Phone: ${user.phone}`);
    console.log(`👤 Name: ${user.name}`);
    console.log(`🔑 New Password: ${newPassword}`);

  } catch (error) {
    console.error('❌ Error:', error);
  } finally {
    await mongoose.connection.close();
    console.log('\n🔌 Disconnected from MongoDB');
  }
}

setPassword();
