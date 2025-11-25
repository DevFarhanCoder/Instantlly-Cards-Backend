const mongoose = require('mongoose');
const User = require('./src/models/User').default;

async function updateUser() {
  try {
    await mongoose.connect(process.env.MONGODB_URI || 'mongodb://localhost:27017/instantlly');
    console.log('Connected to MongoDB');
    
    // Replace with the actual user ID or find by phone
    const user = await User.findOneAndUpdate(
      { phone: '+918070366363' }, // Replace with the phone number you're using
      { phone: '+918070366363' }, // Ensure phone is set
      { new: true }
    );
    
    if (user) {
      console.log('User updated:', user);
    } else {
      console.log('User not found');
    }
    
    await mongoose.disconnect();
  } catch (error) {
    console.error('Error:', error);
  }
}

updateUser();