// Delete user by phone script - USE WITH CAUTION
require('dotenv').config();
const mongoose = require('mongoose');

async function deleteUserByPhone() {
  try {
    if (!process.env.MONGODB_URI) {
      console.error("MONGODB_URI not found in environment variables");
      return;
    }

    await mongoose.connect(process.env.MONGODB_URI);
    console.log("Connected to MongoDB");

    const UserSchema = new mongoose.Schema({
      name: String,
      phone: String,
      password: String,
      email: String,
    }, { timestamps: true });

    const User = mongoose.model('User', UserSchema);

    const phoneToDelete = "+917378356287";
    const normalizedPhone = phoneToDelete.replace(/[\s\-\(\)]/g, '');
    
    console.log(`Looking for user with phone: ${normalizedPhone}`);
    console.log(`Also checking original: ${phoneToDelete}`);
    
    // Check both normalized and original
    const user1 = await User.findOne({ phone: normalizedPhone });
    const user2 = await User.findOne({ phone: phoneToDelete });
    
    if (user1) {
      console.log(`Found user with normalized phone: ${user1.name} (${user1.phone})`);
      await User.deleteOne({ phone: normalizedPhone });
      console.log(`✅ Deleted user with phone: ${normalizedPhone}`);
    } else if (user2) {
      console.log(`Found user with original phone: ${user2.name} (${user2.phone})`);
      await User.deleteOne({ phone: phoneToDelete });
      console.log(`✅ Deleted user with phone: ${phoneToDelete}`);
    } else {
      console.log(`❌ No user found with either phone format`);
      
      // Let's see what users exist
      const allUsers = await User.find({}).limit(5);
      console.log(`Total users in database: ${await User.countDocuments()}`);
      console.log("Recent users:", allUsers.map(u => ({ name: u.name, phone: u.phone })));
    }

    await mongoose.disconnect();
    console.log("Disconnected from MongoDB");
  } catch (error) {
    console.error("Error:", error);
  }
}

deleteUserByPhone();