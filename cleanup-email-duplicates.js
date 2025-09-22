// Clean up users with duplicate null email
require('dotenv').config();
const mongoose = require('mongoose');

async function cleanupDatabase() {
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

    // Find all users with null email
    const usersWithNullEmail = await User.find({ email: null });
    console.log(`Found ${usersWithNullEmail.length} users with null email`);

    if (usersWithNullEmail.length > 0) {
      // Keep only the first user, delete the rest
      for (let i = 1; i < usersWithNullEmail.length; i++) {
        const user = usersWithNullEmail[i];
        console.log(`Deleting duplicate user: ${user.name} (${user.phone})`);
        await User.deleteOne({ _id: user._id });
      }
      
      // Update the remaining user to remove the email field entirely
      if (usersWithNullEmail.length > 0) {
        const keepUser = usersWithNullEmail[0];
        console.log(`Updating ${keepUser.name} to remove null email`);
        await User.updateOne({ _id: keepUser._id }, { $unset: { email: 1 } });
      }
    }

    console.log("✅ Database cleanup completed");
    await mongoose.disconnect();
    console.log("Disconnected from MongoDB");
  } catch (error) {
    console.error("Error:", error);
  }
}

cleanupDatabase();