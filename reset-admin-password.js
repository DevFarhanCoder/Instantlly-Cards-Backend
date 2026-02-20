// Reset Admin Password
// Run this to reset an admin's password
// Usage: node reset-admin-password.js <username> <new-password>
// Example: node reset-admin-password.js admin MyNewPassword123

const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');
require('dotenv').config();

// Admin Schema
const AdminSchema = new mongoose.Schema({
  username: { type: String, required: true, unique: true },
  email: { type: String, required: true, unique: true },
  password: { type: String, required: true },
  role: { type: String, enum: ['super_admin', 'admin', 'moderator'], default: 'admin' },
  isActive: { type: Boolean, default: true },
  lastLogin: { type: Date },
  createdAt: { type: Date, default: Date.now },
  updatedAt: { type: Date, default: Date.now }
});

const Admin = mongoose.model('Admin', AdminSchema);

async function resetPassword() {
  try {
    // Get command line arguments
    const username = process.argv[2];
    const newPassword = process.argv[3];

    if (!username || !newPassword) {
      console.log('\n❌ Usage: node reset-admin-password.js <username> <new-password>\n');
      console.log('Example: node reset-admin-password.js admin MyNewPassword123\n');
      process.exit(1);
    }

    if (newPassword.length < 6) {
      console.log('\n❌ Password must be at least 6 characters long!\n');
      process.exit(1);
    }

    // Connect to MongoDB
    const MONGODB_URI = process.env.MONGODB_URI;
    
    if (!MONGODB_URI) {
      console.error('❌ MONGODB_URI not found in .env file!');
      process.exit(1);
    }

    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Find admin
    const admin = await Admin.findOne({ username });
    
    if (!admin) {
      console.log(`❌ Admin with username "${username}" not found!\n`);
      console.log('💡 Run "node check-admin.js" to see all admin accounts.\n');
      await mongoose.connection.close();
      process.exit(1);
    }

    console.log(`📝 Found admin: ${admin.username} (${admin.email})`);
    console.log(`🔄 Resetting password...\n`);

    // Hash new password
    const salt = await bcrypt.genSalt(10);
    admin.password = await bcrypt.hash(newPassword, salt);
    admin.updatedAt = new Date();
    await admin.save();

    console.log('✅ Password reset successfully!\n');
    console.log('━'.repeat(70));
    console.log(`👤 Username: ${admin.username}`);
    console.log(`📧 Email: ${admin.email}`);
    console.log(`🔑 New Password: ${newPassword}`);
    console.log('━'.repeat(70));
    console.log('\n🚀 You can now login at: https://instantllychannelpatneradmin.vercel.app\n');

    await mongoose.connection.close();
    process.exit(0);

  } catch (error) {
    console.error('\n❌ Error:', error.message);
    await mongoose.connection.close();
    process.exit(1);
  }
}

// Run the script
resetPassword();
