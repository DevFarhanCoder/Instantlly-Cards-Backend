// Check Admin Account Details
// Run this to see admin info and optionally reset password
// Usage: node check-admin.js

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

async function checkAdmin() {
  try {
    // Connect to MongoDB
    const MONGODB_URI = process.env.MONGODB_URI;
    
    if (!MONGODB_URI) {
      console.error('❌ MONGODB_URI not found in .env file!');
      process.exit(1);
    }

    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Get all admins
    const admins = await Admin.find({}).select('-password');
    
    if (admins.length === 0) {
      console.log('⚠️  No admin accounts found!');
      console.log('Run "node create-admin.js" to create the first admin.\n');
      await mongoose.connection.close();
      process.exit(0);
    }

    console.log('📋 Admin Accounts Found:\n');
    console.log('━'.repeat(70));
    
    admins.forEach((admin, index) => {
      console.log(`\n${index + 1}. Username: ${admin.username}`);
      console.log(`   Email: ${admin.email}`);
      console.log(`   Role: ${admin.role}`);
      console.log(`   Active: ${admin.isActive ? '✅ Yes' : '❌ No'}`);
      console.log(`   Last Login: ${admin.lastLogin ? new Date(admin.lastLogin).toLocaleString() : 'Never'}`);
      console.log(`   Created: ${new Date(admin.createdAt).toLocaleString()}`);
    });
    
    console.log('\n' + '━'.repeat(70));
    console.log('\n💡 To login to the admin panel, use one of these usernames.');
    console.log('💡 If you forgot the password, you can reset it by running:');
    console.log('   node reset-admin-password.js <username> <new-password>');
    console.log('\n🌐 Admin Panel URL: https://instantllychannelpatneradmin.vercel.app\n');

    await mongoose.connection.close();
    process.exit(0);

  } catch (error) {
    console.error('\n❌ Error:', error.message);
    await mongoose.connection.close();
    process.exit(1);
  }
}

// Run the script
checkAdmin();
