// create-admin.js - Script to create the first admin user
const mongoose = require('mongoose');
const readline = require('readline');

// Load environment variables
require('dotenv').config();

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

function question(query) {
  return new Promise(resolve => rl.question(query, resolve));
}

async function createFirstAdmin() {
  try {
    console.log('\n🔐 InstantllyAds - Admin Setup\n');

    // Connect to MongoDB
    const mongoUri = process.env.MONGODB_URI;
    if (!mongoUri) {
      throw new Error('MONGODB_URI not found in .env file');
    }

    console.log('📡 Connecting to MongoDB...');
    await mongoose.connect(mongoUri);
    console.log('✅ Connected to MongoDB\n');

    // Import Admin model (using require since this is a .js file)
    const Admin = mongoose.model('Admin', new mongoose.Schema({
      username: { type: String, required: true, unique: true },
      email: { type: String, required: true, unique: true },
      password: { type: String, required: true },
      role: { type: String, enum: ['super_admin', 'admin'], default: 'super_admin' },
      isActive: { type: Boolean, default: true },
      lastLogin: Date
    }, { timestamps: true }));

    // Check if admin already exists
    const existingAdmins = await Admin.countDocuments();
    if (existingAdmins > 0) {
      console.log('❌ Admin user already exists!');
      console.log('📧 Contact your super admin to create additional admin accounts.\n');
      rl.close();
      await mongoose.disconnect();
      return;
    }

    // Get admin details from user
    console.log('No admin found. Let\'s create the first super admin!\n');
    
    const username = await question('Enter admin username: ');
    const email = await question('Enter admin email: ');
    const password = await question('Enter admin password (min 6 chars): ');

    if (!username || !email || !password) {
      throw new Error('All fields are required!');
    }

    if (password.length < 6) {
      throw new Error('Password must be at least 6 characters!');
    }

    // Hash password
    const bcrypt = require('bcryptjs');
    const salt = await bcrypt.genSalt(10);
    const hashedPassword = await bcrypt.hash(password, salt);

    // Create admin
    const admin = new Admin({
      username,
      email,
      password: hashedPassword,
      role: 'super_admin',
      isActive: true
    });

    await admin.save();

    console.log('\n✅ Super Admin created successfully!');
    console.log('\n📋 Admin Details:');
    console.log(`   Username: ${username}`);
    console.log(`   Email: ${email}`);
    console.log(`   Role: Super Admin`);
    console.log('\n🔑 You can now login to the InstantllyAds dashboard!\n');

    rl.close();
    await mongoose.disconnect();
  } catch (error) {
    console.error('\n❌ Error:', error.message);
    rl.close();
    await mongoose.disconnect();
    process.exit(1);
  }
}

createFirstAdmin();
