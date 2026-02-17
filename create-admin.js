// Create First Admin Account
// Run this script ONCE to create the first admin user
// Usage: node create-admin.js

const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');
require('dotenv').config();

// Admin Schema (inline for this script)
const AdminSchema = new mongoose.Schema({
  username: { type: String, required: true, unique: true, trim: true },
  email: { type: String, required: true, unique: true, trim: true, lowercase: true },
  password: { type: String, required: true },
  role: { type: String, enum: ['super_admin', 'admin', 'moderator'], default: 'admin' },
  isActive: { type: Boolean, default: true },
  lastLogin: { type: Date },
  createdAt: { type: Date, default: Date.now },
  updatedAt: { type: Date, default: Date.now }
});

// Hash password before saving
AdminSchema.pre('save', async function (next) {
  if (!this.isModified('password')) return next();
  const salt = await bcrypt.genSalt(10);
  this.password = await bcrypt.hash(this.password, salt);
  next();
});

const Admin = mongoose.model('Admin', AdminSchema);

async function createFirstAdmin() {
  try {
    // Connect to MongoDB
    const MONGODB_URI = process.env.MONGODB_URI;
    
    if (!MONGODB_URI) {
      console.error('❌ MONGODB_URI not found in .env file!');
      process.exit(1);
    }

    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    // Check if admin already exists
    const existingAdmin = await Admin.findOne({});
    
    if (existingAdmin) {
      console.log('\n⚠️  Admin account already exists!');
      console.log('Username:', existingAdmin.username);
      console.log('Email:', existingAdmin.email);
      console.log('\nIf you forgot the password, you need to reset it in the database.');
      await mongoose.connection.close();
      process.exit(0);
    }

    // Create new admin
    console.log('\n📝 Creating first admin account...');
    
    const admin = new Admin({
      username: 'admin',
      email: 'admin@instantllycards.com',
      password: 'Admin@123',  // Change this password!
      role: 'super_admin',
      isActive: true
    });

    await admin.save();

    console.log('\n✅ First admin created successfully!');
    console.log('━'.repeat(50));
    console.log('👤 Username: admin');
    console.log('📧 Email: admin@instantllycards.com');
    console.log('🔑 Password: Admin@123');
    console.log('━'.repeat(50));
    console.log('\n⚠️  IMPORTANT: Change this password after first login!');
    console.log('\n🚀 You can now login at: https://instantllychannelpatneradmin.vercel.app');

    await mongoose.connection.close();
    process.exit(0);

  } catch (error) {
    console.error('\n❌ Error creating admin:', error.message);
    console.error(error);
    await mongoose.connection.close();
    process.exit(1);
  }
}

// Run the script
createFirstAdmin();
