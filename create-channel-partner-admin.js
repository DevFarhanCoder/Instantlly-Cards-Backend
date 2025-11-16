const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');

const MONGODB_URI = "mongodb+srv://rajeshmodi:Newpass1234@cluster0.9yfi96i.mongodb.net/instantlly?retryWrites=true&w=majority&appName=Cluster0";

const AdminSchema = new mongoose.Schema({
  username: { type: String, required: true, unique: true },
  email: { type: String, required: true, unique: true },
  password: { type: String, required: true },
  role: { type: String, enum: ['super_admin', 'admin', 'moderator'], default: 'admin' },
  isActive: { type: Boolean, default: true },
  lastLogin: Date
}, { timestamps: true });

// Hash password before saving
AdminSchema.pre('save', async function(next) {
  if (!this.isModified('password')) return next();
  this.password = await bcrypt.hash(this.password, 10);
  next();
});

// Compare password method
AdminSchema.methods.comparePassword = async function(candidatePassword) {
  return await bcrypt.compare(candidatePassword, this.password);
};

const Admin = mongoose.model('Admin', AdminSchema);

async function createAdmin() {
  try {
    await mongoose.connect(MONGODB_URI);
    console.log('✅ Connected to MongoDB');
    
    // Check if admin already exists
    const existing = await Admin.findOne({ username: 'admin' });
    if (existing) {
      console.log('❌ Admin user already exists!');
      console.log('Username:', existing.username);
      console.log('Email:', existing.email);
      console.log('Role:', existing.role);
      console.log('Active:', existing.isActive);
      await mongoose.disconnect();
      return;
    }
    
    // Create admin user
    const admin = new Admin({
      username: 'admin',
      email: 'admin@instantlly.com',
      password: 'admin123', // Will be hashed by pre-save hook
      role: 'super_admin',
      isActive: true
    });
    
    await admin.save();
    console.log('\n✅ Channel Partner Admin created successfully!');
    console.log('Username: admin');
    console.log('Password: admin123');
    console.log('Role:', admin.role);
    
    await mongoose.disconnect();
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

createAdmin();
