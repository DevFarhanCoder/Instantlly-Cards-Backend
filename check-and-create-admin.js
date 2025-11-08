const mongoose = require('mongoose');
const bcrypt = require('bcryptjs');
require('dotenv').config();

const AdminSchema = new mongoose.Schema({
  username: String,
  email: String,
  password: String,
  role: String,
  isActive: Boolean,
  lastLogin: Date,
}, { timestamps: true });

const Admin = mongoose.model('Admin', AdminSchema);

async function checkAndCreateAdmin() {
  try {
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    const adminCount = await Admin.countDocuments();
    console.log(`📊 Found ${adminCount} admin(s) in database`);

    if (adminCount === 0) {
      console.log('\n🔧 No admin found. Creating default admin...');
      
      const salt = await bcrypt.genSalt(10);
      const hashedPassword = await bcrypt.hash('admin123', salt);
      
      const admin = await Admin.create({
        username: 'admin',
        email: 'admin@instantlly.com',
        password: hashedPassword,
        role: 'super_admin',
        isActive: true,
      });

      console.log('\n✅ Default admin created successfully!');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log('Username: admin');
      console.log('Password: admin123');
      console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
      console.log('\n⚠️  IMPORTANT: Change this password after first login!');
    } else {
      console.log('\n✅ Admin(s) already exist in database');
      const admins = await Admin.find({}, 'username email role isActive').lean();
      console.log('\n📋 Existing admins:');
      admins.forEach((admin, index) => {
        console.log(`${index + 1}. ${admin.username} (${admin.email}) - Role: ${admin.role} - Active: ${admin.isActive}`);
      });
    }

    await mongoose.disconnect();
    console.log('\n✅ Disconnected from MongoDB');
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

checkAndCreateAdmin();
