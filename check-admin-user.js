const mongoose = require('mongoose');
require('dotenv').config();

const AdminSchema = new mongoose.Schema({
  username: String,
  email: String,
  password: String,
  role: String,
  isActive: Boolean,
  lastLogin: Date
}, { timestamps: true });

const Admin = mongoose.model('Admin', AdminSchema);

async function checkAdmin() {
  try {
    await mongoose.connect(process.env.MONGO_URI);
    console.log('Connected to MongoDB');
    
    const admins = await Admin.find({});
    console.log('\n📋 ALL ADMINS:');
    console.log(JSON.stringify(admins, null, 2));
    
    const adminUser = await Admin.findOne({ username: 'admin' });
    if (adminUser) {
      console.log('\n✅ Found admin user:', adminUser);
    } else {
      console.log('\n❌ No admin user found with username "admin"');
    }
    
    await mongoose.disconnect();
  } catch (error) {
    console.error('Error:', error);
    process.exit(1);
  }
}

checkAdmin();
