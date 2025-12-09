// Script to clean up invalid adminTransferInfo where previousAdmin is same as current admin
require('dotenv').config();
const mongoose = require('mongoose');

const groupSchema = new mongoose.Schema({
  name: String,
  admin: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
  adminTransferInfo: {
    previousAdmin: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
    transferredAt: Date,
    seen: Boolean
  }
}, { timestamps: true });

const Group = mongoose.model('Group', groupSchema);

async function cleanInvalidTransferInfo() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    // Find groups where adminTransferInfo exists
    const groupsWithTransferInfo = await Group.find({
      'adminTransferInfo.previousAdmin': { $exists: true }
    });

    console.log(`\n📊 Found ${groupsWithTransferInfo.length} groups with adminTransferInfo`);

    let cleaned = 0;
    let kept = 0;

    for (const group of groupsWithTransferInfo) {
      const adminId = group.admin.toString();
      const previousAdminId = group.adminTransferInfo.previousAdmin.toString();

      console.log(`\n🔍 Group: ${group.name}`);
      console.log(`   Admin ID: ${adminId}`);
      console.log(`   Previous Admin ID: ${previousAdminId}`);

      // If previous admin is same as current admin, it's invalid (group was created, not transferred)
      if (adminId === previousAdminId) {
        console.log(`   ❌ INVALID - previousAdmin same as current admin (removing)`);
        group.adminTransferInfo = undefined;
        await group.save();
        cleaned++;
      } else {
        console.log(`   ✅ VALID - actual transfer happened`);
        kept++;
      }
    }

    console.log(`\n📊 Summary:`);
    console.log(`   Cleaned: ${cleaned} groups`);
    console.log(`   Kept: ${kept} valid transfers`);

    await mongoose.disconnect();
    console.log('\n✅ Disconnected from MongoDB');
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

cleanInvalidTransferInfo();
