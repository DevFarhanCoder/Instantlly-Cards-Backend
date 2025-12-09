// Script to check admin transfer information in groups
require('dotenv').config();
const mongoose = require('mongoose');

const groupSchema = new mongoose.Schema({
  name: String,
  members: [{ type: mongoose.Schema.Types.ObjectId, ref: 'User' }],
  admin: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
  adminTransferInfo: {
    previousAdmin: { type: mongoose.Schema.Types.ObjectId, ref: 'User' },
    transferredAt: Date,
    seen: Boolean
  }
}, { timestamps: true });

const userSchema = new mongoose.Schema({
  name: String,
  phone: String
});

const Group = mongoose.model('Group', groupSchema);
const User = mongoose.model('User', userSchema);

async function checkAdminTransfers() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB');

    // Find all groups
    const groups = await Group.find({})
      .populate('admin', 'name phone')
      .populate('adminTransferInfo.previousAdmin', 'name phone')
      .populate('members', 'name phone');

    console.log(`\n📊 Total groups: ${groups.length}\n`);

    for (const group of groups) {
      console.log(`\n${'='.repeat(60)}`);
      console.log(`📁 Group: ${group.name}`);
      console.log(`   ID: ${group._id}`);
      console.log(`   Members: ${group.members.length}`);
      
      // Show admin info
      if (group.admin) {
        console.log(`\n👑 Current Admin:`);
        console.log(`   Name: ${group.admin.name || 'Unknown'}`);
        console.log(`   Phone: ${group.admin.phone || 'N/A'}`);
        console.log(`   ID: ${group.admin._id}`);
      }

      // Show transfer info
      if (group.adminTransferInfo) {
        console.log(`\n🔄 Admin Transfer Info:`);
        console.log(`   Has Transfer Info: ✅ YES`);
        
        if (group.adminTransferInfo.previousAdmin) {
          console.log(`   Previous Admin: ${group.adminTransferInfo.previousAdmin.name || 'Unknown'}`);
          console.log(`   Previous Admin Phone: ${group.adminTransferInfo.previousAdmin.phone || 'N/A'}`);
          console.log(`   Previous Admin ID: ${group.adminTransferInfo.previousAdmin._id}`);
        } else {
          console.log(`   Previous Admin: ❌ NOT POPULATED`);
        }
        
        console.log(`   Transferred At: ${group.adminTransferInfo.transferredAt || 'N/A'}`);
        console.log(`   Seen: ${group.adminTransferInfo.seen ? '✅ Yes' : '❌ No'}`);

        // Check if this is a valid transfer
        const currentAdminId = group.admin._id.toString();
        const previousAdminId = group.adminTransferInfo.previousAdmin?._id?.toString();
        
        if (previousAdminId) {
          if (currentAdminId === previousAdminId) {
            console.log(`   ⚠️  WARNING: Previous admin same as current admin (invalid)`);
          } else {
            console.log(`   ✅ VALID: Actual transfer happened`);
          }
        }
      } else {
        console.log(`\n🔄 Admin Transfer Info: ❌ None (group was created, not transferred)`);
      }

      // Show all members
      console.log(`\n👥 Members:`);
      group.members.forEach((member, idx) => {
        const isAdmin = member._id.toString() === group.admin._id.toString();
        console.log(`   ${idx + 1}. ${member.name || 'Unknown'} (${member.phone || 'N/A'})${isAdmin ? ' 👑 ADMIN' : ''}`);
      });
    }

    console.log(`\n${'='.repeat(60)}\n`);

    await mongoose.disconnect();
    console.log('✅ Disconnected from MongoDB');
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

checkAdminTransfers();
