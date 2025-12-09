// Test script to check what Farhan sees when loading groups
require('dotenv').config();
const mongoose = require('mongoose');

const userSchema = new mongoose.Schema({
  name: String,
  phone: String
});

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

const User = mongoose.model('User', userSchema);
const Group = mongoose.model('Group', groupSchema);

// Farhan's user ID from your logs
const FARHAN_ID = '68d0cd97ac4bcbf7358cfe82';
const SHALINI_ID = '6933fdefc7d2bb6c0c0ae4b2';

async function testFarhanGroups() {
  try {
    console.log('🔌 Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    console.log('✅ Connected to MongoDB\n');

    // Find groups where Farhan is a member
    const groups = await Group.find({
      members: new mongoose.Types.ObjectId(FARHAN_ID)
    }).populate('members', 'name phone')
      .populate('admin', 'name phone')
      .populate('adminTransferInfo.previousAdmin', 'name phone');

    console.log(`📊 Found ${groups.length} groups for Farhan\n`);

    for (const group of groups) {
      const groupObj = group.toObject();
      
      console.log(`${'='.repeat(70)}`);
      console.log(`📁 Group: ${groupObj.name} (${groupObj._id})`);
      console.log(`\n👑 Current Admin:`);
      console.log(`   Name: ${groupObj.admin?.name || 'Unknown'}`);
      console.log(`   ID: ${groupObj.admin?._id}`);
      console.log(`   Is Farhan admin: ${groupObj.admin?._id?.toString() === FARHAN_ID ? '✅ YES' : '❌ NO'}`);

      if (groupObj.adminTransferInfo) {
        console.log(`\n🔄 Admin Transfer Info:`);
        console.log(`   Has info: ✅ YES`);
        
        if (groupObj.adminTransferInfo.previousAdmin) {
          console.log(`   Previous Admin: ${groupObj.adminTransferInfo.previousAdmin.name || 'Unknown'}`);
          console.log(`   Previous Admin ID: ${groupObj.adminTransferInfo.previousAdmin._id}`);
        } else {
          console.log(`   Previous Admin: ❌ NOT SET`);
        }
        
        console.log(`   Transferred At: ${groupObj.adminTransferInfo.transferredAt || 'N/A'}`);
        console.log(`   Seen: ${groupObj.adminTransferInfo.seen ? '✅ Yes' : '❌ No (SHOULD SHOW MESSAGE!)'}`);

        // Simulate the backend logic
        console.log(`\n🧪 Backend Logic Test:`);
        const isFarhanAdmin = groupObj.admin?._id?.toString() === FARHAN_ID;
        const hasTransferInfo = !!groupObj.adminTransferInfo;
        const hasPreviousAdmin = !!groupObj.adminTransferInfo?.previousAdmin;
        const notSeen = !groupObj.adminTransferInfo?.seen;
        const previousAdminId = groupObj.adminTransferInfo?.previousAdmin?._id?.toString();
        const isDifferentAdmin = previousAdminId && previousAdminId !== FARHAN_ID;

        console.log(`   1. Farhan is admin: ${isFarhanAdmin ? '✅' : '❌'}`);
        console.log(`   2. Has transfer info: ${hasTransferInfo ? '✅' : '❌'}`);
        console.log(`   3. Has previous admin: ${hasPreviousAdmin ? '✅' : '❌'}`);
        console.log(`   4. Not seen yet: ${notSeen ? '✅' : '❌'}`);
        console.log(`   5. Previous admin different: ${isDifferentAdmin ? '✅' : '❌'}`);

        const shouldShow = isFarhanAdmin && hasTransferInfo && hasPreviousAdmin && notSeen && isDifferentAdmin;
        
        if (shouldShow) {
          console.log(`\n   ✅✅✅ SHOULD SHOW: "${groupObj.adminTransferInfo.previousAdmin.name} made you admin"`);
        } else {
          console.log(`\n   ❌ WILL NOT SHOW MESSAGE`);
        }
      } else {
        console.log(`\n🔄 Admin Transfer Info: ❌ None`);
      }

      console.log(`\n`);
    }

    await mongoose.disconnect();
    console.log('✅ Disconnected from MongoDB');
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

testFarhanGroups();
