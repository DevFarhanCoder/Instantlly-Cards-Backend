const mongoose = require('mongoose');
require('dotenv').config();

const AdSchema = new mongoose.Schema({}, { strict: false });
const Ad = mongoose.model('Ad', AdSchema);

async function approveOldAds() {
  await mongoose.connect(process.env.MONGODB_URI);
  
  console.log('🔍 Finding ads without status field...');
  const adsWithoutStatus = await Ad.find({ 
    $or: [
      { status: { $exists: false } },
      { status: null },
      { status: '' }
    ]
  });
  
  console.log(`Found ${adsWithoutStatus.length} ads without status`);
  console.log('');
  
  if (adsWithoutStatus.length > 0) {
    console.log('📝 Updating all old ads to approved status...');
    
    const result = await Ad.updateMany(
      { 
        $or: [
          { status: { $exists: false } },
          { status: null },
          { status: '' }
        ]
      },
      { 
        $set: { 
          status: 'approved',
          uploadedBy: 'admin',
          uploaderName: 'Admin'
        } 
      }
    );
    
    console.log(`✅ Updated ${result.modifiedCount} ads to approved status`);
    console.log('');
    
    // Verify
    const now = new Date();
    const activeApprovedCount = await Ad.countDocuments({
      status: 'approved',
      startDate: { $lte: now },
      endDate: { $gte: now }
    });
    
    console.log(`✅ Now you have ${activeApprovedCount} active + approved ads in the mobile app!`);
  }
  
  await mongoose.disconnect();
}

approveOldAds().catch(console.error);
