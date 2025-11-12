const mongoose = require('mongoose');
require('dotenv').config();

const AdSchema = new mongoose.Schema({}, { strict: false });
const Ad = mongoose.model('Ad', AdSchema);

async function checkAndFixAds() {
  await mongoose.connect(process.env.MONGODB_URI);
  
  console.log('🔍 Checking all ads...');
  const allAds = await Ad.find({}).select('title status uploadedBy uploaderName startDate endDate').lean();
  
  console.log(`Total ads: ${allAds.length}`);
  console.log('');
  
  // Show first 10 ads status
  console.log('First 10 ads:');
  allAds.slice(0, 10).forEach((ad, i) => {
    console.log(`${i+1}. ${ad.title}`);
    console.log(`   status: "${ad.status}" (type: ${typeof ad.status})`);
    console.log(`   uploadedBy: "${ad.uploadedBy}"`);
    console.log('');
  });
  
  // Count ads by status value
  const statusCounts = {};
  allAds.forEach(ad => {
    const statusKey = ad.status === undefined ? 'undefined' : 
                      ad.status === null ? 'null' :
                      ad.status === '' ? 'empty-string' :
                      ad.status;
    statusCounts[statusKey] = (statusCounts[statusKey] || 0) + 1;
  });
  
  console.log('Status distribution:');
  console.log(statusCounts);
  console.log('');
  
  // Fix ads with undefined/null/empty status
  console.log('📝 Setting all ads without proper status to approved...');
  const result = await Ad.updateMany(
    {
      $or: [
        { status: { $exists: false } },
        { status: null },
        { status: '' },
        { status: 'no-status' }
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
  
  console.log(`✅ Modified ${result.modifiedCount} ads`);
  console.log('');
  
  // Check how many are now active and approved
  const now = new Date();
  const activeCount = await Ad.countDocuments({
    status: 'approved',
    startDate: { $lte: now },
    endDate: { $gte: now }
  });
  
  console.log(`✅ Active approved ads that will show in app: ${activeCount}`);
  
  await mongoose.disconnect();
}

checkAndFixAds().catch(console.error);
