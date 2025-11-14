const mongoose = require('mongoose');
require('dotenv').config();

const AdSchema = new mongoose.Schema({}, { strict: false });
const Ad = mongoose.model('Ad', AdSchema);

async function checkActiveAds() {
  await mongoose.connect(process.env.MONGODB_URI);
  
  const now = new Date();
  console.log('Current date:', now.toISOString());
  console.log('');
  
  // Check what the API would return
  const activeAds = await Ad.find({
    startDate: { $lte: now },
    endDate: { $gte: now },
    status: 'approved'
  }).select('title startDate endDate status priority').lean();
  
  console.log(`Active + Approved ads: ${activeAds.length}`);
  console.log('');
  
  if (activeAds.length > 0) {
    console.log('These ads should show in the app:');
    activeAds.slice(0, 10).forEach((ad, i) => {
      console.log(`${i+1}. ${ad.title}`);
      console.log(`   Start: ${new Date(ad.startDate).toLocaleDateString()}`);
      console.log(`   End: ${new Date(ad.endDate).toLocaleDateString()}`);
      console.log(`   Priority: ${ad.priority || 5}`);
      console.log('');
    });
  }
  
  // Check all approved ads regardless of date
  const allApproved = await Ad.find({ status: 'approved' }).lean();
  console.log(`Total approved ads (any date): ${allApproved.length}`);
  
  // Check ads that are expired
  const expiredAds = await Ad.find({
    endDate: { $lt: now },
    status: 'approved'
  }).lean();
  console.log(`Expired ads: ${expiredAds.length}`);
  
  // Check ads that haven't started yet
  const futureAds = await Ad.find({
    startDate: { $gt: now },
    status: 'approved'
  }).lean();
  console.log(`Future ads (not started yet): ${futureAds.length}`);
  
  await mongoose.disconnect();
}

checkActiveAds().catch(console.error);
