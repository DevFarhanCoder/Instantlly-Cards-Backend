const mongoose = require('mongoose');
require('dotenv').config();

async function main() {
  await mongoose.connect(process.env.MONGODB_URI);
  console.log('Connected to MongoDB');
  
  const UserSchema = new mongoose.Schema({
    name: String,
    phone: String,
    credits: Number,
    email: String
  });
  
  const User = mongoose.model('User', UserSchema);
  
  const badId = '695b867521bc61ddc3764b23';
  
  console.log('\n🔍 Searching for ID:', badId);
  
  try {
    const user = await User.findById(badId).lean();
    console.log('Result:', user ? user : 'NOT FOUND');
  } catch (e) {
    console.log('Error:', e.message);
  }
  
  // Check if this ID exists in any collection
  const collections = await mongoose.connection.db.listCollections().toArray();
  console.log('\n📋 Checking all collections for this ID...');
  
  for (const coll of collections) {
    try {
      const doc = await mongoose.connection.db.collection(coll.name).findOne({ _id: new mongoose.Types.ObjectId(badId) });
      if (doc) {
        console.log(`  ✅ Found in ${coll.name}:`, doc);
      }
    } catch (e) {
      // Ignore
    }
  }
  
  console.log('\n✅ Search complete');
  
  await mongoose.disconnect();
}

main().catch(console.error);
