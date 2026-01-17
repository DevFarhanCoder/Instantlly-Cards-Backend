const mongoose = require('mongoose');
require('dotenv').config();

async function main() {
  await mongoose.connect(process.env.MONGODB_URI);
  console.log('Connected to MongoDB');
  
  const UserSchema = new mongoose.Schema({
    name: String,
    phone: String,
    credits: Number,
    password: String
  });
  
  const User = mongoose.model('User', UserSchema);
  
  // Simulate login logic - same as auth.ts
  const inputPhone = '+918070366363';
  let normalizedPhone = inputPhone.replace(/[\s\-\(\)]/g, '');
  if (!normalizedPhone.startsWith('+')) {
    normalizedPhone = '+' + normalizedPhone;
  }
  
  const phoneVariants = [
    normalizedPhone,
    normalizedPhone.replace(/^\+/, ''),
    normalizedPhone.replace(/^\+91/, ''),
    (normalizedPhone.startsWith('+') ? normalizedPhone : '+' + normalizedPhone)
  ].filter(Boolean);
  
  console.log('\n📱 Input phone:', inputPhone);
  console.log('📋 Phone variants:', phoneVariants);
  
  const user = await User.findOne({ phone: { $in: phoneVariants } }).lean();
  
  if (user) {
    console.log('\n✅ Found user:');
    console.log('  ID:', user._id);
    console.log('  Name:', user.name);
    console.log('  Phone:', user.phone);
    console.log('  Credits:', user.credits);
    console.log('  Has password:', !!user.password);
  } else {
    console.log('\n❌ No user found!');
  }
  
  // Now check what findOne returns for all variants
  console.log('\n\n🔍 Checking each variant individually:');
  for (const variant of phoneVariants) {
    const u = await User.findOne({ phone: variant }).lean();
    console.log(`  ${variant}: ${u ? `ID=${u._id}, Name=${u.name}` : 'NOT FOUND'}`);
  }
  
  await mongoose.disconnect();
}

main().catch(console.error);
