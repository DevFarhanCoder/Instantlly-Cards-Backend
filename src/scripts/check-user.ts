import dotenv from 'dotenv';
import mongoose from 'mongoose';
import User from '../models/User';

dotenv.config();

async function main() {
  const uri = process.env.MONGODB_URI;
  if (!uri) {
    console.error('Please set MONGODB_URI in your environment or .env');
    process.exit(2);
  }

  const phoneArg = process.argv[2];
  if (!phoneArg) {
    console.error('Usage: npx ts-node src/scripts/check-user.ts <phone>');
    process.exit(2);
  }

  const normalized = phoneArg.replace(/[\s\-()]/g, '');
  const variants = [
    normalized,
    normalized.startsWith('+') ? normalized : ('+' + normalized),
    normalized.replace(/^\+/, ''),
    normalized.replace(/^\+91/, ''),
  ].filter(Boolean);

  console.log('Connecting to MongoDB...');
  await mongoose.connect(uri, { });

  console.log('Looking for user using variants:', variants);
  const user = (await User.findOne({ phone: { $in: variants } }).lean()) as any;
  if (!user) {
    console.log('User not found for any variant.');
    process.exit(0);
  }

  console.log('User found:');
  console.log({ _id: user._id, phone: user.phone, hasPassword: !!user.password });
  await mongoose.disconnect();
}

main().catch(err => {
  console.error('ERROR', err);
  process.exit(1);
});
