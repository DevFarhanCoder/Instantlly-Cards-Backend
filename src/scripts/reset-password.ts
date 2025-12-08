import dotenv from 'dotenv';
import mongoose from 'mongoose';
import bcrypt from 'bcryptjs';
import User from '../models/User';

dotenv.config();

async function main() {
  const uri = process.env.MONGODB_URI;
  if (!uri) {
    console.error('Please set MONGODB_URI in your environment or .env');
    process.exit(2);
  }

  const phoneArg = process.argv[2];
  const newPass = process.argv[3];
  if (!phoneArg || !newPass) {
    console.error('Usage: npx ts-node src/scripts/reset-password.ts <phone> <newPassword>');
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
  const user = await User.findOne({ phone: { $in: variants } });
  if (!user) {
    console.log('User not found for any variant.');
    process.exit(0);
  }

  const hashed = await bcrypt.hash(newPass, 12);
  (user as any).password = hashed;
  await user.save();
  console.log('Password reset successfully for user:', user._id.toString());
  await mongoose.disconnect();
}

main().catch(err => {
  console.error('ERROR', err);
  process.exit(1);
});
