#!/usr/bin/env node
/**
 * verify-referral-codes.js
 *
 * Simple utility to verify all users have valid, unique referral codes
 * and to report any duplicates or invalid-format codes.
 *
 * Usage: set MONGODB_URI and run `node verify-referral-codes.js`
 */
require('dotenv').config();
const mongoose = require('mongoose');
const User = require('./src/models/User').default || require('./src/models/User');

async function main() {
  const uri = process.env.MONGODB_URI;
  if (!uri) {
    console.error('MONGODB_URI is not set in environment.');
    process.exit(1);
  }

  await mongoose.connect(uri, { useNewUrlParser: true, useUnifiedTopology: true });
  console.log('Connected to MongoDB');

  const users = await User.find({}, 'name phone referralCode').lean();
  console.log(`Found ${users.length} users`);

  const codeCounts = new Map();
  const invalidFormat = [];

  const pattern = /^[A-Z0-9]{8}$/; // same as generateReferralCode

  for (const u of users) {
    const code = (u.referralCode || '').toString();
    if (!code) {
      invalidFormat.push({ id: u._id, name: u.name, phone: u.phone, reason: 'MISSING' });
      continue;
    }
    if (!pattern.test(code)) {
      invalidFormat.push({ id: u._id, name: u.name, phone: u.phone, referralCode: code, reason: 'BAD_FORMAT' });
    }
    codeCounts.set(code, (codeCounts.get(code) || 0) + 1);
  }

  const duplicates = [];
  for (const [code, count] of codeCounts.entries()) {
    if (count > 1) duplicates.push({ code, count });
  }

  console.log('\nSummary:');
  console.log(`Invalid / missing codes: ${invalidFormat.length}`);
  if (invalidFormat.length > 0) console.table(invalidFormat.slice(0, 50));
  console.log(`Duplicate codes found: ${duplicates.length}`);
  if (duplicates.length > 0) console.table(duplicates.slice(0, 50));

  await mongoose.disconnect();
  console.log('Done');
}

main().catch(err => {
  console.error('Error running verification script:', err);
  process.exit(1);
});
