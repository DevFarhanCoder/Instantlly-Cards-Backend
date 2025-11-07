// Create test JWT tokens to validate ObjectId checking
const jwt = require('jsonwebtoken');

console.log('🔑 Creating test JWT tokens for ObjectId validation...\n');

// Use a dummy secret for testing (in real production, this would be from environment)
const testSecret = 'test-secret-key';

// Test 1: Create token with INVALID ObjectId format (like user_1762428529619)
console.log('1. Creating token with INVALID ObjectId format:');
const invalidObjectIdToken = jwt.sign(
  { sub: 'user_1762428529619' }, 
  testSecret, 
  { expiresIn: '1h' }
);
console.log('   Token:', invalidObjectIdToken.substring(0, 50) + '...');
console.log('   UserID in token:', jwt.decode(invalidObjectIdToken).sub);
console.log('   This should be REJECTED by our validation ❌\n');

// Test 2: Create token with VALID ObjectId format
console.log('2. Creating token with VALID ObjectId format:');
const validObjectIdToken = jwt.sign(
  { sub: '671a352b217820ae084b505b' }, 
  testSecret, 
  { expiresIn: '1h' }
);
console.log('   Token:', validObjectIdToken.substring(0, 50) + '...');
console.log('   UserID in token:', jwt.decode(validObjectIdToken).sub);
console.log('   This format should be ACCEPTED by our validation ✅\n');

// Test 3: Verify ObjectId validation logic locally
console.log('3. Testing ObjectId validation logic locally:');
const mongoose = require('mongoose');

const testIds = [
  'user_1762428529619',          // Invalid format (the problem causing Cast errors)
  '671a352b217820ae084b505b',    // Valid 24-character hex ObjectId
  '12345',                       // Too short
  'not-a-valid-objectid',        // Invalid characters
  '671a352b217820ae084b505z',    // Invalid character (z)
];

testIds.forEach((id, index) => {
  const isValid = mongoose.Types.ObjectId.isValid(id);
  const status = isValid ? '✅ VALID' : '❌ INVALID';
  console.log(`   ${index + 1}. "${id}" → ${status}`);
});

console.log('\n🎯 Summary:');
console.log('Our authentication middleware now validates ObjectId format BEFORE any database queries.');
console.log('This prevents "Cast to ObjectId failed" errors that were crashing the app.');
console.log('Invalid tokens like "user_1762428529619" will be rejected with 401 status.');