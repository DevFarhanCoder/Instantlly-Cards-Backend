// Script to encode Firebase service account for environment variables
// Usage: node encode-firebase-credentials.js

const fs = require('fs');
const path = require('path');

const serviceAccountPath = path.join(__dirname, 'firebase-service-account.json');

if (!fs.existsSync(serviceAccountPath)) {
  console.error('❌ firebase-service-account.json not found');
  console.error('💡 Make sure the file exists in the project root');
  process.exit(1);
}

const serviceAccount = JSON.parse(fs.readFileSync(serviceAccountPath, 'utf-8'));

// Option 1: Base64 encoded (recommended for deployment)
const base64Encoded = Buffer.from(JSON.stringify(serviceAccount)).toString('base64');

console.log('\n✅ Firebase credentials encoded successfully!\n');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
console.log('📋 OPTION 1: Base64 Encoded (Recommended for Render/Vercel)');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
console.log('Add this to your .env file or deployment environment:\n');
console.log('FIREBASE_SERVICE_ACCOUNT_BASE64=' + base64Encoded);
console.log('\n');

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
console.log('📋 OPTION 2: Individual Environment Variables');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
console.log('Add these to your .env file or deployment environment:\n');
console.log('FIREBASE_PROJECT_ID=' + serviceAccount.project_id);
console.log('FIREBASE_CLIENT_EMAIL=' + serviceAccount.client_email);
console.log('FIREBASE_PRIVATE_KEY="' + serviceAccount.private_key.replace(/\n/g, '\\n') + '"');
console.log('\n');

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
console.log('⚠️  IMPORTANT SECURITY NOTES:');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
console.log('1. NEVER commit firebase-service-account.json to Git');
console.log('2. NEVER share these credentials publicly');
console.log('3. Add them only to your deployment platform (Render/Vercel)');
console.log('4. Keep firebase-service-account.json in .gitignore');
console.log('\n');
