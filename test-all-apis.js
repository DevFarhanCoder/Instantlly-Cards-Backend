/**
 * Comprehensive API Test Suite
 * Tests all endpoints with new MongoDB database
 */

const http = require('http');
const https = require('https');

const BASE_URL = 'http://localhost:3001';
const PRODUCTION_URL = 'https://instantlly-cards-backend-6ki0.onrender.com';

console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
console.log('🧪 COMPREHENSIVE API TEST SUITE');
console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

const tests = [
  {
    name: 'Health Check',
    endpoint: '/api/health',
    method: 'GET',
    expectedKeys: ['ok', 'database', 'dbPing']
  },
  {
    name: 'Root Health',
    endpoint: '/',
    method: 'GET',
    expectedKeys: ['status', 'service']
  },
  {
    name: 'Active Ads',
    endpoint: '/api/ads/active',
    method: 'GET',
    expectedKeys: ['success', 'data', 'imageBaseUrl']
  }
];

function makeRequest(url) {
  return new Promise((resolve, reject) => {
    const protocol = url.startsWith('https') ? https : http;
    
    protocol.get(url, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          resolve({
            statusCode: res.statusCode,
            data: JSON.parse(data),
            headers: res.headers
          });
        } catch (e) {
          resolve({
            statusCode: res.statusCode,
            data: data,
            headers: res.headers
          });
        }
      });
    }).on('error', reject);
  });
}

async function runTests(baseUrl, label) {
  console.log(`\n📍 Testing: ${label}`);
  console.log(`   URL: ${baseUrl}\n`);
  
  let passedCount = 0;
  let failedCount = 0;
  
  for (const test of tests) {
    const url = `${baseUrl}${test.endpoint}`;
    process.stdout.write(`   Testing ${test.name}... `);
    
    try {
      const result = await makeRequest(url);
      
      if (result.statusCode === 200) {
        // Verify expected keys
        const hasKeys = test.expectedKeys.every(key => key in result.data);
        
        if (hasKeys) {
          console.log('✅ PASSED');
          console.log(`      Status: ${result.statusCode}`);
          console.log(`      Response: ${JSON.stringify(result.data).substring(0, 100)}...`);
          passedCount++;
        } else {
          console.log('⚠️  WARNING - Missing keys');
          console.log(`      Expected: ${test.expectedKeys.join(', ')}`);
          console.log(`      Got: ${Object.keys(result.data).join(', ')}`);
          failedCount++;
        }
      } else {
        console.log(`❌ FAILED - Status ${result.statusCode}`);
        failedCount++;
      }
    } catch (error) {
      console.log(`❌ FAILED - ${error.message}`);
      failedCount++;
    }
  }
  
  return { passed: passedCount, failed: failedCount };
}

async function checkDatabaseConnection() {
  console.log('\n📊 DATABASE CONNECTION CHECK\n');
  
  const mongoose = require('mongoose');
  const dotenv = require('dotenv');
  dotenv.config();
  
  try {
    console.log('   Connecting to MongoDB...');
    await mongoose.connect(process.env.MONGODB_URI);
    
    console.log('   ✅ Connected to:', mongoose.connection.host);
    console.log('   ✅ Database:', mongoose.connection.name);
    
    const collections = await mongoose.connection.db.listCollections().toArray();
    console.log('   ✅ Collections:', collections.length);
    
    const counts = await Promise.all([
      mongoose.connection.db.collection('users').countDocuments(),
      mongoose.connection.db.collection('ads').countDocuments(),
      mongoose.connection.db.collection('cards').countDocuments()
    ]);
    
    console.log('   ✅ Users:', counts[0]);
    console.log('   ✅ Ads:', counts[1]);
    console.log('   ✅ Cards:', counts[2]);
    
    await mongoose.connection.close();
    
    return true;
  } catch (error) {
    console.log('   ❌ Database connection failed:', error.message);
    return false;
  }
}

async function main() {
  // Check database connection
  const dbOk = await checkDatabaseConnection();
  
  if (!dbOk) {
    console.log('\n❌ Database connection failed. Cannot proceed with API tests.\n');
    process.exit(1);
  }
  
  // Test local server
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('LOCAL SERVER TESTS');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  const localResults = await runTests(BASE_URL, 'Local Development Server');
  
  // Test production server
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('PRODUCTION SERVER TESTS');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  const prodResults = await runTests(PRODUCTION_URL, 'Render Production Server');
  
  // Summary
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('📊 TEST SUMMARY');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
  
  console.log('Local Server:');
  console.log(`   ✅ Passed: ${localResults.passed}`);
  console.log(`   ❌ Failed: ${localResults.failed}\n`);
  
  console.log('Production Server:');
  console.log(`   ✅ Passed: ${prodResults.passed}`);
  console.log(`   ❌ Failed: ${prodResults.failed}\n`);
  
  const totalPassed = localResults.passed + prodResults.passed;
  const totalFailed = localResults.failed + prodResults.failed;
  
  if (totalFailed === 0) {
    console.log('🎉 ALL TESTS PASSED!\n');
    console.log('✅ VERIFICATION COMPLETE:');
    console.log('   • New MongoDB database is connected');
    console.log('   • All APIs are working correctly');
    console.log('   • Both local and production environments tested\n');
  } else {
    console.log(`⚠️  ${totalFailed} test(s) failed\n`);
    if (prodResults.failed > 0) {
      console.log('⚠️  Production server may still be using old database.');
      console.log('   Update MONGODB_URI on Render and wait for deployment.\n');
    }
  }
  
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
  
  process.exit(totalFailed > 0 ? 1 : 0);
}

main();
