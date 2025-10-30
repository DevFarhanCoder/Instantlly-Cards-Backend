// verify-migration.js
// Verify data migration between MongoDB Atlas and DocumentDB
require('dotenv').config();

const mongoose = require('mongoose');
const fs = require('fs');
const path = require('path');

async function verifyMigration() {
  console.log('\n🔍 Data Migration Verification Tool');
  console.log('═══════════════════════════════════════════\n');
  
  const mongodbUri = process.env.MONGODB_URI;
  const documentdbUri = process.env.DOCUMENTDB_URI;
  const sslCertPath = path.join(__dirname, 'global-bundle.pem');
  
  if (!mongodbUri || !documentdbUri) {
    console.error('❌ Missing database URIs in environment variables');
    process.exit(1);
  }
  
  let sourceConnection, targetConnection;
  
  try {
    // Connect to both databases
    console.log('📡 Connecting to MongoDB Atlas...');
    sourceConnection = await mongoose.createConnection(mongodbUri, {
      serverSelectionTimeoutMS: 10000,
    }).asPromise();
    console.log('✅ Connected to MongoDB Atlas\n');
    
    console.log('📡 Connecting to AWS DocumentDB...');
    targetConnection = await mongoose.createConnection(documentdbUri, {
      tls: true,
      tlsCAFile: sslCertPath,
      serverSelectionTimeoutMS: 30000,
      retryWrites: false,
    }).asPromise();
    console.log('✅ Connected to AWS DocumentDB\n');
    
    // Get collections from both
    const sourceCollections = await sourceConnection.db.listCollections().toArray();
    const targetCollections = await targetConnection.db.listCollections().toArray();
    
    console.log('📊 Comparison Results:');
    console.log('═══════════════════════════════════════════\n');
    
    console.log(`MongoDB Atlas Collections: ${sourceCollections.length}`);
    console.log(`DocumentDB Collections: ${targetCollections.length}\n`);
    
    // Compare each collection
    const sourceNames = sourceCollections.map(c => c.name).sort();
    const targetNames = targetCollections.map(c => c.name).sort();
    
    let allMatch = true;
    
    for (const collName of sourceNames) {
      const sourceCol = sourceConnection.db.collection(collName);
      const targetCol = targetConnection.db.collection(collName);
      
      const sourceCount = await sourceCol.countDocuments();
      const targetCount = targetNames.includes(collName) 
        ? await targetCol.countDocuments() 
        : 0;
      
      const match = sourceCount === targetCount;
      const icon = match ? '✅' : '❌';
      
      console.log(`${icon} ${collName}:`);
      console.log(`   MongoDB Atlas: ${sourceCount} documents`);
      console.log(`   DocumentDB: ${targetCount} documents`);
      
      if (!match) {
        allMatch = false;
        console.log(`   ⚠️  MISMATCH: Difference of ${Math.abs(sourceCount - targetCount)} documents`);
      }
      console.log('');
    }
    
    // Check for collections only in DocumentDB
    const onlyInTarget = targetNames.filter(name => !sourceNames.includes(name));
    if (onlyInTarget.length > 0) {
      console.log('⚠️  Collections only in DocumentDB:');
      onlyInTarget.forEach(name => console.log(`   - ${name}`));
      console.log('');
    }
    
    console.log('═══════════════════════════════════════════');
    if (allMatch && onlyInTarget.length === 0) {
      console.log('🎉 SUCCESS: All collections match perfectly!');
    } else {
      console.log('⚠️  WARNING: Some differences found');
    }
    console.log('═══════════════════════════════════════════\n');
    
  } catch (error) {
    console.error('❌ Verification failed:', error.message);
    process.exit(1);
  } finally {
    if (sourceConnection) await sourceConnection.close();
    if (targetConnection) await targetConnection.close();
  }
}

verifyMigration()
  .then(() => process.exit(0))
  .catch(error => {
    console.error(error);
    process.exit(1);
  });
