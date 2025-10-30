// migrate-to-documentdb.js
// Complete Data Migration from MongoDB Atlas to AWS DocumentDB
require('dotenv').config();

const mongoose = require('mongoose');
const fs = require('fs');
const path = require('path');

// Color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

function log(message, color = colors.reset) {
  console.log(`${color}${message}${colors.reset}`);
}

async function migrateDatabaseToDocumentDB() {
  log('\n╔══════════════════════════════════════════════════════════╗', colors.cyan);
  log('║   MongoDB Atlas → AWS DocumentDB Migration Tool         ║', colors.cyan);
  log('╚══════════════════════════════════════════════════════════╝', colors.cyan);
  
  const startTime = Date.now();
  
  // Verify environment variables
  log('\n📋 Step 1: Verifying Environment Configuration...', colors.blue);
  
  const mongodbUri = process.env.MONGODB_URI;
  const documentdbUri = process.env.DOCUMENTDB_URI;
  
  if (!mongodbUri) {
    log('❌ ERROR: MONGODB_URI not found in environment variables', colors.red);
    process.exit(1);
  }
  
  if (!documentdbUri) {
    log('❌ ERROR: DOCUMENTDB_URI not found in environment variables', colors.red);
    process.exit(1);
  }
  
  log('✅ MongoDB Atlas URI: Found', colors.green);
  log('✅ DocumentDB URI: Found', colors.green);
  
  // SSL Certificate check
  const sslCertPath = path.join(__dirname, 'global-bundle.pem');
  if (!fs.existsSync(sslCertPath)) {
    log('❌ ERROR: SSL certificate (global-bundle.pem) not found', colors.red);
    process.exit(1);
  }
  log('✅ SSL Certificate: Found', colors.green);
  
  let sourceConnection, targetConnection;
  let migratedCollections = [];
  let failedCollections = [];
  
  try {
    // Step 2: Connect to MongoDB Atlas (Source)
    log('\n📡 Step 2: Connecting to MongoDB Atlas (Source)...', colors.blue);
    sourceConnection = await mongoose.createConnection(mongodbUri, {
      serverSelectionTimeoutMS: 10000,
      socketTimeoutMS: 15000,
    }).asPromise();
    log('✅ Connected to MongoDB Atlas', colors.green);
    
    // Step 3: Connect to DocumentDB (Target)
    log('\n📡 Step 3: Connecting to AWS DocumentDB (Target)...', colors.blue);
    targetConnection = await mongoose.createConnection(documentdbUri, {
      tls: true,
      tlsCAFile: sslCertPath,
      serverSelectionTimeoutMS: 30000,
      socketTimeoutMS: 45000,
      retryWrites: false,
    }).asPromise();
    log('✅ Connected to AWS DocumentDB', colors.green);
    
    // Step 4: Get all collections from source
    log('\n📚 Step 4: Discovering Collections in MongoDB Atlas...', colors.blue);
    const collections = await sourceConnection.db.listCollections().toArray();
    
    if (collections.length === 0) {
      log('⚠️  No collections found in MongoDB Atlas', colors.yellow);
      return;
    }
    
    log(`✅ Found ${collections.length} collections:`, colors.green);
    collections.forEach((col, index) => {
      log(`   ${index + 1}. ${col.name}`, colors.cyan);
    });
    
    // Step 5: Migrate each collection
    log('\n🔄 Step 5: Starting Data Migration...', colors.blue);
    log('═══════════════════════════════════════════════════', colors.cyan);
    
    for (const collectionInfo of collections) {
      const collectionName = collectionInfo.name;
      
      try {
        log(`\n📦 Migrating: ${collectionName}`, colors.bright);
        
        // Get source collection
        const sourceCollection = sourceConnection.db.collection(collectionName);
        const targetCollection = targetConnection.db.collection(collectionName);
        
        // Count documents
        const count = await sourceCollection.countDocuments();
        log(`   Documents to migrate: ${count}`, colors.cyan);
        
        if (count === 0) {
          log(`   ⚠️  Collection is empty, skipping...`, colors.yellow);
          continue;
        }
        
        // Fetch all documents
        log(`   📥 Fetching documents...`, colors.cyan);
        const documents = await sourceCollection.find({}).toArray();
        
        // Clear target collection (optional - comment out if you want to preserve existing data)
        const existingCount = await targetCollection.countDocuments();
        if (existingCount > 0) {
          log(`   ⚠️  Target collection has ${existingCount} existing documents`, colors.yellow);
          log(`   🗑️  Clearing target collection...`, colors.yellow);
          await targetCollection.deleteMany({});
        }
        
        // Insert documents in batches (1000 at a time)
        const batchSize = 1000;
        let inserted = 0;
        
        for (let i = 0; i < documents.length; i += batchSize) {
          const batch = documents.slice(i, i + batchSize);
          await targetCollection.insertMany(batch, { ordered: false });
          inserted += batch.length;
          
          const progress = Math.round((inserted / documents.length) * 100);
          log(`   📊 Progress: ${inserted}/${documents.length} (${progress}%)`, colors.cyan);
        }
        
        // Verify migration
        const targetCount = await targetCollection.countDocuments();
        
        if (targetCount === count) {
          log(`   ✅ SUCCESS: ${collectionName} (${targetCount} documents)`, colors.green);
          migratedCollections.push({ name: collectionName, count: targetCount });
        } else {
          log(`   ⚠️  WARNING: Count mismatch! Source: ${count}, Target: ${targetCount}`, colors.yellow);
          migratedCollections.push({ name: collectionName, count: targetCount, warning: true });
        }
        
        // Copy indexes
        log(`   🔍 Copying indexes...`, colors.cyan);
        const indexes = await sourceCollection.indexes();
        
        for (const index of indexes) {
          // Skip the default _id index
          if (index.name === '_id_') continue;
          
          try {
            const indexSpec = { ...index.key };
            const options = { name: index.name };
            if (index.unique) options.unique = true;
            if (index.sparse) options.sparse = true;
            
            await targetCollection.createIndex(indexSpec, options);
            log(`   ✅ Created index: ${index.name}`, colors.green);
          } catch (indexError) {
            log(`   ⚠️  Index already exists or failed: ${index.name}`, colors.yellow);
          }
        }
        
      } catch (collectionError) {
        log(`   ❌ FAILED: ${collectionName}`, colors.red);
        log(`   Error: ${collectionError.message}`, colors.red);
        failedCollections.push({ name: collectionName, error: collectionError.message });
      }
    }
    
    // Step 6: Summary
    log('\n╔══════════════════════════════════════════════════════════╗', colors.cyan);
    log('║                   Migration Summary                      ║', colors.cyan);
    log('╚══════════════════════════════════════════════════════════╝', colors.cyan);
    
    const duration = ((Date.now() - startTime) / 1000).toFixed(2);
    
    log(`\n⏱️  Total Duration: ${duration} seconds`, colors.cyan);
    log(`✅ Successfully Migrated: ${migratedCollections.length} collections`, colors.green);
    
    if (migratedCollections.length > 0) {
      log('\n📊 Migrated Collections:', colors.green);
      migratedCollections.forEach((col, index) => {
        const warning = col.warning ? ' ⚠️' : '';
        log(`   ${index + 1}. ${col.name}: ${col.count} documents${warning}`, colors.cyan);
      });
    }
    
    if (failedCollections.length > 0) {
      log(`\n❌ Failed Migrations: ${failedCollections.length} collections`, colors.red);
      failedCollections.forEach((col, index) => {
        log(`   ${index + 1}. ${col.name}: ${col.error}`, colors.red);
      });
    }
    
    log('\n═══════════════════════════════════════════════════', colors.cyan);
    log('🎉 Migration Complete!', colors.green);
    log('═══════════════════════════════════════════════════', colors.cyan);
    
    log('\n📝 Next Steps:', colors.blue);
    log('1. Verify data in AWS DocumentDB', colors.cyan);
    log('2. Test your application with DocumentDB', colors.cyan);
    log('3. Update Render.com to use DOCUMENTDB_URI', colors.cyan);
    log('4. Monitor for any issues', colors.cyan);
    log('5. Keep MongoDB Atlas as backup for now\n', colors.cyan);
    
  } catch (error) {
    log('\n❌ Migration Failed!', colors.red);
    log(`Error: ${error.message}`, colors.red);
    log(`Stack: ${error.stack}`, colors.red);
    process.exit(1);
  } finally {
    // Close connections
    if (sourceConnection) {
      await sourceConnection.close();
      log('🔒 Closed MongoDB Atlas connection', colors.cyan);
    }
    if (targetConnection) {
      await targetConnection.close();
      log('🔒 Closed DocumentDB connection', colors.cyan);
    }
  }
}

// Run migration
migrateDatabaseToDocumentDB()
  .then(() => {
    log('\n✅ Migration process completed successfully!\n', colors.green);
    process.exit(0);
  })
  .catch((error) => {
    log('\n❌ Migration process failed!\n', colors.red);
    console.error(error);
    process.exit(1);
  });
