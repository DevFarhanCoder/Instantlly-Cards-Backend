/**
 * Migrate all data from old MongoDB cluster to new MongoDB cluster
 * 
 * OLD: mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/
 * NEW: mongodb+srv://rajeshmodi:Farhan_90@cluster0.9yfi96i.mongodb.net/
 * 
 * This script will:
 * 1. Connect to both databases
 * 2. Copy all collections from old to new
 * 3. Verify data integrity
 * 4. Report statistics
 */

import mongoose from "mongoose";
import dotenv from "dotenv";

dotenv.config();

// Database URIs
const OLD_MONGODB_URI = "mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0";
const NEW_MONGODB_URI = process.env.MONGODB_URI || "mongodb+srv://rajeshmodi:Farhan_90@cluster0.9yfi96i.mongodb.net/?appName=Cluster0";

interface MigrationStats {
  collection: string;
  oldCount: number;
  newCount: number;
  migrated: number;
  status: 'success' | 'failed' | 'skipped';
  error?: string;
}

async function connectToDatabase(uri: string, name: string): Promise<mongoose.Connection> {
  const connection = mongoose.createConnection(uri);
  
  await new Promise<void>((resolve, reject) => {
    connection.once('open', () => {
      console.log(`✅ Connected to ${name} database`);
      resolve();
    });
    connection.once('error', (error) => {
      console.error(`❌ Failed to connect to ${name} database:`, error);
      reject(error);
    });
  });
  
  return connection;
}

async function migrateCollection(
  oldDb: mongoose.Connection,
  newDb: mongoose.Connection,
  collectionName: string
): Promise<MigrationStats> {
  const stats: MigrationStats = {
    collection: collectionName,
    oldCount: 0,
    newCount: 0,
    migrated: 0,
    status: 'success'
  };

  try {
    console.log(`\n📦 Migrating collection: ${collectionName}`);
    
    // Get collections
    const oldCollection = oldDb.collection(collectionName);
    const newCollection = newDb.collection(collectionName);
    
    // Count documents in old database
    stats.oldCount = await oldCollection.countDocuments();
    console.log(`   Old DB count: ${stats.oldCount}`);
    
    if (stats.oldCount === 0) {
      console.log(`   ⏭️  Skipping (no documents)`);
      stats.status = 'skipped';
      return stats;
    }
    
    // Check if new database already has data
    stats.newCount = await newCollection.countDocuments();
    if (stats.newCount > 0) {
      console.log(`   ⚠️  New DB already has ${stats.newCount} documents`);
      console.log(`   ❓ Skip migration? (Collection will not be modified)`);
      stats.status = 'skipped';
      return stats;
    }
    
    // Fetch all documents from old database
    const documents = await oldCollection.find({}).toArray();
    console.log(`   📥 Fetched ${documents.length} documents`);
    
    // Insert into new database
    if (documents.length > 0) {
      await newCollection.insertMany(documents, { ordered: false });
      stats.migrated = documents.length;
      console.log(`   ✅ Inserted ${stats.migrated} documents`);
    }
    
    // Verify count
    stats.newCount = await newCollection.countDocuments();
    if (stats.newCount === stats.oldCount) {
      console.log(`   ✅ Verification passed: ${stats.newCount}/${stats.oldCount}`);
      stats.status = 'success';
    } else {
      console.log(`   ⚠️  Count mismatch: ${stats.newCount}/${stats.oldCount}`);
      stats.status = 'failed';
    }
    
  } catch (error: any) {
    console.error(`   ❌ Error migrating ${collectionName}:`, error.message);
    stats.status = 'failed';
    stats.error = error.message;
  }
  
  return stats;
}

async function migrateDatabase() {
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('🔄 DATABASE MIGRATION TOOL');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('📍 Old DB: farhan@cluster0.txtst7k');
  console.log('📍 New DB: rajeshmodi@cluster0.9yfi96i');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  let oldDb: mongoose.Connection | null = null;
  let newDb: mongoose.Connection | null = null;
  
  try {
    // Connect to both databases
    console.log('🔌 Connecting to databases...\n');
    oldDb = await connectToDatabase(OLD_MONGODB_URI, 'OLD');
    newDb = await connectToDatabase(NEW_MONGODB_URI, 'NEW');
    
    // Get all collections from old database
    const collections = await oldDb.db!.listCollections().toArray();
    const collectionNames = collections.map(c => c.name);
    
    console.log(`\n📚 Found ${collectionNames.length} collections to migrate:`);
    collectionNames.forEach(name => console.log(`   - ${name}`));
    
    // Migrate each collection
    const allStats: MigrationStats[] = [];
    
    for (const collectionName of collectionNames) {
      const stats = await migrateCollection(oldDb, newDb, collectionName);
      allStats.push(stats);
    }
    
    // Print summary
    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📊 MIGRATION SUMMARY');
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    const successCount = allStats.filter(s => s.status === 'success').length;
    const failedCount = allStats.filter(s => s.status === 'failed').length;
    const skippedCount = allStats.filter(s => s.status === 'skipped').length;
    const totalMigrated = allStats.reduce((sum, s) => sum + s.migrated, 0);
    
    console.log(`\n✅ Successful: ${successCount}`);
    console.log(`❌ Failed: ${failedCount}`);
    console.log(`⏭️  Skipped: ${skippedCount}`);
    console.log(`📦 Total documents migrated: ${totalMigrated}\n`);
    
    // Detailed breakdown
    console.table(allStats.map(s => ({
      Collection: s.collection,
      'Old Count': s.oldCount,
      'New Count': s.newCount,
      Migrated: s.migrated,
      Status: s.status,
      Error: s.error || '-'
    })));
    
    console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    if (failedCount === 0) {
      console.log('✅ Migration completed successfully!');
      console.log('\n📝 Next Steps:');
      console.log('1. Update Render environment variable MONGODB_URI');
      console.log('2. Restart Render service');
      console.log('3. Test application with new database');
    } else {
      console.log('⚠️  Migration completed with errors. Review failed collections.');
    }
    
  } catch (error: any) {
    console.error('❌ Migration failed:', error.message);
    process.exit(1);
  } finally {
    // Close connections
    if (oldDb) await oldDb.close();
    if (newDb) await newDb.close();
    console.log('\n🔌 Database connections closed');
  }
}

// Run migration
migrateDatabase()
  .then(() => {
    console.log('\n✅ Migration script completed');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Migration script failed:', error);
    process.exit(1);
  });
