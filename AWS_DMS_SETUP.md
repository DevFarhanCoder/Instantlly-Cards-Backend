# AWS DMS Migration Guide
*Complete guide to migrate from MongoDB Atlas to AWS DocumentDB*

## Overview
Using AWS Database Migration Service (DMS) to migrate data without SSH or EC2 access.

## Migration Details

### Source Database (MongoDB Atlas)
- **Cluster**: cluster0.txtst7k.mongodb.net
- **Port**: 27017
- **Database**: test
- **Username**: farhan
- **Password**: farhan90
- **SSL**: Required
- **Auth Mechanism**: SCRAM-SHA-1

### Target Database (AWS DocumentDB)
- **Cluster**: instantlly-docdb-cluster.cads82eikgcx.us-east-1.docdb.amazonaws.com
- **Port**: 27017
- **Database**: test
- **Username**: rajeshmodi
- **Password**: InstantllySecure2024!
- **SSL**: Required
- **Auth Mechanism**: SCRAM-SHA-1

## Step-by-Step Setup

### 1. Create Replication Instance
```
Name: instantlly-migration
Instance Class: dms.t3.micro (free tier)
VPC: instantlly-vpc
Publicly Accessible: Yes
Storage: 20 GB
Multi-AZ: No
```

**Estimated time**: 5-10 minutes to become available

### 2. Create Source Endpoint (MongoDB Atlas)
```
Endpoint ID: mongodb-atlas-source
Engine: MongoDB
Server: cluster0.txtst7k.mongodb.net
Port: 27017
SSL: Require
Database: test
Username: farhan
Password: farhan90
Auth Source: admin
Auth Mechanism: SCRAM-SHA-1
```

**Endpoint Settings** (JSON):
```json
{
  "nestingLevel": "none"
}
```

### 3. Create Target Endpoint (DocumentDB)
```
Endpoint ID: documentdb-target
Engine: Amazon DocumentDB
Server: instantlly-docdb-cluster.cads82eikgcx.us-east-1.docdb.amazonaws.com
Port: 27017
SSL: Require
Database: test
Username: rajeshmodi
Password: InstantllySecure2024!
Auth Source: admin
Auth Mechanism: SCRAM-SHA-1
```

**Endpoint Settings** (JSON):
```json
{
  "nestingLevel": "none"
}
```

### 4. Test Connections
1. Select each endpoint
2. Actions → Test connection
3. Choose replication instance: instantlly-migration
4. Run test → Wait for "successful" status

### 5. Create Migration Task
```
Task ID: migrate-atlas-to-documentdb
Replication Instance: instantlly-migration
Source: mongodb-atlas-source
Target: documentdb-target
Migration Type: Migrate existing data
Start on Create: No (start manually after review)
```

**Task Settings**:
```
Target Table Prep: Do nothing
LOB Mode: Limited LOB mode
Max LOB Size: 32 KB
```

**Table Mappings**:
```json
{
  "rules": [
    {
      "rule-type": "selection",
      "rule-id": "1",
      "rule-name": "1",
      "object-locator": {
        "schema-name": "%",
        "table-name": "%"
      },
      "rule-action": "include"
    }
  ]
}
```

### 6. Start Migration
1. Select the task
2. Actions → Start
3. Monitor progress in task details

## Monitoring Migration

### Check Task Status
- **Running**: Migration in progress
- **Load complete**: All data migrated
- **Stopped**: Task completed or stopped manually
- **Failed**: Check task logs for errors

### View Statistics
The task dashboard shows:
- Tables loaded
- Rows migrated
- Full load progress
- Errors/warnings

## Common Issues & Solutions

### Issue: Source Endpoint Test Fails
**Solution**: 
- Ensure MongoDB Atlas allows connections from AWS IP ranges
- Check MongoDB Atlas Network Access whitelist
- Add `0.0.0.0/0` temporarily for testing

### Issue: Target Endpoint Test Fails
**Solution**:
- Verify DocumentDB security group allows inbound from DMS replication instance
- Check VPC configuration
- Ensure replication instance is in same VPC as DocumentDB

### Issue: Migration Stuck at 0%
**Solution**:
- Check CloudWatch logs for DMS task
- Verify source database has data
- Check database permissions

### Issue: SSL Certificate Errors
**Solution**:
- DMS automatically handles SSL for DocumentDB
- For MongoDB Atlas, ensure SSL mode is "require" not "verify-ca"

## Post-Migration Steps

### 1. Verify Data Migration
Run this query in DocumentDB (via EC2 bastion or AWS Cloud9):
```javascript
// Connect to DocumentDB
mongosh "mongodb://rajeshmodi:InstantllySecure2024!@instantlly-docdb-cluster.cads82eikgcx.us-east-1.docdb.amazonaws.com:27017/?tls=true&tlsCAFile=global-bundle.pem&replicaSet=rs0&readPreference=secondaryPreferred&retryWrites=false"

// List all collections
show collections

// Count documents in each collection
db.users.countDocuments()
db.cards.countDocuments()
// ... repeat for all collections
```

### 2. Compare Counts with MongoDB Atlas
```javascript
// Connect to Atlas
mongosh "mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/"

// Count documents
db.users.countDocuments()
db.cards.countDocuments()
// Should match DocumentDB counts
```

### 3. Update Backend Environment Variables
In Render.com dashboard:
1. Go to your backend service
2. Environment → Edit
3. Ensure `DOCUMENTDB_URI` is set correctly
4. Save and redeploy

### 4. Test Backend Connection
```bash
curl https://instantlly-cards-backend-6ki0.onrender.com/api/health
```

Expected response:
```json
{
  "ok": true,
  "database": "AWS DocumentDB",
  "dbStatus": "connected",
  "dbPing": "ok",
  "version": "1.6",
  "hasDocumentDbUri": true,
  "dbHost": "instantlly-docdb-cluster.cads82eikgcx.us-east-1.docdb.amazonaws.com"
}
```

### 5. Stop DMS Resources (to save costs)
After successful migration:
1. Stop the migration task
2. Delete the migration task
3. Delete both endpoints
4. Delete the replication instance

**Cost savings**: ~$50/month by deleting DMS resources after migration

## Cost Breakdown

### DMS Costs (temporary, during migration only)
- Replication Instance (dms.t3.micro): ~$0.036/hour (~$26/month if kept running)
- **Recommendation**: Delete after migration completes

### Ongoing Costs (after migration)
- DocumentDB (db.t3.medium): ~$60/month
- MongoDB Atlas M10 (current): ~$57/month
- **Net change**: +$3/month (negligible)

## Timeline Estimate

| Step | Duration | Status |
|------|----------|--------|
| Create replication instance | 5-10 min | ⏳ Pending |
| Create endpoints | 2-3 min | ⏳ Pending |
| Test connections | 1-2 min | ⏳ Pending |
| Create migration task | 2-3 min | ⏳ Pending |
| Run migration | 10-30 min | ⏳ Pending |
| Verify data | 5-10 min | ⏳ Pending |
| Update backend | 2-3 min | ⏳ Pending |
| **Total** | **30-60 min** | - |

## Success Criteria
- ✅ All collections migrated
- ✅ Document counts match between Atlas and DocumentDB
- ✅ Backend health check shows "AWS DocumentDB"
- ✅ All API endpoints working correctly
- ✅ No data loss or corruption

## Rollback Plan
If migration fails or issues occur:
1. Backend automatically falls back to MongoDB Atlas
2. No code changes needed
3. Remove DOCUMENTDB_URI from Render.com to force Atlas usage
4. Delete DocumentDB cluster if not needed

## Support Resources
- AWS DMS Documentation: https://docs.aws.amazon.com/dms/
- DocumentDB Guide: https://docs.aws.amazon.com/documentdb/
- MongoDB Atlas Network Access: https://cloud.mongodb.com/

---

**Status**: Ready to begin migration
**Last Updated**: 30 October 2025
**Next Step**: Create DMS Replication Instance in AWS Console
