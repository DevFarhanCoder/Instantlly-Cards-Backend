# DMS Migration Checklist
Quick reference for AWS DMS migration process

## Pre-Migration Checklist
- ✅ DocumentDB cluster running
- ✅ MongoDB Atlas accessible
- ✅ Backend deployed with DocumentDB support (v1.6)
- ✅ Migration scripts created (fallback option)
- ⏳ AWS account verified (CloudShell pending, not required for DMS)

## Migration Steps

### Phase 1: Setup DMS (15 minutes)
- [ ] **1.1** Go to AWS Console → Database Migration Service
- [ ] **1.2** Create Replication Instance
  - Name: `instantlly-migration`
  - Class: `dms.t3.micro`
  - VPC: `instantlly-vpc`
  - Public: Yes
  - Storage: 20 GB
- [ ] **1.3** Wait for instance to become "Available" (5-10 min)

### Phase 2: Configure Endpoints (5 minutes)
- [ ] **2.1** Create Source Endpoint
  - ID: `mongodb-atlas-source`
  - Engine: MongoDB
  - Server: `cluster0.txtst7k.mongodb.net`
  - Port: `27017`
  - Database: `test`
  - User: `farhan`
  - Pass: `farhan90`
  - SSL: Require
- [ ] **2.2** Create Target Endpoint
  - ID: `documentdb-target`
  - Engine: Amazon DocumentDB
  - Server: `instantlly-docdb-cluster.cads82eikgcx.us-east-1.docdb.amazonaws.com`
  - Port: `27017`
  - Database: `test`
  - User: `rajeshmodi`
  - Pass: `InstantllySecure2024!`
  - SSL: Require
- [ ] **2.3** Test both endpoint connections ✓

### Phase 3: Create Migration Task (5 minutes)
- [ ] **3.1** Create new database migration task
  - ID: `migrate-atlas-to-documentdb`
  - Source: `mongodb-atlas-source`
  - Target: `documentdb-target`
  - Type: Migrate existing data
  - Tables: Include all (%)
- [ ] **3.2** Review task configuration
- [ ] **3.3** Create task (don't start yet)

### Phase 4: Execute Migration (10-30 minutes)
- [ ] **4.1** Review all settings one last time
- [ ] **4.2** Start migration task
- [ ] **4.3** Monitor progress in DMS console
- [ ] **4.4** Wait for "Load complete" status
- [ ] **4.5** Check for any errors/warnings

### Phase 5: Verification (10 minutes)
- [ ] **5.1** Check task statistics
  - Tables loaded: ___
  - Total rows: ___
  - Errors: 0
- [ ] **5.2** Verify backend connection
  ```bash
  curl https://instantlly-cards-backend-6ki0.onrender.com/api/health
  ```
  - [ ] Should show "AWS DocumentDB" in response
- [ ] **5.3** Test API endpoints
  - [ ] User login works
  - [ ] Card operations work
  - [ ] Messages load correctly

### Phase 6: Cleanup (5 minutes)
- [ ] **6.1** Stop migration task
- [ ] **6.2** Delete migration task
- [ ] **6.3** Delete endpoints (both source and target)
- [ ] **6.4** Delete replication instance
- [ ] **6.5** Verify no DMS charges in next billing cycle

## Quick Commands

### Check Backend Status
```bash
curl https://instantlly-cards-backend-6ki0.onrender.com/api/health | jq
```

### Expected Response (Success)
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

## Troubleshooting

### DMS Task Stuck at 0%
1. Check CloudWatch Logs for the task
2. Verify source database has collections
3. Check endpoint connections are successful

### Endpoint Connection Fails
**MongoDB Atlas**:
- Add `0.0.0.0/0` to Network Access whitelist
- Ensure SSL mode is "require" not "verify-ca"

**DocumentDB**:
- Check security group allows DMS replication instance
- Verify VPC settings match

### Migration Completes But No Data
1. Check source database name is correct (`test`)
2. Verify table mapping includes all collections (%)
3. Review task logs in CloudWatch

## Important Notes

⚠️ **MongoDB Atlas Network Access**
You may need to whitelist AWS IP ranges in MongoDB Atlas:
1. Go to MongoDB Atlas → Network Access
2. Click "Add IP Address"
3. Select "Allow Access from Anywhere" (0.0.0.0/0) temporarily
4. Remove after migration completes

⚠️ **DocumentDB Security Group**
Ensure the DMS replication instance can access DocumentDB:
1. Go to EC2 → Security Groups → instantlly
2. Add inbound rule: Custom TCP, Port 27017, Source: DMS security group
3. Or Source: VPC CIDR (10.0.0.0/16)

⚠️ **Cost Management**
- DMS charges hourly for replication instance (~$0.036/hour)
- Delete all DMS resources immediately after migration
- Keep DocumentDB running for production use

## Success Indicators
✅ Task status: "Load complete"
✅ Tables loaded matches collection count
✅ Total rows > 0
✅ Errors: 0
✅ Backend health shows DocumentDB
✅ All APIs working

## Rollback If Needed
If anything goes wrong:
1. Backend automatically uses MongoDB Atlas (fallback)
2. No immediate action required
3. Can retry migration later
4. Delete failed DMS resources to stop charges

---

**Start Time**: ___________
**End Time**: ___________
**Total Duration**: ___________
**Data Migrated**: ___________ rows
**Status**: ⏳ Not Started

**Next Action**: Go to AWS Console → Database Migration Service
