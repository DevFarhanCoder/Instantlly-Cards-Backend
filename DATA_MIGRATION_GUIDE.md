# 🚀 Data Migration Guide: MongoDB Atlas → AWS DocumentDB

## ⚠️ IMPORTANT - Read Before Running

This migration will:
- ✅ Copy ALL data from MongoDB Atlas to AWS DocumentDB
- ✅ Preserve indexes and collection structures
- ✅ Keep MongoDB Atlas data intact (as backup)
- ⚠️  Clear existing DocumentDB collections before migration
- ⚠️  Require network access to DocumentDB (may need EC2 instance)

---

## 📋 Prerequisites Checklist

Before starting migration:

- [ ] ✅ Code is deployed to Render.com
- [ ] ✅ DocumentDB cluster is running in AWS
- [ ] ✅ Security group allows access on port 27017
- [ ] ✅ SSL certificate (global-bundle.pem) is present
- [ ] ✅ Environment variables are set (.env file)
- [ ] ✅ Backup of MongoDB Atlas data (optional but recommended)

---

## 🔧 Migration Options

### Option A: Run from Local Machine (May Fail Due to VPC)

This will likely **timeout** because DocumentDB is in a VPC:

```bash
cd "/Users/muskaan7862407/Desktop/Instantlly app/Instantlly-Cards-Backend"
node migrate-to-documentdb.js
```

**Expected Result**: 
```
❌ Connection timeout - DocumentDB not accessible
```

### Option B: Run from AWS EC2 Instance (RECOMMENDED)

You need an EC2 instance in the same VPC as DocumentDB:

#### Step 1: Launch EC2 Instance

1. Go to AWS Console → EC2
2. Launch instance:
   - **AMI**: Amazon Linux 2 or Ubuntu
   - **Instance Type**: t2.micro (free tier)
   - **VPC**: Same as DocumentDB (`instantlly-vpc`)
   - **Subnet**: Same as DocumentDB
   - **Security Group**: Allow SSH (port 22)

#### Step 2: Connect to EC2

```bash
ssh -i your-key.pem ec2-user@your-ec2-ip
```

#### Step 3: Install Node.js on EC2

```bash
# For Amazon Linux 2
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
source ~/.bashrc
nvm install 18
nvm use 18

# For Ubuntu
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs
```

#### Step 4: Transfer Files to EC2

From your local machine:

```bash
# Create a migration package
cd "/Users/muskaan7862407/Desktop/Instantlly app/Instantlly-Cards-Backend"

# Create temporary directory
mkdir migration-package
cp migrate-to-documentdb.js migration-package/
cp verify-migration.js migration-package/
cp global-bundle.pem migration-package/
cp package.json migration-package/
cp .env migration-package/

# Transfer to EC2
scp -i your-key.pem -r migration-package ec2-user@your-ec2-ip:~/
```

#### Step 5: Run Migration on EC2

```bash
# On EC2 instance
cd ~/migration-package
npm install mongoose dotenv
node migrate-to-documentdb.js
```

---

## 🎯 Alternative: Use MongoDB Tools

If EC2 setup is too complex, use MongoDB's native tools:

### Step 1: Export from MongoDB Atlas

```bash
# Export each collection
mongoexport --uri="mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/test" \
  --collection=users --out=users.json

mongoexport --uri="mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/test" \
  --collection=cards --out=cards.json

mongoexport --uri="mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/test" \
  --collection=contacts --out=contacts.json

mongoexport --uri="mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/test" \
  --collection=messages --out=messages.json

mongoexport --uri="mongodb+srv://farhan:farhan90@cluster0.txtst7k.mongodb.net/test" \
  --collection=groups --out=groups.json
```

### Step 2: Transfer to EC2

```bash
scp -i your-key.pem *.json ec2-user@your-ec2-ip:~/
```

### Step 3: Import to DocumentDB (from EC2)

```bash
# Install mongo tools on EC2
sudo yum install mongodb-org-tools

# Import each collection
mongoimport \
  --host=instantlly-docdb-cluster.cads82eikgcx.us-east-1.docdb.amazonaws.com:27017 \
  --ssl --tlsCAFile=global-bundle.pem \
  --username=rajeshmodi \
  --password=InstantllySecure2024! \
  --db=instantlly \
  --collection=users \
  --file=users.json

# Repeat for all collections (cards, contacts, messages, groups, etc.)
```

---

## ✅ Post-Migration Verification

### Step 1: Run Verification Script

```bash
node verify-migration.js
```

Expected output:
```
🔍 Data Migration Verification Tool
═══════════════════════════════════════════

✅ users:
   MongoDB Atlas: 150 documents
   DocumentDB: 150 documents

✅ cards:
   MongoDB Atlas: 300 documents
   DocumentDB: 300 documents

🎉 SUCCESS: All collections match perfectly!
```

### Step 2: Test Connection from Render

Once data is migrated, your Render.com app will automatically use DocumentDB.

Check health endpoint:
```bash
curl https://instantlly-cards-backend-6ki0.onrender.com/api/health
```

Should show:
```json
{
  "database": "AWS DocumentDB",
  "dbStatus": "connected",
  "dbPing": "ok"
}
```

---

## 🔄 Rollback Plan

If something goes wrong:

1. **App automatically falls back** to MongoDB Atlas
2. **Your data is safe** in MongoDB Atlas
3. **DocumentDB can be cleared** and retried

### To Force Use MongoDB Atlas:

Remove `DOCUMENTDB_URI` from Render.com environment variables temporarily.

---

## 📊 Migration Checklist

- [ ] Backup MongoDB Atlas data (optional)
- [ ] Launch EC2 instance in same VPC
- [ ] Install Node.js on EC2
- [ ] Transfer migration scripts to EC2
- [ ] Run migration script
- [ ] Verify data counts match
- [ ] Test application with DocumentDB
- [ ] Monitor for errors
- [ ] Keep MongoDB Atlas active for 1-2 weeks
- [ ] After stability, optionally remove MongoDB Atlas

---

## 🆘 Troubleshooting

### Migration Script Hangs

**Issue**: Script connects but hangs during migration

**Solution**: 
- Increase timeout values
- Run from EC2 in same VPC
- Check AWS Security Group rules

### Data Count Mismatch

**Issue**: DocumentDB has fewer documents

**Solution**:
- Re-run migration
- Check for errors in migration logs
- Verify unique indexes aren't causing conflicts

### Cannot Connect to DocumentDB

**Issue**: Connection timeout from local machine

**Solution**:
- This is normal! DocumentDB is in VPC
- Must run from EC2 instance in same VPC
- Or use MongoDB tools with EC2

---

## 📝 Summary

**Recommended Approach**:
1. Set up small EC2 instance in same VPC
2. Transfer migration script and .env to EC2  
3. Run migration from EC2
4. Verify data
5. App will automatically use DocumentDB
6. Keep MongoDB Atlas as backup

**Time Estimate**: 30-60 minutes (depending on data size)

**Risk Level**: Low (MongoDB Atlas stays as backup)

---

**Questions? Check the main guides:**
- `DOCUMENTDB_MIGRATION_GUIDE.md`
- `AWS_DOCUMENTDB_SETUP.md`
- `RENDER_DEPLOYMENT_STEPS.md`
