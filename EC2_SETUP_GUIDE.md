# 🖥️ EC2 Setup Guide for DocumentDB Migration

## Why EC2 is Needed

Your DocumentDB is in an AWS VPC (private network). To access it, you need a server **inside AWS**.

---

## 📋 Step-by-Step EC2 Setup

### Step 1: Launch EC2 Instance (5 minutes)

1. **Go to AWS Console**: https://console.aws.amazon.com/ec2/

2. **Click "Launch Instance"**

3. **Configure**:

   **Name**: `migration-server`
   
   **Amazon Machine Image (AMI)**:
   - Select: **Amazon Linux 2023** or **Amazon Linux 2**
   
   **Instance Type**:
   - Select: **t2.micro** (Free tier eligible)
   
   **Key pair**:
   - Create new key pair:
     - Name: `migration-key`
     - Type: RSA
     - Format: .pem
     - **Download and save the .pem file!**
   
   **Network Settings**:
   - VPC: **instantlly-vpc** (IMPORTANT - same as DocumentDB!)
   - Subnet: **instantlly-subnet-1** or **instantlly-subnet-2**
   - Auto-assign Public IP: **Enable**
   - Security Group: Create new
     - Rule 1: SSH, Port 22, Source: My IP
   
   **Storage**:
   - 8 GB (default is fine)
   
4. **Click "Launch Instance"**

5. **Wait 2-3 minutes** for instance to start

6. **Note the Public IP address** (e.g., `3.89.123.45`)

---

### Step 2: Connect to EC2 (2 minutes)

#### On Mac/Linux:

```bash
# Make key file secure
chmod 400 ~/Downloads/migration-key.pem

# Connect to EC2
ssh -i ~/Downloads/migration-key.pem ec2-user@YOUR_EC2_PUBLIC_IP
```

#### On Windows (using PuTTY):

1. Convert .pem to .ppk using PuTTYgen
2. Use PuTTY to connect:
   - Host: `ec2-user@YOUR_EC2_PUBLIC_IP`
   - Auth: Browse to .ppk file

---

### Step 3: Setup EC2 Environment (3 minutes)

Once connected to EC2, run these commands:

```bash
# Update system
sudo yum update -y

# Install Node.js
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
source ~/.bashrc
nvm install 18
nvm use 18

# Verify installation
node --version
npm --version
```

---

### Step 4: Transfer Files to EC2 (5 minutes)

**On your local machine** (new terminal, don't close EC2 connection):

```bash
# Navigate to your backend folder
cd "/Users/muskaan7862407/Desktop/Instantlly app/Instantlly-Cards-Backend"

# Transfer migration files
scp -i ~/Downloads/migration-key.pem \
  migrate-to-documentdb.js \
  verify-migration.js \
  global-bundle.pem \
  .env \
  package.json \
  ec2-user@YOUR_EC2_PUBLIC_IP:~/
```

Replace `YOUR_EC2_PUBLIC_IP` with your actual EC2 IP address.

**Expected output**:
```
migrate-to-documentdb.js    100%
verify-migration.js         100%
global-bundle.pem          100%
.env                       100%
package.json               100%
```

---

### Step 5: Run Migration on EC2 (10-30 minutes)

**Back on your EC2 terminal**:

```bash
# Install dependencies
npm install mongoose dotenv

# Verify files are present
ls -lh

# Run migration
node migrate-to-documentdb.js
```

**Expected Output**:
```
╔══════════════════════════════════════════════════════════╗
║   MongoDB Atlas → AWS DocumentDB Migration Tool         ║
╚══════════════════════════════════════════════════════════╝

📋 Step 1: Verifying Environment Configuration...
✅ MongoDB Atlas URI: Found
✅ DocumentDB URI: Found
✅ SSL Certificate: Found

📡 Step 2: Connecting to MongoDB Atlas (Source)...
✅ Connected to MongoDB Atlas

📡 Step 3: Connecting to AWS DocumentDB (Target)...
✅ Connected to AWS DocumentDB  ← THIS WILL WORK!

📚 Step 4: Discovering Collections...
✅ Found 5 collections:
   1. users
   2. cards
   3. contacts
   4. messages
   5. groups

🔄 Step 5: Starting Data Migration...
...
```

---

### Step 6: Verify Migration

```bash
node verify-migration.js
```

**Expected Output**:
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

---

## 🎯 Troubleshooting

### Issue: "Permission denied" when using SSH key

**Solution**:
```bash
chmod 400 ~/Downloads/migration-key.pem
```

### Issue: "Connection refused" when connecting to EC2

**Check**:
1. EC2 instance is running (green light in console)
2. Security group allows SSH from your IP
3. Using correct public IP address
4. Key file path is correct

### Issue: Migration connects but hangs

**Solution**:
- Check DocumentDB cluster is "Available" in AWS Console
- Verify EC2 and DocumentDB are in same VPC
- Check security group allows port 27017

### Issue: "Cannot find module 'mongoose'"

**Solution**:
```bash
npm install mongoose dotenv
```

---

## 🧹 Cleanup After Migration

Once migration is complete and verified:

```bash
# On EC2, clean up
exit

# On your local machine, terminate EC2
# Go to AWS Console → EC2 → Instances
# Select instance → Instance State → Terminate
```

**Cost**: t2.micro is free tier eligible (750 hours/month free)

---

## 📊 Quick Reference

**EC2 Commands** (from local machine):
```bash
# Connect to EC2
ssh -i migration-key.pem ec2-user@YOUR_IP

# Transfer files
scp -i migration-key.pem file.txt ec2-user@YOUR_IP:~/

# Transfer directory
scp -i migration-key.pem -r folder/ ec2-user@YOUR_IP:~/
```

**EC2 Instance Details Needed**:
- VPC: `instantlly-vpc`
- Subnet: `instantlly-subnet-1` or `instantlly-subnet-2`
- Security Group: Allow SSH (port 22)
- Instance Type: t2.micro
- AMI: Amazon Linux 2

---

## ⏱️ Time Estimate

- EC2 Launch: 5 minutes
- EC2 Setup: 5 minutes
- File Transfer: 2 minutes
- Migration Run: 10-30 minutes (depending on data size)
- Verification: 2 minutes
- **Total**: 25-45 minutes

---

## ✅ Success Indicators

After migration:
- [ ] All collections copied
- [ ] Document counts match
- [ ] Indexes created
- [ ] Verification passes
- [ ] Backend health check shows DocumentDB
- [ ] App works normally

---

**Need help with any step? Let me know!** 🚀
