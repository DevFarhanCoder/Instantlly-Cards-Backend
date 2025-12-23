# Credit System Migration Guide

## 🎯 Problem: Existing Users Have Wrong Credit Amounts

### Issue
All existing users who installed the app before this update have various credit amounts (500,000, test values, etc.). The new system gives:
- ✅ **New users**: 200 credits on signup
- ✅ **Referrers**: +300 credits when someone uses their code
- ❌ **Existing users**: Need migration to 200 credits

---

## 🚀 **MIGRATION FOR ALL EXISTING USERS**

Run this **ONCE** after deploying the new credit system.

### Method 1: API Migration (Recommended ⭐)

**Advantages:**
- ✅ No downtime
- ✅ Can run while app is live
- ✅ See results immediately
- ✅ Easy rollback if needed

**Steps:**

1. **Deploy backend** with migration endpoint
   ```bash
   cd Instantlly-Cards-Backend
   git add .
   git commit -m "Add credit migration system"
   git push
   ```

2. **Run PowerShell migration script**
   ```powershell
   cd Instantlly-Cards-Backend\scripts
   
   # Edit the script first
   notepad migrate-all-users.ps1
   # Update: API_BASE and ADMIN_KEY
   
   # Run migration
   .\migrate-all-users.ps1
   ```

3. **Or use API directly (Postman/cURL)**
   ```bash
   POST https://api-test.instantllycards.com/api/credits/admin/migrate-all-users
   
   Headers:
   Content-Type: application/json
   x-admin-key: your-admin-secret-key
   
   Response:
   {
     "success": true,
     "message": "Successfully migrated 150 users",
     "stats": {
       "totalUsers": 200,
       "migratedUsers": 150,
       "targetCredits": 200,
       "expiryDate": "2025-01-24T..."
     },
     "sample": [...]
   }
   ```

---

### Method 2: Script Migration (Offline)

**Advantages:**
- ✅ Safer (dry run first)
- ✅ Detailed logs
- ✅ Better for large datasets

**Steps:**

```bash
cd Instantlly-Cards-Backend

# Dry run (preview changes)
npm run migrate:credits

# You'll see:
# 📊 Migration Summary:
#    - Total users: 200
#    - Users to migrate: 150
#    - Users already correct: 50

# Actual migration (with confirmation)
npm run migrate:credits -- --confirm
```

**Output:**
```
🚀 Starting credit migration...
✅ Connected to MongoDB

📋 Target credit amount: 200

👥 Found 200 total users

📊 Migration Summary:
   - Total users: 200
   - Users to migrate: 150 (75%)
   - Users already correct: 50 (25%)

📝 Sample users to migrate (first 5):
   - John Doe (+919876543210): 500000 → 200
   - Jane Smith (+918765432109): 1000 → 200
   - Bob Wilson (+917654321098): 50 → 200

🔄 Starting migration...
   ✅ Migrated 100/150 users...
   ⚠️  Notable: John Doe had 500000 credits (now 200)
   ✅ Migrated 150/150 users...

✅ Migration Complete!

📊 Results:
   - Successfully migrated: 150
   - Errors: 0
   - Total processed: 150
   - New credit amount: 200
   - New expiry date: 2025-01-24T07:29:42.221Z
```

---

### Method 3: Direct Database (Advanced)

**For database admins only:**

```javascript
// Connect to MongoDB
mongo "your-mongodb-connection-string"

// Check current state
db.users.aggregate([
  {
    $group: {
      _id: "$credits",
      count: { $sum: 1 }
    }
  },
  { $sort: { count: -1 } }
])

// Output:
// { "_id": 500000, "count": 100 }
// { "_id": 200, "count": 50 }
// { "_id": 0, "count": 30 }

// Migrate all users
db.users.updateMany(
  { credits: { $ne: 200 } },
  { 
    $set: { 
      credits: 200,
      creditsExpiryDate: new Date(Date.now() + 30*24*60*60*1000)
    } 
  }
)

// Verify
db.users.find({ credits: { $ne: 200 } }).count() // Should be 0
```

---

## 📊 What Happens to Existing Users?

### Before Update:
```
User A: 500,000 credits (old test value)
User B: 1,000 credits (custom value)
User C: 50 credits (low balance)
User D: 200 credits (already correct) ✅
```

### After Migration:
```
User A: 200 credits + 30 days validity
User B: 200 credits + 30 days validity
User C: 200 credits + 30 days validity
User D: 200 credits (unchanged)
```

---

## 🔐 Admin Endpoints

### 1. Migrate All Users
```bash
POST /api/credits/admin/migrate-all-users

Headers:
x-admin-key: your-secure-admin-key-here

Response:
{
  "success": true,
  "stats": {
    "totalUsers": 200,
    "migratedUsers": 150,
    "targetCredits": 200
  }
}
```

### 2. Reset Single User
```bash
POST /api/credits/admin/reset-user-credits

Headers:
x-admin-key: your-secure-admin-key-here

Body:
{
  "phone": "+919876543210",
  "resetToDefault": true
}
```

### 3. Update Credit Config
```bash
POST /api/credits/admin/update-config

Body:
{
  "signupBonus": 200,
  "referralReward": 300
}
```

---

## 🧪 Testing Plan

### Before Migration:
1. ✅ Check current user credits distribution
2. ✅ Backup database (if possible)
3. ✅ Test on staging environment first
4. ✅ Verify admin key works

### Run Migration:
```powershell
# Production
.\migrate-all-users.ps1
```

### After Migration:
1. ✅ Verify all users have 200 credits
2. ✅ Check expiry dates are set
3. ✅ Test new user signup (should get 200)
4. ✅ Test referral system (should get +300)

---

## 🎯 Timeline

1. **Now**: Deploy backend with migration code
2. **After deployment**: Run migration via API (takes ~1-2 minutes for 1000 users)
3. **Verify**: Check a few user accounts manually
4. **Release app update**: Users will now see correct 200 credits

---

## ⚠️ Important Notes

### Won't Break Existing Users:
- ✅ Users currently logged in will see updated credits on next refresh
- ✅ No re-login required
- ✅ Referral codes still work
- ✅ Credits history preserved

### Communication to Users:
Consider adding an in-app announcement:
```
"🎉 Credit System Update!

We've updated our credit system:
• All users start with 200 credits (valid 30 days)
• Refer friends → Get 300 bonus credits!
• Your credits have been refreshed

Enjoy sharing cards! 🎴"
```

---

## 📞 Troubleshooting

### Issue: "All users already have correct credits"
**Solution**: This means migration already ran successfully!

### Issue: "Unauthorized - Admin access required"
**Solution**: Check `ADMIN_SECRET_KEY` in `.env` matches the one in script

### Issue: Some users still show wrong credits
**Solution**: Run migration again - it's idempotent (safe to run multiple times)

### Issue: Users complaining about lost credits
**Expected**: This is intentional. Existing users had test/inflated values. 200 credits is the new standard for all users.

---

## 📁 Files Created

1. ✅ `src/scripts/migrate-credits.ts` - TypeScript migration script
2. ✅ `scripts/migrate-all-users.ps1` - PowerShell wrapper
3. ✅ `src/routes/credits.ts` - Added `/admin/migrate-all-users` endpoint
4. ✅ `package.json` - Added `migrate:credits` npm script

---

## 🚀 Quick Start

```bash
# 1. Deploy backend
git add . && git commit -m "Add credit migration" && git push

# 2. Wait for deployment (2-3 mins)

# 3. Run migration
cd scripts
.\migrate-all-users.ps1
# Type "MIGRATE" when prompted

# 4. Done! ✅
```

---

**Status**: ✅ Ready to deploy and migrate
**Estimated Time**: 5 minutes (including deployment)
**Risk Level**: Low (idempotent, can be re-run safely)
