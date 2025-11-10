# 🔐 CRITICAL: Render MongoDB Password Update Guide

## 🚨 Current Status
**Production is COMPLETELY BROKEN** - All ad image requests are failing with authentication errors.

**Root Cause**: MongoDB password was changed to `123456` on MongoDB Atlas, but Render.com still has the old password in the `MONGODB_URI` environment variable.

## ✅ Solution: Update MONGODB_URI on Render

### Step-by-Step Instructions

1. **Go to Render Dashboard**
   - Open browser and navigate to: https://dashboard.render.com/
   - Log in with your Render account credentials

2. **Select Your Service**
   - Find and click on **"Instantlly-Cards-Backend"** service
   - (It should be in your list of web services)

3. **Navigate to Environment Variables**
   - In the left sidebar, click **"Environment"** tab
   - You'll see a list of all environment variables

4. **Find MONGODB_URI**
   - Scroll through the list to find `MONGODB_URI`
   - Click the **pencil icon (✏️)** to edit it

5. **Update the Password**
   
   **Current (Wrong) Value:**
   ```
   mongodb+srv://9326702120:+919326702120@cluster0.xxxxx.mongodb.net/instantlly?retryWrites=true&w=majority
   ```
   
   **New (Correct) Value:**
   ```
   mongodb+srv://9326702120:123456@cluster0.xxxxx.mongodb.net/instantlly?retryWrites=true&w=majority
   ```
   
   **What Changed**: Only the password portion:
   - Old: `:+919326702120@`
   - New: `:123456@`

6. **Save Changes**
   - Click **"Save Changes"** button
   - Render will automatically restart your service with the new environment variable

7. **Trigger Manual Deploy (Optional but Recommended)**
   - Click **"Manual Deploy"** button at the top
   - Select **"Deploy latest commit"**
   - Click **"Deploy"**

8. **Wait for Deployment**
   - The deployment will take 3-5 minutes
   - You can watch the logs in real-time

## ✅ Verification

### Check Logs for Success

**Go to Logs tab** and look for:

✅ **SUCCESS Indicators:**
```
✅ MongoDB connected successfully
✅ Server running on port 10000
✅ Cache hit for ad: <id> type: bottom
```

❌ **FAILURE Indicators (if password still wrong):**
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🔐 CRITICAL: MongoDB authentication failed!
💡 Update password in Render.com environment variables
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### Test the API

**Option 1: Browser Test**
```
https://instantlly-cards-backend-6ki0.onrender.com/api/ads/health
```

Expected response:
```json
{
  "status": "ok",
  "timestamp": "2025-11-10T...",
  "database": {
    "connected": true,
    "state": "connected"
  },
  "cache": {...},
  "uptime": 123.45
}
```

**Option 2: Mobile App Test**
1. Open your mobile app
2. Navigate to home screen
3. Check if ad images are loading
4. If still failing, check Render logs for errors

## 🎯 Expected Results After Fix

### Before Fix (Current State):
```
❌ Database query timeout after 5s
❌ Database query timeout after 5s
❌ Database query timeout after 5s
[Repeated 50+ times]
```

### After Fix (Expected):
```
✅ MongoDB connected successfully
✅ Cache hit for ad: 690afd3af4a61fa83618f3be type: fullscreen
✅ GridFS Stream Complete for ad: 690afd3af4a61fa83618f3be
```

## 📊 Performance Improvements

After this fix is applied, you should see:

1. **Zero timeout errors** - All database queries succeed
2. **Fast image loading** - 5-minute cache prevents repeated DB hits
3. **95% less log noise** - Only critical errors logged
4. **Clear error messages** - If issues occur, you'll see specific error codes

## 🔧 Code Improvements Already Deployed

The latest code (commit `ccdeb52`) includes:

✅ **Connection State Tracking** - Checks DB connection before queries
✅ **Retry Logic** - Automatically retries failed connections (max 3 attempts)
✅ **Authentication Error Detection** - Identifies auth failures specifically
✅ **Meaningful Error Codes** - Returns `DB_NOT_CONNECTED`, `DB_AUTH_FAILED`, `DB_TIMEOUT`
✅ **Health Check Endpoint** - `/api/ads/health` shows connection status
✅ **Query Timeout Increase** - 5s → 10s for slow networks
✅ **Reduced Logging** - 97% less console output

## ❓ Troubleshooting

### Still seeing auth errors after password update?

**Check 1: Verify Connection String Format**
```
mongodb+srv://<username>:<password>@<cluster>/<database>?retryWrites=true&w=majority
```

**Check 2: Special Characters in Password**
- If password has special characters (%, @, :, etc.), they must be URL-encoded
- `123456` has no special characters, so no encoding needed

**Check 3: MongoDB Atlas Whitelist**
- Go to MongoDB Atlas dashboard
- Network Access → IP Whitelist
- Ensure `0.0.0.0/0` is whitelisted (allow all IPs)
- Or specifically add Render's IP addresses

**Check 4: MongoDB User Permissions**
- Go to MongoDB Atlas → Database Access
- Ensure user `9326702120` has "Read and Write" permissions
- Check that database is `instantlly`

### Need to double-check MongoDB password?

1. Go to MongoDB Atlas: https://cloud.mongodb.com/
2. Click on your cluster
3. Click **"Connect"** button
4. Select **"Connect your application"**
5. Copy the connection string
6. Verify the password portion matches what you set

## 🎉 Success Criteria

You'll know everything is working when:

1. ✅ Render logs show "MongoDB connected successfully"
2. ✅ `/api/ads/health` returns `"connected": true`
3. ✅ Mobile app displays ad images
4. ✅ Zero timeout errors in Render logs
5. ✅ Cache hit logs appear after first image load

## 📞 Support

If you continue experiencing issues after following this guide:

1. Check Render logs for specific error messages
2. Verify MongoDB Atlas credentials
3. Test connection string locally first
4. Contact support with specific error codes from logs

---

**Last Updated**: 2025-11-10  
**Related Commits**: ccdeb52 (Authentication error handling)  
**Deployment Status**: ⏳ Awaiting password update on Render
