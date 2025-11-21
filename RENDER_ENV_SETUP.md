# 🔑 Render Environment Variable Setup

## ✅ Backend Changes Complete!

All code changes have been made to the backend. Now you just need to add **ONE environment variable** on Render.

---

## 📝 Environment Variable to Add

### Variable Name:
```
FAST2SMS_API_KEY
```

### Variable Value:
```
zhMCegUxl6nARYyTEMEtnlLYFgZhMWRzJTgnaFJA6sh2j79DUIDyy5RLFxos
```

---

## 🚀 How to Add on Render

### Step 1: Go to Your Render Dashboard
1. Login to https://render.com
2. Click on your **Instantlly-Cards-Backend** service

### Step 2: Add Environment Variable
1. Click on **"Environment"** in the left sidebar
2. Click **"Add Environment Variable"**
3. Enter:
   - **Key:** `FAST2SMS_API_KEY`
   - **Value:** `zhMCegUxl6nARYyTEMEtnlLYFgZhMWRzJTgnaFJA6sh2j79DUIDyy5RLFxos`
4. Click **"Save Changes"**

### Step 3: Deploy Backend
Render will automatically redeploy with the new environment variable.

---

## 📦 Backend Files Modified

✅ **New Files Created:**
- `src/services/otpService.ts` - OTP storage and verification service

✅ **Files Modified:**
- `src/routes/auth.ts` - Updated `/api/auth/check-phone` and added `/api/auth/verify-otp`

✅ **Dependencies:**
- `node-cache` - Already installed in package.json ✅

---

## 🔄 How It Works

### 1. User enters phone number in mobile app
```
Mobile App → POST /api/auth/check-phone
```

### 2. Backend checks if user exists
```typescript
// If user exists → return { exists: true }
// If new user → Generate OTP and send via Fast2SMS
```

### 3. Backend sends OTP via Fast2SMS
```typescript
// Generate 6-digit OTP
// Store in cache (expires in 5 min)
// Call Fast2SMS API with FAST2SMS_API_KEY
// Return { exists: false, otpSent: true }
```

### 4. User enters OTP in mobile app
```
Mobile App → POST /api/auth/verify-otp
```

### 5. Backend verifies OTP
```typescript
// Check OTP from cache
// If valid → return { success: true, verified: true }
// If invalid → return { success: false }
// Max 3 attempts allowed
```

### 6. User continues signup
```
Mobile App → POST /api/auth/signup
```

---

## 🧪 Testing After Deployment

### Test 1: Check if environment variable is set
```bash
# Backend should log this on startup:
[CHECK-PHONE] 🔑 FAST2SMS_API_KEY is set ✅
```

### Test 2: Try signup flow
1. Open mobile app
2. Enter phone number (10-digit Indian number)
3. Click "Send OTP"
4. Check backend logs on Render
5. You should see SMS on your phone
6. Enter OTP and verify

---

## 📊 Expected Backend Logs

### When OTP is sent:
```
[CHECK-PHONE] 📱 New signup - sending OTP to +919892254636
[OTP-STORE] ✅ Stored OTP for +919892254636 (expires in 5 min)
[CHECK-PHONE] 🔑 Generated OTP: 123456 for 9892254636
[CHECK-PHONE] 📤 Calling Fast2SMS API...
[CHECK-PHONE] ✅ Fast2SMS response: { return: true, ... }
```

### When OTP is verified:
```
[VERIFY-OTP] 🔐 Verification request for +919892254636
[OTP-VERIFY] ✅ OTP verified and deleted for +919892254636
[VERIFY-OTP] ✅ OTP verified successfully for +919892254636
```

---

## ⚠️ Important Notes

### OTP Security:
- OTPs expire after **5 minutes**
- Maximum **3 verification attempts** per OTP
- OTP is **deleted** after successful verification (one-time use)
- Stored in **memory** (node-cache), not database

### Development Mode:
- In development (`NODE_ENV=development`), backend returns OTP in response for testing
- In production, OTP is only sent via SMS

---

## 🐛 Troubleshooting

### Issue: "OTP service not configured"
**Solution:** Make sure `FAST2SMS_API_KEY` is added to Render environment variables

### Issue: "Invalid Indian phone number"
**Solution:** Phone number must be 10 digits (for +91 country code)

### Issue: SMS not received
**Check:**
1. Fast2SMS account has credits
2. Phone number is valid Indian number
3. Check Render logs for Fast2SMS API response

### Issue: "Invalid or expired OTP"
**Possible reasons:**
1. OTP expired (5 min limit)
2. Wrong OTP entered
3. Too many attempts (3 max)
4. Backend restarted (cache cleared)

---

## ✅ Summary

### What You Need to Do:
1. ✅ Add `FAST2SMS_API_KEY` environment variable on Render
2. ✅ Wait for auto-deploy to complete
3. ✅ Test signup flow in mobile app

### What's Already Done:
- ✅ Backend code fully updated
- ✅ Mobile app updated to use backend
- ✅ OTP service implemented
- ✅ All dependencies installed

---

**🎉 After adding the environment variable, your OTP system is ready!**
