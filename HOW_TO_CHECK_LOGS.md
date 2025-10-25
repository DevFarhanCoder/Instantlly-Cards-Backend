# 📋 HOW TO CHECK LOGS - Complete Guide

## 🎯 **What We're Looking For**
We need to see if your phone app is **actually sending** the push token to the backend when you login.

---

## ✅ **METHOD 1: Check Backend Logs (EASIEST)**

Your backend on Render already has console logs built-in. Here's how to see them:

### **Step 1: Open Render Dashboard**
1. Go to: https://render.com
2. Login with your account
3. Click on your backend service: **instantlly-cards-backend**

### **Step 2: View Logs**
1. Click the **"Logs"** tab at the top
2. You'll see a live stream of backend logs

### **Step 3: Test Token Registration**
1. **Keep the Render logs page open** in your browser
2. On your phone:
   - Open the Instantlly app
   - Logout
   - Force close the app (swipe from recents)
   - Reopen the app
   - Login again
3. **Watch the Render logs** - you should see:

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📱 [TOKEN-REGISTER] New push token registration request
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
User ID: 67028a9bc3f0d86b79d14adc
Platform: android
Push Token: ExponentPushToken[xxxxxx]...
Device Info: {...}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ [TOKEN-REGISTER] Push token registered successfully!
```

### **What the Logs Mean:**

#### ✅ **GOOD - If you see these logs:**
- `📱 [TOKEN-REGISTER] New push token registration request`
- `✅ [TOKEN-REGISTER] Push token registered successfully!`

**This means:** The app IS sending tokens, registration is working! 🎉

#### ❌ **BAD - If you DON'T see these logs:**
- No `[TOKEN-REGISTER]` messages at all

**This means:** The app is NOT sending tokens to backend. Could be:
1. **Permissions denied** on phone
2. **Using Expo Go** instead of standalone APK
3. **Old APK** installed (before notification fixes)

---

## 📱 **METHOD 2: Check Phone Permissions**

### **On Android:**
1. Go to: **Settings**
2. Tap: **Apps** (or "Applications")
3. Find: **Instantlly** (or InstantllyCards)
4. Tap: **Permissions**
5. Tap: **Notifications**
6. Make sure it says: **"Allowed"** ✅

### **If Notifications is "Denied":**
- Change it to **"Allowed"**
- Logout from app
- Force close app
- Reopen app
- Login again
- Check Render logs again

---

## 🔍 **METHOD 3: Database Check (Quick)**

Run this script to see if tokens are saved:

```bash
node test-push-token-registration.js
```

**What to look for:**
- ✅ `Push Token: ExponentPushToken[...]` = WORKING!
- ❌ `Push Token: NOT SET` = NOT WORKING!

---

## 🚨 **TROUBLESHOOTING CHECKLIST**

### **If tokens are NOT registering, check:**

1. **[ ] Using Standalone APK?**
   - ✅ Downloaded APK from https://expo.dev/artifacts/...
   - ❌ Scanned QR code and opened in "Expo Go" app
   - **Fix:** Download and install the APK directly

2. **[ ] Notification Permissions Allowed?**
   - ✅ Settings → Apps → Instantlly → Permissions → Notifications = Allowed
   - ❌ Notifications permission denied
   - **Fix:** Enable notification permissions

3. **[ ] Using NEW APK (after fixes)?**
   - ✅ APK downloaded after Oct 6, 2:57 PM
   - ❌ Old APK from before Oct 6, 2:57 PM
   - **Fix:** Download latest APK: https://expo.dev/artifacts/eas/gMhyiCtiNaBqKtxLKpb9sC.apk

4. **[ ] Saw Permission Popup?**
   - ✅ Saw "Allow Instantlly to send notifications?" popup when first opening app
   - ❌ Never saw any popup
   - **Fix:** Uninstall app, reinstall, allow permissions when prompted

5. **[ ] Backend is Awake?**
   - ✅ Backend responds to https://instantlly-cards-backend.onrender.com/api/health
   - ❌ Backend is sleeping (cold start)
   - **Fix:** Wait 60-90 seconds for backend to wake up

---

## 🎯 **NEXT STEPS**

### **Do this RIGHT NOW:**

1. **Open Render Dashboard** (https://render.com)
2. **Go to Logs tab** for your backend
3. **Keep logs visible** on your computer screen
4. **On your phone:**
   - Logout from Instantlly app
   - Force close the app
   - Reopen the app
   - Login again
5. **Watch the Render logs** - do you see `[TOKEN-REGISTER]` messages?

### **Then tell me:**
- ✅ **"YES, I see [TOKEN-REGISTER] logs"** → Token is being sent! We can debug why it's not saving
- ❌ **"NO, I don't see any [TOKEN-REGISTER] logs"** → App isn't sending tokens. Check permissions!

---

## 💡 **Quick Diagnosis**

| What You See | What It Means | Solution |
|-------------|---------------|----------|
| **Render logs show [TOKEN-REGISTER]** | Backend is receiving tokens ✅ | Check if tokens are saving in database |
| **Render logs show NO [TOKEN-REGISTER]** | App not sending tokens ❌ | Check app permissions, verify using APK not Expo Go |
| **Database shows tokens** | Everything working! 🎉 | Send test notification |
| **Database shows "NOT SET"** | Tokens not saving ❌ | Check backend error logs |

---

## 📞 **Need Help?**

**After checking Render logs, tell me:**

1. Did you see `[TOKEN-REGISTER]` logs? (YES/NO)
2. What do phone notification permissions show? (Allowed/Denied)
3. How did you install the app? (APK download / Expo Go QR code)
4. What does `node test-push-token-registration.js` show?

This will help me diagnose the exact issue! 🔍
