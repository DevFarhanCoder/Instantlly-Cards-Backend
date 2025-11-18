# How to Check Production Logs

## The Issue
Users updating from Play Store (v1.0.31) cannot login. The backend receives requests but `phone` field is `undefined`.

## Backend Status
✅ Enhanced logging is deployed and working  
✅ Debug information is being returned in error responses  
✅ Body parser handles multiple content types  

## What to Look For in Logs

### Step 1: Access Render Logs
1. Go to https://render.com
2. Select your service: "instantlly-cards-backend"
3. Click "Logs" tab
4. Enable "Auto-scroll" to see real-time logs

### Step 2: Look for Login Attempts
When a user tries to login, you should see:

```
📥 POST /api/auth/login - Content-Type: application/json
📦 Body keys: <WHAT KEYS ARE HERE?>
Login attempt - Headers: { contentType: '...', contentLength: '...', userAgent: 'okhttp/4.12.0' }
Login attempt - Raw body: { <WHAT'S IN THE BODY?> }
```

### Step 3: Identify the Problem

#### Scenario A: Empty Body
```
📦 Body keys: no body
❌ Empty request body received!
```
**Meaning**: Android app is NOT sending request body at all  
**Fix**: Check Android app's HTTP client configuration

#### Scenario B: Wrong Field Names
```
📦 Body keys: phoneNumber, pwd
Body keys: phoneNumber,pwd
```
**Meaning**: App is using different field names  
**Fix**: Update backend to accept alternative field names OR update app

#### Scenario C: Fields Exist But Values Are Undefined
```
📦 Body keys: phone, password
Login attempt - Raw body: { phone: undefined, password: '...' }
```
**Meaning**: App is sending fields but values are `undefined`  
**Fix**: Check app's state management (phone value not being captured)

#### Scenario D: Body Completely Missing
```
📦 Body keys: no body
Login attempt - Raw body: undefined
```
**Meaning**: Request body is stripped somewhere in the network  
**Fix**: Check if using a proxy/VPN/CDN that strips POST bodies

## Immediate Actions Based on Logs

### If Body is Empty
The Android app needs to be fixed. Check:
1. `InstantllyCards/lib/api.ts` - Is body being stringified?
2. `InstantllyCards/app/(auth)/sign-in.tsx` - Are phone/password values set?

### If Wrong Field Names
Quick backend fix (add to `src/routes/auth.ts`):
```typescript
// Support alternative field names from older app versions
const phone = req.body?.phone || req.body?.phoneNumber || req.body?.mobile;
const password = req.body?.password || req.body?.pwd || req.body?.pass;
```

### If Phone Value is Undefined in App
This is a React Native state issue. The user is typing but the value isn't being captured.

Check in `sign-in.tsx`:
```typescript
const [phone, setPhone] = useState("");
// Make sure the TextInput is calling setPhone:
<TextInput value={phone} onChangeText={setPhone} ... />
```

## Quick Fixes You Can Deploy Now

### Option 1: Add Backward Compatibility (Safest)
Support multiple field name variations:

```typescript
// In src/routes/auth.ts login endpoint
const phone = req.body?.phone || req.body?.phoneNumber || req.body?.mobile;
const password = req.body?.password || req.body?.pwd;
```

### Option 2: Force App Update with Server-Side Block
If the app is truly broken, force users to update:

```typescript
// Check User-Agent for old app version
const userAgent = req.get('user-agent') || '';
if (userAgent.includes('okhttp') && !req.body?.phone) {
  return res.status(426).json({
    message: "Please update your app from the Play Store to continue",
    updateRequired: true,
    updateUrl: "https://play.google.com/store/apps/details?id=com.instantllycards.www.twa"
  });
}
```

## Testing with Real User

Ask a user experiencing the issue to:
1. Clear app cache: Settings → Apps → Instantlly Cards → Storage → Clear Cache
2. Try login again
3. If it fails, uninstall and reinstall from Play Store

## Next Steps

1. **Check Render logs NOW** - Look for the pattern above
2. **Share the log snippet** - Send me what you see
3. **Apply quick fix** - Based on what logs show
4. **Monitor** - Watch if login success rate improves

## Log Retention
Render keeps logs for limited time. If you need longer retention:
- Set up external logging (Papertrail, Loggly, etc.)
- Or download logs regularly

## Contact
If you see the logs but need help interpreting them, share:
- The `📥 POST /api/auth/login` line
- The `📦 Body keys` line  
- The `Login attempt - Raw body` line

This will immediately tell us what's wrong.
