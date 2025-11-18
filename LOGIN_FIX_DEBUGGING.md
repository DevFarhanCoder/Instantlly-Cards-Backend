# Login Issue Debugging - Nov 18, 2025

## Problem
Users are experiencing login failures after updating the app from Play Store. The logs show:
```
Login attempt - Request body: { phone: undefined, hasPassword: true }
Missing fields in login request
```

## Root Cause
The `phone` field in the request body is coming through as `undefined`, which indicates either:
1. The Android app is not sending the phone field correctly
2. The request body is not being parsed properly by the backend
3. Content-Type header mismatch between client and server

## Changes Made

### 1. Enhanced Body Parsing (`src/index.ts`)
- Added support for multiple JSON content types: `application/json`, `application/*+json`, `text/plain`
- Added URL-encoded body parser for form submissions
- Added request logging to capture Content-Type and body keys

### 2. Enhanced Request Logging (`src/routes/auth.ts`)
- Added detailed header logging (content-type, content-length, user-agent)
- Added raw body logging to see exactly what's being received
- Added detailed field-level logging to show which field is missing
- Added empty body detection with debug information
- Added field existence checking with detailed error messages

### 3. Enhanced Debugging Middleware (`src/index.ts`)
- Logs all login/signup requests with:
  - HTTP method and path
  - Content-Type header
  - Available body keys

## Deployment Instructions

### 1. Build and Deploy Backend
```bash
# Navigate to backend directory
cd Instantlly-Cards-Backend

# Install dependencies (if needed)
npm install

# Build TypeScript
npm run build

# Push to Render (if using git-based deployment)
git add .
git commit -m "fix: Enhanced logging and body parsing for login issues"
git push

# Or deploy via Render Dashboard
```

### 2. Monitor Logs
After deployment, watch logs in real-time:
- Go to Render Dashboard → Your Service → Logs
- Or use Render CLI: `render logs -f`

### 3. Test with curl
```bash
# Test successful login
curl -X POST https://instantlly-cards-backend-6ki0.onrender.com/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"phone":"+1234567890","password":"testpass"}'

# Test missing phone (should show debug info)
curl -X POST https://instantlly-cards-backend-6ki0.onrender.com/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"password":"testpass"}'
```

## Next Steps for Monitoring

### Deploy and Monitor Logs
After deploying these changes, monitor the logs for:

```
📥 POST /api/auth/login - Content-Type: <what-content-type?>
📦 Body keys: <what-keys-are-present?>
Login attempt - Headers: { contentType: '...', contentLength: '...', userAgent: '...' }
Login attempt - Raw body: { ... }
```

### Expected Scenarios

#### Scenario A: Content-Type Mismatch
If you see:
```
📥 POST /api/auth/login - Content-Type: text/plain
📦 Body keys: no body
```
**Solution**: Android app needs to set `Content-Type: application/json` header

#### Scenario B: Wrong Body Format
If you see:
```
📥 POST /api/auth/login - Content-Type: application/json
📦 Body keys: <unexpected-keys>
Login attempt - Raw body: { phoneNumber: '+123...', pwd: '...' }
```
**Solution**: Android app is sending wrong field names (e.g., `phoneNumber` instead of `phone`)

#### Scenario C: Empty Body
If you see:
```
📥 POST /api/auth/login - Content-Type: application/json
📦 Body keys: no body
```
**Solution**: Android app is not sending request body at all

## Android App Checklist

The Android app should:
1. Set proper headers:
   ```kotlin
   headers: {
     "Content-Type": "application/json"
   }
   ```

2. Send correct field names:
   ```json
   {
     "phone": "+1234567890",
     "password": "user-password"
   }
   ```

3. Use POST method for `/api/auth/login`

4. Ensure the request body is properly stringified

## Testing Commands

### Test Login Endpoint
```bash
curl -X POST https://your-backend-url.com/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"phone":"+1234567890","password":"testpass"}'
```

### Check Logs
Monitor your Render.com or hosting platform logs for the enhanced debug output.

## Rollback Plan
If these changes cause issues, remove the logging middleware:
1. Remove the request logging middleware from `src/index.ts`
2. Revert auth.ts logging changes
3. Keep the enhanced body parser (it's safe)

## Version Info
- Backend Version: 1.5
- App Version Experiencing Issue: 1.0.31
- Date: November 18, 2025
