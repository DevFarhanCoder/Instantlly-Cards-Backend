# Firebase Setup for OTP Authentication

## ⚠️ SECURITY WARNING

**NEVER commit `firebase-service-account.json` to Git!** This file contains sensitive credentials.

## Local Development Setup

1. **Get Firebase Service Account**
   - Download from Firebase Console → Project Settings → Service Accounts
   - Save as `firebase-service-account.json` in project root
   - This file is already in `.gitignore` (do not commit it!)

2. **The file will be automatically detected** in development mode

## Production Deployment (Render/Vercel)

### Method 1: Base64 Encoded (Recommended)

1. Run the encoding script:
   ```bash
   node encode-firebase-credentials.js
   ```

2. Copy the `FIREBASE_SERVICE_ACCOUNT_BASE64` value

3. Add it to your deployment platform:
   - **Render**: Dashboard → Environment → Add Environment Variable
   - **Vercel**: Settings → Environment Variables

### Method 2: Individual Environment Variables

Alternatively, set these three variables:
- `FIREBASE_PROJECT_ID`
- `FIREBASE_CLIENT_EMAIL`
- `FIREBASE_PRIVATE_KEY`

(Get values by running `node encode-firebase-credentials.js`)

## How It Works

The Firebase service (`src/services/firebase.ts`) checks for credentials in this order:

1. ✅ `FIREBASE_SERVICE_ACCOUNT_BASE64` environment variable
2. ✅ Individual env vars (`FIREBASE_PROJECT_ID`, etc.)
3. ✅ Local file `firebase-service-account.json` (dev only)

## Testing

Start the server and look for:
```
✅ Firebase Admin SDK initialized successfully
📱 Project ID: your-project-id
```

If you see this, Firebase is ready to verify OTP tokens from the mobile app!
