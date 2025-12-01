// Firebase Admin SDK initialization for server-side phone authentication verification
import admin from 'firebase-admin';
import path from 'path';
import fs from 'fs';

let firebaseInitialized = false;

// Initialize Firebase Admin SDK
export const initializeFirebase = () => {
  if (firebaseInitialized) {
    return admin;
  }

  try {
    // Option 1: Use environment variable with base64-encoded credentials
    if (process.env.FIREBASE_SERVICE_ACCOUNT_BASE64) {
      console.log('🔑 Using Firebase credentials from environment variable');
      const serviceAccountJson = Buffer.from(
        process.env.FIREBASE_SERVICE_ACCOUNT_BASE64,
        'base64'
      ).toString('utf-8');
      const serviceAccount = JSON.parse(serviceAccountJson);

      admin.initializeApp({
        credential: admin.credential.cert(serviceAccount),
        projectId: serviceAccount.project_id
      });

      firebaseInitialized = true;
      console.log('✅ Firebase Admin SDK initialized successfully');
      console.log('📱 Project ID:', serviceAccount.project_id);
      return admin;
    }

    // Option 2: Use individual environment variables
    if (process.env.FIREBASE_PROJECT_ID && 
        process.env.FIREBASE_PRIVATE_KEY && 
        process.env.FIREBASE_CLIENT_EMAIL) {
      console.log('🔑 Using Firebase credentials from individual environment variables');
      
      admin.initializeApp({
        credential: admin.credential.cert({
          projectId: process.env.FIREBASE_PROJECT_ID,
          privateKey: process.env.FIREBASE_PRIVATE_KEY.replace(/\\n/g, '\n'),
          clientEmail: process.env.FIREBASE_CLIENT_EMAIL
        }),
        projectId: process.env.FIREBASE_PROJECT_ID
      });

      firebaseInitialized = true;
      console.log('✅ Firebase Admin SDK initialized successfully');
      console.log('📱 Project ID:', process.env.FIREBASE_PROJECT_ID);
      return admin;
    }

    // Option 3: Use local file (for development only - never commit this file!)
    const serviceAccountPath = path.join(__dirname, '../../firebase-service-account.json');
    
    if (fs.existsSync(serviceAccountPath)) {
      console.log('⚠️ Using local Firebase service account file (development only)');
      const serviceAccount = require(serviceAccountPath);

      admin.initializeApp({
        credential: admin.credential.cert(serviceAccount),
        projectId: serviceAccount.project_id
      });

      firebaseInitialized = true;
      console.log('✅ Firebase Admin SDK initialized successfully');
      console.log('📱 Project ID:', serviceAccount.project_id);
      return admin;
    }

    // Firebase is optional - only used for Firebase authentication
    // Using Fast2SMS for OTP instead
    console.log('ℹ️  Firebase not configured (optional - using Fast2SMS for OTP)');
    return null;
  } catch (error) {
    console.error('❌ Failed to initialize Firebase Admin SDK:', error);
    return null;
  }
};

// Verify Firebase ID token
export const verifyFirebaseToken = async (idToken: string) => {
  try {
    if (!firebaseInitialized) {
      initializeFirebase();
    }

    if (!firebaseInitialized) {
      throw new Error('Firebase Admin SDK is not initialized');
    }

    const decodedToken = await admin.auth().verifyIdToken(idToken);
    return {
      success: true,
      uid: decodedToken.uid,
      phoneNumber: decodedToken.phone_number,
      email: decodedToken.email,
      decodedToken
    };
  } catch (error: any) {
    console.error('❌ Firebase token verification failed:', error.message);
    return {
      success: false,
      error: error.message
    };
  }
};

// Get Firebase user by UID
export const getFirebaseUser = async (uid: string) => {
  try {
    if (!firebaseInitialized) {
      initializeFirebase();
    }

    if (!firebaseInitialized) {
      throw new Error('Firebase Admin SDK is not initialized');
    }

    const userRecord = await admin.auth().getUser(uid);
    return {
      success: true,
      user: userRecord
    };
  } catch (error: any) {
    console.error('❌ Failed to get Firebase user:', error.message);
    return {
      success: false,
      error: error.message
    };
  }
};

export default admin;
