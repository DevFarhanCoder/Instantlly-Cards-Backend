// Instantlly-Cards-Backend/src/routes/otp.ts
import express from 'express';
import crypto from 'crypto';
import User from '../models/User';

const router = express.Router();

// In-memory OTP store (use Redis in production for scalability)
// This stores OTPs temporarily for verification
interface OTPRecord {
  code: string;
  expiresAt: number;
  attempts: number;
}

const otpStore = new Map<string, OTPRecord>();

// Generate 6-digit OTP
function generateOTP(): string {
  return crypto.randomInt(100000, 999999).toString();
}

// Debug endpoint to check configuration
router.get('/debug-config', (req, res) => {
  res.json({
    service: 'Firebase Phone Authentication',
    nodeEnv: process.env.NODE_ENV,
    otpStoreSize: otpStore.size
  });
});

// NOTE: /check-phone endpoint is now in auth.ts to avoid duplication

// Diagnostic endpoint to test if backend is reachable
router.post('/ping', (req, res) => {
  const { phone, timestamp } = req.body;
  console.log('🏓 [PING] Received from mobile app:', { phone, timestamp });
  res.json({
    success: true,
    message: 'Backend is reachable',
    receivedPhone: phone,
    serverTime: new Date().toISOString()
  });
});

// POST /api/auth/send-otp
// This endpoint generates and stores an OTP
// The actual SMS sending is handled by Firebase on the frontend
router.post('/send-otp', async (req, res) => {
  try {
    const { phone } = req.body;

    console.log('📥 [SEND-OTP] Request received for phone:', phone);

    if (!phone) {
      console.log('❌ [SEND-OTP] No phone number provided');
      return res.status(400).json({
        success: false,
        message: 'Phone number is required'
      });
    }

    console.log('📱 Generating OTP for:', phone);

    // Generate OTP
    const code = generateOTP();
    const ttlSeconds = 300; // 5 minutes
    const expiresAt = Date.now() + ttlSeconds * 1000;

    // Store OTP for backend verification
    otpStore.set(phone, { code, expiresAt, attempts: 0 });

    console.log('✅ OTP generated and stored:', code);
    console.log('⏰ Expires at:', new Date(expiresAt).toISOString());

    // Return success - Firebase will handle SMS on the frontend
    return res.json({
      success: true,
      message: 'OTP ready for Firebase verification',
      ttl: ttlSeconds,
      // In development, you can return the OTP for testing
      // Remove this in production when using                                                                                                                      real Firebase Phone Auth
      ...(process.env.NODE_ENV === 'development' && { devOTP: code })
    });
  } catch (error: any) {
    console.error('💥 Generate OTP error:', error);
    return res.status(500).json({
      success: false,
      message: 'Failed to generate OTP. Please try again.'
    });
  }
});

// POST /api/auth/verify-otp
// Verifies the OTP entered by the user
router.post('/verify-otp', (req, res) => {
  try {
    const { phone, otp } = req.body;

    if (!phone || !otp) {
      return res.status(400).json({
        success: false,
        message: 'Phone number and OTP are required'
      });
    }

    console.log('🔐 Verifying OTP for:', phone);

    const record = otpStore.get(phone);

    if (!record) {
      return res.status(400).json({
        success: false,
        message: 'No OTP found for this phone number. Please request a new OTP.'
      });
    }

    // Check expiry
    if (Date.now() > record.expiresAt) {
      otpStore.delete(phone);
      return res.status(400).json({
        success: false,
        message: 'OTP has expired. Please request a new OTP.'
      });
    }

    // Check attempts
    if (record.attempts >= 3) {
      otpStore.delete(phone);
      return res.status(429).json({
        success: false,
        message: 'Too many failed attempts. Please request a new OTP.'
      });
    }

    // Verify OTP
    if (record.code !== String(otp)) {
      record.attempts++;
      return res.status(400).json({
        success: false,
        message: `Invalid OTP. ${3 - record.attempts} attempts remaining.`
      });
    }

    // OTP verified successfully
    otpStore.delete(phone);
    console.log('✅ OTP verified successfully for:', phone);

    return res.json({
      success: true,
      verified: true,
      message: 'Phone number verified successfully'
    });
  } catch (error: any) {
    console.error('💥 Verify OTP error:', error);
    return res.status(500).json({
      success: false,
      message: 'Failed to verify OTP. Please try again.'
    });
  }
});

// POST /api/auth/resend-otp (alias to send-otp)
router.post('/resend-otp', async (req, res) => {
  // Delete old OTP first to allow resend
  const { phone } = req.body;
  if (phone) {
    otpStore.delete(phone);
  }
  // Forward to send-otp handler by manually calling it
  req.url = '/send-otp';
  return;
});

export default router;
