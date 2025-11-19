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
  const requestTimestamp = new Date().toISOString();
  const requestId = Math.random().toString(36).substring(7);
  
  try {
    const { phone } = req.body;

    console.log(`\n${'='.repeat(70)}`);
    console.log(`📥 [SEND-OTP] REQUEST START - ID: ${requestId}`);
    console.log(`⏰ Timestamp: ${requestTimestamp}`);
    console.log(`📱 Phone: ${phone}`);
    console.log(`${'='.repeat(70)}`);

    if (!phone) {
      console.log(`❌ [SEND-OTP] ERROR: No phone number provided - ID: ${requestId}`);
      return res.status(400).json({
        success: false,
        message: 'Phone number is required',
        requestId
      });
    }

    console.log(`✓ [SEND-OTP] Phone validation passed - ID: ${requestId}`);
    console.log(`🔄 [SEND-OTP] Generating 6-digit OTP...`);

    // Generate OTP
    const code = generateOTP();
    const ttlSeconds = 300; // 5 minutes
    const expiresAt = Date.now() + ttlSeconds * 1000;

    console.log(`✓ [SEND-OTP] OTP Generated: ${code}`);
    console.log(`✓ [SEND-OTP] TTL: ${ttlSeconds} seconds`);
    console.log(`✓ [SEND-OTP] Expires at: ${new Date(expiresAt).toISOString()}`);
    
    // Store OTP for backend verification
    otpStore.set(phone, { code, expiresAt, attempts: 0 });
    console.log(`✓ [SEND-OTP] OTP stored in memory map. Store size: ${otpStore.size}`);
    console.log(`⏳ [SEND-OTP] Waiting for Firebase to send SMS...`);

    // Return success - Firebase will handle SMS on the frontend
    console.log(`✅ [SEND-OTP] SUCCESS: Sending response to mobile app`);
    console.log(`${'='.repeat(70)}\n`);
    
    return res.json({
      success: true,
      message: 'OTP ready for Firebase verification',
      ttl: ttlSeconds,
      requestId,
      serverTime: requestTimestamp,
      // In development, you can return the OTP for testing
      // Remove this in production when using real Firebase Phone Auth
      ...(process.env.NODE_ENV === 'development' && { devOTP: code })
    });
  } catch (error: any) {
    console.error(`\n💥 [SEND-OTP] EXCEPTION ERROR - ID: ${requestId}`);
    console.error(`📋 Error Type: ${error.name}`);
    console.error(`📝 Error Message: ${error.message}`);
    console.error(`📚 Error Stack: ${error.stack}`);
    console.error(`${'='.repeat(70)}\n`);
    
    return res.status(500).json({
      success: false,
      message: 'Failed to generate OTP. Please try again.',
      requestId,
      error: error.message
    });
  }
});

// POST /api/auth/verify-otp
// Verifies the OTP entered by the user
router.post('/verify-otp', (req, res) => {
  const requestTimestamp = new Date().toISOString();
  const requestId = Math.random().toString(36).substring(7);
  
  try {
    const { phone, otp } = req.body;

    console.log(`\n${'='.repeat(70)}`);
    console.log(`🔐 [VERIFY-OTP] REQUEST START - ID: ${requestId}`);
    console.log(`⏰ Timestamp: ${requestTimestamp}`);
    console.log(`📱 Phone: ${phone}`);
    console.log(`🔑 OTP Provided: ${otp ? '***' + otp.slice(-2) : 'MISSING'}`);
    console.log(`${'='.repeat(70)}`);

    // Validation
    if (!phone || !otp) {
      console.log(`❌ [VERIFY-OTP] ERROR: Missing required fields - ID: ${requestId}`);
      console.log(`   Phone: ${phone ? '✓' : '✗ MISSING'}, OTP: ${otp ? '✓' : '✗ MISSING'}`);
      return res.status(400).json({
        success: false,
        message: 'Phone number and OTP are required',
        requestId
      });
    }

    console.log(`✓ [VERIFY-OTP] Validation passed`);
    console.log(`🔍 [VERIFY-OTP] Checking OTP store. Total entries: ${otpStore.size}`);

    const record = otpStore.get(phone);

    if (!record) {
      console.log(`❌ [VERIFY-OTP] ERROR: No OTP found for this phone - ID: ${requestId}`);
      console.log(`   Store contains: ${Array.from(otpStore.keys()).join(', ') || 'EMPTY'}`);
      return res.status(400).json({
        success: false,
        message: 'No OTP found for this phone number. Please request a new OTP.',
        requestId
      });
    }

    console.log(`✓ [VERIFY-OTP] OTP record found in store`);

    // Check expiry
    const now = Date.now();
    const timeRemaining = record.expiresAt - now;
    
    if (now > record.expiresAt) {
      otpStore.delete(phone);
      console.log(`❌ [VERIFY-OTP] ERROR: OTP expired - ID: ${requestId}`);
      console.log(`   Expired: ${new Date(record.expiresAt).toISOString()}`);
      console.log(`   Current: ${new Date(now).toISOString()}`);
      return res.status(400).json({
        success: false,
        message: 'OTP has expired. Please request a new OTP.',
        requestId
      });
    }
    
    console.log(`✓ [VERIFY-OTP] OTP not expired. Time remaining: ${Math.round(timeRemaining / 1000)}s`);

    // Check attempts
    if (record.attempts >= 3) {
      otpStore.delete(phone);
      console.log(`❌ [VERIFY-OTP] ERROR: Too many failed attempts - ID: ${requestId}`);
      console.log(`   Attempts: ${record.attempts}`);
      return res.status(429).json({
        success: false,
        message: 'Too many failed attempts. Please request a new OTP.',
        requestId
      });
    }

    console.log(`✓ [VERIFY-OTP] Attempt count OK. Attempts used: ${record.attempts}/3`);
    console.log(`🔄 [VERIFY-OTP] Comparing codes...`);
    console.log(`   Stored code: ${record.code}`);
    console.log(`   Provided code: ${String(otp)}`);
    console.log(`   Match: ${record.code === String(otp) ? '✓ YES' : '✗ NO'}`);

    // Verify OTP
    if (record.code !== String(otp)) {
      record.attempts++;
      const attemptsRemaining = 3 - record.attempts;
      console.log(`❌ [VERIFY-OTP] ERROR: Invalid OTP - ID: ${requestId}`);
      console.log(`   Attempts remaining: ${attemptsRemaining}`);
      return res.status(400).json({
        success: false,
        message: `Invalid OTP. ${attemptsRemaining} attempts remaining.`,
        requestId,
        attemptsRemaining
      });
    }

    // OTP verified successfully
    otpStore.delete(phone);
    console.log(`✅ [VERIFY-OTP] SUCCESS: OTP verified successfully - ID: ${requestId}`);
    console.log(`   OTP record deleted from store. Store size: ${otpStore.size}`);
    console.log(`${'='.repeat(70)}\n`);

    return res.json({
      success: true,
      verified: true,
      message: 'Phone number verified successfully',
      requestId,
      serverTime: requestTimestamp
    });
  } catch (error: any) {
    console.error(`\n💥 [VERIFY-OTP] EXCEPTION ERROR - ID: ${requestId}`);
    console.error(`📋 Error Type: ${error.name}`);
    console.error(`📝 Error Message: ${error.message}`);
    console.error(`📚 Error Stack: ${error.stack}`);
    console.error(`${'='.repeat(70)}\n`);
    
    return res.status(500).json({
      success: false,
      message: 'Failed to verify OTP. Please try again.',
      requestId,
      error: error.message
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
