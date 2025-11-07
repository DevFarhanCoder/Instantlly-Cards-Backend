// Instantlly-Cards-Backend/src/routes/otp.ts
import express from 'express';
import axios from 'axios';
import crypto from 'crypto';
import User from '../models/User';

const router = express.Router();

// Use environment variable for Fast2SMS API key
const FAST2SMS_API_KEY = process.env.FAST2SMS_API_KEY || 'tH2an11rgORVwQE5FT8sHLqOYbn6AexAVGe3Y47JH9BszQM79JsISCg7aqGy';
const FAST2SMS_URL = 'https://www.fast2sms.com/dev/bulkV2';

// In-memory OTP store (use Redis in production)
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

// POST /api/auth/check-phone - Check if phone number exists
router.post('/check-phone', async (req, res) => {
  try {
    const { phone } = req.body;

    if (!phone) {
      return res.status(400).json({
        success: false,
        message: 'Phone number is required'
      });
    }

    console.log('🔍 Checking if phone exists:', phone);

    // Check if user exists with this phone number
    const existingUser = await User.findOne({ phone });

    if (existingUser) {
      return res.json({
        success: true,
        exists: true,
        message: 'Phone number is already registered. Please login.'
      });
    }

    return res.json({
      success: true,
      exists: false,
      message: 'Phone number is available for registration'
    });
  } catch (error: any) {
    console.error('💥 Check phone error:', error);
    return res.status(500).json({
      success: false,
      message: 'Failed to check phone number'
    });
  }
});

// POST /api/auth/send-otp
router.post('/send-otp', async (req, res) => {
  try {
    const { phone } = req.body;

    if (!phone) {
      return res.status(400).json({
        success: false,
        message: 'Phone number is required'
      });
    }

    console.log('📱 Sending OTP to:', phone);

    // Generate OTP
    const code = generateOTP();
    const ttlSeconds = 300; // 5 minutes
    const expiresAt = Date.now() + ttlSeconds * 1000;

    // Store OTP
    otpStore.set(phone, { code, expiresAt, attempts: 0 });

    // Prepare SMS message
    const message = `Your InstantllyCards verification code is ${code}. It will expire in 5 minutes.`;

    try {
      // Clean phone number (remove +, spaces, hyphens)
      const cleanPhone = phone.replace(/\D/g, '');
      
      console.log('📞 Attempting to send SMS to:', cleanPhone);
      console.log('📝 Message:', message);
      console.log('🔑 Using Fast2SMS route: q (promotional - works but may fail for DND numbers)');
      
      // Send OTP via Fast2SMS using route 'q' (promotional)
      // Note: Route 'v3' doesn't work with this account, 'dlt' needs DLT registration
      // Route 'q' works but may be blocked for DND numbers
      const response = await axios.get(FAST2SMS_URL, {
        params: {
          authorization: FAST2SMS_API_KEY,
          message,
          language: 'english',
          route: 'q', // Promotional route - only one that works with this API key
          numbers: cleanPhone
        },
        timeout: 10000 // 10 second timeout
      });

      console.log('✅ Fast2SMS full response:', JSON.stringify(response.data, null, 2));
      console.log('📊 Response status:', response.status);
      console.log('📊 Response return:', response.data?.return);
      console.log('📊 Response message:', response.data?.message);

      if (response.data && response.data.return === true) {
        console.log('✅ SMS sent successfully via Fast2SMS');
        return res.json({
          success: true,
          message: 'OTP sent successfully',
          ttl: ttlSeconds
        });
      } else {
        // Log the specific error from Fast2SMS
        console.error('❌ Fast2SMS error response:', JSON.stringify(response.data, null, 2));
        console.error('   Return:', response.data?.return);
        console.error('   Message:', response.data?.message);
        console.error('   Status Code:', response.data?.status_code);
        
        // If DND blocked, still return success with devOTP
        if (response.data?.status_code === 427 || response.data?.message?.includes('DND')) {
          console.log('📵 Number is in DND list - returning devOTP for manual entry');
          return res.json({
            success: true,
            message: 'OTP generated. If SMS not received, use code from app.',
            ttl: ttlSeconds,
            devOTP: code, // Return OTP for DND numbers
            smsStatus: 'blocked_dnd',
            smsError: 'Number in DND list'
          });
        }
        
        throw new Error(response.data?.message || 'Fast2SMS returned error');
      }
    } catch (smsError: any) {
      console.error('❌ Fast2SMS error full details:');
      console.error('   Type:', smsError.constructor.name);
      console.error('   Message:', smsError.message);
      console.error('   Response status:', smsError?.response?.status);
      console.error('   Response data:', JSON.stringify(smsError?.response?.data, null, 2));
      console.error('   Is timeout:', smsError.code === 'ECONNABORTED');
      console.error('   Error code:', smsError.code);
      
      // For development/testing: still return success and log the OTP
      console.log('🔐 [DEV/FALLBACK] OTP for', phone, ':', code);
      console.log('⚠️  SMS failed - returning OTP in response for user to enter manually');
      console.log('   Possible causes:');
      console.log('   - DND number (blocked in Fast2SMS)');
      console.log('   - Insufficient credits in Fast2SMS account');
      console.log('   - Invalid API key or route not enabled');
      console.log('   - Network error or timeout');
      console.log('   - Fast2SMS service temporarily down');
      
      return res.json({
        success: true,
        message: 'OTP generated. If SMS not received, use code from notification.',
        ttl: ttlSeconds,
        // Always return OTP when SMS fails so user can proceed
        devOTP: code,
        smsStatus: 'failed',
        smsError: smsError?.response?.data?.message || smsError.message || 'SMS delivery failed',
        errorDetails: {
          type: smsError.constructor.name,
          code: smsError.code,
          statusCode: smsError?.response?.data?.status_code
        }
      });
    }
  } catch (error: any) {
    console.error('💥 Send OTP error:', error);
    return res.status(500).json({
      success: false,
      message: 'Failed to send OTP. Please try again.'
    });
  }
});

// POST /api/auth/verify-otp
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
    console.log('✅ OTP verified for:', phone);

    // You could mark the phone as verified in your database here
    // or create a short-lived verification token

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
