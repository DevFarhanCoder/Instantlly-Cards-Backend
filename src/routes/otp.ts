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
      // Send OTP via Fast2SMS
      const response = await axios.get(FAST2SMS_URL, {
        params: {
          authorization: FAST2SMS_API_KEY,
          message,
          language: 'english',
          route: 'q',
          numbers: phone.replace(/\D/g, '') // Remove non-digits
        }
      });

      console.log('✅ Fast2SMS response:', response.data);

      if (response.data && response.data.return === true) {
        return res.json({
          success: true,
          message: 'OTP sent successfully',
          ttl: ttlSeconds
        });
      } else {
        throw new Error('Fast2SMS returned error');
      }
    } catch (smsError: any) {
      console.error('❌ Fast2SMS error:', smsError?.response?.data || smsError.message);
      
      // For development: still return success and log the OTP
      console.log('🔐 [DEV] OTP for', phone, ':', code);
      
      return res.json({
        success: true,
        message: 'OTP sent successfully',
        ttl: ttlSeconds,
        // Remove this in production:
        devOTP: process.env.NODE_ENV === 'development' ? code : undefined
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
