/**
 * Fast2SMS OTP Service
 * 
 * This file contains ONLY Fast2SMS OTP functionality.
 * Separated for easier debugging of OTP delays.
 * 
 * Features:
 * - Send OTP via Fast2SMS
 * - Store OTP in memory with expiry
 * - Verify OTP
 * - Rate limiting and attempt tracking
 */

import axios from 'axios';

// In-memory OTP store (use Redis in production for scalability)
interface OTPRecord {
  code: string;
  expiresAt: number;
  attempts: number;
  createdAt: number;
}

const otpStore = new Map<string, OTPRecord>();

// Configuration
const OTP_TTL_SECONDS = 300; // 5 minutes
const MAX_ATTEMPTS = 3;
const OTP_LENGTH = 6;

/**
 * Generate 6-digit OTP
 */
export function generateOTP(): string {
  return Math.floor(100000 + Math.random() * 900000).toString();
}

/**
 * Store OTP for a phone number
 */
export function storeOTP(phone: string, code: string): void {
  const expiresAt = Date.now() + OTP_TTL_SECONDS * 1000;
  otpStore.set(phone, {
    code,
    expiresAt,
    attempts: 0,
    createdAt: Date.now()
  });
  console.log(`[FAST2SMS-STORE] ✅ OTP stored for ${phone} (expires in ${OTP_TTL_SECONDS}s)`);
}

/**
 * Verify OTP entered by user
 */
export function verifyOTP(phone: string, code: string): {
  success: boolean;
  message: string;
  attemptsRemaining?: number;
} {
  const record = otpStore.get(phone);

  if (!record) {
    return {
      success: false,
      message: 'No OTP found. Please request a new OTP.'
    };
  }

  // Check expiry
  if (Date.now() > record.expiresAt) {
    otpStore.delete(phone);
    return {
      success: false,
      message: 'OTP has expired. Please request a new OTP.'
    };
  }

  // Check attempts
  if (record.attempts >= MAX_ATTEMPTS) {
    otpStore.delete(phone);
    return {
      success: false,
      message: 'Too many failed attempts. Please request a new OTP.'
    };
  }

  // Verify code
  if (record.code !== String(code)) {
    record.attempts++;
    const attemptsRemaining = MAX_ATTEMPTS - record.attempts;
    console.log(`[FAST2SMS-VERIFY] ❌ Invalid OTP for ${phone}. Attempts remaining: ${attemptsRemaining}`);
    
    return {
      success: false,
      message: `Invalid OTP. ${attemptsRemaining} attempts remaining.`,
      attemptsRemaining
    };
  }

  // Success - delete OTP (one-time use)
  otpStore.delete(phone);
  console.log(`[FAST2SMS-VERIFY] ✅ OTP verified for ${phone}`);
  
  return {
    success: true,
    message: 'OTP verified successfully'
  };
}

/**
 * Delete OTP for a phone number
 */
export function deleteOTP(phone: string): void {
  otpStore.delete(phone);
  console.log(`[FAST2SMS-DELETE] 🗑️ Deleted OTP for ${phone}`);
}

/**
 * Check if OTP exists for a phone number
 */
export function hasOTP(phone: string): boolean {
  return otpStore.has(phone);
}

/**
 * Get OTP store size (for monitoring)
 */
export function getOTPStoreSize(): number {
  return otpStore.size;
}

/**
 * Send OTP via Fast2SMS API
 * 
 * @param phone - 10-digit Indian phone number (without +91)
 * @param otp - 6-digit OTP code
 * @param appHash - Optional Android SMS Retriever hash
 * @returns Promise with success status and message
 */
export async function sendOTPViaFast2SMS(
  phone: string,
  otp: string,
  appHash?: string
): Promise<{
  success: boolean;
  message: string;
  error?: string;
  debug?: any;
}> {
  const startTime = Date.now();
  const requestId = Math.random().toString(36).substring(7);

  try {
    console.log(`\n${'='.repeat(70)}`);
    console.log(`[FAST2SMS-SEND] 📤 REQUEST START - ID: ${requestId}`);
    console.log(`[FAST2SMS-SEND] 📱 Phone: ${phone}`);
    console.log(`[FAST2SMS-SEND] 🔑 OTP: ${otp}`);
    console.log(`[FAST2SMS-SEND] ⏰ Timestamp: ${new Date().toISOString()}`);
    console.log(`${'='.repeat(70)}`);

    // Validate API key
    const apiKey = process.env.FAST2SMS_API_KEY;
    if (!apiKey) {
      console.error(`[FAST2SMS-SEND] ❌ API key not configured`);
      return {
        success: false,
        message: 'OTP service not configured',
        error: 'FAST2SMS_API_KEY not set'
      };
    }

    // Clean phone number (remove +91 prefix and non-digits)
    const cleanPhone = phone.replace(/^\+91/, '').replace(/\D/g, '');
    
    if (cleanPhone.length !== 10) {
      console.error(`[FAST2SMS-SEND] ❌ Invalid phone: ${cleanPhone}`);
      return {
        success: false,
        message: 'Invalid phone number',
        error: 'Phone must be 10 digits'
      };
    }

    // Prepare message with Android SMS Retriever format if appHash provided
    const finalAppHash = appHash?.trim() || '';
    const message = finalAppHash
      ? `<#> ${otp} is your OTP for Instantlly Cards\n${finalAppHash}`
      : `${otp} is your OTP for Instantlly Cards. Valid for 5 minutes. Do not share.`;

    // Prepare Fast2SMS API request
    const payload = new URLSearchParams({
      authorization: apiKey,
      sender_id: 'FSTSMS',
      message,
      language: 'english',
      route: 'q', // Quick SMS route
      numbers: cleanPhone
    });

    console.log(`[FAST2SMS-SEND] 📡 Calling Fast2SMS API...`);
    console.log(`[FAST2SMS-SEND] 📝 Message: ${message}`);
    console.log(`[FAST2SMS-SEND] 📞 Clean Phone: ${cleanPhone}`);

    // Make API request
    const response = await axios.get(
      `https://www.fast2sms.com/dev/bulkV2?${payload.toString()}`,
      {
        headers: {
          'Cache-Control': 'no-cache'
        },
        timeout: 10000 // 10 second timeout
      }
    );

    const duration = Date.now() - startTime;
    console.log(`[FAST2SMS-SEND] ⏱️ API call took ${duration}ms`);
    console.log(`[FAST2SMS-SEND] 📨 Response:`, JSON.stringify(response.data, null, 2));

    // Check response
    if (!response.data || response.data.return === false) {
      console.error(`[FAST2SMS-SEND] ❌ API returned error:`, response.data);
      return {
        success: false,
        message: 'Failed to send OTP',
        error: 'Fast2SMS API error',
        debug: response.data
      };
    }

    console.log(`[FAST2SMS-SEND] ✅ OTP sent successfully in ${duration}ms`);
    console.log(`${'='.repeat(70)}\n`);

    return {
      success: true,
      message: 'OTP sent successfully',
      debug: {
        duration,
        requestId,
        response: response.data
      }
    };

  } catch (error: any) {
    const duration = Date.now() - startTime;
    console.error(`\n[FAST2SMS-SEND] ❌ ERROR after ${duration}ms - ID: ${requestId}`);
    console.error(`[FAST2SMS-SEND] 📋 Error:`, error.message);
    console.error(`[FAST2SMS-SEND] 📊 Status:`, error.response?.status);
    console.error(`[FAST2SMS-SEND] 📝 Response:`, error.response?.data);
    console.error(`${'='.repeat(70)}\n`);

    return {
      success: false,
      message: 'Failed to send OTP',
      error: error.message,
      debug: {
        duration,
        requestId,
        status: error.response?.status,
        data: error.response?.data
      }
    };
  }
}

/**
 * Clean up expired OTPs (run periodically)
 */
export function cleanupExpiredOTPs(): number {
  const now = Date.now();
  let removed = 0;
  
  for (const [phone, record] of otpStore.entries()) {
    if (now > record.expiresAt) {
      otpStore.delete(phone);
      removed++;
    }
  }
  
  if (removed > 0) {
    console.log(`[FAST2SMS-CLEANUP] 🧹 Removed ${removed} expired OTPs`);
  }
  
  return removed;
}

// Run cleanup every minute
setInterval(cleanupExpiredOTPs, 60000);
