import NodeCache from 'node-cache';

// Store OTPs in memory for 5 minutes (300 seconds)
const otpCache = new NodeCache({ stdTTL: 300, checkperiod: 60 });

interface OTPData {
  otp: string;
  phone: string;
  timestamp: number;
  attempts: number;
}

export const otpService = {
  /**
   * Store OTP for a phone number
   * @param phone - Phone number (with country code)
   * @param otp - 6-digit OTP
   */
  storeOTP: (phone: string, otp: string): void => {
    const data: OTPData = {
      otp,
      phone,
      timestamp: Date.now(),
      attempts: 0
    };
    otpCache.set(phone, data);
    console.log(`[OTP-STORE] ✅ Stored OTP for ${phone} (expires in 5 min)`);
  },

  /**
   * Verify OTP entered by user
   * @param phone - Phone number
   * @param otp - OTP to verify
   * @returns boolean - true if OTP is valid
   */
  verifyOTP: (phone: string, otp: string): boolean => {
    const data = otpCache.get<OTPData>(phone);
    
    if (!data) {
      console.log(`[OTP-VERIFY] ❌ No OTP found for ${phone}`);
      return false;
    }

    // Increment attempt counter
    data.attempts += 1;
    
    // Max 3 attempts
    if (data.attempts > 3) {
      console.log(`[OTP-VERIFY] ❌ Too many attempts for ${phone}`);
      otpCache.del(phone);
      return false;
    }

    // Check if OTP matches
    if (data.otp !== otp) {
      console.log(`[OTP-VERIFY] ❌ Invalid OTP for ${phone} (attempt ${data.attempts}/3)`);
      otpCache.set(phone, data); // Update attempt count
      return false;
    }

    // OTP is valid - delete it (one-time use)
    otpCache.del(phone);
    console.log(`[OTP-VERIFY] ✅ OTP verified and deleted for ${phone}`);
    return true;
  },

  /**
   * Delete OTP for a phone number
   * @param phone - Phone number
   */
  deleteOTP: (phone: string): void => {
    otpCache.del(phone);
    console.log(`[OTP-DELETE] 🗑️  Deleted OTP for ${phone}`);
  },

  /**
   * Check if OTP exists for a phone number
   * @param phone - Phone number
   * @returns boolean
   */
  hasOTP: (phone: string): boolean => {
    return otpCache.has(phone);
  },

  /**
   * Get OTP data (for debugging only - should not be exposed to API)
   * @param phone - Phone number
   */
  getOTPData: (phone: string): OTPData | undefined => {
    return otpCache.get<OTPData>(phone);
  }
};
