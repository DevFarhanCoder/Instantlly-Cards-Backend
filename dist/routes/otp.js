"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// Instantlly-Cards-Backend/src/routes/otp.ts
// OTP routes using Fast2SMS service
const express_1 = __importDefault(require("express"));
const fast2smsOtpService_1 = require("../services/fast2smsOtpService");
const router = express_1.default.Router();
// Debug endpoint to check configuration
router.get('/debug-config', (req, res) => {
    res.json({
        service: 'Fast2SMS OTP Service',
        nodeEnv: process.env.NODE_ENV,
        hasFast2smsApiKey: !!process.env.FAST2SMS_API_KEY,
        otpStoreSize: (0, fast2smsOtpService_1.getOTPStoreSize)()
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
// Generates OTP and sends it via Fast2SMS
router.post('/send-otp', async (req, res) => {
    const requestTimestamp = new Date().toISOString();
    const requestId = Math.random().toString(36).substring(7);
    try {
        const { phone, appHash } = req.body;
        console.log(`\n${'='.repeat(70)}`);
        console.log(`📥 [SEND-OTP] REQUEST START - ID: ${requestId}`);
        console.log(`⏰ Timestamp: ${requestTimestamp}`);
        console.log(`📱 Phone: ${phone}`);
        console.log(`🔐 AppHash: ${appHash ? 'PROVIDED' : 'NOT PROVIDED'}`);
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
        const code = (0, fast2smsOtpService_1.generateOTP)();
        const ttlSeconds = 300; // 5 minutes
        console.log(`✓ [SEND-OTP] OTP Generated: ${code}`);
        console.log(`✓ [SEND-OTP] TTL: ${ttlSeconds} seconds`);
        // Store OTP for verification
        (0, fast2smsOtpService_1.storeOTP)(phone, code);
        console.log(`✓ [SEND-OTP] OTP stored. Store size: ${(0, fast2smsOtpService_1.getOTPStoreSize)()}`);
        // Send OTP via Fast2SMS
        console.log(`📤 [SEND-OTP] Sending OTP via Fast2SMS...`);
        const sendResult = await (0, fast2smsOtpService_1.sendOTPViaFast2SMS)(phone, code, appHash);
        if (!sendResult.success) {
            console.error(`❌ [SEND-OTP] Fast2SMS send failed - ID: ${requestId}`);
            console.error(`   Error: ${sendResult.error}`);
            // Still return success since OTP is stored (useful for development/testing)
            return res.json({
                success: true,
                message: 'OTP generated (SMS delivery pending)',
                ttl: ttlSeconds,
                requestId,
                serverTime: requestTimestamp,
                warning: sendResult.error,
                // In development, return the OTP for testing
                ...(process.env.NODE_ENV === 'development' && { devOTP: code })
            });
        }
        console.log(`✅ [SEND-OTP] SUCCESS: OTP sent via Fast2SMS - ID: ${requestId}`);
        console.log(`${'='.repeat(70)}\n`);
        return res.json({
            success: true,
            message: 'OTP sent successfully',
            ttl: ttlSeconds,
            requestId,
            serverTime: requestTimestamp,
            // In development, return the OTP for testing
            ...(process.env.NODE_ENV === 'development' && { devOTP: code })
        });
    }
    catch (error) {
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
        console.log(`🔍 [VERIFY-OTP] Verifying OTP with Fast2SMS service...`);
        // Verify OTP using the Fast2SMS service
        const result = (0, fast2smsOtpService_1.verifyOTP)(phone, otp);
        if (!result.success) {
            console.log(`❌ [VERIFY-OTP] Verification failed - ID: ${requestId}`);
            console.log(`   Message: ${result.message}`);
            return res.status(400).json({
                success: false,
                message: result.message,
                requestId,
                attemptsRemaining: result.attemptsRemaining
            });
        }
        // OTP verified successfully
        console.log(`✅ [VERIFY-OTP] SUCCESS: OTP verified successfully - ID: ${requestId}`);
        console.log(`${'='.repeat(70)}\n`);
        return res.json({
            success: true,
            verified: true,
            message: 'Phone number verified successfully',
            requestId,
            serverTime: requestTimestamp
        });
    }
    catch (error) {
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
        (0, fast2smsOtpService_1.deleteOTP)(phone);
        console.log(`[RESEND-OTP] Deleted old OTP for ${phone}`);
    }
    // Forward to send-otp by calling it directly
    req.url = '/send-otp';
    return;
});
exports.default = router;
