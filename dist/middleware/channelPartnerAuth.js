"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.authenticateChannelPartner = void 0;
const jsonwebtoken_1 = __importDefault(require("jsonwebtoken"));
const mongoose_1 = __importDefault(require("mongoose"));
// JWT Secret for channel partner tokens (should match channel partner backend)
const JWT_SECRET = process.env.CHANNEL_PARTNER_JWT_SECRET || 'inst@ntly_c@rds_pr0ducti0n_s3cr3t_k3y_2024_v1.0';
/**
 * Middleware to authenticate channel partner JWT tokens
 * Token format: { userId, phone }
 */
const authenticateChannelPartner = async (req, res, next) => {
    try {
        const authHeader = req.headers.authorization;
        if (!authHeader || !authHeader.startsWith('Bearer ')) {
            console.log('❌ No authorization header or invalid format');
            return res.status(401).json({ message: 'No token provided' });
        }
        const token = authHeader.substring(7); // Remove 'Bearer ' prefix
        if (!token) {
            console.log('❌ Empty token');
            return res.status(401).json({ message: 'No token provided' });
        }
        // Verify token
        const payload = jsonwebtoken_1.default.verify(token, JWT_SECRET);
        if (!payload.userId || !payload.phone) {
            console.log('❌ Invalid token payload:', payload);
            return res.status(401).json({ message: 'Invalid token format' });
        }
        // Validate ObjectId format
        if (!mongoose_1.default.Types.ObjectId.isValid(payload.userId)) {
            console.log(`❌ Invalid userId format in channel partner token: ${payload.userId}`);
            return res.status(401).json({ message: 'Invalid user ID format in token' });
        }
        // Attach channel partner info to request
        req.channelPartnerId = payload.userId;
        req.channelPartnerPhone = payload.phone;
        console.log(`✅ Channel partner authenticated: ${payload.phone} (ID: ${payload.userId})`);
        next();
    }
    catch (error) {
        if (error instanceof jsonwebtoken_1.default.JsonWebTokenError) {
            console.log('❌ Invalid token:', error.message);
            return res.status(401).json({ message: 'Invalid token' });
        }
        if (error instanceof jsonwebtoken_1.default.TokenExpiredError) {
            console.log('❌ Token expired');
            return res.status(401).json({ message: 'Token expired' });
        }
        console.error('❌ Channel partner auth error:', error);
        return res.status(500).json({ message: 'Authentication error' });
    }
};
exports.authenticateChannelPartner = authenticateChannelPartner;
