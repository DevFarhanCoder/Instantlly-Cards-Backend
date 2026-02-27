"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.requireAuth = requireAuth;
const jsonwebtoken_1 = __importDefault(require("jsonwebtoken"));
const mongoose_1 = __importDefault(require("mongoose"));
function requireAuth(req, res, next) {
    const h = req.header('authorization') || '';
    const token = h.startsWith('Bearer ') ? h.slice(7) : undefined;
    if (!token) {
        console.log('❌ No token provided in request');
        return res.status(401).json({ message: 'Missing token' });
    }
    try {
        const payload = jsonwebtoken_1.default.verify(token, process.env.JWT_SECRET);
        console.log('🔍 Token payload:', { sub: payload.sub, id: payload.id, exp: payload.exp });
        // Support both user tokens (payload.sub) and admin tokens (payload.id)
        const userId = payload.sub || payload.id;
        // Validate that the userId is a proper ObjectId format
        if (!userId || !mongoose_1.default.Types.ObjectId.isValid(userId)) {
            console.log(`❌ Invalid ObjectId format in token: ${userId}`);
            console.log(`❌ Full payload:`, payload);
            return res.status(401).json({ message: 'Invalid user ID format in token' });
        }
        req.userId = userId;
        next();
    }
    catch (error) {
        console.log(`❌ Token verification failed:`, error.message);
        if (error.name === 'TokenExpiredError') {
            return res.status(401).json({ message: 'Token expired' });
        }
        if (error.name === 'JsonWebTokenError') {
            return res.status(401).json({ message: 'Invalid token format' });
        }
        res.status(401).json({ message: 'Invalid token' });
    }
}
