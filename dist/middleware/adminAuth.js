"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.requireAdminAuth = requireAdminAuth;
const jsonwebtoken_1 = __importDefault(require("jsonwebtoken"));
const JWT_SECRET = process.env.JWT_SECRET || 'your-secret-key-change-in-production';
function requireAdminAuth(req, res, next) {
    const authHeader = req.header('authorization') || '';
    const token = authHeader.startsWith('Bearer ') ? authHeader.slice(7) : undefined;
    if (!token) {
        return res.status(401).json({
            success: false,
            message: 'Missing authentication token'
        });
    }
    try {
        const payload = jsonwebtoken_1.default.verify(token, JWT_SECRET);
        // Set admin info on request
        req.adminId = payload.id;
        req.adminUsername = payload.username;
        req.adminRole = payload.role;
        next();
    }
    catch (error) {
        console.log(`❌ Admin token verification failed:`, error);
        res.status(401).json({
            success: false,
            message: 'Invalid or expired token'
        });
    }
}
