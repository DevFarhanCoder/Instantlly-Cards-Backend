"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const jsonwebtoken_1 = __importDefault(require("jsonwebtoken"));
const Admin_1 = __importDefault(require("../models/Admin"));
const router = express_1.default.Router();
const JWT_SECRET = process.env.JWT_SECRET || 'your-secret-key-change-in-production';
// Admin Login
router.post('/login', async (req, res) => {
    try {
        const { username, password } = req.body;
        if (!username || !password) {
            return res.status(400).json({
                success: false,
                message: 'Username and password are required',
            });
        }
        // Find admin by username
        const admin = await Admin_1.default.findOne({ username });
        if (!admin) {
            return res.status(401).json({
                success: false,
                message: 'Invalid credentials',
            });
        }
        // Check if admin is active
        if (!admin.isActive) {
            return res.status(403).json({
                success: false,
                message: 'Your account has been deactivated',
            });
        }
        // Compare password
        const isPasswordValid = await admin.comparePassword(password);
        if (!isPasswordValid) {
            return res.status(401).json({
                success: false,
                message: 'Invalid credentials',
            });
        }
        // Update last login
        admin.lastLogin = new Date();
        await admin.save();
        // Generate JWT token
        const token = jsonwebtoken_1.default.sign({
            id: String(admin._id),
            username: admin.username,
            role: admin.role,
        }, JWT_SECRET, { expiresIn: '7d' });
        res.json({
            success: true,
            message: 'Login successful',
            data: {
                token,
                admin: {
                    id: String(admin._id),
                    username: admin.username,
                    email: admin.email,
                    role: admin.role,
                },
            },
        });
    }
    catch (error) {
        console.error('Admin login error:', error);
        res.status(500).json({
            success: false,
            message: 'Server error during login',
        });
    }
});
// Create First Admin (only works if no admins exist)
router.post('/create-first-admin', async (req, res) => {
    try {
        const { username, email, password } = req.body;
        // Check if any admin already exists
        const existingAdmins = await Admin_1.default.countDocuments();
        if (existingAdmins > 0) {
            return res.status(403).json({
                success: false,
                message: 'Admin already exists. Contact super admin to create new admins.',
            });
        }
        if (!username || !email || !password) {
            return res.status(400).json({
                success: false,
                message: 'Username, email, and password are required',
            });
        }
        if (password.length < 6) {
            return res.status(400).json({
                success: false,
                message: 'Password must be at least 6 characters',
            });
        }
        // Create first super admin
        const admin = new Admin_1.default({
            username,
            email,
            password,
            role: 'super_admin',
            isActive: true,
        });
        await admin.save();
        res.status(201).json({
            success: true,
            message: 'First admin created successfully',
            data: {
                username: admin.username,
                email: admin.email,
                role: admin.role,
            },
        });
    }
    catch (error) {
        console.error('Create first admin error:', error);
        res.status(500).json({
            success: false,
            message: error.message || 'Server error during admin creation',
        });
    }
});
// Verify Token (for dashboard to check if logged in)
router.get('/verify', async (req, res) => {
    try {
        const token = req.headers.authorization?.replace('Bearer ', '');
        if (!token) {
            return res.status(401).json({
                success: false,
                message: 'No token provided',
            });
        }
        const decoded = jsonwebtoken_1.default.verify(token, JWT_SECRET);
        const admin = await Admin_1.default.findById(decoded.id).select('-password');
        if (!admin || !admin.isActive) {
            return res.status(401).json({
                success: false,
                message: 'Invalid or inactive admin',
            });
        }
        res.json({
            success: true,
            data: {
                admin: {
                    id: String(admin._id),
                    username: admin.username,
                    email: admin.email,
                    role: admin.role,
                },
            },
        });
    }
    catch (error) {
        res.status(401).json({
            success: false,
            message: 'Invalid token',
        });
    }
});
exports.default = router;
