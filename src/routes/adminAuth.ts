import express, { Request, Response } from 'express';
import jwt from 'jsonwebtoken';
import Admin from '../models/Admin';

const router = express.Router();

const JWT_SECRET = process.env.JWT_SECRET || 'your-secret-key-change-in-production';

// Admin Login
router.post('/login', async (req: Request, res: Response) => {
  try {
    const { username, password } = req.body;

    if (!username || !password) {
      return res.status(400).json({
        success: false,
        message: 'Username and password are required',
      });
    }

    // Find admin by username
    const admin = await Admin.findOne({ username });

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
    const token = jwt.sign(
      {
        id: String(admin._id),
        username: admin.username,
        role: admin.role,
      },
      JWT_SECRET,
      { expiresIn: '7d' }
    );

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
  } catch (error: any) {
    console.error('Admin login error:', error);
    res.status(500).json({
      success: false,
      message: 'Server error during login',
    });
  }
});

// Create First Admin (only works if no admins exist)
router.post('/create-first-admin', async (req: Request, res: Response) => {
  try {
    const { username, email, password } = req.body;

    // Check if any admin already exists
    const existingAdmins = await Admin.countDocuments();

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
    const admin = new Admin({
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
  } catch (error: any) {
    console.error('Create first admin error:', error);
    res.status(500).json({
      success: false,
      message: error.message || 'Server error during admin creation',
    });
  }
});

// Verify Token (for dashboard to check if logged in)
router.get('/verify', async (req: Request, res: Response) => {
  try {
    const token = req.headers.authorization?.replace('Bearer ', '');

    if (!token) {
      return res.status(401).json({
        success: false,
        message: 'No token provided',
      });
    }

    const decoded: any = jwt.verify(token, JWT_SECRET);
    
    const admin = await Admin.findById(decoded.id).select('-password');

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
  } catch (error: any) {
    res.status(401).json({
      success: false,
      message: 'Invalid token',
    });
  }
});

export default router;
