import { Router, Request } from "express";
import bcrypt from "bcryptjs";
import jwt from "jsonwebtoken";
import multer from "multer";
import path from "path";
import User from "../models/User";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

// Configure multer for file uploads
const storage = multer.diskStorage({
  destination: (req, file, cb) => {
    cb(null, 'uploads/profiles/');
  },
  filename: (req, file, cb) => {
    const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9);
    cb(null, `profile-${uniqueSuffix}${path.extname(file.originalname)}`);
  }
});

const upload = multer({ 
  storage,
  limits: { fileSize: 5 * 1024 * 1024 }, // 5MB limit
  fileFilter: (req, file, cb) => {
    if (file.mimetype.startsWith('image/')) {
      cb(null, true);
    } else {
      cb(new Error('Only image files are allowed'));
    }
  }
});

// POST /api/auth/signup
router.post("/signup", async (req, res) => {
  try {
    const { name, phone, password, email } = req.body ?? {};
    if (!name || !phone || !password) {
      return res.status(400).json({ message: "Missing required fields: name, phone, password" });
    }

    // Validate phone number format
    if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
      return res.status(400).json({ message: "Invalid phone number format" });
    }

    // Normalize phone number (remove spaces, dashes, parentheses)
    const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');

    // Check if phone number already exists
    const existingPhone = await User.findOne({ phone: normalizedPhone });
    if (existingPhone) return res.status(409).json({ message: "Phone number already exists" });

    // Handle email if provided
    let finalEmail = undefined;
    if (email && email.trim() !== "") {
      const existingEmail = await User.findOne({ email: email.trim() });
      if (existingEmail) return res.status(409).json({ message: "Email already exists" });
      finalEmail = email.trim();
    }

    // Hash password
    const hashedPassword = await bcrypt.hash(password, 12);

    const user = await User.create({ 
      name, 
      phone: normalizedPhone,
      password: hashedPassword,
      email: finalEmail
    });

    const token = jwt.sign(
      { sub: user._id, phone: user.phone },
      process.env.JWT_SECRET!,
      { expiresIn: "7d" }
    );

    res.status(201).json({
      token,
      user: { 
        id: user._id, 
        name: user.name, 
        phone: user.phone,
        email: user.email 
      },
    });
  } catch (e) {
    console.error("SIGNUP ERROR", e);
    res.status(500).json({ message: "Server error" });
  }
});

// POST /api/auth/login
router.post("/login", async (req, res) => {
  try {
    const { phone, password } = req.body ?? {};
    if (!phone || !password) {
      return res.status(400).json({ message: "Missing fields: phone, password" });
    }

    // Validate phone number format
    if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
      return res.status(400).json({ message: "Invalid phone number format" });
    }

    // Normalize phone number
    const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');

    const user = await User.findOne({ phone: normalizedPhone });
    if (!user) return res.status(401).json({ message: "Invalid credentials" });

    const ok = await bcrypt.compare(password, user.password);
    if (!ok) return res.status(401).json({ message: "Invalid credentials" });

    const token = jwt.sign(
      { sub: user._id, phone: user.phone },
      process.env.JWT_SECRET!,
      { expiresIn: "7d" }
    );

    res.json({
      token,
      user: { 
        id: user._id, 
        name: user.name, 
        phone: user.phone,
        email: user.email 
      },
    });
  } catch (e) {
    console.error("LOGIN ERROR", e);
    res.status(500).json({ message: "Server error" });
  }
});

// GET /api/auth/profile - Get user profile
router.get("/profile", requireAuth, async (req: AuthReq, res) => {
  try {
    console.log("Profile request - User ID:", req.userId);
    const user = await User.findById(req.userId);
    console.log("Found user:", user ? "Yes" : "No");
    
    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    const profileData = {
      _id: user._id,
      name: user.name,
      email: user.email,
      phone: user.phone || "",
      profilePicture: user.profilePicture || "",
      about: (user as any).about || "Available",
    };
    
    console.log("Sending profile data:", profileData);
    res.json(profileData);
  } catch (error) {
    console.error("GET PROFILE ERROR", error);
    res.status(500).json({ message: "Server error" });
  }
});

// PUT /api/auth/update-profile - Update user profile
router.put("/update-profile", requireAuth, async (req: AuthReq, res) => {
  try {
    const { name, phone, about } = req.body;
    const userId = req.userId;

    const updateData: any = {};
    if (name !== undefined) updateData.name = name;
    if (about !== undefined) updateData.about = about;
    
    // Handle phone number update with validation
    if (phone !== undefined) {
      if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
        return res.status(400).json({ message: "Invalid phone number format" });
      }
      
      const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');
      
      // Check if phone number is already taken by another user
      const existingUser = await User.findOne({ phone: normalizedPhone });
      
      if (existingUser && existingUser._id.toString() !== userId) {
        return res.status(409).json({ message: "Phone number already exists" });
      }
      
      updateData.phone = normalizedPhone;
    }

    const user = await User.findByIdAndUpdate(userId, updateData, { new: true });

    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    res.json({
      _id: user._id,
      name: user.name,
      email: user.email,
      phone: user.phone || "",
      profilePicture: user.profilePicture || "",
      about: (user as any).about || "Available",
    });
  } catch (error) {
    console.error("UPDATE PROFILE ERROR", error);
    res.status(500).json({ message: "Server error" });
  }
});

// POST /api/auth/upload-profile-picture - Upload profile picture
router.post("/upload-profile-picture", requireAuth, upload.single('profilePicture'), async (req: AuthReq, res) => {
  try {
    if (!req.file) {
      return res.status(400).json({ message: "No file uploaded" });
    }

    const userId = req.userId;
    const profilePictureUrl = `/uploads/profiles/${req.file.filename}`;

    const user = await User.findByIdAndUpdate(userId, { profilePicture: profilePictureUrl }, { new: true });

    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    res.json({
      profilePicture: profilePictureUrl,
      message: "Profile picture updated successfully"
    });
  } catch (error) {
    console.error("UPLOAD PROFILE PICTURE ERROR", error);
    res.status(500).json({ message: "Server error" });
  }
});

// POST /api/auth/check-phone - Check if phone number is registered
router.post("/check-phone", async (req, res) => {
  try {
    const { phone } = req.body ?? {};
    if (!phone) {
      return res.status(400).json({ message: "Phone number is required" });
    }

    // Validate phone number format
    if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
      return res.status(400).json({ message: "Invalid phone number format" });
    }

    // Normalize phone number
    const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');

    const user = await User.findOne({ phone: normalizedPhone });
    
    res.json({
      exists: !!user,
      user: user ? {
        name: user.name,
        phone: user.phone,
        profilePicture: user.profilePicture
      } : null
    });
  } catch (error) {
    console.error("CHECK PHONE ERROR", error);
    res.status(500).json({ message: "Server error" });
  }
});

export default router;