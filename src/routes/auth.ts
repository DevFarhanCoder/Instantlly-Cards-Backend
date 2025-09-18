import { Router, Request } from "express";
import bcrypt from "bcryptjs";
import jwt from "jsonwebtoken";
import multer from "multer";
import path from "path";
import mongoose from "mongoose";
import User from "../models/User";
import Contact from "../models/Contact";
import Notification from "../models/Notification";
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

// Function to notify contacts when a new user joins
async function notifyContactsOfNewUser(newUserId: string, phoneNumber: string) {
  try {
    // Find all contacts that have this phone number
    const contacts = await Contact.find({ 
      phoneNumber: phoneNumber,
      isAppUser: false // They weren't app users before
    }).populate('userId', 'name');

    if (contacts.length === 0) {
      console.log(`No contacts found for phone number: ${phoneNumber}`);
      return;
    }

    // Get the new user's info
    const newUser = await User.findById(newUserId).select('name phone');
    if (!newUser) {
      console.error(`New user ${newUserId} not found`);
      return;
    }

    // Update all contacts to mark them as app users
    await Contact.updateMany(
      { phoneNumber: phoneNumber },
      { 
        $set: { 
          isAppUser: true, 
          appUserId: new mongoose.Types.ObjectId(newUserId),
          lastSynced: new Date()
        } 
      }
    );

    // Create notifications for all users who had this contact
    const notificationPromises = contacts.map(async (contact) => {
      const ownerName = (contact.userId as any)?.name || 'Unknown';
      
      return Notification.create({
        userId: contact.userId,
        type: 'CONTACT_JOINED',
        title: '🎉 Contact joined InstantllyCards!',
        message: `${contact.name} is now on InstantllyCards`,
        data: {
          contactId: contact._id,
          newUserId: new mongoose.Types.ObjectId(newUserId),
          contactName: contact.name,
          contactPhone: phoneNumber
        }
      });
    });

    await Promise.all(notificationPromises);
    console.log(`Created ${contacts.length} notifications for new user ${newUser.name}`);
  } catch (error) {
    console.error('Error in notifyContactsOfNewUser:', error);
    throw error;
  }
}

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
    const existingPhone = await User.findOne({ phone: normalizedPhone }).lean();
    if (existingPhone) return res.status(409).json({ message: "Phone number already exists" });

    // Check if email exists (if provided)
    if (email) {
      const existingEmail = await User.findOne({ email }).lean();
      if (existingEmail) return res.status(409).json({ message: "Email already exists" });
    }

    const hash = await bcrypt.hash(password, 10);
    const user = await User.create({ 
      name, 
      phone: normalizedPhone,
      password: hash, 
      email: email || ""
    });

    const token = jwt.sign(
      { sub: user._id.toString(), phone: user.phone },
      process.env.JWT_SECRET!,
      { expiresIn: "7d" }
    );

    // After successful registration, notify users who have this phone in their contacts
    try {
      await notifyContactsOfNewUser(user._id.toString(), normalizedPhone);
    } catch (notificationError) {
      console.error("Error sending contact notifications:", notificationError);
      // Don't fail the signup if notifications fail
    }

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
    if ((e as any).code === 11000) {
      // Duplicate key error
      const field = Object.keys((e as any).keyPattern)[0];
      return res.status(409).json({ message: `${field} already exists` });
    }
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

    // IMPORTANT: select the password hash
    const user = await User.findOne({ phone: normalizedPhone }).select("+password");
    if (!user) return res.status(401).json({ message: "Invalid credentials" });

    const ok = await bcrypt.compare(password, user.password);
    if (!ok) return res.status(401).json({ message: "Invalid credentials" });

    const token = jwt.sign(
      { sub: user._id.toString(), phone: user.phone },
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
      about: user.about || "Available",
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
      const existingUser = await User.findOne({ 
        phone: normalizedPhone, 
        _id: { $ne: userId } 
      }).lean();
      
      if (existingUser) {
        return res.status(409).json({ message: "Phone number already exists" });
      }
      
      updateData.phone = normalizedPhone;
    }

    const user = await User.findByIdAndUpdate(
      userId,
      updateData,
      { new: true }
    );

    if (!user) {
      return res.status(404).json({ message: "User not found" });
    }

    // If phone number was updated, trigger contact refresh and notifications
    if (phone !== undefined && userId) {
      try {
        // Notify users who have this phone in their contacts
        await notifyContactsOfNewUser(userId, updateData.phone);
        console.log(`Phone number updated for user ${userId}, notifications sent`);
      } catch (notificationError) {
        console.error("Error sending contact notifications after phone update:", notificationError);
        // Don't fail the update if notifications fail
      }
    }

    res.json({
      _id: user._id,
      name: user.name,
      email: user.email,
      phone: user.phone || "",
      profilePicture: user.profilePicture || "",
      about: user.about || "Available",
    });
  } catch (error) {
    console.error("UPDATE PROFILE ERROR", error);
    if ((error as any).code === 11000) {
      return res.status(409).json({ message: "Phone number already exists" });
    }
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

    const user = await User.findByIdAndUpdate(
      userId,
      { profilePicture: profilePictureUrl },
      { new: true }
    );

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

    const user = await User.findOne({ phone: normalizedPhone }).select('name phone profilePicture');
    
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
