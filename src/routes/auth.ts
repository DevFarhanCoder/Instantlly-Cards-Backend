import { Router, Request } from "express";
import bcrypt from "bcryptjs";
import jwt from "jsonwebtoken";
import multer from "multer";
import path from "path";
import mongoose from "mongoose";
import User from "../models/User";
import Contact from "../models/Contact";
import { requireAuth, AuthReq } from "../middleware/auth";
import { sendContactJoinedNotification } from "../services/pushNotifications";

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
    console.log('🚀 Starting simple signup process...');
    
    const { name, phone, password } = req.body;
    
    console.log('📝 Raw signup data received:', {
      name: name || 'undefined',
      phone: phone || 'undefined', 
      password: password ? '***' + password.slice(-2) : 'undefined',
      bodyKeys: Object.keys(req.body)
    });

    // Validate environment variables
    if (!process.env.JWT_SECRET || !process.env.MONGODB_URI) {
      console.error('❌ Missing environment variables');
      return res.status(500).json({ 
        message: 'Server configuration error' 
      });
    }

    // Check database connection
    if (mongoose.connection.readyState !== 1) {
      console.error('❌ Database not connected');
      return res.status(503).json({ 
        message: 'Database connection unavailable. Please try again.' 
      });
    }

    // Validate required fields - ONLY name, phone, password
    if (!name?.trim() || !phone?.trim() || !password?.trim()) {
      console.log('❌ Missing required fields');
      return res.status(400).json({ 
        message: 'Name, phone, and password are required'
      });
    }

    // Clean input data
    const cleanPhone = phone.trim();
    const cleanName = name.trim();
    const cleanPassword = password.trim();
    
    // Validate phone format
    if (!/^\+[1-9]\d{1,14}$/.test(cleanPhone)) {
      console.log('❌ Invalid phone format:', cleanPhone);
      return res.status(400).json({ 
        message: 'Phone number must be in international format (e.g., +1234567890)' 
      });
    }

    // Check if user already exists by phone
    console.log('🔍 Checking if phone already exists:', cleanPhone);
    const existingUser = await User.findOne({ phone: cleanPhone });
    if (existingUser) {
      console.log('❌ Phone already exists - User found:', {
        id: existingUser._id,
        name: existingUser.name,
        phone: existingUser.phone,
        hasEmail: !!existingUser.email
      });
      return res.status(409).json({ 
        message: 'Phone number already registered' 
      });
    }
    console.log('✅ Phone number is available');

    // Hash password
    console.log('🔐 Hashing password...');
    const hashedPassword = await bcrypt.hash(cleanPassword, 12);

    // Create user data object - ONLY name, phone, password
    const userData = {
      name: cleanName,
      phone: cleanPhone,
      password: hashedPassword
    };

    console.log('👤 Creating new user with data:', { 
      name: userData.name, 
      phone: userData.phone, 
      hasPassword: !!userData.password 
    });

    // Create and save user
    const user = new User(userData);
    const savedUser = await user.save();

    console.log('✅ User created successfully with ID:', savedUser._id);

    // Notify all contacts who have this user's phone number in their contact list
    try {
      const contactsWithThisNumber = await Contact.find({
        phoneNumber: cleanPhone,
        isAppUser: false // They saved this contact before user joined
      }).populate('userId', 'name pushToken');

      if (contactsWithThisNumber.length > 0) {
        console.log(`📱 Found ${contactsWithThisNumber.length} users who have ${cleanPhone} in their contacts`);
        
        // Update all these contacts to mark user as app user
        await Contact.updateMany(
          { phoneNumber: cleanPhone },
          { 
            $set: { 
              isAppUser: true, 
              appUserId: savedUser._id,
              lastSynced: new Date()
            }
          }
        );

        // Send notifications
        for (const contact of contactsWithThisNumber) {
          const contactOwner = contact.userId as any;
          if (contactOwner && contactOwner.pushToken && contactOwner.pushToken !== 'expo-go-local-mode') {
            try {
              await sendContactJoinedNotification(
                contactOwner.pushToken,
                cleanName,
                cleanPhone,
                savedUser._id.toString()
              );
              console.log(`📱 Sent "contact joined" notification to ${contactOwner.name}`);
            } catch (error) {
              console.error(`Failed to send notification to ${contactOwner.name}:`, error);
            }
          }
        }
      }
    } catch (error) {
      console.error('Error sending contact joined notifications:', error);
      // Don't fail signup if notifications fail
    }

    // Generate JWT token
    const token = jwt.sign(
      { 
        sub: savedUser._id, 
        phone: savedUser.phone,
        name: savedUser.name 
      },
      process.env.JWT_SECRET as string,
      { expiresIn: '365d' }
    );

    // Prepare response (exclude password)
    const userResponse = {
      id: savedUser._id,
      _id: savedUser._id,
      name: savedUser.name,
      phone: savedUser.phone,
      profilePicture: savedUser.profilePicture || "",
      about: (savedUser as any).about || "Available"
    };

    console.log('🎉 Signup successful for phone:', cleanPhone);

    res.status(201).json({
      token,
      user: userResponse
    });

  } catch (error: any) {
    console.error('💥 Signup error:', error);
    console.error('💥 Error details:', {
      name: error.name,
      code: error.code,
      message: error.message,
      keyValue: error.keyValue,
      keyPattern: error.keyPattern,
      stack: error.stack?.split('\n')[0]
    });
    
    // Handle MongoDB duplicate key errors
    if (error.code === 11000) {
      const duplicateField = Object.keys(error.keyValue || {})[0];
      
      console.log('❌ Duplicate key error details:', { 
        field: duplicateField,
        value: error.keyValue?.[duplicateField],
        keyPattern: error.keyPattern
      });
      
      if (duplicateField === 'phone') {
        return res.status(409).json({ 
          message: 'Phone number already registered' 
        });
      }
      
      return res.status(409).json({ 
        message: 'This information is already registered' 
      });
    }

    // Handle validation errors
    if (error.name === 'ValidationError') {
      console.log('❌ Validation error:', error.message);
      return res.status(400).json({ 
        message: 'Invalid data provided'
      });
    }

    // Generic server error
    console.error('❌ Unexpected error during signup:', error);
    res.status(500).json({ 
      message: 'An unexpected error occurred. Please try again.'
    });
  }
});

// POST /api/auth/login
router.post("/login", async (req, res) => {
  try {
    console.log("Login attempt - Request body:", { phone: req.body?.phone, hasPassword: !!req.body?.password });
    
    const { phone, password } = req.body ?? {};
    if (!phone || !password) {
      console.log("Missing fields in login request");
      return res.status(400).json({ message: "Missing fields: phone, password" });
    }

    // Validate phone number format
    if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
      console.log("Invalid phone format:", phone);
      return res.status(400).json({ message: "Invalid phone number format" });
    }

    // Normalize phone number
    const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');
    console.log("Looking for user with normalized phone:", normalizedPhone);

    const user = await User.findOne({ phone: normalizedPhone }).select('+password');
    if (!user) {
      console.log("User not found for phone:", normalizedPhone);
      return res.status(401).json({ message: "Invalid credentials" });
    }

    console.log("User found:", user.name);
    console.log("User password exists:", !!user.password);
    console.log("User password type:", typeof user.password);

    if (!user.password) {
      console.log("User has no password stored in database!");
      return res.status(401).json({ message: "Invalid credentials" });
    }

    const ok = await bcrypt.compare(password, user.password);
    if (!ok) {
      console.log("Password mismatch for user:", user.name);
      return res.status(401).json({ message: "Invalid credentials" });
    }

    console.log("Password verified. JWT_SECRET exists:", !!process.env.JWT_SECRET);
    
    if (!process.env.JWT_SECRET) {
      console.error("JWT_SECRET is not set!");
      return res.status(500).json({ message: "Server configuration error" });
    }

    const token = jwt.sign(
      { sub: user._id.toString(), phone: user.phone },
      process.env.JWT_SECRET,
      { expiresIn: "365d" } // 1 year expiration instead of 24 hours
    );

    console.log("Token generated successfully for user:", user.name);

    res.json({
      token,
      user: { 
        id: user._id, 
        _id: user._id,
        name: user.name, 
        phone: user.phone,
        email: user.email,
        profilePicture: user.profilePicture || "",
        about: (user as any).about || "Available"
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

// PUT /api/auth/update-profile - Update user profile (including Base64 profile picture)
router.put("/update-profile", requireAuth, async (req: AuthReq, res) => {
  try {
    const { name, phone, about, profilePicture } = req.body;
    const userId = req.userId;

    console.log('📝 Update profile request:', {
      userId,
      hasName: !!name,
      hasPhone: !!phone,
      hasAbout: !!about,
      hasProfilePicture: !!profilePicture,
      profilePictureLength: profilePicture?.length,
      profilePicturePrefix: profilePicture?.substring(0, 30)
    });

    const updateData: any = {};
    if (name !== undefined) updateData.name = name;
    if (about !== undefined) updateData.about = about;
    
    // Handle Base64 profile picture
    if (profilePicture !== undefined) {
      if (profilePicture.startsWith('data:image/')) {
        // Store Base64 directly in MongoDB
        console.log('🖼️ Storing profile picture as Base64 in MongoDB, length:', profilePicture.length);
        updateData.profilePicture = profilePicture;
      } else if (profilePicture === '' || profilePicture === null) {
        // Allow clearing profile picture
        console.log('🗑️ Clearing profile picture');
        updateData.profilePicture = '';
      } else {
        // Keep existing URL format for backward compatibility
        console.log('🔗 Storing profile picture URL');
        updateData.profilePicture = profilePicture;
      }
    }
    
    console.log('💾 Update data:', {
      ...updateData,
      profilePicture: updateData.profilePicture ? `${updateData.profilePicture.substring(0, 30)}... (${updateData.profilePicture.length} chars)` : undefined
    });
    
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

    console.log('✅ User updated:', {
      _id: user._id,
      name: user.name,
      profilePictureLength: user.profilePicture?.length,
      profilePicturePrefix: user.profilePicture?.substring(0, 30)
    });

    // If profile picture was updated, update all contacts who have this user
    if (profilePicture !== undefined) {
      try {
        console.log('🔄 Updating profile picture in contacts collection...');
        await Contact.updateMany(
          { appUserId: userId },
          { $set: { profilePicture: updateData.profilePicture } }
        );
        console.log('✅ Profile picture updated in contacts');
      } catch (contactError) {
        console.error('⚠️ Error updating contacts:', contactError);
        // Continue anyway - user profile is updated
      }
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

// GET /api/users/search-by-phone/:phone - Search user by phone number for carousel messaging
router.get("/users/search-by-phone/:phone", async (req, res) => {
  try {
    const { phone } = req.params;
    
    if (!phone) {
      return res.status(400).json({ 
        success: false,
        message: "Phone number is required" 
      });
    }

    console.log(`🔍 Searching for user with phone: ${phone}`);

    // Normalize phone number - remove spaces, dashes, parentheses
    const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');
    
    // Try different phone number formats
    const phonePatterns = [
      normalizedPhone,                                    // As is: 9867477227
      `+91${normalizedPhone}`,                           // With +91: +919867477227
      normalizedPhone.replace(/^\+91/, ''),              // Remove +91 if present: 9867477227
      normalizedPhone.replace(/^91/, ''),                // Remove 91 prefix: 9867477227
    ];

    console.log(`📱 Trying phone patterns:`, phonePatterns);

    // Search for user with any of these phone number formats
    const user = await User.findOne({
      phone: { $in: phonePatterns }
    }).select('_id name phone profilePicture about');

    if (!user) {
      console.log(`❌ User not found with phone: ${phone}`);
      return res.status(404).json({ 
        success: false,
        message: "User not found with this phone number" 
      });
    }

    console.log(`✅ User found:`, {
      id: user._id,
      name: user.name,
      phone: user.phone
    });

    res.json({
      success: true,
      user: {
        _id: user._id,
        name: user.name,
        phone: user.phone,
        profilePicture: user.profilePicture,
        about: user.about
      }
    });
  } catch (error) {
    console.error("SEARCH USER BY PHONE ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error while searching for user" 
    });
  }
});

// GET /api/users/:userIdOrPhone - Fetch user by MongoDB ID or phone number
router.get("/users/:userIdOrPhone", async (req, res) => {
  try {
    const { userIdOrPhone } = req.params;
    
    if (!userIdOrPhone) {
      return res.status(400).json({ 
        success: false,
        message: "User ID or phone number is required" 
      });
    }

    console.log(`🔍 Fetching user with ID or phone: ${userIdOrPhone}`);

    let user;

    // Check if it's a valid MongoDB ObjectId (24 hex characters)
    const isValidObjectId = /^[0-9a-fA-F]{24}$/.test(userIdOrPhone);

    if (isValidObjectId) {
      // Fetch by MongoDB _id
      user = await User.findById(userIdOrPhone).select('_id name phone profilePicture about');
      console.log(`📇 Searched by ObjectId: ${userIdOrPhone}`, user ? '✅ Found' : '❌ Not found');
    } else {
      // Assume it's a phone number - normalize and search
      const normalizedPhone = userIdOrPhone.replace(/[\s\-\(\)]/g, '');
      
      const phonePatterns = [
        normalizedPhone,
        `+91${normalizedPhone}`,
        normalizedPhone.replace(/^\+91/, ''),
        normalizedPhone.replace(/^91/, ''),
      ];

      console.log(`📱 Searched by phone patterns:`, phonePatterns);

      user = await User.findOne({
        phone: { $in: phonePatterns }
      }).select('_id name phone profilePicture about');
      
      console.log(`📞 Searched by phone: ${userIdOrPhone}`, user ? '✅ Found' : '❌ Not found');
    }

    if (!user) {
      console.log(`❌ User not found with identifier: ${userIdOrPhone}`);
      return res.status(404).json({ 
        success: false,
        message: "User not found" 
      });
    }

    console.log(`✅ User fetched successfully:`, {
      id: user._id,
      name: user.name,
      phone: user.phone
    });

    res.json({
      success: true,
      user: {
        _id: user._id,
        name: user.name,
        phone: user.phone,
        profilePicture: user.profilePicture,
        about: user.about
      }
    });
  } catch (error) {
    console.error("FETCH USER ERROR", error);
    res.status(500).json({ 
      success: false,
      message: "Server error while fetching user" 
    });
  }
});

// GET /api/users/version-check - Check if app version is supported
router.get("/version-check", async (req, res) => {
  try {
    const { version, platform } = req.query;
    
    console.log(`📱 Version check request - Version: ${version}, Platform: ${platform}`);
    
    // Minimum supported versions for force update
    const MIN_SUPPORTED_VERSIONS = {
      android: "1.0.15",  // Minimum supported version - users below this will be forced to update
      ios: "1.0.15"       // Keep this at 1.0.15 to allow older versions
    };

    // Latest versions available in app stores
    const LATEST_VERSIONS = {
      android: "1.0.20",  // Current latest version (update when publishing to Play Store)
      ios: "1.0.20"       // Current latest version (update when publishing to App Store)
    };

    const PLAY_STORE_URL = "https://play.google.com/store/apps/details?id=com.instantllycards.www.twa";
    const APP_STORE_URL = "https://apps.apple.com/app/YOUR_APP_ID"; // Update with your iOS app ID

    if (!version || !platform) {
      return res.status(400).json({
        success: false,
        message: "Version and platform are required"
      });
    }

    const requestedVersion = version as string;
    const requestedPlatform = platform as string;
    const minVersion = MIN_SUPPORTED_VERSIONS[requestedPlatform as keyof typeof MIN_SUPPORTED_VERSIONS];
    const latestVersion = LATEST_VERSIONS[requestedPlatform as keyof typeof LATEST_VERSIONS];

    if (!minVersion || !latestVersion) {
      return res.json({
        success: true,
        updateRequired: false,
        message: "Platform not configured"
      });
    }

    // Compare versions (simple string comparison works for semantic versioning)
    const isUpdateRequired = compareVersions(requestedVersion, minVersion) < 0;

    console.log(`🔍 Version comparison: ${requestedVersion} vs ${minVersion} - Update required: ${isUpdateRequired}`);
    console.log(`📱 Latest available version: ${latestVersion}`);

    res.json({
      success: true,
      updateRequired: isUpdateRequired,
      currentVersion: requestedVersion,
      minimumVersion: minVersion,
      latestVersion: latestVersion,
      updateUrl: requestedPlatform === 'android' ? PLAY_STORE_URL : APP_STORE_URL,
      message: isUpdateRequired 
        ? "Please update to the latest version to continue using the app"
        : "You are using the latest version"
    });
  } catch (error) {
    console.error("VERSION CHECK ERROR", error);
    res.status(500).json({
      success: false,
      updateRequired: false, // Don't block users on error
      message: "Error checking version"
    });
  }
});

// Helper function to compare semantic versions (e.g., "1.0.16" vs "1.0.15")
function compareVersions(version1: string, version2: string): number {
  const v1Parts = version1.split('.').map(Number);
  const v2Parts = version2.split('.').map(Number);
  
  for (let i = 0; i < Math.max(v1Parts.length, v2Parts.length); i++) {
    const v1Part = v1Parts[i] || 0;
    const v2Part = v2Parts[i] || 0;
    
    if (v1Part > v2Part) return 1;
    if (v1Part < v2Part) return -1;
  }
  
  return 0; // Versions are equal
}

export default router;