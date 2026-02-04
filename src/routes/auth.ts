import { Router, Request } from "express";
import bcrypt from "bcryptjs";
import jwt from "jsonwebtoken";
import multer from "multer";
import path from "path";
import mongoose from "mongoose";
import axios from "axios";
import User from "../models/User";
import Contact from "../models/Contact";
import Transaction from "../models/Transaction";
import Card from "../models/Card";
import { requireAuth, AuthReq } from "../middleware/auth";
import { sendContactJoinedNotification } from "../services/pushNotifications";
import { otpService } from "../services/otpService";

const router = Router();

// Helper function to generate unique referral code
function generateReferralCode(): string {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let code = '';
  for (let i = 0; i < 8; i++) {
    code += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return code;
}

// Normalize phone into a canonical stored form: '+<countrycode><number>' (no spaces)
function canonicalPhone(phone: string): string {
  if (!phone) return phone;
  let p = phone.toString().trim();
  // remove spaces, dashes, parentheses
  p = p.replace(/[\s\-\(\)]/g, '');
  if (!p.startsWith('+')) {
    p = '+' + p;
  }
  return p;
}

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
    const reqId = req.get('x-req-id') || req.get('X-REQ-ID') || 'no-req-id';
    console.log(`🔖 [REQ:${reqId}] 🚀 Starting simple signup process...`);
    
    const { name, phone, password, referralCode } = req.body;
    
    console.log(`🔖 [REQ:${reqId}] 📝 Raw signup data received:`, {
      name: name || 'undefined',
      phone: phone || 'undefined', 
      password: password ? '***' + password.slice(-2) : 'undefined',
      referralCode: referralCode || 'none',
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
    const cleanReferralCode = referralCode?.trim().toUpperCase();
    
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

    // Check if referral code is valid (if provided)
    let referrer = null;
    if (cleanReferralCode) {
      referrer = await User.findOne({ referralCode: cleanReferralCode });
      if (!referrer) {
        console.log('❌ Invalid referral code:', cleanReferralCode);
        return res.status(400).json({ 
          message: 'Invalid referral code' 
        });
      }
      console.log('✅ Valid referral code from:', referrer.name);
    }

    // Hash password
    console.log('🔐 Hashing password...');
    const hashedPassword = await bcrypt.hash(cleanPassword, 12);

    // Generate unique referral code for new user
    let newReferralCode = generateReferralCode();
    let codeExists = await User.findOne({ referralCode: newReferralCode });
    while (codeExists) {
      newReferralCode = generateReferralCode();
      codeExists = await User.findOne({ referralCode: newReferralCode });
    }

    // Set credits expiry date to 1 month from signup
    const creditsExpiryDate = new Date();
    creditsExpiryDate.setMonth(creditsExpiryDate.getMonth() + 1);

    // Create user data object with 500,000 credits valid for 1 month
    const userData: any = {
      name: cleanName,
      phone: cleanPhone,
      password: hashedPassword,
      credits: 500000, // 5 lac credits
      creditsExpiryDate: creditsExpiryDate, // Expire after 1 month
      referralCode: newReferralCode
    };

    if (referrer) {
      userData.referredBy = referrer._id;
    }

    console.log('👤 Creating new user with data:', { 
      name: userData.name, 
      phone: userData.phone, 
      hasPassword: !!userData.password,
      credits: userData.credits,
      referralCode: userData.referralCode
    });

    // Create and save user
    const user = new User(userData);
    const savedUser = await user.save();

    console.log('✅ User created successfully with ID:', savedUser._id);

    // Create a single idempotent default card for the new user (use name + phone)
    // Robust behavior:
    // - Search by both ObjectId and string forms of userId (some records store string)
    // - Save userId as string when creating the card to avoid mismatches
    // - If card creation fails, return a fallback defaultCard object so client can
    //   immediately show a card UI with name + phone while background repairs occur
    let defaultCard = null;
    try {
      const userIdCandidates = [savedUser._id, savedUser._id.toString()];
      const existingCard = await Card.findOne({ userId: { $in: userIdCandidates } }).lean();
      if (existingCard) {
        console.log(`🔖 [REQ:${reqId}] ℹ️ Default card already exists for user, skipping creation:`, savedUser._id);
        defaultCard = existingCard;
      } else {
        const phoneDigits = (savedUser.phone || '').replace(/\D/g, '');
        const cardData: any = {
          userId: savedUser._id.toString(),
          name: savedUser.name || cleanName,
          personalPhone: phoneDigits,
        };

        // Create card; ensure created object is converted to plain object for response
        const createdCard = await Card.create(cardData);
        defaultCard = (createdCard && typeof createdCard.toObject === 'function') ? createdCard.toObject() : createdCard;
        console.log(`🔖 [REQ:${reqId}] 🆕 Default card created for user:`, savedUser._id, 'phone:', phoneDigits);
      }
    } catch (cardError) {
      console.error(`🔖 [REQ:${reqId}] ❌ Failed to ensure default card for new user:`, cardError);
      // Fallback: construct a minimal defaultCard object so frontend can display
      try {
        const phoneDigits = (savedUser.phone || '').replace(/\D/g, '');
        defaultCard = {
          _id: null,
          userId: savedUser._id.toString(),
          name: savedUser.name || cleanName,
          personalPhone: phoneDigits,
          isFallback: true
        };
        console.log(`🔖 [REQ:${reqId}] ⚠️ Using fallback defaultCard for response (client will see a provisional card)`);
      } catch (fallbackError) {
        console.error(`🔖 [REQ:${reqId}] ❌ Failed to create fallback defaultCard:`, fallbackError);
        defaultCard = null;
      }
    }

    // Create signup bonus transaction
    await Transaction.create({
      type: 'signup_bonus',
      toUser: savedUser._id,
      amount: 500000,
      description: 'Signup bonus - 5 lac credits',
      balanceBefore: 0,
      balanceAfter: 500000,
      status: 'completed'
    });

    // 🎴 AUTO-CREATE FIRST CARD: Create a default card with name and phone number
    try {
      console.log('🎴 Creating default card for new user...');
      
      // Extract country code and phone number from fullPhone
      let personalCountryCode = '';
      let personalPhone = '';
      
      if (cleanPhone.startsWith('+')) {
        // Extract country code (e.g., +91 from +919876543210)
        const phoneWithoutPlus = cleanPhone.substring(1);
        if (phoneWithoutPlus.startsWith('91') && phoneWithoutPlus.length === 12) {
          // Indian number
          personalCountryCode = '91';
          personalPhone = phoneWithoutPlus.substring(2);
        } else if (phoneWithoutPlus.startsWith('1') && phoneWithoutPlus.length === 11) {
          // US/Canada number
          personalCountryCode = '1';
          personalPhone = phoneWithoutPlus.substring(1);
        } else {
          // Generic: take first 2-3 digits as country code
          const match = phoneWithoutPlus.match(/^(\d{1,3})(\d{7,})$/);
          if (match) {
            personalCountryCode = match[1];
            personalPhone = match[2];
          }
        }
      }
      
      const defaultCard = await Card.create({
        userId: savedUser._id.toString(),
        name: cleanName,
        personalCountryCode: personalCountryCode,
        personalPhone: personalPhone,
        // All other fields will use default empty values from the schema
        gender: '',
        email: '',
        location: '',
        mapsLink: '',
        companyName: '',
        designation: '',
        companyCountryCode: '',
        companyPhone: '',
        companyEmail: '',
        companyWebsite: '',
        companyAddress: '',
        companyMapsLink: '',
        message: '',
        companyPhoto: '',
        linkedin: '',
        twitter: '',
        instagram: '',
        facebook: '',
        youtube: '',
        whatsapp: '',
        telegram: ''
      });
      
      console.log('✅ Default card created successfully with ID:', defaultCard._id);
      console.log('📇 Card details - Name:', defaultCard.name, 'Phone:', `+${personalCountryCode}${personalPhone}`);
    } catch (cardError) {
      console.error('⚠️ Failed to create default card:', cardError);
      // Don't fail signup if card creation fails
    }

    // If referred by someone, give referrer 20% bonus (100,000 credits)
    if (referrer) {
      const referralBonus = 100000; // 20% of 500,000
      referrer.credits = (referrer.credits || 0) + referralBonus;
      await referrer.save();

      // Create referral bonus transaction
      await Transaction.create({
        type: 'referral_bonus',
        fromUser: savedUser._id,
        toUser: referrer._id,
        amount: referralBonus,
        description: `Referral bonus - ${cleanName} joined using your code`,
        balanceBefore: (referrer.credits || 0) - referralBonus,
        balanceAfter: referrer.credits,
        status: 'completed'
      });

      console.log(`💰 Referral bonus of ${referralBonus} credits given to ${referrer.name}`);
    }

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
        sub: savedUser._id.toString(), 
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
      about: (savedUser as any).about || "Available",
      credits: savedUser.credits,
      referralCode: savedUser.referralCode
    };

    console.log(`🔖 [REQ:${reqId}] 🎉 Signup successful for phone:`, cleanPhone);

    // Return token, user and default card info to help client show cards immediately
    res.status(201).json({
      token,
      user: userResponse,
      defaultCard: defaultCard || null
    });
    console.log(`🔖 [REQ:${reqId}] ✅ Response sent for signup - defaultCard present:`, !!defaultCard);

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
    console.log("Login attempt - Headers:", { 
      contentType: req.get('content-type'),
      contentLength: req.get('content-length'),
      userAgent: req.get('user-agent')
    });
    console.log("Login attempt - Raw body:", req.body);
    console.log("Login attempt - Request body:", { phone: req.body?.phone, hasPassword: !!req.body?.password });
    
    // Check if body exists at all
    if (!req.body || Object.keys(req.body).length === 0) {
      console.log("❌ Empty request body received!");
      return res.status(400).json({ 
        message: "No data received. Please check your internet connection and try again.",
        debug: {
          bodyExists: !!req.body,
          bodyKeys: req.body ? Object.keys(req.body) : [],
          contentType: req.get('content-type')
        }
      });
    }
    
    const { phone, password } = req.body ?? {};
    if (!phone || !password) {
      console.log("Missing fields in login request - phone:", phone, "password:", !!password);
      console.log("Body keys:", Object.keys(req.body));
      console.log("All body values:", JSON.stringify(req.body));
      return res.status(400).json({ 
        message: "Phone number and password are required"
      });
    }

    // Validate phone number format
    if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
      console.log("Invalid phone format:", phone);
      return res.status(400).json({ message: "Invalid phone number format" });
    }

    // Normalize phone number - ensure + prefix for international format
    let normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');
    if (!normalizedPhone.startsWith('+')) {
      normalizedPhone = '+' + normalizedPhone;
    }
    console.log("Looking for user with normalized phone:", normalizedPhone);

    // Try several phone formats to be tolerant of how phone was stored in DB
    const phoneVariants = [
      normalizedPhone, // +911234567890
      normalizedPhone.replace(/^\+/, ''), // 911234567890
      normalizedPhone.replace(/^\+91/, ''), // local 10-digit (9123456789)
      (normalizedPhone.startsWith('+') ? normalizedPhone : '+' + normalizedPhone)
    ].filter(Boolean).map(p => p.toString());

    console.log('Login - phone variants to try:', phoneVariants);

    const user = await User.findOne({ phone: { $in: phoneVariants } }).select('+password');
    if (!user) {
      console.log("User not found for any variant of phone:", phoneVariants);
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
        about: (user as any).about || "Available",
        credits: (user as any).credits || 0,
        referralCode: (user as any).referralCode
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
      credits: (user as any).credits || 0,
      creditsExpiryDate: (user as any).creditsExpiryDate,
      referralCode: (user as any).referralCode,
      gender: (user as any).gender,
      birthdate: (user as any).birthdate,
      anniversary: (user as any).anniversary
    };
    
    console.log("Sending profile data:", profileData);
    console.log("Credits value:", (user as any).credits);
    console.log("Credits expiry:", (user as any).creditsExpiryDate);
    
    // Return with 'user' wrapper for frontend compatibility
    res.json({ user: profileData });
  } catch (error) {
    console.error("GET PROFILE ERROR", error);
    res.status(500).json({ message: "Server error" });
  }
});

// PUT /api/auth/update-profile - Update user profile (including Base64 profile picture)
router.put("/update-profile", requireAuth, async (req: AuthReq, res) => {
  try {
    const { name, phone, about, profilePicture, birthdate, anniversary, gender } = req.body;
    const userId = req.userId;

    console.log('📝 Update profile request:', {
      userId,
      hasName: !!name,
      hasPhone: !!phone,
      hasAbout: !!about,
      hasProfilePicture: !!profilePicture,
      hasBirthdate: !!birthdate,
      hasAnniversary: !!anniversary,
      hasGender: !!gender,
      birthdateValue: birthdate,
      anniversaryValue: anniversary,
      genderValue: gender,
      profilePictureLength: profilePicture?.length,
      profilePicturePrefix: profilePicture?.substring(0, 30)
    });

    const updateData: any = {};
    if (name !== undefined) updateData.name = name;
    if (about !== undefined) updateData.about = about;
    if (gender !== undefined) updateData.gender = gender;
    
    // Handle date fields
    if (birthdate !== undefined) {
      console.log('📅 Processing birthdate:', birthdate, 'Type:', typeof birthdate);
      updateData.birthdate = birthdate ? new Date(birthdate) : null;
      console.log('📅 Converted birthdate to:', updateData.birthdate);
    }
    if (anniversary !== undefined) {
      console.log('💍 Processing anniversary:', anniversary, 'Type:', typeof anniversary);
      updateData.anniversary = anniversary ? new Date(anniversary) : null;
      console.log('💍 Converted anniversary to:', updateData.anniversary);
    }
    
    console.log('💾 Final updateData:', updateData);
    
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
// router.post("/check-phone", async (req, res) => {
//   try {
//     const { phone, appHash } = req.body ?? {};
//     if (!phone) {
//       return res.status(400).json({ message: "Phone number is required" });
//     }

//     // Validate phone number format
//     if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
//       return res.status(400).json({ message: "Invalid phone number format" });
//     }
//     if (!appHash) {
//       return res.status(400).json({ message: "App hash is required" });
//     }
//     console.log("[CHECK-PHONE] 📩 Received appHash:", appHash);

//     // Normalize phone number
//     const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');

//     const user = await User.findOne({ phone: normalizedPhone });
    
//     // If user exists, return without sending OTP
//     if (user) {
//       return res.json({
//         exists: true,
//         user: {
//           name: user.name,
//           phone: user.phone,
//           profilePicture: user.profilePicture
//         }
//       });
//     }

//     // User doesn't exist - this is a signup attempt
//     // Send OTP via Fast2SMS
//     console.log(`[CHECK-PHONE] 📱 New signup - sending OTP to ${normalizedPhone}`);
    
//     // Generate 6-digit OTP
//     const otp = Math.floor(100000 + Math.random() * 900000).toString();
    
//     // Store OTP in cache (expires in 5 minutes)
//     otpService.storeOTP(normalizedPhone, otp);
    
//     // Get Fast2SMS API key from environment
//     const fast2smsApiKey = process.env.FAST2SMS_API_KEY;
    
//     if (!fast2smsApiKey) {
//       console.error('[CHECK-PHONE] ❌ FAST2SMS_API_KEY not set in environment');
//       return res.status(500).json({ 
//         message: 'OTP service not configured. Please contact support.' 
//       });
//     }
    
//     // Remove +91 prefix for Fast2SMS (expects 10-digit Indian number)
//     const cleanPhone = normalizedPhone.replace(/^\+91/, '').replace(/\D/g, '');
    
//     if (cleanPhone.length !== 10) {
//       return res.status(400).json({ 
//         message: 'Invalid Indian phone number. Must be 10 digits.' 
//       });
//     }
    
//     console.log(`[CHECK-PHONE] 🔑 Generated OTP: ${otp} for ${cleanPhone}`);
//     console.log(`[CHECK-PHONE] 📤 Calling Fast2SMS API (DLT route)...`);
    
//     // Send OTP via Fast2SMS using DLT/Quick SMS route (not OTP route)
//     try {
//       // Fast2SMS Quick SMS API format (works without website verification)
//       const finalAppHash = appHash?.trim() || "";
//       const message = `<#> ${otp} is your verification code for Instantlly Cards. Valid for 5 minutes. Do not share with anyone. ${finalAppHash}`;
      
//       const fast2smsPayload = new URLSearchParams({
//         authorization: fast2smsApiKey,
//         sender_id: 'FSTSMS',  // Default sender ID
//         message: message,
//         language: 'english',
//         route: 'q',  // Quick SMS route (instead of 'otp')
//         numbers: cleanPhone
//       });

//       const fast2smsResponse = await axios.get(
//         `https://www.fast2sms.com/dev/bulkV2?${fast2smsPayload.toString()}`,
//         {
//           headers: {
//             'Cache-Control': 'no-cache'
//           },
//           timeout: 10000 // 10 second timeout
//         }
//       );
      
//       console.log(`[CHECK-PHONE] ✅ Fast2SMS response:`, fast2smsResponse.data);
      
//       if (!fast2smsResponse.data.return) {
//         console.error(`[CHECK-PHONE] ❌ Fast2SMS API error:`, fast2smsResponse.data);
//         throw new Error('Failed to send OTP via Fast2SMS');
//       }
      
//       return res.json({
//         exists: false,
//         user: null,
//         otpSent: true,
//         message: 'OTP sent successfully to your phone number'
//       });
      
//     } catch (fast2smsError: any) {
//       console.error(`[CHECK-PHONE] ❌ Fast2SMS API error:`, fast2smsError.message);
//       console.error(`[CHECK-PHONE] 📋 Error response:`, fast2smsError.response?.data);
//       console.error(`[CHECK-PHONE] 📊 Status:`, fast2smsError.response?.status);
      
//       // Still return success to user, OTP is stored in cache for testing
//       return res.json({
//         exists: false,
//         user: null,
//         otpSent: true,
//         message: 'OTP sent successfully',
//         _debug: process.env.NODE_ENV === 'development' ? { otp } : undefined
//       });
//     }
    
//   } catch (error) {
//     console.error("CHECK PHONE ERROR", error);
//     res.status(500).json({ message: "Server error" });
//   }
// });
router.post("/check-phone", async (req, res) => {
  try {
    const { phone, appHash } = req.body ?? {};

    if (!phone) {
      return res.status(400).json({ message: "Phone number is required" });
    }

    if (!/^\+?[\d\s\-\(\)]{10,15}$/.test(phone)) {
      return res.status(400).json({ message: "Invalid phone number format" });
    }

    const finalAppHash = (appHash || "").trim();
    console.log("[CHECK-PHONE] 📩 Received appHash:", finalAppHash);

    const normalizedPhone = phone.replace(/[\s\-\(\)]/g, '');
    const user = await User.findOne({ phone: normalizedPhone });

    if (user) {
      return res.json({
        exists: true,
        user: {
          name: user.name,
          phone: user.phone,
          profilePicture: user.profilePicture
        }
      });
    }

    console.log(`[CHECK-PHONE] 📱 New signup - sending OTP to ${normalizedPhone}`);

    const otp = Math.floor(100000 + Math.random() * 900000).toString();
    otpService.storeOTP(normalizedPhone, otp);

    const fast2smsApiKey = process.env.FAST2SMS_API_KEY;
    if (!fast2smsApiKey) {
      return res.status(500).json({ message: "OTP service not configured" });
    }

    const cleanPhone = normalizedPhone.replace(/^\+91/, '').replace(/\D/g, '');
    if (cleanPhone.length !== 10) {
      return res.status(400).json({ message: "Invalid Indian phone number" });
    }

    console.log(`[CHECK-PHONE] 🔑 Generated OTP: ${otp} for ${cleanPhone}`);

    try {
      const message = `<#> ${otp} is your verification code for Instantlly Cards.
${finalAppHash}`;

      const fast2smsPayload = new URLSearchParams({
        authorization: fast2smsApiKey,
        sender_id: 'FSTSMS',
        message,
        language: 'english',
        route: 'q',
        numbers: cleanPhone
      });

      const fast2smsResponse = await axios.get(
        `https://www.fast2sms.com/dev/bulkV2?${fast2smsPayload.toString()}`,
        {
          headers: { 'Cache-Control': 'no-cache' },
          timeout: 10000
        }
      );

      return res.json({
        exists: false,
        user: null,
        otpSent: true,
        message: "OTP sent successfully"
      });

    } catch (err) {
      return res.json({
        exists: false,
        user: null,
        otpSent: true,
        message: "OTP sent",
        _debug: process.env.NODE_ENV === "development" ? { otp } : undefined
      });
    }

  } catch (error) {
    console.error("CHECK PHONE ERROR", error);
    res.status(500).json({ message: "Server error" });
  }
});

// POST /api/auth/send-reset-otp - Send OTP for password reset if phone is registered
router.post("/send-reset-otp", async (req, res) => {
  try {
    const { phone } = req.body ?? {};
    if (!phone) {
      return res.status(400).json({ message: 'Phone number is required' });
    }

    // Normalize phone number to canonical form for storage and lookup
    const normalizedPhone = canonicalPhone(phone);

    // Try to find user by several phone variants (tolerant lookup)
    const phoneVariants = [
      normalizedPhone, // +911234567890
      normalizedPhone.replace(/^\+/, ''), // 911234567890
      normalizedPhone.replace(/^\+91/, ''), // local 10-digit 1234567890
      normalizedPhone.replace(/^\+/, '')?.replace(/^91/, '') // fallback
    ].filter(Boolean);

    const user = await User.findOne({ phone: { $in: phoneVariants } });

    if (!user) {
      // Phone not registered
      return res.status(404).json({ message: 'Phone number is not registered / account not created' });
    }

    // At this point user exists - generate and send OTP
    const otp = Math.floor(100000 + Math.random() * 900000).toString();
    // Store OTP in cache (expires in 5 minutes) under canonical phone
    otpService.storeOTP(normalizedPhone, otp);

    const fast2smsApiKey = process.env.FAST2SMS_API_KEY;

    // Prepare 10-digit number for Fast2SMS (remove +91 if present)
    const cleanPhone = normalizedPhone.replace(/^\+91/, '').replace(/\D/g, '');
    console.log('[SEND-RESET-OTP] Final number sent to Fast2SMS (10-digit):', cleanPhone);
    if (cleanPhone.length !== 10) {
      console.warn('[SEND-RESET-OTP] Non-Indian number, but OTP stored in cache for normalized phone');
    }

    const message = `${otp} is your password reset code for Instantlly Cards. Valid for 5 minutes. Do not share with anyone.`;

    // If Fast2SMS API key is not configured, fallback to development mode: store OTP and
    // return success with debug info so local testing can continue without SMS provider.
    if (!fast2smsApiKey) {
      console.warn('[SEND-RESET-OTP] FAST2SMS_API_KEY not set - running in dev fallback (OTP stored, no SMS will be sent)');
      return res.json({ otpSent: true, message: 'OTP stored (no SMS sent)', _debug: process.env.NODE_ENV === 'development' ? { otp } : undefined });
    }

    try {
      const fast2smsPayload = new URLSearchParams({
        authorization: fast2smsApiKey,
        sender_id: 'FSTSMS',
        message,
        language: 'english',
        route: 'q',
        numbers: cleanPhone
      });

      const fast2smsResponse = await axios.get(
        `https://www.fast2sms.com/dev/bulkV2?${fast2smsPayload.toString()}`,
        { headers: { 'Cache-Control': 'no-cache' }, timeout: 10000 }
      );

      console.log('[SEND-RESET-OTP] Fast2SMS response:', fast2smsResponse.data);
      if (!fast2smsResponse.data?.return) {
        console.error('[SEND-RESET-OTP] Fast2SMS error:', fast2smsResponse.data);
        // still return success since OTP is stored for testing
        return res.json({ otpSent: true, message: 'OTP sent (stored) but SMS provider returned an error' });
      }

      return res.json({ otpSent: true, message: 'OTP sent successfully' });
    } catch (fastErr: any) {
      console.error('[SEND-RESET-OTP] Fast2SMS request failed:', fastErr?.message || fastErr);
      // Return success with debug in dev to allow testing
      return res.json({ otpSent: true, message: 'OTP sent (stored)', _debug: process.env.NODE_ENV === 'development' ? { otp } : undefined });
    }
  } catch (error) {
    console.error('SEND-RESET-OTP ERROR', error);
    res.status(500).json({ message: 'Server error' });
  }
});

// POST /api/auth/verify-otp - Verify OTP sent to phone number
router.post("/verify-otp", async (req, res) => {
  try {
    const { phone, otp } = req.body;

    console.log(`[VERIFY-OTP] 🔐 Verification request for ${phone}`);

    if (!phone || !otp) {
      return res.status(400).json({
        success: false,
        message: 'Phone and OTP are required'
      });
    }

    // Normalize phone to canonical form for verification
    const normalizedPhone = canonicalPhone(phone);

    // Verify OTP using otpService (stored under canonical phone)
    const isValid = otpService.verifyOTP(normalizedPhone, otp);

    if (!isValid) {
      console.log(`[VERIFY-OTP] ❌ Invalid OTP for ${normalizedPhone}`);
      return res.status(400).json({
        success: false,
        message: 'Invalid or expired OTP. Please try again.'
      });
    }

    console.log(`[VERIFY-OTP] ✅ OTP verified successfully for ${normalizedPhone}`);

    // Generate a short-lived reset token so frontend can call reset-password securely
    if (!process.env.JWT_SECRET) {
      console.error('[VERIFY-OTP] JWT_SECRET not configured - cannot generate reset token');
      return res.status(500).json({ success: true, message: 'OTP verified', verified: true, phone: normalizedPhone });
    }

    const resetToken = jwt.sign(
      { phone: normalizedPhone, purpose: 'reset' },
      process.env.JWT_SECRET as string,
      { expiresIn: '15m' }
    );

    return res.json({
      success: true,
      message: 'OTP verified successfully',
      verified: true,
      phone: normalizedPhone,
      resetToken
    });

  } catch (error) {
    console.error("[VERIFY-OTP ERROR]", error);
    res.status(500).json({
      success: false,
      message: 'Server error'
    });
  }
});

// POST /api/auth/change-password - Change password for authenticated user
router.post("/change-password", requireAuth, async (req: AuthReq, res) => {
  try {
    const { oldPassword, newPassword } = req.body ?? {};

    if (!oldPassword || !newPassword) {
      return res.status(400).json({ message: 'Old password and new password are required' });
    }

    if (typeof newPassword !== 'string' || newPassword.length < 6) {
      return res.status(400).json({ message: 'New password must be at least 6 characters long' });
    }

    // Load user with password
    const user = await User.findById(req.userId).select('+password');
    if (!user) return res.status(404).json({ message: 'User not found' });

    if (!user.password) {
      return res.status(400).json({ message: 'No existing password set for this account' });
    }

    const match = await bcrypt.compare(oldPassword, user.password);
    if (!match) {
      return res.status(401).json({ message: 'Current password is incorrect' });
    }

    const hashed = await bcrypt.hash(newPassword, 12);
    user.password = hashed;
    await user.save();

    res.json({ message: 'Password changed successfully' });
  } catch (error) {
    console.error('CHANGE PASSWORD ERROR', error);
    res.status(500).json({ message: 'Server error' });
  }
});

// POST /api/auth/reset-password - Reset password using a short-lived reset token
router.post("/reset-password", async (req, res) => {
  try {
    const { resetToken, newPassword } = req.body ?? {};

    if (!resetToken || !newPassword) {
      return res.status(400).json({ message: 'Reset token and new password are required' });
    }

    if (!process.env.JWT_SECRET) {
      console.error('[RESET-PASSWORD] JWT_SECRET not configured');
      return res.status(500).json({ message: 'Server configuration error' });
    }

    let payload: any;
    try {
      payload = jwt.verify(resetToken, process.env.JWT_SECRET as string) as any;
    } catch (err: any) {
      console.error('[RESET-PASSWORD] Invalid or expired reset token', err?.message || err);
      return res.status(400).json({ message: 'Invalid or expired reset token' });
    }

    if (!payload?.phone || payload?.purpose !== 'reset') {
      return res.status(400).json({ message: 'Invalid reset token payload' });
    }

    const normalizedPhone = payload.phone;

    // Find user by phone (tolerant variants)
    const phoneVariants = [
      normalizedPhone,
      normalizedPhone.replace(/^\+/, ''),
      normalizedPhone.replace(/^\+91/, ''),
      normalizedPhone.replace(/^\+/, '')?.replace(/^91/, '')
    ].filter(Boolean);

    const user = await User.findOne({ phone: { $in: phoneVariants } }).select('+password');
    if (!user) return res.status(404).json({ message: 'User not found' });

    if (typeof newPassword !== 'string' || newPassword.length < 6) {
      return res.status(400).json({ message: 'New password must be at least 6 characters long' });
    }

    const hashed = await bcrypt.hash(newPassword, 12);
    user.password = hashed;
    await user.save();

    return res.json({ message: 'Password reset successfully' });
  } catch (error) {
    console.error('[RESET-PASSWORD] ERROR', error);
    res.status(500).json({ message: 'Server error' });
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
    
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log(`📱 [VERSION-CHECK] Request received`);
    console.log(`   Version: ${version}`);
    console.log(`   Platform: ${platform}`);
    console.log(`   User-Agent: ${req.get('user-agent')}`);
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
    
    // ⚠️ FORCE UPDATE POLICY: Everyone must have the LATEST version
    // Simply update these versions when you publish to app stores
    const LATEST_VERSIONS = {
      android: "1.0.68",  // ← Update this when publishing new version to Play Store
      ios: "1.0.68"    // ← Update this when publishing new version to App Store
    };

    const PLAY_STORE_URL = "https://play.google.com/store/apps/details?id=com.instantllycards.www.twa";
    const APP_STORE_URL = "https://apps.apple.com/app/YOUR_APP_ID"; // Update with your iOS app ID

    if (!version || !platform) {
      console.log('❌ [VERSION-CHECK] Missing version or platform');
      return res.status(400).json({
        success: false,
        message: "Version and platform are required"
      });
    }

    const requestedVersion = version as string;
    const requestedPlatform = platform as string;
    const latestVersion = LATEST_VERSIONS[requestedPlatform as keyof typeof LATEST_VERSIONS];

    if (!latestVersion) {
      console.log('⚠️ [VERSION-CHECK] Platform not configured:', requestedPlatform);
      return res.json({
        success: true,
        updateRequired: false,
        message: "Platform not configured"
      });
    }

    // Force update if user's version is NOT the latest version
    // Even 1.0.23 will be forced to update to 1.0.24
    const isUpdateRequired = compareVersions(requestedVersion, latestVersion) < 0;

    console.log(`🔍 [VERSION-CHECK] Comparison: ${requestedVersion} vs ${latestVersion} (latest)`);
    console.log(`⚠️ [VERSION-CHECK] Update required: ${isUpdateRequired}`);
    console.log(`📋 [VERSION-CHECK] Policy: Must have latest version`);

    const responseData = {
      success: true,
      updateRequired: isUpdateRequired,
      currentVersion: requestedVersion,
      minimumVersion: latestVersion, // No separate minimum - latest IS the minimum
      latestVersion: latestVersion,
      updateUrl: requestedPlatform === 'android' ? PLAY_STORE_URL : APP_STORE_URL,
      message: isUpdateRequired 
        ? `Please update to version ${latestVersion} to continue using the app`
        : "You are using the latest version"
    };

    console.log('📤 [VERSION-CHECK] Response:', JSON.stringify(responseData, null, 2));
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');

    res.json(responseData);
  } catch (error) {
    console.error("❌ [VERSION-CHECK] ERROR", error);
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