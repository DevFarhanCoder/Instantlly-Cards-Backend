// Firebase-based authentication routes
import { Router } from "express";
import jwt from "jsonwebtoken";
import User from "../models/User";
import Contact from "../models/Contact";
import Transaction from "../models/Transaction";
import CreditConfig from "../models/CreditConfig";
// Force language server refresh
import { initializeFirebase } from "../services/firebase";
import admin from "firebase-admin";
import { sendContactJoinedNotification } from "../services/pushNotifications";

const router = Router();

// Initialize Firebase on route load (optional - only used if Firebase auth is needed)
// Primary authentication uses Fast2SMS OTP
initializeFirebase();

// Helper function to generate unique referral code
function generateReferralCode(): string {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let code = '';
  for (let i = 0; i < 8; i++) {
    code += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return code;
}

// POST /api/auth/firebase-auth
// Handles both signup and login for Firebase-authenticated users
router.post("/firebase-auth", async (req, res) => {
  try {
    console.log('🔥 Firebase authentication request received');
    
    const { firebaseToken, name, referralCode } = req.body;

    if (!firebaseToken) {
      return res.status(400).json({
        success: false,
        message: 'Firebase token is required'
      });
    }

    // Verify Firebase token
    let decodedToken;
    try {
      decodedToken = await admin.auth().verifyIdToken(firebaseToken);
      console.log('✅ Firebase token verified:', {
        uid: decodedToken.uid,
        phone: decodedToken.phone_number
      });
    } catch (error: any) {
      console.error('❌ Firebase token verification failed:', error);
      return res.status(401).json({
        success: false,
        message: 'Invalid Firebase token',
        error: error.message
      });
    }

    const firebaseUid = decodedToken.uid;
    const phoneNumber = decodedToken.phone_number;

    if (!phoneNumber) {
      return res.status(400).json({
        success: false,
        message: 'Phone number not found in Firebase token'
      });
    }

    // Check if user exists by Firebase UID or phone number
    let user = await User.findOne({
      $or: [
        { firebaseUid },
        { phone: phoneNumber }
      ]
    });

    // If user exists - LOGIN
    if (user) {
      console.log('👤 Existing user found:', user.name);

      // Update Firebase UID if not set
      if (!user.firebaseUid) {
        user.firebaseUid = firebaseUid;
        await user.save();
        console.log('🔄 Updated user with Firebase UID');
      }

      // Generate JWT token
      const token = jwt.sign(
        { sub: user._id.toString(), phone: user.phone },
        process.env.JWT_SECRET!,
        { expiresIn: "365d" }
      );

      return res.json({
        success: true,
        isNewUser: false,
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
          creditsExpiryDate: (user as any).creditsExpiryDate,
          referralCode: (user as any).referralCode
        }
      });
    }

    // If user doesn't exist - SIGNUP
    console.log('🆕 New user signup via Firebase');

    if (!name?.trim()) {
      return res.status(400).json({
        success: false,
        message: 'Name is required for new users'
      });
    }

    const cleanName = name.trim();
    const cleanReferralCode = referralCode?.trim().toUpperCase();

    // Check if referral code is valid (if provided)
    let referrer = null;
    if (cleanReferralCode) {
      referrer = await User.findOne({ referralCode: cleanReferralCode });
      if (!referrer) {
        return res.status(400).json({
          success: false,
          message: 'Invalid referral code'
        });
      }
      console.log('✅ Valid referral code from:', referrer.name);
    }

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

    // Get credit amounts from config
    let creditConfig = await CreditConfig.findOne();
    if (!creditConfig) {
      creditConfig = await CreditConfig.create({
        signupBonus: 200,
        referralReward: 300,
        lastUpdatedBy: 'system',
        lastUpdatedAt: new Date()
      });
    }
    const signupCredits = creditConfig.signupBonus;

    // Create user data object
    const userData: any = {
      name: cleanName,
      phone: phoneNumber,
      firebaseUid: firebaseUid,
      credits: signupCredits,
      creditsExpiryDate: creditsExpiryDate,
      referralCode: newReferralCode
    };

    if (referrer) {
      userData.referredBy = referrer._id;
    }

    console.log('👤 Creating new Firebase user:', {
      name: cleanName,
      phone: phoneNumber,
      firebaseUid
    });

    // Create and save user
    user = new User(userData);
    const savedUser = await user.save();

    console.log('✅ User created successfully with ID:', savedUser._id);

    // Create signup bonus transaction
    await Transaction.create({
      type: 'signup_bonus',
      toUser: savedUser._id,
      amount: signupCredits,
      description: `Signup bonus - ${signupCredits} credits`,
      balanceBefore: 0,
      balanceAfter: signupCredits,
      status: 'completed'
    });

    // If referred by someone, give referrer the configured bonus
    if (referrer) {
      const referralBonus = creditConfig.referralReward;
      referrer.credits = (referrer.credits || 0) + referralBonus;
      await referrer.save();

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

    // Notify contacts who have this phone number
    try {
      const contactsWithThisNumber = await Contact.find({
        phoneNumber: phoneNumber,
        isAppUser: false
      }).populate('userId', 'name pushToken');

      if (contactsWithThisNumber.length > 0) {
        console.log(`📱 Found ${contactsWithThisNumber.length} users who have ${phoneNumber} in their contacts`);

        await Contact.updateMany(
          { phoneNumber: phoneNumber },
          {
            $set: {
              isAppUser: true,
              appUserId: savedUser._id,
              lastSynced: new Date()
            }
          }
        );

        for (const contact of contactsWithThisNumber) {
          const contactOwner = contact.userId as any;
          if (contactOwner && contactOwner.pushToken && contactOwner.pushToken !== 'expo-go-local-mode') {
            try {
              await sendContactJoinedNotification(
                contactOwner.pushToken,
                cleanName,
                phoneNumber,
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
    }

    // Generate JWT token
    const token = jwt.sign(
      { sub: savedUser._id.toString(), phone: savedUser.phone },
      process.env.JWT_SECRET!,
      { expiresIn: "365d" }
    );

    res.json({
      success: true,
      isNewUser: true,
      token,
      user: {
        id: savedUser._id,
        _id: savedUser._id,
        name: savedUser.name,
        phone: savedUser.phone,
        email: savedUser.email,
        profilePicture: savedUser.profilePicture || "",
        about: (savedUser as any).about || "Available",
        credits: (savedUser as any).credits || 0,
        creditsExpiryDate: (savedUser as any).creditsExpiryDate,
        referralCode: (savedUser as any).referralCode
      }
    });
  } catch (error: any) {
    console.error('❌ Firebase auth error:', error);
    
    // Handle duplicate key errors
    if (error.code === 11000) {
      const duplicateField = Object.keys(error.keyPattern || {})[0];
      
      if (duplicateField === 'phone') {
        return res.status(409).json({
          success: false,
          message: 'Phone number already registered'
        });
      }
    }

    res.status(500).json({
      success: false,
      message: 'An error occurred during authentication',
      error: error.message
    });
  }
});

export default router;
