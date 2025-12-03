import express from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import User from "../models/User";
import Contact from "../models/Contact";
import Notification from "../models/Notification";

const router = express.Router();


router.get("/count", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    const total = await Contact.countDocuments({ userId });
    res.status(200).json({ total });
  } catch (error) {
    console.error("Error counting contacts:", error);
    res.status(500).json({ error: "Failed to count contacts" });
  }
});

// Helper function to create notifications when a contact joins the app
const createContactJoinedNotification = async (userId: string, contactName: string, contactUserId: string) => {
  try {
    const notification = new Notification({
      userId,
      type: 'contact_joined',
      title: 'Contact Joined InstantllyCards',
      message: `${contactName} is now on InstantllyCards! Say hello.`,
      data: {
        contactUserId,
        contactName
      },
      read: false
    });
    
    await notification.save();
    console.log(`Created notification for user ${userId}: ${contactName} joined`);
  } catch (error) {
    console.error('Error creating contact joined notification:', error);
  }
};

// Contact model (you might want to create a separate model)
interface DeviceContact {
  name: string;
  phoneNumber: string;
}

interface AppContact {
  _id: string;
  name: string;
  phoneNumber: string;
  profilePicture?: string;
  isAppUser: boolean;
  lastMessage?: string;
  lastMessageTime?: string;
  unreadCount?: number;
}

// Sync contacts and find app users
router.post("/sync", requireAuth, async (req: AuthReq, res) => {
  try {
    const { contacts } = req.body as { contacts: DeviceContact[] };
    const userId = req.userId;
    
    if (!contacts || !Array.isArray(contacts)) {
      return res.status(400).json({ error: "Invalid contacts data" });
    }

    // Normalize phone numbers for consistent matching
    const normalizedContacts = contacts.map(contact => ({
      name: contact.name,
      phoneNumber: contact.phoneNumber.replace(/[\s\-\(\)]/g, '') // Remove formatting
    }));

    // Extract phone numbers from contacts
    const phoneNumbers = normalizedContacts.map(contact => contact.phoneNumber);
    
    // Find users in the app with these phone numbers
    const appUsers = await User.find({
      phone: { $in: phoneNumbers },
      _id: { $ne: userId } // Exclude current user
    }).select('name phone profilePicture about');

    // Create a map of phone numbers to users
    const phoneToUserMap = new Map();
    appUsers.forEach(user => {
      if (user.phone) {
        phoneToUserMap.set(user.phone, user);
      }
    });

    // Match contacts with app users
    const syncedContacts = normalizedContacts.map(contact => {
      const appUser = phoneToUserMap.get(contact.phoneNumber);
      return {
        name: appUser ? appUser.name : contact.name,
        phoneNumber: contact.phoneNumber,
        isAppUser: !!appUser,
        _id: appUser ? appUser._id : `contact_${contact.phoneNumber}`,
        profilePicture: appUser?.profilePicture
      };
    }).filter(contact => contact.isAppUser); // Only return app users

    res.json({ 
      success: true, 
      message: `Found ${syncedContacts.length} contacts using the app`,
      data: syncedContacts 
    });
  } catch (error) {
    console.error("Error syncing contacts:", error);
    res.status(500).json({ error: "Failed to sync contacts" });
  }
});

// Get total number of contacts for the authenticated user


// Get app users from synced contacts
router.get("/app-users", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    
    // For now, return all users except current user
    // In a real app, you'd return only synced contacts
    const appUsers = await User.find({
      _id: { $ne: userId }
    }).select('name phone profilePicture about').limit(50);

    const contacts: AppContact[] = appUsers.map(user => ({
      _id: user._id.toString(),
      name: user.name || 'Unknown User',
      phoneNumber: user.phone || '',
      profilePicture: user.profilePicture,
      isAppUser: true,
      lastMessage: "Tap to start messaging",
      unreadCount: 0
    }));

    res.json({ success: true, data: contacts });
  } catch (error) {
    console.error("Error fetching app users:", error);
    res.status(500).json({ error: "Failed to fetch contacts" });
  }
});

// Sync all contacts (including non-app users) and store them in database
router.post("/sync-all", requireAuth, async (req: AuthReq, res) => {
  try {
    const { contacts } = req.body as { contacts: DeviceContact[] };
    const userId = req.userId;
    
    console.log(`📞 [${userId}] Syncing ${contacts?.length || 0} contacts`);
    
    if (!contacts || !Array.isArray(contacts)) {
      return res.status(400).json({ error: "Invalid contacts data" });
    }

    const startTime = Date.now();

    // Normalize phone numbers for consistent matching
    const normalizedContacts = contacts.map(contact => ({
      name: contact.name,
      phoneNumber: contact.phoneNumber.replace(/[\s\-\(\)]/g, '') // Remove formatting
    }));

    // Extract phone numbers from contacts and create variations for matching
    const phoneNumbers = normalizedContacts.map(contact => contact.phoneNumber);
    
    // Create all possible phone number variations for matching
    const allPhoneVariations: string[] = [];
    phoneNumbers.forEach(phone => {
      allPhoneVariations.push(phone); // Original: 9326664680
      if (phone.startsWith('91') && phone.length === 12) {
        allPhoneVariations.push('+' + phone); // Add +: +919326664680
      } else if (!phone.startsWith('91') && phone.length === 10) {
        allPhoneVariations.push('91' + phone); // Add 91: 919326664680
        allPhoneVariations.push('+91' + phone); // Add +91: +919326664680
      }
    });
    
    // Find users in the app with these phone numbers (check all variations) - OPTIMIZED
    const appUsers = await User.find({
      phone: { $in: allPhoneVariations },
      _id: { $ne: userId } // Exclude current user
    })
    .select('name phone profilePicture about')
    .lean()
    .exec();

    // Create a map of phone numbers to users (normalize for comparison)
    const phoneToUserMap = new Map();
    appUsers.forEach(user => {
      if (user.phone) {
        const normalizedUserPhone = user.phone.replace(/[\s\-\(\)\+]/g, ''); // Remove + and formatting
        phoneToUserMap.set(normalizedUserPhone, user);
        
        // Also map original contact phone format
        phoneNumbers.forEach(contactPhone => {
          const normalizedContactPhone = contactPhone.replace(/[\s\-\(\)\+]/g, '');
          if (normalizedUserPhone === normalizedContactPhone || 
              normalizedUserPhone === '91' + normalizedContactPhone ||
              normalizedUserPhone === normalizedContactPhone.substring(2)) { // Remove country code
            phoneToUserMap.set(contactPhone, user);
          }
        });
      }
    });

    // Prepare contacts for bulk upsert
    const contactsToSave = normalizedContacts.map(contact => {
      const appUser = phoneToUserMap.get(contact.phoneNumber);
      return {
        updateOne: {
          filter: { userId, phoneNumber: contact.phoneNumber },
          update: {
            $set: {
              name: contact.name,
              phoneNumber: contact.phoneNumber,
              isAppUser: !!appUser,
              appUserId: appUser ? appUser._id : undefined,
              lastSynced: new Date()
            },
            $setOnInsert: {
              userId,
              createdAt: new Date()
            }
          },
          upsert: true
        }
      };
    });

    // Bulk upsert contacts - OPTIMIZED
    if (contactsToSave.length > 0) {
      await Contact.bulkWrite(contactsToSave, { ordered: false });
    }

    // Return summary
    const appUserContacts = normalizedContacts.filter(contact => {
      return phoneToUserMap.has(contact.phoneNumber);
    });

    const elapsed = Date.now() - startTime;
    console.log(`✅ [${userId}] Synced ${normalizedContacts.length} contacts in ${elapsed}ms - ${appUserContacts.length} app users found`);

    res.json({ 
      success: true, 
      message: `Synced ${normalizedContacts.length} contacts`,
      stats: {
        total: normalizedContacts.length,
        appUsers: appUserContacts.length,
        nonAppUsers: normalizedContacts.length - appUserContacts.length,
        syncTimeMs: elapsed
      }
    });
  } catch (error) {
    console.error("❌ Error syncing all contacts:", error);
    res.status(500).json({ error: "Failed to sync contacts" });
  }
});

// INCREMENTAL SYNC - Only check for new app users among existing contacts
router.post("/sync-incremental", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    const startTime = Date.now();
    
    console.log(`🔄 [${userId}] Incremental contact sync started`);
    
    // Get all existing contacts for this user
    const existingContacts = await Contact.find({ userId })
      .select('phoneNumber isAppUser appUserId')
      .lean()
      .exec();
    
    if (existingContacts.length === 0) {
      return res.json({
        success: true,
        message: "No contacts to update. Please do a full sync first.",
        stats: { updated: 0, newAppUsers: 0 }
      });
    }
    
    // Extract phone numbers
    const phoneNumbers = existingContacts.map(c => c.phoneNumber);
    
    // Create all possible phone number variations
    const allPhoneVariations: string[] = [];
    phoneNumbers.forEach(phone => {
      allPhoneVariations.push(phone);
      if (phone.startsWith('91') && phone.length === 12) {
        allPhoneVariations.push('+' + phone);
      } else if (!phone.startsWith('91') && phone.length === 10) {
        allPhoneVariations.push('91' + phone);
        allPhoneVariations.push('+91' + phone);
      }
    });
    
    // Find app users with these phone numbers
    const appUsers = await User.find({
      phone: { $in: allPhoneVariations },
      _id: { $ne: userId }
    })
    .select('name phone profilePicture about')
    .lean()
    .exec();
    
    // Create phone to user map
    const phoneToUserMap = new Map();
    appUsers.forEach(user => {
      if (user.phone) {
        const normalizedUserPhone = user.phone.replace(/[\s\-\(\)\+]/g, '');
        phoneNumbers.forEach(contactPhone => {
          const normalizedContactPhone = contactPhone.replace(/[\s\-\(\)\+]/g, '');
          if (normalizedUserPhone === normalizedContactPhone || 
              normalizedUserPhone === '91' + normalizedContactPhone ||
              normalizedUserPhone === normalizedContactPhone.substring(2)) {
            phoneToUserMap.set(contactPhone, user);
          }
        });
      }
    });
    
    // Update contacts that became app users
    const updateOperations = existingContacts
      .filter(contact => {
        const appUser = phoneToUserMap.get(contact.phoneNumber);
        return appUser && !contact.isAppUser; // Only update if newly became app user
      })
      .map(contact => {
        const appUser = phoneToUserMap.get(contact.phoneNumber);
        return {
          updateOne: {
            filter: { userId, phoneNumber: contact.phoneNumber },
            update: {
              $set: {
                isAppUser: true,
                appUserId: appUser._id,
                lastSynced: new Date()
              }
            }
          }
        };
      });
    
    let newAppUsersCount = 0;
    if (updateOperations.length > 0) {
      await Contact.bulkWrite(updateOperations, { ordered: false });
      newAppUsersCount = updateOperations.length;
    }
    
    const elapsed = Date.now() - startTime;
    console.log(`✅ [${userId}] Incremental sync complete in ${elapsed}ms - ${newAppUsersCount} new app users found`);
    
    res.json({
      success: true,
      message: `Incremental sync complete`,
      stats: {
        total: existingContacts.length,
        updated: newAppUsersCount,
        newAppUsers: newAppUsersCount,
        syncTimeMs: elapsed
      }
    });
  } catch (error) {
    console.error("❌ Error in incremental sync:", error);
    res.status(500).json({ error: "Failed to perform incremental sync" });
  }
});



// Get all stored contacts for the user (with pagination for large contact lists) - OPTIMIZED WITH CACHING
router.get("/all", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    const startTime = Date.now();
    
    // Pagination parameters
    const page = parseInt(req.query.page as string) || 1;
    const limit = parseInt(req.query.limit as string) || 1000; // Default 1000 contacts per page
    const skip = (page - 1) * limit;
    
    // Search query parameter
    const searchQuery = req.query.search as string;
    
    console.log(`📋 [${userId}] Fetching contacts - page ${page}, search: ${searchQuery || 'none'}`);
    
    // Build query
    const query: any = { userId };
    if (searchQuery) {
      query.name = { $regex: searchQuery, $options: 'i' }; // Case-insensitive search
    }
    
    // Run count and query in parallel for better performance
    const [totalContacts, contacts] = await Promise.all([
      Contact.countDocuments(query),
      Contact.find(query)
        .populate({ path: 'appUserId', select: 'name profilePicture about', model: User })
        .sort({ isAppUser: -1, name: 1 }) // App users first, then alphabetical
        .skip(skip)
        .limit(limit)
        .lean()
        .exec()
    ]);

    const formattedContacts = contacts.map(contact => ({
      _id: contact._id,
      name: contact.name,
      phoneNumber: contact.phoneNumber,
      isAppUser: contact.isAppUser,
      profilePicture: (contact.appUserId && typeof contact.appUserId === 'object' && 'profilePicture' in contact.appUserId)
        ? (contact.appUserId as any).profilePicture
        : undefined,
      about: (contact.appUserId && typeof contact.appUserId === 'object' && 'about' in contact.appUserId)
        ? (contact.appUserId as any).about
        : undefined,
      appUserId: (contact.appUserId && typeof contact.appUserId === 'object' && '_id' in contact.appUserId)
        ? (contact.appUserId as any)._id
        : contact.appUserId,
      lastSynced: contact.lastSynced
    }));

    const elapsed = Date.now() - startTime;
    console.log(`✅ [${userId}] Fetched ${formattedContacts.length} contacts in ${elapsed}ms`);
    
    // Add ETag for caching
    const etag = `"contacts-${userId}-${page}-${formattedContacts.length}-${totalContacts}"`;
    if (req.headers['if-none-match'] === etag) {
      console.log(`💾 [${userId}] Client has cached version - returning 304`);
      return res.status(304).end();
    }
    
    res.setHeader('ETag', etag);
    res.setHeader('Cache-Control', 'private, max-age=300'); // Cache for 5 minutes

    res.json({ 
      success: true, 
      data: formattedContacts,
      pagination: {
        page,
        limit,
        total: totalContacts,
        totalPages: Math.ceil(totalContacts / limit),
        hasMore: skip + contacts.length < totalContacts
      }
    });
  } catch (error) {
    console.error("Error fetching all contacts:", error);
    res.status(500).json({ error: "Failed to fetch contacts" });
  }
});

// Refresh contact app user status (should be called when users sign up or update phone numbers)
router.post("/refresh-app-status", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    
    if (!userId) {
      return res.status(401).json({ error: "User not authenticated" });
    }
    
    // Get all contacts for this user
    const contacts = await Contact.find({ userId });
    
    if (contacts.length === 0) {
      return res.json({ success: true, message: "No contacts to refresh", updated: 0 });
    }
    
    // Extract phone numbers and create variations for matching
    const phoneNumbers = contacts.map(contact => contact.phoneNumber);
    
    // Create all possible phone number variations for matching
    const allPhoneVariations: string[] = [];
    phoneNumbers.forEach(phone => {
      allPhoneVariations.push(phone); // Original: 9326664680
      if (phone.startsWith('91') && phone.length === 12) {
        allPhoneVariations.push('+' + phone); // Add +: +919326664680
      } else if (!phone.startsWith('91') && phone.length === 10) {
        allPhoneVariations.push('91' + phone); // Add 91: 919326664680
        allPhoneVariations.push('+91' + phone); // Add +91: +919326664680
      }
    });
    
    // Find users in the app with these phone numbers (check all variations)
    const appUsers = await User.find({
      phone: { $in: allPhoneVariations },
      _id: { $ne: userId }
    }).select('name phone profilePicture about');
    
    // Create a map of phone numbers to users (normalize for comparison)
    const phoneToUserMap = new Map();
    appUsers.forEach(user => {
      if (user.phone) {
        const normalizedUserPhone = user.phone.replace(/[\s\-\(\)\+]/g, ''); // Remove + and formatting
        phoneToUserMap.set(normalizedUserPhone, user);
        
        // Also map original contact phone format
        phoneNumbers.forEach(contactPhone => {
          const normalizedContactPhone = contactPhone.replace(/[\s\-\(\)\+]/g, '');
          if (normalizedUserPhone === normalizedContactPhone || 
              normalizedUserPhone === '91' + normalizedContactPhone ||
              normalizedUserPhone === normalizedContactPhone.substring(2)) { // Remove country code
            phoneToUserMap.set(contactPhone, user);
          }
        });
      }
    });
    
    // Update contact records
    let updatedCount = 0;
    const newlyJoinedContacts: any[] = [];
    const bulkOps = contacts.map(contact => {
      const appUser = phoneToUserMap.get(contact.phoneNumber);
      const wasAppUser = contact.isAppUser;
      const isAppUser = !!appUser;
      
      if (wasAppUser !== isAppUser || (isAppUser && !contact.appUserId)) {
        updatedCount++;
        
        // Track newly joined contacts for notifications
        if (!wasAppUser && isAppUser && appUser) {
          newlyJoinedContacts.push({
            name: contact.name || appUser.name,
            userId: appUser._id
          });
        }
        
        return {
          updateOne: {
            filter: { _id: contact._id },
            update: {
              $set: {
                isAppUser,
                appUserId: appUser ? appUser._id : undefined,
                lastSynced: new Date()
              }
            }
          }
        };
      }
      return null;
    }).filter(op => op !== null);
    
    if (bulkOps.length > 0) {
      await Contact.bulkWrite(bulkOps);
    }
    
    // Create notifications for newly joined contacts
    for (const joinedContact of newlyJoinedContacts) {
      await createContactJoinedNotification(userId, joinedContact.name, joinedContact.userId);
    }
    res.json({ 
      success: true, 
      message: `Refreshed contact status`,
      updated: updatedCount,
      totalChecked: contacts.length,
      newNotifications: newlyJoinedContacts.length
    });
  } catch (error) {
    console.error("Error refreshing contact app status:", error);
    res.status(500).json({ error: "Failed to refresh contact status" });
  }
});

// Get contacts categorized by app usage status
router.get("/categorized", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    if (!userId) {
      return res.status(401).json({ error: "Unauthorized" });
    }
    
    const contacts = await Contact.find({ userId })
      .populate({ path: 'appUserId', select: 'name phone profilePicture about', model: User })
      .sort({ name: 1 })
      .lean();

    // Separate contacts into app users and non-app users
    const appUsers: any[] = [];
    const inviteContacts: any[] = [];

    contacts.forEach(contact => {
      const contactData = {
        _id: contact._id,
        name: contact.name,
        phoneNumber: contact.phoneNumber,
        lastSynced: contact.lastSynced
      };

      if (contact.isAppUser && contact.appUserId) {
        const appUserData = contact.appUserId as any;
        appUsers.push({
          ...contactData,
          profilePicture: appUserData.profilePicture || "",
          about: appUserData.about || "Available",
          appUserId: appUserData._id,
          isAppUser: true
        });
      } else {
        inviteContacts.push({
          ...contactData,
          isAppUser: false
        });
      }
    });

    res.json({ 
      success: true, 
      data: {
        appUsers,
        inviteContacts,
        stats: {
          total: contacts.length,
          onApp: appUsers.length,
          toInvite: inviteContacts.length
        }
      }
    });
  } catch (error) {
    console.error("Error fetching categorized contacts:", error);
    res.status(500).json({ error: "Failed to fetch categorized contacts" });
  }
});

// Smart sync endpoint - only sync new contacts not already in database
router.post("/smart-sync", requireAuth, async (req: AuthReq, res) => {
  try {
    const { contacts } = req.body as { contacts: DeviceContact[] };
    const userId = req.userId;
    
    if (!contacts || !Array.isArray(contacts)) {
      return res.status(400).json({ error: "Invalid contacts data" });
    }

    // Normalize phone numbers for consistent matching
    const normalizedContacts = contacts.map(contact => ({
      name: contact.name,
      phoneNumber: contact.phoneNumber.replace(/[\s\-\(\)]/g, '') // Remove formatting
    }));

    // Get existing contact phone numbers for this user
    const existingContacts = await Contact.find({ userId }).select('phoneNumber').lean();
    const existingPhoneNumbers = new Set(existingContacts.map(c => c.phoneNumber));

    // Filter to only NEW contacts (not already in database)
    const newContacts = normalizedContacts.filter(contact => 
      !existingPhoneNumbers.has(contact.phoneNumber)
    );

    console.log(`Smart sync for user ${userId}: ${contacts.length} device contacts, ${existingContacts.length} stored, ${newContacts.length} new`);

    if (newContacts.length === 0) {
      return res.json({ 
        success: true, 
        message: "No new contacts found",
        stats: {
          deviceContacts: contacts.length,
          storedContacts: existingContacts.length,
          newContacts: 0,
          syncedContacts: 0
        }
      });
    }

    // For new contacts, check which ones are app users
    const newPhoneNumbers = newContacts.map(contact => contact.phoneNumber);
    
    // Create phone number variations for matching
    const allPhoneVariations: string[] = [];
    newPhoneNumbers.forEach(phone => {
      allPhoneVariations.push(phone); // Original: 9326664680
      if (phone.startsWith('91') && phone.length === 12) {
        allPhoneVariations.push(phone.substring(2)); // Remove country code: 9326664680
      } else if (phone.length === 10) {
        allPhoneVariations.push('91' + phone); // Add country code: 919326664680
      }
    });

    // Find app users with these phone numbers
    const appUsers = await User.find({
      phone: { $in: allPhoneVariations },
      _id: { $ne: userId }
    }).select('name phone profilePicture about');

    // Create phone to user mapping
    const phoneToUserMap = new Map();
    appUsers.forEach(user => {
      if (user.phone) {
        const normalizedUserPhone = user.phone.replace(/[\s\-\(\)\+]/g, '');
        phoneToUserMap.set(user.phone, user);
        
        // Also map original contact phone format
        newPhoneNumbers.forEach(contactPhone => {
          const normalizedContactPhone = contactPhone.replace(/[\s\-\(\)\+]/g, '');
          if (normalizedUserPhone === normalizedContactPhone || 
              normalizedUserPhone === '91' + normalizedContactPhone ||
              normalizedUserPhone === normalizedContactPhone.substring(2)) {
            phoneToUserMap.set(contactPhone, user);
          }
        });
      }
    });

    // Prepare new contacts for bulk insert
    const contactsToSave = newContacts.map(contact => {
      const appUser = phoneToUserMap.get(contact.phoneNumber);
      return {
        userId,
        name: contact.name,
        phoneNumber: contact.phoneNumber,
        isAppUser: !!appUser,
        appUserId: appUser ? appUser._id : undefined,
        lastSynced: new Date(),
        createdAt: new Date()
      };
    });

    // Bulk insert new contacts
    await Contact.insertMany(contactsToSave);

    // Count app users in new contacts
    const newAppUserContacts = newContacts.filter(contact => 
      phoneToUserMap.has(contact.phoneNumber)
    );

    res.json({ 
      success: true, 
      message: `Smart sync completed: added ${newContacts.length} new contacts`,
      stats: {
        deviceContacts: contacts.length,
        storedContacts: existingContacts.length,
        newContacts: newContacts.length,
        newAppUsers: newAppUserContacts.length,
        syncedContacts: newContacts.length
      }
    });
  } catch (error) {
    console.error("Error in smart sync:", error);
    res.status(500).json({ error: "Failed to smart sync contacts" });
  }
});

export default router;