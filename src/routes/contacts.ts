import express from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import User from "../models/User";
import Contact from "../models/Contact";

const router = express.Router();

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

    // Extract phone numbers from contacts
    const phoneNumbers = contacts.map(contact => contact.phoneNumber);
    
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
    const syncedContacts = contacts.map(contact => {
      const appUser = phoneToUserMap.get(contact.phoneNumber);
      return {
        name: appUser ? appUser.name : contact.name,
        phoneNumber: contact.phoneNumber,
        isAppUser: !!appUser,
        _id: appUser ? appUser._id : `contact_${contact.phoneNumber}`,
        profilePicture: appUser?.profilePicture
      };
    }).filter(contact => contact.isAppUser); // Only return app users

    // You might want to save this sync data to database for future use
    // await ContactSync.create({ userId, syncedAt: new Date(), contacts: syncedContacts });

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
    
    if (!contacts || !Array.isArray(contacts)) {
      return res.status(400).json({ error: "Invalid contacts data" });
    }

    // Extract phone numbers from contacts to find app users
    const phoneNumbers = contacts.map(contact => contact.phoneNumber);
    
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

    // Prepare contacts for bulk upsert
    const contactsToSave = contacts.map(contact => {
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

    // Bulk upsert contacts
    if (contactsToSave.length > 0) {
      await Contact.bulkWrite(contactsToSave);
    }

    // Return summary
    const appUserContacts = contacts.filter(contact => {
      return phoneToUserMap.has(contact.phoneNumber);
    });

    res.json({ 
      success: true, 
      message: `Synced ${contacts.length} contacts`,
      stats: {
        total: contacts.length,
        appUsers: appUserContacts.length,
        nonAppUsers: contacts.length - appUserContacts.length
      }
    });
  } catch (error) {
    console.error("Error syncing all contacts:", error);
    res.status(500).json({ error: "Failed to sync contacts" });
  }
});

// Get all stored contacts for the user
router.get("/all", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    
    const contacts = await Contact.find({ userId })
      .populate({ path: 'appUserId', select: 'name profilePicture about', model: User })
      .sort({ isAppUser: -1, name: 1 }) // App users first, then alphabetical
      .lean();

    const formattedContacts = contacts.map(contact => ({
      _id: contact._id,
      name: contact.name,
      phoneNumber: contact.phoneNumber,
      isAppUser: contact.isAppUser,
      profilePicture: (contact.appUserId && typeof contact.appUserId === 'object' && 'profilePicture' in contact.appUserId)
        ? (contact.appUserId as any).profilePicture
        : undefined,
      appUserId: (contact.appUserId && typeof contact.appUserId === 'object' && '_id' in contact.appUserId)
        ? (contact.appUserId as any)._id
        : contact.appUserId,
      lastSynced: contact.lastSynced
    }));

    res.json({ success: true, data: formattedContacts });
  } catch (error) {
    console.error("Error fetching all contacts:", error);
    res.status(500).json({ error: "Failed to fetch contacts" });
  }
});

// Refresh contact app user status (should be called when users sign up or update phone numbers)
router.post("/refresh-app-status", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;
    
    // Get all contacts for this user
    const contacts = await Contact.find({ userId });
    
    if (contacts.length === 0) {
      return res.json({ success: true, message: "No contacts to refresh", updated: 0 });
    }
    
    // Extract phone numbers
    const phoneNumbers = contacts.map(contact => contact.phoneNumber);
    
    // Find users in the app with these phone numbers
    const appUsers = await User.find({
      phone: { $in: phoneNumbers },
      _id: { $ne: userId }
    }).select('name phone profilePicture about');
    
    // Create a map of phone numbers to users
    const phoneToUserMap = new Map();
    appUsers.forEach(user => {
      if (user.phone) {
        phoneToUserMap.set(user.phone, user);
      }
    });
    
    // Update contact records
    let updatedCount = 0;
    const bulkOps = contacts.map(contact => {
      const appUser = phoneToUserMap.get(contact.phoneNumber);
      const wasAppUser = contact.isAppUser;
      const isAppUser = !!appUser;
      
      if (wasAppUser !== isAppUser || (isAppUser && !contact.appUserId)) {
        updatedCount++;
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
    
    res.json({ 
      success: true, 
      message: `Refreshed contact status`,
      updated: updatedCount,
      totalChecked: contacts.length
    });
  } catch (error) {
    console.error("Error refreshing contact app status:", error);
    res.status(500).json({ error: "Failed to refresh contact status" });
  }
});

export default router;