import express from "express";
import { requireAuth, AuthReq } from "../middleware/auth";
import User from "../models/User";

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
      $or: [
        { personalPhone: { $in: phoneNumbers } },
        { companyPhone: { $in: phoneNumbers } }
      ],
      _id: { $ne: userId } // Exclude current user
    }).select('name personalPhone companyPhone profilePicture');

    // Create a map of phone numbers to users
    const phoneToUserMap = new Map();
    appUsers.forEach(user => {
      if (user.personalPhone) {
        phoneToUserMap.set(user.personalPhone, user);
      }
      if (user.companyPhone) {
        phoneToUserMap.set(user.companyPhone, user);
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
    }).select('name personalPhone companyPhone profilePicture').limit(50);

    const contacts: AppContact[] = appUsers.map(user => ({
      _id: user._id.toString(),
      name: user.name || 'Unknown User',
      phoneNumber: user.personalPhone || user.companyPhone || '',
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

export default router;