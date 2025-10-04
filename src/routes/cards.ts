import { Router } from "express";
import Card from "../models/Card";
import SharedCard from "../models/SharedCard";
import GroupSharedCard from "../models/GroupSharedCard";
import User from "../models/User";
import Group from "../models/Group";
import Contact from "../models/Contact";
import { AuthReq, requireAuth } from "../middleware/auth";
import { sendCardSharingNotification, sendCardCreationNotification } from "../services/pushNotifications";

const r = Router();

// Public feed (no auth) – latest 50 cards
r.get("/feed/public", async (_req, res) => {
  const items = await Card.find({}).sort({ createdAt: -1 }).limit(50).lean();
  res.json({ data: items });
});

// Require auth for everything below
r.use(requireAuth);

// CONTACTS FEED - Get cards from my contacts AND my own cards
r.get("/feed/contacts", async (req: AuthReq, res) => {
  try {
    const userId = req.userId!;
    
    // Get all contacts who are app users
    const myContacts = await Contact.find({ 
      userId,
      isAppUser: true 
    }).select('appUserId').lean();
    
    // Extract contact user IDs with proper typing
    const contactUserIds = myContacts.map((contact: any) => contact.appUserId).filter(Boolean);
    
    // Add current user's ID to see their own cards too
    const allUserIds = [userId, ...contactUserIds];
    
    console.log(`📋 User ${userId} has ${contactUserIds.length} contacts on the app`);
    console.log(`👤 Including user's own cards in feed`);
    
    // Get cards from contacts AND own cards
    const allCards = await Card.find({
      userId: { $in: allUserIds }
    })
    .sort({ createdAt: -1 })
    .limit(100) // Increased limit for contacts feed
    .lean();
    
    // Separate own cards and contact cards for metadata
    const ownCards = allCards.filter((card: any) => card.userId.toString() === userId);
    const contactCards = allCards.filter((card: any) => card.userId.toString() !== userId);
    
    console.log(`📇 Found ${ownCards.length} own cards and ${contactCards.length} cards from contacts`);
    
    res.json({ 
      success: true,
      data: allCards,
      meta: {
        totalContacts: contactUserIds.length,
        totalCards: allCards.length,
        ownCards: ownCards.length,
        contactCards: contactCards.length
      }
    });
  } catch (err) {
    console.error("CONTACTS FEED ERROR", err);
    res.status(500).json({ 
      success: false,
      message: "Failed to fetch contacts feed",
      data: [] 
    });
  }
});

// CREATE (expects flat body)
r.post("/", async (req: AuthReq, res) => {
  try {
    const userId = req.userId!;
    const doc = await Card.create({ ...req.body, userId });
    
    // Send notifications to contacts who have this user in their contacts
    try {
      // Get user details
      const creator: any = await User.findById(userId).select('name phoneNumber').lean();
      
      if (creator && creator.name) {
        // Find all contacts who have this user as a contact AND are app users
        const myContactsWhoAreAppUsers: any[] = await Contact.find({ 
          appUserId: userId,
          isAppUser: true
        }).populate('userId', 'pushToken name').lean();
        
        console.log(`📢 Notifying ${myContactsWhoAreAppUsers.length} contacts about new card creation`);
        
        // Send notification to each contact
        for (const contact of myContactsWhoAreAppUsers) {
          const contactUser = contact.userId;
          
          if (contactUser?.pushToken) {
            await sendCardCreationNotification(
              contactUser.pushToken,
              creator.name || 'A contact',
              req.body.name || 'a new card',
              doc._id.toString(),
              userId
            );
          }
        }
      }
    } catch (notifError) {
      console.error('Error sending card creation notifications:', notifError);
      // Don't fail the card creation if notifications fail
    }
    
    res.status(201).json({ data: doc });
  } catch (err: any) {
    console.error("CREATE CARD ERROR", err);
    res.status(400).json({ message: err?.message || "Bad request" });
  }
});

// LIST own cards
r.get("/", async (req: AuthReq, res) => {
  const items = await Card.find({ userId: req.userId! }).sort({ createdAt: -1 }).lean();
  res.json({ data: items });
});

// UPDATE
r.put("/:id", async (req: AuthReq, res) => {
  try {
    const doc = await Card.findOneAndUpdate(
      { _id: req.params.id, userId: req.userId! },
      req.body,
      { new: true }
    );
    if (!doc) return res.status(404).json({ message: "Not found" });
    res.json({ data: doc });
  } catch (err) {
    console.error("UPDATE CARD ERROR", err);
    res.status(400).json({ message: "Bad request" });
  }
});

// DELETE
r.delete("/:id", async (req: AuthReq, res) => {
  try {
    const doc = await Card.findOneAndDelete({ _id: req.params.id, userId: req.userId! });
    if (!doc) return res.status(404).json({ message: "Not found" });
    res.json({ ok: true });
  } catch (err) {
    console.error("DELETE CARD ERROR", err);
    res.status(400).json({ message: "Bad request" });
  }
});

// SHARE CARD (send to another user within the app)
r.post("/:id/share", async (req: AuthReq, res) => {
  try {
    const { recipientId, message } = req.body;
    const cardId = req.params.id;
    const senderId = req.userId!;
    
    if (!recipientId) {
      return res.status(400).json({ message: "Recipient ID is required" });
    }

    // Verify the card belongs to the sender
    const card = await Card.findOne({ _id: cardId, userId: senderId });
    if (!card) {
      return res.status(404).json({ message: "Card not found or access denied" });
    }

    // Verify recipient exists
    const recipient = await User.findById(recipientId);
    if (!recipient) {
      return res.status(404).json({ message: "Recipient not found" });
    }

    // Get sender info
    const sender = await User.findById(senderId);
    if (!sender) {
      return res.status(404).json({ message: "Sender not found" });
    }

    // Create shared card record (allowing duplicates)
    const sharedCard = await SharedCard.create({
      cardId,
      senderId,
      recipientId,
      message: message || "",
      cardTitle: card.companyName || card.name || 'Business Card',
      senderName: sender.name,
      recipientName: recipient.name,
      status: 'sent'
    });

    console.log(`📧 Card shared: ${sender.name} → ${recipient.name} (${card.companyName || card.name})`);
    
    // Send push notification to recipient if they have a push token
    if (recipient.pushToken) {
      try {
        const notificationSent = await sendCardSharingNotification(
          recipient.pushToken,
          sender.name,
          sharedCard.cardTitle,
          senderId,
          cardId
        );
        
        if (notificationSent) {
          console.log(`🔔 Card sharing notification sent to ${recipient.name}`);
        } else {
          console.log(`❌ Failed to send card sharing notification to ${recipient.name}`);
        }
      } catch (notificationError) {
        console.error('Error sending card sharing notification:', notificationError);
        // Don't fail the card sharing if notification fails
      }
    } else {
      console.log(`📱 No push token for ${recipient.name}, skipping notification`);
    }
    
    res.json({ 
      success: true, 
      message: "Card shared successfully",
      data: {
        sharedCardId: sharedCard._id,
        cardTitle: sharedCard.cardTitle,
        recipientName: recipient.name,
        sentAt: sharedCard.sentAt
      }
    });
  } catch (err) {
    console.error("SHARE CARD ERROR", err);
    res.status(500).json({ message: "Failed to share card" });
  }
});

// GET SENT CARDS
r.get("/sent", async (req: AuthReq, res) => {
  try {
    const senderId = req.userId!;
    
    // Find all cards shared by this user, populate with card and recipient details
    const sentCards = await SharedCard.find({ senderId })
      .populate('cardId', 'companyName name companyPhoto')
      .populate('recipientId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .lean();

    // Format the response
    const formattedCards = sentCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId._id,
      recipientId: share.recipientId._id,
      recipientName: share.recipientName,
      recipientProfilePicture: share.recipientId.profilePicture,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId.companyPhoto,
      sentAt: share.sentAt,
      status: share.status,
      message: share.message,
      viewedAt: share.viewedAt
    }));

    res.json({
      success: true,
      data: formattedCards
    });
  } catch (err) {
    console.error("GET SENT CARDS ERROR", err);
    res.status(500).json({ message: "Failed to fetch sent cards" });
  }
});

// GET RECEIVED CARDS - Cards that have been shared with the current user
r.get("/received", async (req: AuthReq, res) => {
  try {
    const recipientId = req.userId!;
    
    // Find all cards shared with this user, populate with card and sender details
    const receivedCards = await SharedCard.find({ recipientId })
      .populate('cardId', 'companyName name companyPhoto')
      .populate('senderId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .lean();

    // Format the response
    const formattedCards = receivedCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId._id,
      senderId: share.senderId._id,
      senderName: share.senderName,
      senderProfilePicture: share.senderId.profilePicture,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId.companyPhoto,
      receivedAt: share.sentAt, // Use sentAt as receivedAt
      sentAt: share.sentAt,
      isViewed: share.status === 'viewed',
      status: share.status,
      message: share.message,
      viewedAt: share.viewedAt
    }));

    res.json({
      success: true,
      data: formattedCards
    });
  } catch (err) {
    console.error("GET RECEIVED CARDS ERROR", err);
    res.status(500).json({ message: "Failed to fetch received cards" });
  }
});

// MARK CARD AS VIEWED
r.post("/shared/:id/view", async (req: AuthReq, res) => {
  try {
    const sharedCardId = req.params.id;
    const userId = req.userId!;
    
    // Update the shared card status to viewed
    const updatedCard = await SharedCard.findOneAndUpdate(
      { 
        _id: sharedCardId, 
        recipientId: userId,
        status: { $ne: 'viewed' } // Only update if not already viewed
      },
      { 
        status: 'viewed',
        viewedAt: new Date()
      },
      { new: true }
    );

    if (!updatedCard) {
      return res.status(404).json({ message: "Shared card not found or already viewed" });
    }

    res.json({ 
      success: true, 
      message: "Card marked as viewed",
      viewedAt: updatedCard.viewedAt
    });
  } catch (err) {
    console.error("MARK CARD VIEWED ERROR", err);
    res.status(500).json({ message: "Failed to mark card as viewed" });
  }
});

// GET SHARED CARDS BETWEEN TWO USERS (for chat conversations)
r.get("/shared-with/:userId", async (req: AuthReq, res) => {
  try {
    const currentUserId = req.userId!;
    const otherUserId = req.params.userId;
    
    // Find all shared cards between these two users (in both directions)
    const sharedCards = await SharedCard.find({
      $or: [
        { senderId: currentUserId, recipientId: otherUserId },
        { senderId: otherUserId, recipientId: currentUserId }
      ]
    })
    .populate('cardId', 'companyName name companyPhoto')
    .populate('senderId', 'name profilePicture')
    .populate('recipientId', 'name profilePicture')
    .sort({ sentAt: 1 }) // Chronological order for chat timeline
    .lean();

    // Format the response
    const formattedCards = sharedCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId._id,
      senderId: share.senderId._id,
      recipientId: share.recipientId._id,
      senderName: share.senderName,
      recipientName: share.recipientName,
      senderProfilePicture: share.senderId.profilePicture,
      recipientProfilePicture: share.recipientId.profilePicture,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId.companyPhoto,
      sentAt: share.sentAt,
      status: share.status,
      message: share.message,
      viewedAt: share.viewedAt,
      isFromMe: share.senderId.toString() === currentUserId,
      isToMe: share.recipientId.toString() === currentUserId
    }));

    res.json({
      success: true,
      data: formattedCards
    });
  } catch (err) {
    console.error("GET SHARED CARDS BETWEEN USERS ERROR", err);
    res.status(500).json({ message: "Failed to fetch shared cards" });
  }
});

// SHARE CARD TO GROUP
r.post("/:id/share-to-group", async (req: AuthReq, res) => {
  try {
    const { groupId, message } = req.body;
    const cardId = req.params.id;
    const senderId = req.userId!;
    
    if (!groupId) {
      return res.status(400).json({ message: "Group ID is required" });
    }

    // Verify the card belongs to the sender
    const card = await Card.findOne({ _id: cardId, userId: senderId });
    if (!card) {
      return res.status(404).json({ message: "Card not found or access denied" });
    }

    // Verify group exists and user is a member
    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ message: "Group not found" });
    }

    // Check if user is a member of the group
    const isMember = group.members.some(memberId => memberId.toString() === senderId);
    if (!isMember) {
      return res.status(403).json({ message: "You are not a member of this group" });
    }

    // Get sender info
    const sender = await User.findById(senderId);
    if (!sender) {
      return res.status(404).json({ message: "Sender not found" });
    }

    // Create group shared card record
    const groupSharedCard = await GroupSharedCard.create({
      cardId,
      senderId,
      groupId,
      message: message || "",
      cardTitle: card.companyName || card.name || 'Business Card',
      senderName: sender.name,
      groupName: group.name
    });

    console.log(`📧 Card shared to group: ${sender.name} → ${group.name} (${card.companyName || card.name})`);
    
    res.json({ 
      success: true, 
      message: "Card shared to group successfully",
      data: {
        sharedCardId: groupSharedCard._id,
        cardTitle: groupSharedCard.cardTitle,
        groupName: group.name,
        sentAt: groupSharedCard.sentAt
      }
    });
  } catch (err) {
    console.error("SHARE CARD TO GROUP ERROR", err);
    res.status(500).json({ message: "Failed to share card to group" });
  }
});

// GET GROUP SHARED CARDS - Cards shared in a specific group
r.get("/group/:groupId/shared", async (req: AuthReq, res) => {
  try {
    const groupId = req.params.groupId;
    const currentUserId = req.userId!;
    
    // Verify group exists and user is a member
    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ message: "Group not found" });
    }

    // Check if user is a member of the group
    const isMember = group.members.some(memberId => memberId.toString() === currentUserId);
    if (!isMember) {
      return res.status(403).json({ message: "You are not a member of this group" });
    }
    
    // Find all cards shared in this group
    const groupSharedCards = await GroupSharedCard.find({ groupId })
      .populate('cardId', 'companyName name companyPhoto userId')
      .populate('senderId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .lean();

    // Format the response
    const formattedCards = groupSharedCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId._id,
      senderId: share.senderId._id,
      senderName: share.senderName,
      senderProfilePicture: share.senderId.profilePicture,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId.companyPhoto,
      sentAt: share.sentAt,
      message: share.message,
      isFromMe: share.senderId._id.toString() === currentUserId
    }));

    res.json({
      success: true,
      data: formattedCards
    });
  } catch (err) {
    console.error("GET GROUP SHARED CARDS ERROR", err);
    res.status(500).json({ message: "Failed to fetch group shared cards" });
  }
});

// GET GROUP CARDS SUMMARY - Cards sent and received counts for a group
r.get("/group/:groupId/summary", async (req: AuthReq, res) => {
  try {
    const groupId = req.params.groupId;
    const currentUserId = req.userId!;
    
    // Verify group exists and user is a member
    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ message: "Group not found" });
    }

    // Check if user is a member of the group
    const isMember = group.members.some(memberId => memberId.toString() === currentUserId);
    if (!isMember) {
      return res.status(403).json({ message: "You are not a member of this group" });
    }
    
    // Get cards sent by current user to this group
    const sentCards = await GroupSharedCard.find({ 
      groupId, 
      senderId: currentUserId 
    })
    .populate('cardId', 'companyName name companyPhoto')
    .sort({ sentAt: -1 })
    .lean();

    // Get cards received by current user in this group (sent by others)
    const receivedCards = await GroupSharedCard.find({ 
      groupId, 
      senderId: { $ne: currentUserId } 
    })
    .populate('cardId', 'companyName name companyPhoto')
    .populate('senderId', 'name profilePicture')
    .sort({ sentAt: -1 })
    .lean();

    // Format sent cards
    const formattedSentCards = sentCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId._id,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId.companyPhoto,
      sentAt: share.sentAt,
      message: share.message
    }));

    // Format received cards
    const formattedReceivedCards = receivedCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId._id,
      senderId: share.senderId._id,
      senderName: share.senderName,
      senderProfilePicture: share.senderId.profilePicture,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId.companyPhoto,
      sentAt: share.sentAt,
      message: share.message
    }));

    res.json({
      success: true,
      data: {
        sent: {
          count: formattedSentCards.length,
          cards: formattedSentCards
        },
        received: {
          count: formattedReceivedCards.length,
          cards: formattedReceivedCards
        }
      }
    });
  } catch (err) {
    console.error("GET GROUP CARDS SUMMARY ERROR", err);
    res.status(500).json({ message: "Failed to fetch group cards summary" });
  }
});

export default r;
