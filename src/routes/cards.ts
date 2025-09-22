import { Router } from "express";
import Card from "../models/Card";
import SharedCard from "../models/SharedCard";
import User from "../models/User";
import { AuthReq, requireAuth } from "../middleware/auth";
import { sendCardSharingNotification } from "../services/pushNotifications";

const r = Router();

// Public feed (no auth) – latest 50 cards
r.get("/feed/public", async (_req, res) => {
  const items = await Card.find({}).sort({ createdAt: -1 }).limit(50).lean();
  res.json({ data: items });
});

// Require auth for everything below
r.use(requireAuth);

// CREATE (expects flat body)
r.post("/", async (req: AuthReq, res) => {
  try {
    const doc = await Card.create({ ...req.body, userId: req.userId! });
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

    // Check if card was already shared to this recipient
    const existingShare = await SharedCard.findOne({ 
      cardId, 
      senderId, 
      recipientId 
    });

    if (existingShare) {
      return res.status(409).json({ 
        message: "Card already shared with this recipient",
        sharedCardId: existingShare._id 
      });
    }

    // Create shared card record
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

export default r;
