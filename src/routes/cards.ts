import { Router } from "express";
import Card from "../models/Card";
import { AuthReq, requireAuth } from "../middleware/auth";

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

// SHARE CARD (send to another user)
r.post("/:id/share", async (req: AuthReq, res) => {
  try {
    const { recipientId, recipientPhone, message } = req.body;
    const cardId = req.params.id;
    
    // Verify the card belongs to the sender
    const card = await Card.findOne({ _id: cardId, userId: req.userId! });
    if (!card) {
      return res.status(404).json({ message: "Card not found" });
    }

    // For now, we'll create a simple sharing record
    // In a real app, you'd want a separate SharedCard model
    const sharedCard = {
      cardId,
      senderId: req.userId!,
      recipientId: recipientId || null,
      recipientPhone: recipientPhone || null,
      cardTitle: card.companyName || card.name || 'Business Card',
      sentAt: new Date(),
      status: 'sent',
      message: message || null
    };

    // You could save this to a SharedCards collection
    // await SharedCard.create(sharedCard);
    
    res.json({ 
      success: true, 
      message: "Card shared successfully",
      data: sharedCard 
    });
  } catch (err) {
    console.error("SHARE CARD ERROR", err);
    res.status(400).json({ message: "Failed to share card" });
  }
});

// GET SENT CARDS
r.get("/sent", async (req: AuthReq, res) => {
  try {
    // For now, return mock data
    // In a real app, you'd query a SharedCards collection
    const sentCards = [
      {
        _id: "sent1",
        cardId: "card123",
        recipientId: "user456",
        recipientName: "John Doe",
        cardTitle: "My Business Card",
        sentAt: new Date(Date.now() - 86400000), // 1 day ago
        status: "delivered"
      },
      {
        _id: "sent2",
        cardId: "card124",
        recipientId: "user789",
        recipientName: "Jane Smith",
        cardTitle: "Company Card",
        sentAt: new Date(Date.now() - 172800000), // 2 days ago
        status: "viewed"
      }
    ];

    res.json({ success: true, data: sentCards });
  } catch (err) {
    console.error("GET SENT CARDS ERROR", err);
    res.status(500).json({ message: "Failed to fetch sent cards" });
  }
});

// GET RECEIVED CARDS
r.get("/received", async (req: AuthReq, res) => {
  try {
    // For now, return mock data
    // In a real app, you'd query a SharedCards collection
    const receivedCards = [
      {
        _id: "received1",
        cardId: "card789",
        senderId: "user123",
        senderName: "Alice Johnson",
        cardTitle: "Marketing Director",
        receivedAt: new Date(Date.now() - 43200000), // 12 hours ago
        isViewed: false
      },
      {
        _id: "received2",
        cardId: "card790",
        senderId: "user456",
        senderName: "Bob Wilson",
        cardTitle: "Software Engineer",
        receivedAt: new Date(Date.now() - 259200000), // 3 days ago
        isViewed: true
      }
    ];

    res.json({ success: true, data: receivedCards });
  } catch (err) {
    console.error("GET RECEIVED CARDS ERROR", err);
    res.status(500).json({ message: "Failed to fetch received cards" });
  }
});

export default r;
