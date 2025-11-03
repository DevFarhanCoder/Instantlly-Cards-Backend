// Group Sharing Routes - Complete API Implementation
import express, { Request, Response } from "express";
import GroupSession, { IParticipant } from "../models/GroupSession";
import CardShare from "../models/CardShare";
import SharedCard from "../models/SharedCard";
import User from "../models/User";
import Card from "../models/Card";
import { requireAuth, AuthReq } from "../middleware/auth";
import mongoose from "mongoose";

const router = express.Router();

// Helper function to format participant
const formatParticipant = (participant: IParticipant) => ({
  id: participant.userId.toString(),
  name: participant.userName,
  phone: participant.userPhone,
  photo: participant.photo,
  isOnline: participant.isOnline,
  joinedAt: participant.joinedAt,
  cardsToShare: participant.cardsToShare.map(id => id.toString()),
  defaultCardId: participant.defaultCardId?.toString()
});

// Helper function to format session
const formatSession = (session: any) => ({
  id: session._id.toString(),
  code: session.code,
  adminId: session.adminId.toString(),
  adminName: session.adminName,
  adminPhone: session.adminPhone,
  adminPhoto: session.adminPhoto,
  participants: session.participants.map(formatParticipant),
  status: session.status,
  createdAt: session.createdAt,
  expiresAt: session.expiresAt,
  isActive: session.isActive
});

/**
 * POST /api/group-sharing/create
 * Create a new group sharing session
 */
router.post("/create", requireAuth, async (req: Request, res: Response) => {
  try {
    const { code, adminId, adminName, adminPhone, adminPhoto, expirationMinutes = 10 } = req.body;

    console.log("📝 Creating group session:", { code, adminId, adminName, adminPhone });

    // Validate required fields (phone is optional)
    if (!code || !adminId || !adminName) {
      return res.status(400).json({
        success: false,
        error: "Missing required fields: code, adminId, adminName"
      });
    }

    // Check if code already exists
    const existingSession = await GroupSession.findOne({ code, isActive: true });
    if (existingSession) {
      console.log("❌ Code already exists:", code);
      return res.status(400).json({
        success: false,
        error: "Code already exists. Please try a different code."
      });
    }

    // Create expiration date
    const expiresAt = new Date(Date.now() + expirationMinutes * 60 * 1000);

    // Create admin participant
    const adminParticipant: IParticipant = {
      userId: new mongoose.Types.ObjectId(adminId),
      userName: adminName,
      userPhone: adminPhone,
      photo: adminPhoto,
      joinedAt: new Date(),
      cardsToShare: [],
      isOnline: true
    };

    // Create session
    const session = new GroupSession({
      code,
      adminId: new mongoose.Types.ObjectId(adminId),
      adminName,
      adminPhone,
      adminPhoto,
      participants: [adminParticipant],
      status: "waiting",
      expiresAt,
      isActive: true
    });

    await session.save();

    console.log("✅ Group session created successfully:", session._id);

    res.status(201).json({
      success: true,
      session: formatSession(session)
    });

  } catch (error: any) {
    console.error("❌ Error creating group session:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to create group session"
    });
  }
});

/**
 * POST /api/group-sharing/join
 * Join an existing group sharing session
 */
router.post("/join", requireAuth, async (req: Request, res: Response) => {
  try {
    const { code, userId, userName, userPhone, userPhoto } = req.body;

    console.log("🔗 Joining group session:", { code, userId, userName, userPhone });

    // Validate required fields (phone is optional)
    if (!code || !userId || !userName) {
      return res.status(400).json({
        success: false,
        error: "Missing required fields: code, userId, userName"
      });
    }

    // Find active session with this code
    const session = await GroupSession.findOne({ 
      code, 
      isActive: true,
      expiresAt: { $gt: new Date() }
    });

    if (!session) {
      console.log("❌ Invalid or expired code:", code);
      return res.status(404).json({
        success: false,
        error: "Invalid or expired code"
      });
    }

    // Check if user already in session
    const existingParticipant = session.participants.find(
      p => p.userId.toString() === userId
    );

    if (existingParticipant) {
      console.log("⚠️ User already in session:", userId);
      return res.status(200).json({
        success: true,
        session: formatSession(session),
        message: "Already joined"
      });
    }

    // Add participant
    const newParticipant: IParticipant = {
      userId: new mongoose.Types.ObjectId(userId),
      userName,
      userPhone,
      photo: userPhoto,
      joinedAt: new Date(),
      cardsToShare: [],
      isOnline: true
    };

    session.participants.push(newParticipant);
    await session.save();

    console.log("✅ User joined successfully:", userId);

    res.status(200).json({
      success: true,
      session: formatSession(session)
    });

  } catch (error: any) {
    console.error("❌ Error joining group session:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to join group session"
    });
  }
});

/**
 * GET /api/group-sharing/session/:sessionId
 * Get session status (for polling)
 */
router.get("/session/:sessionId", requireAuth, async (req: Request, res: Response) => {
  try {
    const { sessionId } = req.params;

    const session = await GroupSession.findById(sessionId);

    if (!session || !session.isActive) {
      return res.status(404).json({
        success: false,
        error: "Session not found or expired"
      });
    }

    // Check if session expired
    if (session.expiresAt < new Date()) {
      session.status = "expired";
      session.isActive = false;
      await session.save();

      return res.status(404).json({
        success: false,
        error: "Session has expired"
      });
    }

    res.status(200).json({
      success: true,
      session: formatSession(session)
    });

  } catch (error: any) {
    console.error("❌ Error getting session status:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to get session status"
    });
  }
});

/**
 * POST /api/group-sharing/connect/:sessionId
 * Connect all participants (Admin only)
 */
router.post("/connect/:sessionId", requireAuth, async (req: Request, res: Response) => {
  try {
    const { sessionId } = req.params;
    const { adminId } = req.body;

    console.log("🔗 Connecting participants:", { sessionId, adminId });

    const session = await GroupSession.findById(sessionId);

    if (!session || !session.isActive) {
      return res.status(404).json({
        success: false,
        error: "Session not found or expired"
      });
    }

    // Verify admin
    if (session.adminId.toString() !== adminId) {
      console.log("❌ Unauthorized: Not admin");
      return res.status(403).json({
        success: false,
        error: "Only admin can connect participants"
      });
    }

    // Update status
    session.status = "connected";
    await session.save();

    console.log("✅ Participants connected successfully");

    res.status(200).json({
      success: true,
      session: formatSession(session)
    });

  } catch (error: any) {
    console.error("❌ Error connecting participants:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to connect participants"
    });
  }
});

/**
 * POST /api/group-sharing/set-cards/:sessionId
 * Set cards to share for a participant
 */
router.post("/set-cards/:sessionId", requireAuth, async (req: Request, res: Response) => {
  try {
    const { sessionId } = req.params;
    const { userId, cardIds, defaultCardId } = req.body;

    console.log("🎴 Setting cards to share:", { sessionId, userId, cardCount: cardIds?.length });

    const session = await GroupSession.findById(sessionId);

    if (!session || !session.isActive) {
      return res.status(404).json({
        success: false,
        error: "Session not found or expired"
      });
    }

    // Find participant
    const participantIndex = session.participants.findIndex(
      p => p.userId.toString() === userId
    );

    if (participantIndex === -1) {
      return res.status(404).json({
        success: false,
        error: "Participant not found in session"
      });
    }

    // Update cards
    session.participants[participantIndex].cardsToShare = cardIds.map(
      (id: string) => new mongoose.Types.ObjectId(id)
    );
    
    if (defaultCardId) {
      session.participants[participantIndex].defaultCardId = new mongoose.Types.ObjectId(defaultCardId);
    }

    await session.save();

    console.log("✅ Cards set successfully for user:", userId);

    res.status(200).json({
      success: true,
      participant: formatParticipant(session.participants[participantIndex])
    });

  } catch (error: any) {
    console.error("❌ Error setting cards:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to set cards"
    });
  }
});

/**
 * POST /api/group-sharing/execute/:sessionId
 * Execute card sharing (Admin only)
 */
router.post("/execute/:sessionId", requireAuth, async (req: Request, res: Response) => {
  try {
    const { sessionId } = req.params;
    const { adminId } = req.body;

    console.log("🚀 Executing card sharing:", { sessionId, adminId });

    const session = await GroupSession.findById(sessionId);

    if (!session || !session.isActive) {
      return res.status(404).json({
        success: false,
        error: "Session not found or expired"
      });
    }

    // Verify admin
    if (session.adminId.toString() !== adminId) {
      return res.status(403).json({
        success: false,
        error: "Only admin can execute sharing"
      });
    }

    // Update session status
    session.status = "sharing";
    await session.save();

    const results: any[] = [];
    const duplicates: any[] = [];
    const errors: any[] = [];

    // For each participant
    for (const fromParticipant of session.participants) {
      const fromUserId = fromParticipant.userId;
      const cardsToShare = fromParticipant.cardsToShare;

      if (cardsToShare.length === 0) {
        console.log(`⚠️ User ${fromParticipant.userName} has no cards to share`);
        continue;
      }

      // Share with all other participants
      for (const toParticipant of session.participants) {
        const toUserId = toParticipant.userId;

        // Don't share with yourself
        if (fromUserId.toString() === toUserId.toString()) {
          continue;
        }

        // Share each card
        for (const cardId of cardsToShare) {
          try {
            // Check if already shared (duplicate prevention)
            const existingShare = await CardShare.findOne({
              fromUserId,
              toUserId,
              cardId
            });

            if (existingShare) {
              console.log(`⚠️ Duplicate: Card ${cardId} already shared from ${fromUserId} to ${toUserId}`);
              duplicates.push({
                fromUserId: fromUserId.toString(),
                fromUserName: fromParticipant.userName,
                toUserId: toUserId.toString(),
                toUserName: toParticipant.userName,
                cardId: cardId.toString(),
                reason: "Already shared previously"
              });
              continue;
            }

            // Get card details
            const card = await Card.findById(cardId);
            if (!card) {
              console.log(`❌ Card not found: ${cardId}`);
              errors.push({
                cardId: cardId.toString(),
                error: "Card not found"
              });
              continue;
            }

            // Create CardShare record (for duplicate tracking)
            await CardShare.create({
              sessionId: session._id,
              fromUserId,
              toUserId,
              cardId
            });

            // Create SharedCard record (actual card sharing)
            const sharedCard = await SharedCard.create({
              cardId,
              senderId: fromUserId,
              recipientId: toUserId,
              message: `Shared via Group Sharing session ${session.code}`,
              status: 'sent',
              sentAt: new Date(),
              cardTitle: card.name,
              senderName: fromParticipant.userName,
              recipientName: toParticipant.userName
            });

            console.log(`✅ Shared card ${cardId} from ${fromParticipant.userName} to ${toParticipant.userName}`);

            results.push({
              id: sharedCard._id.toString(),
              fromUserId: fromUserId.toString(),
              fromUserName: fromParticipant.userName,
              toUserId: toUserId.toString(),
              toUserName: toParticipant.userName,
              cardId: cardId.toString(),
              cardName: card.name,
              sharedAt: sharedCard.sentAt
            });

          } catch (shareError: any) {
            console.error(`❌ Error sharing card ${cardId}:`, shareError);
            errors.push({
              fromUserId: fromUserId.toString(),
              toUserId: toUserId.toString(),
              cardId: cardId.toString(),
              error: shareError.message
            });
          }
        }
      }
    }

    // Update session to completed
    session.status = "completed";
    await session.save();

    const summary = {
      totalShares: results.length,
      duplicatesSkipped: duplicates.length,
      errors: errors.length,
      participantCount: session.participants.length
    };

    console.log("✅ Card sharing completed:", summary);

    res.status(200).json({
      success: true,
      results,
      duplicates,
      errors,
      summary
    });

  } catch (error: any) {
    console.error("❌ Error executing card sharing:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to execute card sharing"
    });
  }
});

/**
 * POST /api/group-sharing/end/:sessionId
 * End session or leave session
 */
router.post("/end/:sessionId", requireAuth, async (req: Request, res: Response) => {
  try {
    const { sessionId } = req.params;
    const { userId } = req.body;

    console.log("🛑 Ending session:", { sessionId, userId });

    const session = await GroupSession.findById(sessionId);

    if (!session) {
      return res.status(404).json({
        success: false,
        error: "Session not found"
      });
    }

    // If admin is ending, mark session as completed
    if (session.adminId.toString() === userId) {
      session.status = "completed";
      session.isActive = false;
      await session.save();

      console.log("✅ Session ended by admin");

      return res.status(200).json({
        success: true,
        message: "Session ended successfully"
      });
    }

    // If participant is leaving, remove them
    session.participants = session.participants.filter(
      p => p.userId.toString() !== userId
    );

    // If no participants left, mark inactive
    if (session.participants.length === 0) {
      session.isActive = false;
    }

    await session.save();

    console.log("✅ Participant left session");

    res.status(200).json({
      success: true,
      message: "Left session successfully"
    });

  } catch (error: any) {
    console.error("❌ Error ending session:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to end session"
    });
  }
});

/**
 * GET /api/group-sharing/session-by-code/:code
 * Get session by code (for joining)
 */
router.get("/session-by-code/:code", requireAuth, async (req: Request, res: Response) => {
  try {
    const { code } = req.params;

    const session = await GroupSession.findOne({ 
      code, 
      isActive: true,
      expiresAt: { $gt: new Date() }
    });

    if (!session) {
      return res.status(404).json({
        success: false,
        error: "Invalid or expired code"
      });
    }

    res.status(200).json({
      success: true,
      session: formatSession(session)
    });

  } catch (error: any) {
    console.error("❌ Error getting session by code:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to get session"
    });
  }
});

export default router;
