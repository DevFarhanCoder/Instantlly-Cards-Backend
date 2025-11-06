// Group Sharing Routes - Complete API Implementation
import express, { Request, Response } from "express";
import GroupSession, { IParticipant } from "../models/GroupSession";
import CardShare from "../models/CardShare";
import SharedCard from "../models/SharedCard";
import GroupSharedCard from "../models/GroupSharedCard";
import Group from "../models/Group";
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
  allowParticipantSharing: session.allowParticipantSharing, // Admin control for sharing permissions
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
    const { code, adminId, adminName, adminPhone, adminPhoto, expirationMinutes = 10, allowParticipantSharing = false } = req.body;

    console.log("📝 Creating group session:", { code, adminId, adminName, adminPhone, allowParticipantSharing });

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
      userId: adminId, // Now a string, no ObjectId conversion needed
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
      adminId: adminId, // Now a string, no ObjectId conversion needed
      adminName,
      adminPhone,
      adminPhoto,
      participants: [adminParticipant],
      status: "waiting",
      allowParticipantSharing, // Admin controls if participants can share with each other
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
      userId: userId, // Now a string, no ObjectId conversion needed
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
 * Execute the actual card sharing between participants
 * SUPPORTS TWO FLOWS:
 * 1. Create Group (groupName provided) -> Cards stored in GroupSharedCard for Group tabs
 * 2. Quit Sharing (no groupName) -> Cards stored in SharedCard for Messaging tabs
 */
router.post("/execute/:sessionId", requireAuth, async (req: Request, res: Response) => {
  try {
    const { sessionId } = req.params;
    const { adminId, groupName } = req.body; // groupName is optional

    console.log("🚀 Executing card sharing:", { sessionId, adminId, groupName: groupName || "N/A (Quit Sharing)" });

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

    // FLOW SPLIT: Create Group vs Quit Sharing
    let group: any = null;
    let groupId: mongoose.Types.ObjectId | null = null;

    if (groupName && groupName.trim()) {
      // FLOW 1: CREATE GROUP - Store in GroupSharedCard
      console.log("📁 Creating permanent group:", groupName);
      
      try {
        // Extract all unique member IDs from session and convert to ObjectId
        // For temporary userIds (user_timestamp), we'll try to find their actual User document
        const memberObjectIds: mongoose.Types.ObjectId[] = [];
        
        for (const participant of session.participants) {
          const userId = participant.userId;
          
          // Check if it's a valid ObjectId
          if (mongoose.Types.ObjectId.isValid(userId) && userId.length === 24) {
            memberObjectIds.push(new mongoose.Types.ObjectId(userId));
          } else {
            // For temporary userIds like "user_1762425364619", try to find by phone
            if (participant.userPhone) {
              const user = await User.findOne({ phone: participant.userPhone });
              if (user) {
                memberObjectIds.push(user._id);
              } else {
                console.warn(`⚠️ Could not find User for temporary userId ${userId}, phone: ${participant.userPhone}`);
                // Skip this user or create a new User document
                // For now, we'll skip to avoid creating invalid groups
              }
            }
          }
        }
        
        if (memberObjectIds.length === 0) {
          throw new Error("No valid user IDs found for group creation");
        }
        
        // Determine admin ObjectId
        let adminObjectId: mongoose.Types.ObjectId;
        if (mongoose.Types.ObjectId.isValid(session.adminId) && session.adminId.length === 24) {
          adminObjectId = new mongoose.Types.ObjectId(session.adminId);
        } else {
          // Find admin by phone
          const adminParticipant = session.participants.find(p => p.userId === session.adminId);
          if (adminParticipant?.userPhone) {
            const adminUser = await User.findOne({ phone: adminParticipant.userPhone });
            if (!adminUser) {
              throw new Error("Admin user not found in database. Cannot create permanent group.");
            }
            adminObjectId = adminUser._id;
          } else {
            throw new Error("Admin user not found. Cannot create permanent group.");
          }
        }
        
        // Create group document
        group = await Group.create({
          name: groupName.trim(),
          admin: adminObjectId,
          members: memberObjectIds,
          isActive: true,
          lastMessageTime: new Date()
        });
        
        groupId = group._id;
        console.log(`✅ Group created with ID: ${groupId}, joinCode: ${group.joinCode}`);
        
      } catch (groupError: any) {
        console.error("❌ Failed to create group:", groupError);
        return res.status(500).json({
          success: false,
          error: "Failed to create group: " + groupError.message
        });
      }
    } else {
      // FLOW 2: QUIT SHARING - Store in SharedCard (peer-to-peer)
      console.log("🚪 Quit Sharing mode - cards will be saved to Messaging tabs");
    }

    const results: any[] = [];
    const duplicates: any[] = [];
    const errors: any[] = [];

    // BATCH PREPARATION: Collect all cards to share
    const groupCardsBatch: any[] = []; // For GroupSharedCard.insertMany()
    const peerCardsBatch: any[] = []; // For SharedCard.insertMany()
    const cardShareBatch: any[] = []; // For CardShare tracking

    // For each participant
    for (const fromParticipant of session.participants) {
      const fromUserId = fromParticipant.userId;
      const cardsToShare = fromParticipant.cardsToShare;

      if (cardsToShare.length === 0) {
        console.log(`⚠️ User ${fromParticipant.userName} has no cards to share`);
        continue;
      }

      // Determine who to share with based on admin permission
      let recipientsToShareWith: typeof session.participants;
      
      if (session.allowParticipantSharing) {
        // Mode 1: Everyone shares with everyone (current behavior)
        recipientsToShareWith = session.participants.filter(
          p => p.userId.toString() !== fromUserId.toString()
        );
        console.log(`📤 ${fromParticipant.userName} sharing with all participants (full exchange mode)`);
      } else {
        // Mode 2: Hub-spoke model (admin-controlled)
        const isAdmin = fromUserId.toString() === session.adminId.toString();
        
        if (isAdmin) {
          // Admin shares with all participants
          recipientsToShareWith = session.participants.filter(
            p => p.userId.toString() !== session.adminId.toString()
          );
          console.log(`📤 Admin ${fromParticipant.userName} sharing with all participants`);
        } else {
          // Non-admin participants only share with admin
          recipientsToShareWith = session.participants.filter(
            p => p.userId.toString() === session.adminId.toString()
          );
          console.log(`📤 ${fromParticipant.userName} sharing with admin only (restricted mode)`);
        }
      }

      // Prepare cards for batch insert
      for (const cardId of cardsToShare) {
        try {
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

          if (groupId) {
            // FLOW 1: GROUP SHARING - Add to GroupSharedCard batch
            groupCardsBatch.push({
              cardId,
              senderId: fromUserId,
              groupId,
              message: `Shared via Group Sharing session ${session.code}`,
              sentAt: new Date(),
              cardTitle: card.name,
              senderName: fromParticipant.userName,
              groupName: groupName.trim()
            });

            results.push({
              fromUserId: fromUserId.toString(),
              fromUserName: fromParticipant.userName,
              cardId: cardId.toString(),
              cardName: card.name,
              groupId: groupId.toString(),
              groupName: groupName.trim(),
              type: "group"
            });

          } else {
            // FLOW 2: PEER-TO-PEER - Add to SharedCard batch
            for (const toParticipant of recipientsToShareWith) {
              const toUserId = toParticipant.userId;

              // Check for duplicates
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

              // Add to peer cards batch
              peerCardsBatch.push({
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

              // Add to tracking batch
              cardShareBatch.push({
                sessionId: session._id,
                fromUserId,
                toUserId,
                cardId
              });

              results.push({
                fromUserId: fromUserId.toString(),
                fromUserName: fromParticipant.userName,
                toUserId: toUserId.toString(),
                toUserName: toParticipant.userName,
                cardId: cardId.toString(),
                cardName: card.name,
                type: "peer-to-peer"
              });
            }
          }

        } catch (cardError: any) {
          console.error(`❌ Error processing card ${cardId}:`, cardError);
          errors.push({
            cardId: cardId.toString(),
            error: cardError.message
          });
        }
      }
    }

    // BATCH INSERT: 100x faster than sequential inserts
    try {
      if (groupCardsBatch.length > 0) {
        console.log(`📦 Batch inserting ${groupCardsBatch.length} GroupSharedCard records...`);
        await GroupSharedCard.insertMany(groupCardsBatch, { ordered: false });
        console.log(`✅ Successfully inserted ${groupCardsBatch.length} group cards`);
      }

      if (peerCardsBatch.length > 0) {
        console.log(`📦 Batch inserting ${peerCardsBatch.length} SharedCard records...`);
        await SharedCard.insertMany(peerCardsBatch, { ordered: false });
        console.log(`✅ Successfully inserted ${peerCardsBatch.length} peer-to-peer cards`);
      }

      if (cardShareBatch.length > 0) {
        console.log(`📦 Batch inserting ${cardShareBatch.length} CardShare tracking records...`);
        await CardShare.insertMany(cardShareBatch, { ordered: false });
        console.log(`✅ Successfully inserted ${cardShareBatch.length} tracking records`);
      }

    } catch (batchError: any) {
      console.error("❌ Batch insert error:", batchError);
      // Continue even if batch insert partially fails
    }

    // Update session to completed
    session.status = "completed";
    await session.save();

    const summary = {
      totalShares: results.length,
      duplicatesSkipped: duplicates.length,
      errors: errors.length,
      participantCount: session.participants.length,
      sharingType: groupId ? "group" : "peer-to-peer"
    };

    console.log("✅ Card sharing completed:", summary);

    res.status(200).json({
      success: true,
      results,
      duplicates,
      errors,
      summary,
      groupId: groupId?.toString(),
      groupName: groupName?.trim(),
      joinCode: group?.joinCode
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

/**
 * POST /api/group-sharing/create-messaging-group/:sessionId
 * Create a permanent messaging group from a group sharing session (Admin only)
 */
router.post("/create-messaging-group/:sessionId", requireAuth, async (req: Request, res: Response) => {
  try {
    const { sessionId } = req.params;
    const { adminId, groupName, groupDescription, groupIcon } = req.body;

    console.log("👥 Creating messaging group from session:", { sessionId, adminId, groupName });

    // Find the group session
    const session = await GroupSession.findById(sessionId);

    if (!session) {
      return res.status(404).json({
        success: false,
        error: "Session not found"
      });
    }

    // Verify admin
    if (session.adminId.toString() !== adminId) {
      console.log("❌ Unauthorized: Not admin");
      return res.status(403).json({
        success: false,
        error: "Only admin can create group from session"
      });
    }

    // Check if session is completed (cards have been shared)
    if (session.status !== "completed") {
      console.log("❌ Session not completed yet, status:", session.status);
      return res.status(400).json({
        success: false,
        error: "Session must be completed before creating a group"
      });
    }

    // Import Group model dynamically to avoid circular dependencies
    const Group = (await import("../models/Group")).default;

    // Extract participant user IDs (exclude admin as they'll be added separately)
    const memberIds = session.participants
      .filter(p => p.userId.toString() !== adminId)
      .map(p => p.userId);

    console.log("👥 Creating group with members:", {
      admin: adminId,
      memberCount: memberIds.length,
      totalParticipants: session.participants.length
    });

    // Generate group name if not provided
    const defaultGroupName = `Group ${session.code}`;
    const finalGroupName = groupName || defaultGroupName;

    // Generate unique invite code
    let joinCode;
    try {
      joinCode = await Group.generateInviteCode();
      console.log('✅ Generated invite code:', joinCode);
    } catch (error) {
      // Fallback code generation
      const timestamp = Date.now().toString(36).toUpperCase();
      joinCode = timestamp.substring(timestamp.length - 6);
      console.log('✅ Fallback invite code:', joinCode);
    }

    // Create the messaging group
    const group = await Group.create({
      name: finalGroupName,
      description: groupDescription || `Group created from sharing session ${session.code}`,
      icon: groupIcon || '',
      members: [...memberIds, new mongoose.Types.ObjectId(adminId)],
      admin: new mongoose.Types.ObjectId(adminId),
      joinCode: joinCode,
      createdAt: new Date(),
      updatedAt: new Date()
    });

    console.log("✅ Messaging group created successfully:", group._id);

    // Populate the group data
    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phone profilePicture')
      .populate('admin', 'name phone');

    // Send notifications to members (non-blocking)
    setImmediate(async () => {
      try {
        const admin = await User.findById(adminId);
        const adminName = admin?.name || 'Someone';
        
        // Import notification service
        const { sendGroupInviteNotification } = await import('../services/pushNotifications');
        
        for (const memberId of memberIds) {
          try {
            const member = await User.findById(memberId);
            if (member?.pushToken && member.pushToken !== 'expo-go-local-mode') {
              await sendGroupInviteNotification(
                member.pushToken,
                adminName,
                group.name,
                (group._id as any).toString(),
                adminId
              );
              console.log(`📱 Group invite notification sent to ${member.name}`);
            }
          } catch (memberError) {
            console.error(`Failed to send notification to member ${memberId}:`, memberError);
          }
        }
      } catch (notificationError) {
        console.error('Failed to send group notifications:', notificationError);
      }
    });

    res.status(201).json({
      success: true,
      group: populatedGroup,
      joinCode: joinCode,
      message: "Messaging group created successfully"
    });

  } catch (error: any) {
    console.error("❌ Error creating messaging group from session:", error);
    res.status(500).json({
      success: false,
      error: error.message || "Failed to create messaging group"
    });
  }
});

export default router;
