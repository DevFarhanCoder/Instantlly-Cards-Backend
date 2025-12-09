import { Router } from "express";
import Card from "../models/Card";
import SharedCard from "../models/SharedCard";
import GroupSharedCard from "../models/GroupSharedCard";
import User from "../models/User";
import Group from "../models/Group";
import Contact from "../models/Contact";
import { AuthReq, requireAuth } from "../middleware/auth";
import { sendCardSharingNotification, sendCardCreationNotification } from "../services/pushNotifications";
import fs from "fs";
import path from "path";
import { v4 as uuidv4 } from "uuid";

const r = Router();

// Helper function to save Base64 image to file system
const saveBase64Image = async (base64Data: string, userId: string): Promise<string> => {
  try {
    // Check if it's a data URI
    if (!base64Data.startsWith('data:image/')) {
      throw new Error('Invalid image data format');
    }

    // Extract mime type and base64 data
    const matches = base64Data.match(/^data:image\/([^;]+);base64,(.+)$/);
    if (!matches) {
      throw new Error('Invalid Base64 image format');
    }

    const [, extension, base64] = matches;
    const buffer = Buffer.from(base64, 'base64');

    // Validate file size (limit to 5MB)
    if (buffer.length > 5 * 1024 * 1024) {
      throw new Error('Image size too large (max 5MB)');
    }

    // Generate unique filename
    const filename = `${uuidv4()}.${extension}`;
    const uploadDir = path.join(process.cwd(), 'uploads', 'cards');
    const filepath = path.join(uploadDir, filename);

    // Ensure upload directory exists
    if (!fs.existsSync(uploadDir)) {
      fs.mkdirSync(uploadDir, { recursive: true });
    }

    // Save file to disk
    fs.writeFileSync(filepath, buffer);

    // Return relative path for storing in database
    return `/uploads/cards/${filename}`;
  } catch (error) {
    console.error('Error saving Base64 image:', error);
    throw new Error('Failed to process image');
  }
};

// Public feed (no auth) – latest 50 cards
r.get("/feed/public", async (_req, res) => {
  const items = await Card.find({}).sort({ createdAt: -1 }).limit(50).lean();
  res.json({ data: items });
});

// Require auth for everything below
r.use(requireAuth);

// CONTACTS FEED - Get cards from my contacts AND my own cards (OPTIMIZED WITH CACHING)
r.get("/feed/contacts", async (req: AuthReq, res) => {
  try {
    const userId = req.userId!;
    const startTime = Date.now();
    
    console.log(`📱 [${userId}] Fetching contacts feed...`);
    
    // Get all contacts who are app users (optimized query with select)
    const myContacts = await Contact.find({ 
      userId,
      isAppUser: true 
    })
    .select('appUserId')
    .lean()
    .exec();
    
    // Extract contact user IDs with proper typing
    const contactUserIds = myContacts.map((contact: any) => contact.appUserId).filter(Boolean);
    
    // Add current user's ID to see their own cards too
    const allUserIds = [userId, ...contactUserIds];
    
    console.log(`📞 [${userId}] Found ${contactUserIds.length} contacts on app`);
    
    // Get cards from contacts AND own cards (optimized with lean and select)
    const allCards = await Card.find({
      userId: { $in: allUserIds }
    })
    .select('_id userId name companyName designation companyPhoto email companyEmail personalPhone companyPhone location companyAddress createdAt updatedAt')
    .sort({ createdAt: -1 })
    .limit(100)
    .lean()
    .exec();
    
    // Separate own cards and contact cards for metadata
    const ownCards = allCards.filter((card: any) => card.userId.toString() === userId);
    const contactCards = allCards.filter((card: any) => card.userId.toString() !== userId);
    
    const elapsed = Date.now() - startTime;
    console.log(`✅ [${userId}] Feed loaded in ${elapsed}ms - ${ownCards.length} own + ${contactCards.length} from contacts = ${allCards.length} total`);
    
    // Generate ETag based on card IDs and update times for browser caching
    const etag = `"${allCards.map((c: any) => `${c._id}-${c.updatedAt}`).join(',').substring(0, 32)}"`;
    
    // Check if client has cached version
    if (req.headers['if-none-match'] === etag) {
      console.log(`💾 [${userId}] Client has cached version - returning 304 Not Modified`);
      return res.status(304).end();
    }
    
    // Set cache headers
    res.setHeader('ETag', etag);
    res.setHeader('Cache-Control', 'private, max-age=300'); // Cache for 5 minutes
    
    res.json({ 
      success: true,
      data: allCards,
      meta: {
        totalContacts: contactUserIds.length,
        totalCards: allCards.length,
        ownCards: ownCards.length,
        contactCards: contactCards.length,
        loadTimeMs: elapsed
      }
    });
  } catch (err) {
    console.error("❌ CONTACTS FEED ERROR", err);
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
    let cardData = { ...req.body, userId };

    // Handle Base64 image conversion if companyPhoto is provided
    if (cardData.companyPhoto && cardData.companyPhoto.startsWith('data:image/')) {
      console.log('🖼️ Processing Base64 image for card...');
      try {
        const imagePath = await saveBase64Image(cardData.companyPhoto, userId);
        cardData.companyPhoto = imagePath;
        console.log('✅ Image saved successfully:', imagePath);
      } catch (imageError) {
        console.error('❌ Failed to process image:', imageError);
        // Continue without image rather than failing entire card creation
        cardData.companyPhoto = '';
      }
    }

    const doc = await Card.create(cardData);
    
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

// LIST own cards (OPTIMIZED WITH PROPER CACHING AND LOGGING)
r.get("/", async (req: AuthReq, res) => {
  try {
    const reqId = req.get('x-req-id') || req.get('X-REQ-ID') || 'no-req-id';
    const userId = req.userId!;
    const startTime = Date.now();

    console.log(`🔖 [REQ:${reqId}] GET /api/cards called - auth header present? ${!!req.header('authorization')} - resolved userId: ${userId}`);
    console.log(`📇 [${userId}] Fetching user's own cards...`);
    
    // Get user's cards sorted by creation date (descending)
    const items = await Card.find({ userId })
      .sort({ createdAt: -1 })
      .lean()
      .exec();
    
    const elapsed = Date.now() - startTime;
    console.log(`✅ [${userId}] Own cards loaded in ${elapsed}ms - Found ${items.length} cards`);
    items.forEach((card, i) => {
      console.log(`   Card ${i + 1}: "${card.name}"`);
    });
    
    // Generate ETag for proper caching
    const etag = `"own-cards-${userId}-${items.length}-${items[0]?.updatedAt || 'empty'}"`;
    
    // Check if client has cached version
    if (req.headers['if-none-match'] === etag) {
      console.log(`💾 [${userId}] Client has cached own cards - returning 304`);
      return res.status(304).end();
    }
    
    // Set proper cache headers
    res.setHeader('ETag', etag);
    res.setHeader('Cache-Control', 'private, max-age=60'); // Cache for 1 minute
    
    res.json({ 
      success: true,
      data: items,
      meta: {
        totalCards: items.length,
        loadTimeMs: elapsed
      }
    });
  } catch (err) {
    console.error(`❌ [${req.userId}] ERROR fetching own cards:`, err);
    res.status(500).json({ 
      success: false,
      message: "Failed to fetch cards",
      data: [] 
    });
  }
});

// UPDATE
r.put("/:id", async (req: AuthReq, res) => {
  try {
    const userId = req.userId!;
    let updateData = { ...req.body };

    // Handle Base64 image conversion if companyPhoto is provided
    if (updateData.companyPhoto && updateData.companyPhoto.startsWith('data:image/')) {
      console.log('🖼️ Processing Base64 image for card update...');
      try {
        // Get existing card to potentially delete old image
        const existingCard = await Card.findOne({ _id: req.params.id, userId });
        
        const imagePath = await saveBase64Image(updateData.companyPhoto, userId);
        updateData.companyPhoto = imagePath;
        console.log('✅ Image updated successfully:', imagePath);

        // Clean up old image file if it exists and is different
        if (existingCard?.companyPhoto && 
            existingCard.companyPhoto !== imagePath && 
            existingCard.companyPhoto.startsWith('/uploads/')) {
          try {
            const oldPath = path.join(process.cwd(), existingCard.companyPhoto);
            if (fs.existsSync(oldPath)) {
              fs.unlinkSync(oldPath);
              console.log('🗑️ Deleted old image:', existingCard.companyPhoto);
            }
          } catch (deleteError) {
            console.error('Warning: Failed to delete old image:', deleteError);
          }
        }
      } catch (imageError) {
        console.error('❌ Failed to process image:', imageError);
        // Continue without updating image rather than failing entire update
        delete updateData.companyPhoto;
      }
    }

    const doc = await Card.findOneAndUpdate(
      { _id: req.params.id, userId },
      updateData,
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

    // PERFORMANCE OPTIMIZATION: Store all data in shared card (ultra denormalization)
    // This eliminates need for populate queries later
    const sharedCard = await SharedCard.create({
      cardId,
      senderId,
      recipientId,
      message: message || "",
      cardTitle: card.companyName || card.name || 'Business Card',
      senderName: sender.name,
      recipientName: recipient.name,
      cardPhoto: card.companyPhoto || "", // ✅ Store photo URL directly
      senderProfilePicture: sender.profilePicture || "", // ✅ Store sender profile pic
      recipientProfilePicture: recipient.profilePicture || "", // ✅ Store recipient profile pic
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

// GET SENT CARDS (CURSOR-BASED PAGINATION FOR 100K+ RECORDS)
r.get("/sent", async (req: AuthReq, res) => {
  try {
    const senderId = req.userId!;
    const startTime = Date.now();
    
    // Cursor-based pagination parameters
    const cursor = req.query.cursor as string | undefined; // Last _id from previous page
    const limit = Math.min(parseInt(req.query.limit as string) || 20, 50); // Max 50 per page
    
    console.log(`📤 [${senderId}] Fetching sent cards (cursor: ${cursor || 'first'}, limit: ${limit})...`);
    
    // Build query with cursor for pagination
    const query: any = { senderId };
    if (cursor) {
      query._id = { $lt: cursor }; // Get records before this cursor (pagination)
    }
    
    // ULTRA-FAST QUERY: Single query, no populate, uses cached fields
    const sentCards = await SharedCard.find(query)
      .select('_id cardId recipientId recipientName cardTitle cardPhoto recipientProfilePicture sentAt status message viewedAt')
      .sort({ _id: -1 }) // Sort by _id (equivalent to creation time, uses index)
      .limit(limit + 1) // Fetch one extra to check if there's more
      .lean()
      .exec();

    // Check if there are more pages
    const hasMore = sentCards.length > limit;
    const items = sentCards.slice(0, limit);
    const nextCursor = hasMore && items.length > 0 ? items[items.length - 1]._id : null;

    const elapsed = Date.now() - startTime;
    console.log(`✅ [${senderId}] Sent cards loaded in ${elapsed}ms - ${items.length} cards${hasMore ? ' (more available)' : ' (last page)'}`);

    // Generate ETag for HTTP caching
    const etag = `"sent-${senderId}-${cursor || 'first'}-${limit}-${items[0]?._id || 'empty'}-${Date.now()}"`;
    
    // Disable ETag checking to avoid stale data
    // if (req.headers['if-none-match'] === etag) {
    //   console.log(`💾 [${senderId}] Client cache hit - returning 304`);
    //   return res.status(304).end();
    // }
    
    res.setHeader('ETag', etag);
    res.setHeader('Cache-Control', 'private, no-cache, must-revalidate');

    res.json({
      success: true,
      data: items,
      pagination: {
        nextCursor,
        hasMore,
        count: items.length,
        limit
      },
      meta: {
        loadTimeMs: elapsed
      }
    });
  } catch (err) {
    console.error(`❌ [${req.userId}] GET SENT CARDS ERROR:`, err);
    res.status(500).json({ 
      success: false,
      message: "Failed to fetch sent cards",
      data: [],
      pagination: { nextCursor: null, hasMore: false, count: 0 }
    });
  }
});

// GET RECEIVED CARDS (CURSOR-BASED PAGINATION FOR 100K+ RECORDS WITH SEARCH)
r.get("/received", async (req: AuthReq, res) => {
  try {
    const recipientId = req.userId!;
    const startTime = Date.now();
    
    // Cursor-based pagination parameters
    const cursor = req.query.cursor as string | undefined;
    const limit = Math.min(parseInt(req.query.limit as string) || 20, 50); // Max 50 per page
    
    // Search parameters
    const search = req.query.search as string | undefined;
    const fuzzy = req.query.fuzzy === 'true';
    const caseSensitive = req.query.case_sensitive === 'true';
    const sortBy = req.query.sort_by as string | undefined;
    
    console.log(`📥 [${recipientId}] Fetching received cards (cursor: ${cursor || 'first'}, limit: ${limit}, search: "${search || 'none'}", fuzzy: ${fuzzy})...`);
    
    // Build query with cursor for pagination
    const query: any = { recipientId };
    if (cursor) {
      query._id = { $lt: cursor };
    }
    
    // Add search functionality
    if (search && search.trim()) {
      const searchTerm = search.trim();
      const searchOptions = caseSensitive ? undefined : 'i'; // Case insensitive by default
      
      if (fuzzy) {
        // Optimized fuzzy search - create broader patterns for better matching
        const searchWords = searchTerm.toLowerCase().split(/\s+/).filter(word => word.length > 1);
        
        if (searchWords.length > 0) {
          // Create flexible patterns for each word
          const patterns = searchWords.map(word => {
            // Escape special regex characters but keep flexibility
            const escapedWord = word.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
            // Create pattern that matches partial words at beginning, middle, or end
            return new RegExp(`${escapedWord}`, searchOptions);
          });
          
          // Build efficient OR query across fields with AND logic for multiple words
          if (searchWords.length === 1) {
            // Single word - OR across all fields
            const singlePattern = patterns[0];
            query.$or = [
              { cardTitle: singlePattern },
              { senderName: singlePattern },
              { message: singlePattern }
            ];
          } else {
            // Multiple words - each word must match in at least one field
            query.$and = searchWords.map((word, index) => ({
              $or: [
                { cardTitle: patterns[index] },
                { senderName: patterns[index] },
                { message: patterns[index] }
              ]
            }));
          }
        }
      } else {
        // Optimized simple text search
        const escapedTerm = searchTerm.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
        const searchPattern = new RegExp(escapedTerm, searchOptions);
        query.$or = [
          { cardTitle: searchPattern },
          { senderName: searchPattern },
          { message: searchPattern }
        ];
      }
    }
    
    // Build sort criteria
    let sortCriteria: any = { _id: -1 }; // Default sort by creation time
    if (search && sortBy === 'relevance') {
      // For search results, use text score for relevance (requires text index)
      // For now, we'll use a simple relevance approximation
      sortCriteria = { _id: -1 }; // MongoDB will handle relevance in $or queries
    }
    
    // ULTRA-FAST QUERY: Single query, no populate, uses cached fields
    const receivedCards = await SharedCard.find(query)
      .select('_id cardId senderId senderName cardTitle cardPhoto senderProfilePicture sentAt status message viewedAt')
      .sort(sortCriteria)
      .limit(limit + 1)
      .lean()
      .exec();

    // Check if there are more pages
    const hasMore = receivedCards.length > limit;
    const items = receivedCards.slice(0, limit).map((share: any) => ({
      ...share,
      receivedAt: share.sentAt,
      isViewed: share.status === 'viewed'
    }));
    const nextCursor = hasMore && items.length > 0 ? items[items.length - 1]._id : null;

    const elapsed = Date.now() - startTime;
    const unviewedCount = items.filter(card => !card.isViewed).length;
    
    // Enhanced logging for search queries
    if (search && search.trim()) {
      console.log(`🔍 [${recipientId}] SEARCH QUERY: "${search}" (fuzzy: ${fuzzy}) -> ${items.length} results in ${elapsed}ms`);
      if (items.length > 0) {
        console.log(`🔍 [${recipientId}] Sample results: ${items.slice(0, 3).map(item => `"${item.cardTitle}" from ${item.senderName}`).join(', ')}`);
      }
    } else {
      console.log(`✅ [${recipientId}] Received cards loaded in ${elapsed}ms - ${items.length} cards, ${unviewedCount} unviewed${hasMore ? ' (more available)' : ' (last page)'}`);
    }

    // Generate ETag for HTTP caching (disable caching for search queries)
    const etag = search ? null : `"received-${recipientId}-${cursor || 'first'}-${limit}-${items[0]?._id || 'empty'}-${Date.now()}"`;
    
    // Don't use if-none-match for now to avoid stale data issues
    // if (etag && req.headers['if-none-match'] === etag) {
    //   console.log(`💾 [${recipientId}] Client cache hit - returning 304`);
    //   return res.status(304).end();
    // }
    
    if (etag) {
      res.setHeader('ETag', etag);
      res.setHeader('Cache-Control', 'private, no-cache, must-revalidate'); // Changed to no-cache
    } else {
      // Don't cache search results
      res.setHeader('Cache-Control', 'no-cache, no-store, must-revalidate');
    }

    res.json({
      success: true,
      data: items,
      pagination: {
        nextCursor,
        hasMore,
        count: items.length,
        unviewedCount,
        limit
      },
      meta: {
        loadTimeMs: elapsed
      }
    });
  } catch (err) {
    console.error(`❌ [${req.userId}] GET RECEIVED CARDS ERROR:`, err);
    res.status(500).json({ 
      success: false,
      message: "Failed to fetch received cards",
      data: [],
      pagination: { nextCursor: null, hasMore: false, count: 0, unviewedCount: 0 }
    });
  }
});

// GET SINGLE CARD BY ID (for viewing any card)
// IMPORTANT: This route MUST be after /sent and /received to avoid route conflicts
r.get("/:id", async (req: AuthReq, res) => {
  try {
    const cardId = req.params.id;
    const userId = req.userId!;
    const startTime = Date.now();
    
    console.log(`🔍 [${userId}] Fetching single card: ${cardId}`);
    
    // Validate ObjectId format
    if (!cardId.match(/^[0-9a-fA-F]{24}$/)) {
      console.log(`❌ [${userId}] Invalid card ID format: ${cardId}`);
      return res.status(400).json({ 
        success: false,
        message: "Invalid card ID format" 
      });
    }
    
    // First, try to find as a regular Card
    let card = await Card.findById(cardId)
      .populate('userId', 'name profilePicture')
      .lean()
      .maxTimeMS(5000)
      .exec();
    
    // If not found, check if it's a SharedCard ID (mobile app might be sending wrong ID)
    if (!card) {
      console.log(`🔄 [${userId}] Not found as Card, checking SharedCard: ${cardId}`);
      const sharedCard = await SharedCard.findById(cardId)
        .select('cardId')
        .lean()
        .maxTimeMS(3000)
        .exec();
      
      if (sharedCard && sharedCard.cardId) {
        console.log(`🔄 [${userId}] Found SharedCard, fetching actual card: ${sharedCard.cardId}`);
        card = await Card.findById(sharedCard.cardId)
          .populate('userId', 'name profilePicture')
          .lean()
          .maxTimeMS(5000)
          .exec();
      }
    }
    
    if (!card) {
      console.log(`❌ [${userId}] Card not found: ${cardId}`);
      return res.status(404).json({ 
        success: false,
        message: "Card not found" 
      });
    }

    const elapsed = Date.now() - startTime;
    console.log(`✅ [${userId}] Single card fetched in ${elapsed}ms - Card: ${(card as any).name || (card as any).companyName || 'Unnamed'}`);

    // Generate ETag for caching
    const etag = `"card-${card._id}-${(card as any).updatedAt}"`;
    
    // Check if client has cached version
    if (req.headers['if-none-match'] === etag) {
      console.log(`💾 [${userId}] Client has cached card ${cardId} - returning 304`);
      return res.status(304).end();
    }
    
    // Set cache headers
    res.setHeader('ETag', etag);
    res.setHeader('Cache-Control', 'private, max-age=300'); // Cache for 5 minutes

    res.json({ 
      success: true,
      data: card,
      meta: {
        loadTimeMs: elapsed
      }
    });
  } catch (err: any) {
    console.error(`❌ [${req.userId}] GET CARD BY ID ERROR for ${req.params.id}:`, err.message);
    
    // Handle timeout gracefully
    if (err.message?.includes('timeout')) {
      return res.status(504).json({ 
        success: false,
        message: "Request timeout - please try again",
        error: "TIMEOUT"
      });
    }
    
    res.status(500).json({ 
      success: false,
      message: "Failed to fetch card",
      error: "INTERNAL_ERROR"
    });
  }
});

// MARK CARD AS VIEWED
r.post("/shared/:id/view", async (req: AuthReq, res) => {
  try {
    const sharedCardId = req.params.id;
    const userId = req.userId!;
    
    console.log(`👁️ Marking card as viewed: ${sharedCardId} by user: ${userId}`);
    
    // Add timeout protection
    const updatedCard = await Promise.race([
      SharedCard.findOneAndUpdate(
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
      ).maxTimeMS(5000).lean(),
      new Promise<null>((_, reject) => 
        setTimeout(() => reject(new Error('View update timeout')), 5000)
      )
    ]);

    if (!updatedCard) {
      console.log(`⚠️ Shared card not found or already viewed: ${sharedCardId}`);
      
      // Check if card exists at all
      const cardExists = await SharedCard.findById(sharedCardId).select('_id status recipientId').maxTimeMS(3000).lean();
      
      if (!cardExists) {
        return res.status(404).json({ 
          success: false,
          message: "Shared card not found",
          error: "CARD_NOT_FOUND"
        });
      }
      
      // Card exists but either already viewed or user is not recipient
      return res.status(200).json({ 
        success: true, 
        message: "Card already viewed or you are not the recipient",
        alreadyViewed: cardExists.status === 'viewed'
      });
    }

    console.log(`✅ Card marked as viewed: ${sharedCardId}`);
    res.json({ 
      success: true, 
      message: "Card marked as viewed",
      viewedAt: updatedCard.viewedAt
    });
  } catch (err: any) {
    console.error("❌ MARK CARD VIEWED ERROR:", err.message);
    
    // If timeout, return success anyway (update will complete eventually)
    if (err.message?.includes('timeout')) {
      return res.status(200).json({ 
        success: true, 
        message: "View status updating (may take a moment)",
        pending: true
      });
    }
    
    res.status(500).json({ 
      success: false,
      message: "Failed to mark card as viewed",
      error: "INTERNAL_ERROR"
    });
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

// GET GROUP CARDS SUMMARY - Cards sent and received counts for a group (OPTIMIZED)
r.get("/group/:groupId/summary", async (req: AuthReq, res) => {
  try {
    const groupId = req.params.groupId;
    const currentUserId = req.userId!;
    const startTime = Date.now();
    
    console.log(`📊 [${currentUserId}] Fetching group cards summary for group: ${groupId}`);
    
    // Verify group exists and user is a member (lean query)
    const group = await Group.findById(groupId).select('members').lean().exec();
    if (!group) {
      return res.status(404).json({ success: false, message: "Group not found" });
    }

    // Check if user is a member of the group
    const isMember = group.members.some((memberId: any) => memberId.toString() === currentUserId);
    if (!isMember) {
      return res.status(403).json({ success: false, message: "You are not a member of this group" });
    }
    
    // Get cards sent and received in parallel for better performance
    const [sentCards, receivedCards] = await Promise.all([
      // Cards sent by current user to this group
      GroupSharedCard.find({ 
        groupId, 
        senderId: currentUserId 
      })
      .populate('cardId', 'companyName name companyPhoto')
      .sort({ sentAt: -1 })
      .lean()
      .exec(),
      
      // Cards received by current user in this group (sent by others)
      GroupSharedCard.find({ 
        groupId, 
        senderId: { $ne: currentUserId } 
      })
      .populate('cardId', 'companyName name companyPhoto')
      .populate('senderId', 'name profilePicture')
      .sort({ sentAt: -1 })
      .lean()
      .exec()
    ]);

    // Format sent cards
    const formattedSentCards = sentCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId?._id || share.cardId,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId?.companyPhoto,
      sentAt: share.sentAt,
      message: share.message,
      isFromMe: true
    }));

    // Format received cards
    const formattedReceivedCards = receivedCards.map((share: any) => ({
      _id: share._id,
      cardId: share.cardId?._id || share.cardId,
      senderId: share.senderId?._id || share.senderId,
      senderName: share.senderName,
      senderProfilePicture: share.senderId?.profilePicture,
      cardTitle: share.cardTitle,
      cardPhoto: share.cardId?.companyPhoto,
      sentAt: share.sentAt,
      message: share.message,
      isFromMe: false
    }));

    const elapsed = Date.now() - startTime;
    console.log(`✅ [${currentUserId}] Group cards loaded in ${elapsed}ms - ${formattedSentCards.length} sent, ${formattedReceivedCards.length} received`);

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
      },
      meta: {
        loadTimeMs: elapsed
      }
    });
  } catch (err) {
    console.error("❌ GET GROUP CARDS SUMMARY ERROR", err);
    res.status(500).json({ success: false, message: "Failed to fetch group cards summary" });
  }
});

export default r;
