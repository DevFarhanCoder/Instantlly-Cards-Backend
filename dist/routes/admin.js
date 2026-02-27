"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// src/routes/admin.ts
const express_1 = __importDefault(require("express"));
const mongoose_1 = __importDefault(require("mongoose"));
const User_1 = __importDefault(require("../models/User"));
const Card_1 = __importDefault(require("../models/Card"));
const Message_1 = __importDefault(require("../models/Message"));
const Chat_1 = __importDefault(require("../models/Chat"));
const Group_1 = __importDefault(require("../models/Group"));
const Contact_1 = __importDefault(require("../models/Contact"));
const Notification_1 = __importDefault(require("../models/Notification"));
const SharedCard_1 = __importDefault(require("../models/SharedCard"));
const Ad_1 = __importDefault(require("../models/Ad"));
const Transaction_1 = __importDefault(require("../models/Transaction"));
const Designer_1 = __importDefault(require("../models/Designer"));
const DesignerUpload_1 = __importDefault(require("../models/DesignerUpload"));
const DesignRequest_1 = __importDefault(require("../models/DesignRequest"));
const adminAuth_1 = require("../middleware/adminAuth");
const s3Service_1 = require("../services/s3Service");
const router = express_1.default.Router();
// Simple admin authentication middleware (you should replace with proper auth)
const adminAuth = (req, res, next) => {
    const adminKey = req.headers["x-admin-key"];
    // Accept configured admin key from environment or known keys
    const validKeys = [
        process.env.ADMIN_SECRET_KEY,
        "your-secure-admin-key-here",
        "Farhan_90", // Admin dashboard key
    ].filter(Boolean);
    if (adminKey && validKeys.includes(adminKey)) {
        next();
    }
    else {
        console.log(`âŒ Admin auth failed. Received key: ${adminKey}, Expected one of: ${validKeys.join(", ")}`);
        res
            .status(401)
            .json({ error: "Unauthorized", message: "Invalid admin key" });
    }
};
// Dashboard Overview Stats
router.get("/stats", adminAuth, async (req, res) => {
    try {
        const [totalUsers, totalCards, totalMessages, totalGroups, totalContacts, totalNotifications,] = await Promise.all([
            User_1.default.countDocuments(),
            Card_1.default.countDocuments(),
            Message_1.default.countDocuments(),
            Group_1.default.countDocuments(),
            Contact_1.default.countDocuments(),
            Notification_1.default.countDocuments(),
        ]);
        // Get users created in last 7 days
        const sevenDaysAgo = new Date();
        sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);
        const newUsersThisWeek = await User_1.default.countDocuments({
            createdAt: { $gte: sevenDaysAgo },
        });
        // Get active users (users who have cards or sent messages)
        const activeUsers = await User_1.default.countDocuments({
            $or: [
                { _id: { $in: await Card_1.default.distinct("userId") } },
                { _id: { $in: await Message_1.default.distinct("senderId") } },
            ],
        });
        res.json({
            totalUsers,
            totalCards,
            totalMessages,
            totalGroups,
            totalContacts,
            totalNotifications,
            newUsersThisWeek,
            activeUsers,
        });
    }
    catch (error) {
        console.error("Error fetching stats:", error);
        res.status(500).json({ error: "Failed to fetch statistics" });
    }
});
// Get all users with pagination
router.get("/users", adminAuth, async (req, res) => {
    try {
        const page = parseInt(req.query.page) || 1;
        const limit = parseInt(req.query.limit) || 50;
        const search = req.query.search || "";
        const sortBy = req.query.sortBy || "";
        const sortOrder = req.query.sortOrder || "";
        const skip = (page - 1) * limit;
        console.log("ðŸ“Š Admin /users request:", {
            page,
            limit,
            search,
            sortBy,
            sortOrder,
        });
        // Build search query
        const searchQuery = {};
        if (search) {
            searchQuery.$or = [
                { name: { $regex: search, $options: "i" } },
                { phone: { $regex: search, $options: "i" } },
            ];
        }
        // Build sort query
        let sortQuery = { createdAt: -1 }; // Default sort by creation date
        if (sortBy === "credits" && (sortOrder === "asc" || sortOrder === "desc")) {
            sortQuery = { credits: sortOrder === "asc" ? 1 : -1 };
            console.log("ðŸ’³ Sorting by credits:", sortQuery);
        }
        const [users, total] = await Promise.all([
            User_1.default.find(searchQuery)
                .select("name phone profilePicture about createdAt credits")
                .sort(sortQuery)
                .skip(skip)
                .limit(limit),
            User_1.default.countDocuments(searchQuery),
        ]);
        console.log(`âœ… Found ${users.length} users (Total: ${total}). First user credits: ${users[0]?.credits}, Last user credits: ${users[users.length - 1]?.credits}`);
        // Get additional stats for each user
        const usersWithStats = await Promise.all(users.map(async (user) => {
            const [cardCount, messageCount, contactCount] = await Promise.all([
                Card_1.default.countDocuments({ userId: user._id }),
                Message_1.default.countDocuments({ senderId: user._id }),
                Contact_1.default.countDocuments({ userId: user._id }),
            ]);
            // User.credits is the current live balance (already includes all transactions)
            const totalCredits = user.credits || 0;
            return {
                ...user.toObject(),
                stats: {
                    cards: cardCount,
                    messages: messageCount,
                    contacts: contactCount,
                    credits: totalCredits,
                },
            };
        }));
        res.json({
            users: usersWithStats,
            pagination: {
                page,
                limit,
                total,
                totalPages: Math.ceil(total / limit),
            },
        });
    }
    catch (error) {
        console.error("Error fetching users:", error);
        res.status(500).json({ error: "Failed to fetch users" });
    }
});
// Export all users to CSV
router.get("/users/export", adminAuth, async (req, res) => {
    try {
        const users = await User_1.default.find()
            .select("name phone about createdAt")
            .sort({ createdAt: -1 });
        // Create CSV content
        const headers = ["Name", "Phone", "About", "Created At"];
        const csvRows = [headers.join(",")];
        users.forEach((user) => {
            const row = [
                `"${user.name || ""}"`,
                `"${user.phone || ""}"`,
                `"${user.about || ""}"`,
                `"${user.createdAt?.toISOString() || ""}"`,
            ];
            csvRows.push(row.join(","));
        });
        const csvContent = csvRows.join("\n");
        // Set headers for CSV download
        res.setHeader("Content-Type", "text/csv");
        res.setHeader("Content-Disposition", `attachment; filename=users-${Date.now()}.csv`);
        res.send(csvContent);
    }
    catch (error) {
        console.error("Error exporting users:", error);
        res.status(500).json({ error: "Failed to export users" });
    }
});
// Export phone numbers only
router.get("/users/export-phones", adminAuth, async (req, res) => {
    try {
        const users = await User_1.default.find({ phone: { $exists: true, $ne: "" } })
            .select("name phone")
            .sort({ createdAt: -1 });
        // Create CSV content
        const headers = ["Name", "Phone Number"];
        const csvRows = [headers.join(",")];
        users.forEach((user) => {
            const row = [`"${user.name || ""}"`, `"${user.phone || ""}"`];
            csvRows.push(row.join(","));
        });
        const csvContent = csvRows.join("\n");
        // Set headers for CSV download
        res.setHeader("Content-Type", "text/csv");
        res.setHeader("Content-Disposition", `attachment; filename=phone-numbers-${Date.now()}.csv`);
        res.send(csvContent);
    }
    catch (error) {
        console.error("Error exporting phone numbers:", error);
        res.status(500).json({ error: "Failed to export phone numbers" });
    }
});
// Get user growth data (for charts)
router.get("/analytics/user-growth", adminAuth, async (req, res) => {
    try {
        const days = parseInt(req.query.days) || 30;
        const startDate = new Date();
        startDate.setDate(startDate.getDate() - days);
        const userGrowth = await User_1.default.aggregate([
            {
                $match: {
                    createdAt: { $gte: startDate },
                },
            },
            {
                $group: {
                    _id: {
                        $dateToString: { format: "%Y-%m-%d", date: "$createdAt" },
                    },
                    count: { $sum: 1 },
                },
            },
            {
                $sort: { _id: 1 },
            },
        ]);
        res.json(userGrowth);
    }
    catch (error) {
        console.error("Error fetching user growth:", error);
        res.status(500).json({ error: "Failed to fetch user growth data" });
    }
});
// Get recent activity
router.get("/activity/recent", adminAuth, async (req, res) => {
    try {
        const limit = parseInt(req.query.limit) || 20;
        const [recentUsers, recentCards, recentMessages] = await Promise.all([
            User_1.default.find()
                .select("name phone createdAt")
                .sort({ createdAt: -1 })
                .limit(limit),
            Card_1.default.find()
                .populate("userId", "name phone")
                .select("companyName createdAt userId")
                .sort({ createdAt: -1 })
                .limit(limit),
            Message_1.default.find()
                .populate("senderId", "name")
                .populate("receiverId", "name")
                .select("message senderId receiverId createdAt")
                .sort({ createdAt: -1 })
                .limit(limit),
        ]);
        res.json({
            recentUsers,
            recentCards,
            recentMessages,
        });
    }
    catch (error) {
        console.error("Error fetching recent activity:", error);
        res.status(500).json({ error: "Failed to fetch recent activity" });
    }
});
// Export specific user's contacts
router.get("/users/:userId/contacts/export", adminAuth, async (req, res) => {
    try {
        const { userId } = req.params;
        // Get user info
        const user = await User_1.default.findById(userId).select("name phone");
        if (!user) {
            return res.status(404).json({ error: "User not found" });
        }
        // Get all contacts for this user
        const contacts = await Contact_1.default.find({ userId }).sort({ name: 1 });
        // Create CSV content
        const headers = ["Contact Name", "Phone Number", "Email", "Added On"];
        const csvRows = [headers.join(",")];
        contacts.forEach((contact) => {
            const row = [
                `"${contact.name || ""}"`,
                `"${contact.phoneNumber || ""}"`,
                `"${contact.email || ""}"`,
                `"${contact.createdAt?.toISOString() || ""}"`,
            ];
            csvRows.push(row.join(","));
        });
        const csvContent = csvRows.join("\n");
        // Set headers for CSV download
        const fileName = `${user.name || user.phone || "user"}-contacts-${Date.now()}.csv`;
        res.setHeader("Content-Type", "text/csv");
        res.setHeader("Content-Disposition", `attachment; filename="${fileName}"`);
        res.send(csvContent);
    }
    catch (error) {
        console.error("Error exporting user contacts:", error);
        res.status(500).json({ error: "Failed to export user contacts" });
    }
});
// Delete user and all related data
router.delete("/users/:userId", adminAuth, async (req, res) => {
    try {
        const { userId } = req.params;
        // Check if user exists
        const user = await User_1.default.findById(userId);
        if (!user) {
            return res.status(404).json({ error: "User not found" });
        }
        // Delete all related data in parallel
        await Promise.all([
            // Delete user's cards
            Card_1.default.deleteMany({ userId }),
            // Delete user's contacts
            Contact_1.default.deleteMany({ userId }),
            // Delete messages where user is sender or receiver
            Message_1.default.deleteMany({
                $or: [{ senderId: userId }, { receiverId: userId }],
            }),
            // Delete user's notifications
            Notification_1.default.deleteMany({ userId }),
            // Delete user's shared cards
            SharedCard_1.default.deleteMany({
                $or: [{ senderId: userId }, { receiverId: userId }],
            }),
            // Delete user's group memberships
            Group_1.default.updateMany({ members: userId }, { $pull: { members: userId } }),
            // Delete groups created by user (where they are the only member or admin)
            Group_1.default.deleteMany({ createdBy: userId }),
            // Delete user's chats
            Chat_1.default.deleteMany({
                participants: userId,
            }),
        ]);
        // Finally, delete the user
        await User_1.default.findByIdAndDelete(userId);
        res.json({
            success: true,
            message: "User and all related data deleted successfully",
            deletedUser: {
                id: user._id,
                name: user.name,
                phone: user.phone,
            },
        });
    }
    catch (error) {
        console.error("Error deleting user:", error);
        res.status(500).json({ error: "Failed to delete user" });
    }
});
// ==================== AD APPROVAL WORKFLOW ====================
/**
 * GET /api/admin/ads/pending
 * Get all pending ads awaiting approval
 */
router.get("/ads/pending", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        console.log(`ðŸ“‹ Admin ${req.adminUsername} fetching pending ads`);
        const pendingAds = await Ad_1.default.find({ status: "pending" })
            .sort({ createdAt: -1 }) // Most recent first
            .select("-__v");
        console.log(`âœ… Found ${pendingAds.length} pending ads`);
        const adsWithDetails = pendingAds.map((ad) => {
            // Debug: Log raw ad document
            console.log("Raw ad document:", {
                id: ad._id,
                adType: ad.adType,
                bottomVideoGridFS: ad.bottomVideoGridFS,
                fullscreenVideoGridFS: ad.fullscreenVideoGridFS,
                bottomImageGridFS: ad.bottomImageGridFS,
                fullscreenImageGridFS: ad.fullscreenImageGridFS,
            });
            // Auto-detect ad type if not set (for backward compatibility)
            let detectedAdType = ad.adType;
            if (!detectedAdType) {
                // If has video GridFS references, it's a video ad
                if (ad.bottomVideoGridFS ||
                    ad.fullscreenVideoGridFS) {
                    detectedAdType = "video";
                }
                else {
                    detectedAdType = "image";
                }
            }
            return {
                id: ad._id,
                title: ad.title,
                adType: detectedAdType,
                phoneNumber: ad.phoneNumber,
                startDate: ad.startDate,
                endDate: ad.endDate,
                status: ad.status,
                uploadedBy: ad.uploadedBy,
                uploaderName: ad.uploaderName,
                priority: ad.priority,
                bottomImageId: ad.bottomImageGridFS,
                fullscreenImageId: ad.fullscreenImageGridFS,
                bottomVideoId: ad.bottomVideoGridFS,
                fullscreenVideoId: ad.fullscreenVideoGridFS,
                hasBottomVideo: !!ad.bottomVideoGridFS,
                hasFullscreenVideo: !!ad.fullscreenVideoGridFS,
                impressions: ad.impressions,
                clicks: ad.clicks,
                createdAt: ad.createdAt,
                updatedAt: ad.updatedAt,
            };
        });
        res.json({
            success: true,
            count: adsWithDetails.length,
            ads: adsWithDetails,
        });
    }
    catch (error) {
        console.error("âŒ Error fetching pending ads:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch pending ads",
        });
    }
});
/**
 * POST /api/admin/ads/:id/approve
 * Approve a pending ad (credits already deducted at upload)
 */
router.post("/ads/:id/approve", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { priority } = req.body; // Optional: admin can set priority during approval
        console.log(`âœ… Admin ${req.adminUsername} approving ad ${id}`);
        const ad = await Ad_1.default.findById(id);
        if (!ad) {
            return res.status(404).json({
                success: false,
                message: "Advertisement not found",
            });
        }
        if (ad.status !== "pending") {
            return res.status(400).json({
                success: false,
                message: `Advertisement is already ${ad.status}`,
            });
        }
        // Credits are already deducted at upload time in channelPartnerAds.ts
        // No credit deduction needed here - just approve the ad
        console.log('Credits already deducted at upload. Proceeding with approval only.');
        // ðŸ“¦ Move media files from pending-ads to approved-ads in S3
        const uploaderPhone = ad.uploadedBy || "unknown";
        const mediaFiles = [];
        // Collect all S3 media files that need to be moved
        if (ad.bottomImageS3?.key) {
            const filename = ad.bottomImageS3.key.split("/").pop() || "bottom_image.jpg";
            mediaFiles.push({ filename, type: "bottom_image" });
        }
        if (ad.fullscreenImageS3?.key) {
            const filename = ad.fullscreenImageS3.key.split("/").pop() ||
                "fullscreen_image.jpg";
            mediaFiles.push({ filename, type: "fullscreen_image" });
        }
        if (ad.bottomVideoS3?.key) {
            const filename = ad.bottomVideoS3.key.split("/").pop() || "bottom_video.mp4";
            mediaFiles.push({ filename, type: "bottom_video" });
        }
        if (ad.fullscreenVideoS3?.key) {
            const filename = ad.fullscreenVideoS3.key.split("/").pop() ||
                "fullscreen_video.mp4";
            mediaFiles.push({ filename, type: "fullscreen_video" });
        }
        // Move files from pending to approved in S3
        if (mediaFiles.length > 0) {
            console.log(`ðŸ“¦ Moving ${mediaFiles.length} media files from pending to approved in S3...`);
            try {
                const approvedFiles = await (0, s3Service_1.movePendingToApproved)(id, uploaderPhone, mediaFiles);
                // Update ad with new approved S3 URLs
                for (const file of approvedFiles) {
                    switch (file.type) {
                        case "bottom_image":
                            ad.bottomImageS3 = { url: file.url, key: file.key };
                            console.log(`âœ… Bottom image moved: ${file.url}`);
                            break;
                        case "fullscreen_image":
                            ad.fullscreenImageS3 = {
                                url: file.url,
                                key: file.key,
                            };
                            console.log(`âœ… Fullscreen image moved: ${file.url}`);
                            break;
                        case "bottom_video":
                            ad.bottomVideoS3 = { url: file.url, key: file.key };
                            console.log(`âœ… Bottom video moved: ${file.url}`);
                            break;
                        case "fullscreen_video":
                            ad.fullscreenVideoS3 = {
                                url: file.url,
                                key: file.key,
                            };
                            console.log(`âœ… Fullscreen video moved: ${file.url}`);
                            break;
                    }
                }
                console.log(`âœ… Successfully moved all media to approved-ads/${id}/`);
            }
            catch (error) {
                console.error("âŒ Failed to move S3 media files:", error);
                // Continue with approval even if S3 move fails (files can be moved manually)
            }
        }
        // Update ad status to approved
        ad.status = "approved";
        ad.approvedBy = req.adminId || req.adminUsername || "admin";
        ad.approvalDate = new Date();
        if (priority !== undefined) {
            ad.priority = Math.min(Math.max(parseInt(priority), 1), 10); // Clamp between 1-10
        }
        await ad.save();
        console.log(`âœ… Ad ${id} approved by ${req.adminUsername}`);
        res.json({
            success: true,
            message: "Advertisement approved successfully",
            ad: {
                id: ad._id,
                title: ad.title,
                status: ad.status,
                approvedBy: ad.approvedBy,
                approvalDate: ad.approvalDate,
                priority: ad.priority,
            }
        });
    }
    catch (error) {
        console.error("âŒ Error approving ad:", error);
        res.status(500).json({
            success: false,
            message: "Failed to approve advertisement",
        });
    }
});
/**
 * POST /api/admin/ads/:id/reject
 * Reject a pending ad with reason
 */
router.post("/ads/:id/reject", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { reason } = req.body;
        console.log(`âŒ Admin ${req.adminUsername} rejecting ad ${id}`);
        if (!reason || reason.trim() === "") {
            return res.status(400).json({
                success: false,
                message: "Rejection reason is required",
            });
        }
        const ad = await Ad_1.default.findById(id);
        if (!ad) {
            return res.status(404).json({
                success: false,
                message: "Advertisement not found",
            });
        }
        if (ad.status !== "pending") {
            return res.status(400).json({
                success: false,
                message: `Advertisement is already ${ad.status}`,
            });
        }
        // DELETE IMAGES/VIDEOS FROM GRIDFS WHEN REJECTED
        try {
            const db = mongoose_1.default.connection.db;
            if (db) {
                const imageBucket = new (require("mongodb").GridFSBucket)(db, {
                    bucketName: "adImages",
                });
                const videoBucket = new (require("mongodb").GridFSBucket)(db, {
                    bucketName: "adVideos",
                });
                // Delete bottom image if exists
                if (ad.bottomImageGridFS) {
                    try {
                        await imageBucket.delete(ad.bottomImageGridFS);
                        console.log(`ðŸ—‘ï¸ Deleted bottom image: ${ad.bottomImageGridFS}`);
                    }
                    catch (err) {
                        console.log(`âš ï¸ Could not delete bottom image: ${err}`);
                    }
                }
                // Delete fullscreen image if exists
                if (ad.fullscreenImageGridFS) {
                    try {
                        await imageBucket.delete(ad.fullscreenImageGridFS);
                        console.log(`ðŸ—‘ï¸ Deleted fullscreen image: ${ad.fullscreenImageGridFS}`);
                    }
                    catch (err) {
                        console.log(`âš ï¸ Could not delete fullscreen image: ${err}`);
                    }
                }
                // Delete bottom video if exists
                if (ad.bottomVideoGridFS) {
                    try {
                        await videoBucket.delete(ad.bottomVideoGridFS);
                        console.log(`ðŸ—‘ï¸ Deleted bottom video: ${ad.bottomVideoGridFS}`);
                    }
                    catch (err) {
                        console.log(`âš ï¸ Could not delete bottom video: ${err}`);
                    }
                }
                // Delete fullscreen video if exists
                if (ad.fullscreenVideoGridFS) {
                    try {
                        await videoBucket.delete(ad.fullscreenVideoGridFS);
                        console.log(`ðŸ—‘ï¸ Deleted fullscreen video: ${ad.fullscreenVideoGridFS}`);
                    }
                    catch (err) {
                        console.log(`âš ï¸ Could not delete fullscreen video: ${err}`);
                    }
                }
            }
        }
        catch (deleteError) {
            console.error("âŒ Error deleting files:", deleteError);
            // Continue with rejection even if deletion fails
        }
        // Update ad status to rejected
        ad.status = "rejected";
        ad.approvedBy = req.adminId || req.adminUsername || "admin";
        ad.approvalDate = new Date();
        ad.rejectionReason = reason.trim();
        await ad.save();
        console.log(`âŒ Ad ${id} rejected by ${req.adminUsername}: ${reason}`);
        res.json({
            success: true,
            message: "Advertisement rejected",
            ad: {
                id: ad._id,
                title: ad.title,
                status: ad.status,
                approvedBy: ad.approvedBy,
                approvalDate: ad.approvalDate,
                rejectionReason: ad.rejectionReason,
            },
        });
    }
    catch (error) {
        console.error("âŒ Error rejecting ad:", error);
        res.status(500).json({
            success: false,
            message: "Failed to reject advertisement",
        });
    }
});
/**
 * GET /api/admin/ads/all
 * Get all ads with filtering options
 */
router.get("/ads/all", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const { status, uploadedBy } = req.query;
        const filter = {};
        if (status) {
            filter.status = status;
        }
        if (uploadedBy) {
            filter.uploadedBy = uploadedBy;
        }
        console.log(`ðŸ“‹ Admin ${req.adminUsername} fetching ads with filter:`, filter);
        const ads = await Ad_1.default.find(filter).sort({ createdAt: -1 }).select("-__v");
        const adsWithDetails = ads.map((ad) => ({
            id: ad._id,
            title: ad.title,
            adType: ad.adType || "image",
            phoneNumber: ad.phoneNumber,
            startDate: ad.startDate,
            endDate: ad.endDate,
            status: ad.status,
            uploadedBy: ad.uploadedBy,
            uploaderName: ad.uploaderName,
            approvedBy: ad.approvedBy,
            approvalDate: ad.approvalDate,
            rejectionReason: ad.rejectionReason,
            priority: ad.priority,
            bottomImageId: ad.bottomImageGridFS,
            fullscreenImageId: ad.fullscreenImageGridFS,
            bottomVideoId: ad.bottomVideoGridFS,
            fullscreenVideoId: ad.fullscreenVideoGridFS,
            hasBottomVideo: !!ad.bottomVideoGridFS,
            hasFullscreenVideo: !!ad.fullscreenVideoGridFS,
            impressions: ad.impressions,
            clicks: ad.clicks,
            createdAt: ad.createdAt,
            updatedAt: ad.updatedAt,
        }));
        res.json({
            success: true,
            count: adsWithDetails.length,
            ads: adsWithDetails,
        });
    }
    catch (error) {
        console.error("âŒ Error fetching all ads:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch advertisements",
        });
    }
});
// GET /api/admin/users-stats - Get user statistics with total credits
router.get("/users-stats", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const totalUsers = await User_1.default.countDocuments();
        // Calculate total credits across all users
        const creditStats = await User_1.default.aggregate([
            {
                $group: {
                    _id: null,
                    totalCredits: { $sum: "$credits" },
                },
            },
        ]);
        const totalCredits = creditStats.length > 0 ? creditStats[0].totalCredits : 0;
        res.json({
            success: true,
            totalUsers,
            totalCredits,
        });
    }
    catch (error) {
        console.error("âŒ Error fetching user stats:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch user statistics",
        });
    }
});
// GET /api/admin/all-transactions - Get all credit transactions
router.get("/all-transactions", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const { limit = 500, skip = 0, type, userId } = req.query;
        const query = {};
        if (type)
            query.type = type;
        if (userId) {
            query.$or = [{ fromUser: userId }, { toUser: userId }];
        }
        const transactions = await Transaction_1.default.find(query)
            .populate("fromUser", "name phone profilePicture")
            .populate("toUser", "name phone profilePicture")
            .sort({ createdAt: -1 })
            .limit(Number(limit))
            .skip(Number(skip));
        const total = await Transaction_1.default.countDocuments(query);
        res.json({
            success: true,
            transactions,
            total,
            limit: Number(limit),
            skip: Number(skip),
        });
    }
    catch (error) {
        console.error("âŒ Error fetching all transactions:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch transactions",
        });
    }
});
// GET /api/admin/user/:userId - Get specific user details including balance
router.get("/user/:userId", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const { userId } = req.params;
        const user = await User_1.default.findById(userId).select("name phone email credits profilePicture createdAt");
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        res.json({
            success: true,
            user: {
                _id: user._id,
                name: user.name,
                phone: user.phone,
                email: user.email,
                credits: user.credits || 0,
                profilePicture: user.profilePicture,
                createdAt: user.createdAt,
            },
        });
    }
    catch (error) {
        console.error("âŒ Error fetching user:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch user details",
        });
    }
});
// POST /api/admin/transfer-credits - Admin transfer credits to any user
router.post("/transfer-credits", adminAuth_1.requireAdminAuth, async (req, res) => {
    try {
        const { toUserId, amount, description } = req.body;
        // Validation
        if (!toUserId || !amount) {
            return res.status(400).json({
                success: false,
                message: "Recipient and amount are required",
            });
        }
        if (amount <= 0) {
            return res.status(400).json({
                success: false,
                message: "Amount must be greater than 0",
            });
        }
        // Get recipient
        const recipient = await User_1.default.findById(toUserId);
        if (!recipient) {
            return res.status(404).json({
                success: false,
                message: "Recipient not found",
            });
        }
        // Add credits to recipient
        const recipientCredits = recipient.credits || 0;
        recipient.set({ credits: recipientCredits + amount });
        await recipient.save();
        // Create transaction record
        const transferDesc = description || `Credited by Instantlly`;
        await Transaction_1.default.create({
            type: "admin_adjustment",
            toUser: recipient._id,
            amount: amount,
            description: transferDesc,
            balanceBefore: recipientCredits,
            balanceAfter: recipientCredits + amount,
            status: "completed",
        });
        console.log(`âœ… Admin transferred ${amount} credits to ${recipient.name}`);
        res.json({
            success: true,
            message: `Successfully transferred ${amount} credits to ${recipient.name}`,
            newBalance: recipientCredits + amount,
            transfer: {
                amount,
                to: {
                    _id: recipient._id,
                    name: recipient.name,
                    phone: recipient.phone,
                },
            },
        });
    }
    catch (error) {
        console.error("âŒ Admin transfer error:", error);
        res.status(500).json({
            success: false,
            message: "Server error during transfer",
        });
    }
});
// PUT /api/admin/users/:userId/update-credits - Admin update user credits directly
router.put("/users/:userId/update-credits", adminAuth, async (req, res) => {
    try {
        const { userId } = req.params;
        const { credits, reason } = req.body;
        console.log("ðŸ’° Update Credits Request:", { userId, credits, reason });
        // Validate inputs
        if (credits === undefined || credits === null || credits < 0) {
            return res.status(400).json({
                success: false,
                message: "Credits must be 0 or greater",
            });
        }
        // Find user
        const user = await User_1.default.findById(userId);
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        const oldCredits = user.credits || 0;
        const newCredits = parseInt(credits);
        const creditDifference = newCredits - oldCredits;
        // Update user credits
        user.set({ credits: newCredits });
        await user.save();
        // Create transaction record for the adjustment
        const transactionDescription = creditDifference >= 0
            ? "Added by Instantlly Cards"
            : "Deducted by Instantlly Cards";
        await Transaction_1.default.create({
            type: "admin_adjustment",
            toUser: user._id,
            amount: creditDifference, // Keep the sign: positive for add, negative for deduct
            description: transactionDescription,
            note: reason || undefined,
            balanceBefore: oldCredits,
            balanceAfter: newCredits,
            status: "completed",
        });
        // Emit Socket.IO event to notify user's mobile app
        try {
            const io = global.io;
            if (io) {
                io.emit(`credits_updated_${userId}`, {
                    userId: userId,
                    oldCredits,
                    newCredits,
                    creditDifference,
                    timestamp: new Date(),
                });
                console.log(`ðŸ“¡ Emitted credits_updated event for user ${userId}`);
            }
        }
        catch (socketError) {
            console.log("âš ï¸ Socket.IO not available for credit update notification");
        }
        console.log(`âœ… Credits updated for ${user.name}:`, {
            old: oldCredits,
            new: newCredits,
            difference: creditDifference,
        });
        res.json({
            success: true,
            message: `Credits ${creditDifference >= 0 ? "increased" : "decreased"} successfully from ${oldCredits.toLocaleString("en-IN")} to ${newCredits.toLocaleString("en-IN")}`,
            user: {
                id: user._id,
                name: user.name,
                phone: user.phone,
                oldCredits: oldCredits,
                newCredits: newCredits,
                creditDifference: creditDifference,
            },
        });
    }
    catch (error) {
        console.error("âŒ Error updating credits:", error);
        res.status(500).json({
            success: false,
            message: "Failed to update credits",
        });
    }
});
// Credit Transfer endpoint (alternative path for admin dashboard)
router.post("/credits/transfer", adminAuth, async (req, res) => {
    try {
        const { userId, amount, reason } = req.body;
        // Validation
        if (!userId || !amount) {
            return res.status(400).json({
                success: false,
                message: "User ID and amount are required",
            });
        }
        if (amount <= 0) {
            return res.status(400).json({
                success: false,
                message: "Amount must be greater than 0",
            });
        }
        // Get recipient
        const recipient = await User_1.default.findById(userId);
        if (!recipient) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        // Add credits to recipient
        const recipientCredits = recipient.credits || 0;
        recipient.set({ credits: recipientCredits + amount });
        await recipient.save();
        // Create transaction record
        const transferDesc = reason || `Credited by Instantlly`;
        await Transaction_1.default.create({
            type: "admin_adjustment",
            toUser: recipient._id,
            amount: amount,
            description: transferDesc,
            balanceBefore: recipientCredits,
            balanceAfter: recipientCredits + amount,
            status: "completed",
        });
        console.log(`âœ… Admin transferred ${amount} credits to ${recipient.name}`);
        res.json({
            success: true,
            message: `Successfully transferred ${amount} credits to ${recipient.name}`,
            newBalance: recipientCredits + amount,
        });
    }
    catch (error) {
        console.error("âŒ Admin credit transfer error:", error);
        res.status(500).json({
            success: false,
            message: "Server error during transfer",
        });
    }
});
// Edit Application Info (Name & Phone)
router.put("/applications/:id/edit", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { name, phone } = req.body;
        console.log(`ðŸ“ Editing application ${id}:`, { name, phone });
        // Find and update the application (assuming you have an Application model)
        // For now, update the User model if the application is approved
        const user = await User_1.default.findById(id);
        if (!user) {
            return res.status(404).json({ message: "User not found" });
        }
        // Update user info
        if (name)
            user.name = name;
        if (phone)
            user.phone = phone;
        await user.save();
        console.log(`âœ… Updated user: ${user.name} (${user.phone})`);
        res.json({
            success: true,
            message: "Application updated successfully",
            user: {
                _id: user._id,
                name: user.name,
                phone: user.phone,
            },
        });
    }
    catch (error) {
        console.error("âŒ Edit application error:", error);
        res.status(500).json({ message: "Server error" });
    }
});
// Transfer Position
router.put("/applications/:id/transfer", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { newPositionId } = req.body;
        console.log(`ðŸ”„ Transferring application ${id} to position:`, newPositionId);
        // Update user's position (you may need to adjust based on your data model)
        const user = await User_1.default.findById(id);
        if (!user) {
            return res.status(404).json({ message: "User not found" });
        }
        // Update position (assuming you store position in user model)
        user.positionId = newPositionId;
        await user.save();
        console.log(`âœ… Transferred ${user.name} to ${newPositionId}`);
        res.json({
            success: true,
            message: "Position transferred successfully",
            user: {
                _id: user._id,
                name: user.name,
                positionId: newPositionId,
            },
        });
    }
    catch (error) {
        console.error("âŒ Transfer position error:", error);
        res.status(500).json({ message: "Server error" });
    }
});
// GET /api/admin/referral-tracking - Get comprehensive referral statistics
router.get("/referral-tracking", adminAuth, async (req, res) => {
    try {
        const days = parseInt(req.query.days) || 30;
        // Calculate date filter
        let dateFilter = {};
        if (days > 0) {
            const startDate = new Date();
            startDate.setDate(startDate.getDate() - days);
            dateFilter = { createdAt: { $gte: startDate } };
        }
        // Get all users who have made referrals
        const usersWithReferrals = await User_1.default.find({
            referralCode: { $exists: true, $ne: null },
        })
            .select("_id name phone referralCode createdAt")
            .lean();
        // Count referrals for each user
        const topReferrersData = await Promise.all(usersWithReferrals.map(async (user) => {
            // Count users referred by this user
            const referralCount = await User_1.default.countDocuments({
                referredBy: user._id,
                ...dateFilter,
            });
            // Get total credits earned from referrals
            const referralTransactions = await Transaction_1.default.find({
                type: "referral_bonus",
                toUser: user._id,
                ...dateFilter,
            })
                .select("amount")
                .lean();
            const creditsEarned = referralTransactions.reduce((sum, tx) => sum + (tx.amount || 0), 0);
            return {
                userId: user._id.toString(),
                name: user.name,
                phone: user.phone,
                referralCode: user.referralCode,
                totalReferrals: referralCount,
                creditsEarned,
                joinedDate: user.createdAt,
            };
        }));
        // Filter out users with 0 referrals and sort by total referrals
        const topReferrers = topReferrersData
            .filter((r) => r.totalReferrals > 0)
            .sort((a, b) => b.totalReferrals - a.totalReferrals)
            .slice(0, 100); // Top 100 referrers
        // Get overall statistics
        const totalReferrals = await User_1.default.countDocuments({
            referredBy: { $exists: true, $ne: null },
            ...dateFilter,
        });
        const totalReferralTransactions = await Transaction_1.default.find({
            type: "referral_bonus",
            ...dateFilter,
        })
            .select("amount")
            .lean();
        const totalReferralCreditsGiven = totalReferralTransactions.reduce((sum, tx) => sum + (tx.amount || 0), 0);
        const uniqueReferrers = topReferrers.length;
        const averageReferralsPerUser = uniqueReferrers > 0 ? totalReferrals / uniqueReferrers : 0;
        // Get recent referral activity
        const recentReferredUsers = await User_1.default.find({
            referredBy: { $exists: true, $ne: null },
            ...dateFilter,
        })
            .select("name phone referredBy createdAt")
            .populate("referredBy", "name referralCode")
            .sort({ createdAt: -1 })
            .limit(50)
            .lean();
        // Get corresponding transactions for credits awarded
        const recentActivity = await Promise.all(recentReferredUsers.map(async (user) => {
            const transaction = await Transaction_1.default.findOne({
                type: "referral_bonus",
                fromUser: user._id,
            })
                .select("amount")
                .lean();
            return {
                referrerName: user.referredBy?.name || "Unknown",
                referrerCode: user.referredBy?.referralCode || "N/A",
                newUserName: user.name,
                newUserPhone: user.phone,
                date: user.createdAt,
                creditsAwarded: transaction?.amount || 0,
            };
        }));
        // Get referral trends (daily count for the period)
        const referralTrends = [];
        if (days > 0 && days <= 90) {
            const trendDays = days > 30 ? 30 : days;
            for (let i = trendDays - 1; i >= 0; i--) {
                const date = new Date();
                date.setDate(date.getDate() - i);
                date.setHours(0, 0, 0, 0);
                const nextDate = new Date(date);
                nextDate.setDate(nextDate.getDate() + 1);
                const count = await User_1.default.countDocuments({
                    referredBy: { $exists: true, $ne: null },
                    createdAt: { $gte: date, $lt: nextDate },
                });
                referralTrends.push({
                    date: date.toISOString().split("T")[0],
                    count,
                });
            }
        }
        res.json({
            success: true,
            data: {
                totalReferrals,
                totalReferralCreditsGiven,
                uniqueReferrers,
                averageReferralsPerUser,
                topReferrers,
                recentActivity,
                referralTrends,
            },
        });
    }
    catch (error) {
        console.error("âŒ Referral tracking error:", error);
        res.status(500).json({
            success: false,
            message: "Server error",
        });
    }
});
// GET /api/admin/referral-chain/:userId - Get detailed referral chain for a specific user
router.get("/referral-chain/:userId", adminAuth, async (req, res) => {
    try {
        const { userId } = req.params;
        console.log(`ðŸ“Š Fetching referral chain for user: ${userId}`);
        // Get the user's details
        const user = (await User_1.default.findById(userId)
            .select("name phone referralCode createdAt")
            .lean());
        if (!user) {
            return res.status(404).json({
                success: false,
                message: "User not found",
            });
        }
        // Find all users referred by this user
        const referredUsers = (await User_1.default.find({ referredBy: userId })
            .select("_id name phone referralCode createdAt referredBy credits")
            .sort({ createdAt: -1 })
            .lean());
        // For each referred user, count their own referrals
        const referredUsersWithStats = await Promise.all(referredUsers.map(async (refUser) => {
            const refUserReferralCount = await User_1.default.countDocuments({
                referredBy: refUser._id,
            });
            // Use the User.credits field directly (it's the live balance)
            const creditsEarned = refUser.credits || 0;
            return {
                userId: refUser._id.toString(),
                name: refUser.name,
                phone: refUser.phone,
                referralCode: refUser.referralCode,
                totalReferrals: refUserReferralCount,
                creditsEarned,
                joinedDate: refUser.createdAt,
            };
        }));
        res.json({
            success: true,
            data: {
                user: {
                    userId: user._id.toString(),
                    name: user.name,
                    phone: user.phone,
                    referralCode: user.referralCode,
                    joinedDate: user.createdAt,
                },
                referredUsers: referredUsersWithStats,
                totalCount: referredUsersWithStats.length,
            },
        });
    }
    catch (error) {
        console.error("âŒ Error fetching referral chain:", error);
        res.status(500).json({
            success: false,
            message: error.message || "Failed to fetch referral chain",
        });
    }
});
/**
 * GET /api/admin/designers
 * Get all designer accounts
 */
router.get("/designers", async (req, res) => {
    try {
        const designers = await Designer_1.default.find({})
            .select("-password")
            .sort({ createdAt: -1 });
        // Count assigned and completed requests per designer
        const designerStats = await Promise.all(designers.map(async (d) => {
            const assignedCount = await DesignRequest_1.default.countDocuments({
                assignedDesignerId: d._id,
            });
            const completedCount = await DesignRequest_1.default.countDocuments({
                assignedDesignerId: d._id,
                status: "completed",
            });
            return {
                id: d._id,
                _id: d._id,
                username: d.username,
                name: d.username, // Use username as display name
                status: "active",
                createdAt: d.createdAt,
                updatedAt: d.updatedAt,
                assignedRequests: assignedCount,
                completedRequests: completedCount,
            };
        }));
        res.json({
            success: true,
            designers: designerStats,
        });
    }
    catch (error) {
        console.error("Error fetching designers:", error);
        res.status(500).json({ message: "Failed to fetch designers" });
    }
});
/**
 * POST /api/admin/designers
 * Create a new designer account
 */
router.post("/designers", async (req, res) => {
    try {
        const { username, password } = req.body;
        if (!username || !password) {
            return res
                .status(400)
                .json({ message: "Username and password are required" });
        }
        // Check if username already exists
        const existing = await Designer_1.default.findOne({ username });
        if (existing) {
            return res.status(409).json({ message: "Username already exists" });
        }
        const designer = new Designer_1.default({ username, password });
        await designer.save();
        console.log(`âœ… Designer created: ${username}`);
        res.status(201).json({
            success: true,
            message: "Designer account created successfully",
            designer: {
                _id: designer._id,
                username: designer.username,
                createdAt: designer.createdAt,
            },
        });
    }
    catch (error) {
        console.error("Error creating designer:", error);
        res.status(500).json({ message: "Failed to create designer account" });
    }
});
/**
 * DELETE /api/admin/designers/:id
 * Delete a designer account
 */
router.delete("/designers/:id", async (req, res) => {
    try {
        const { id } = req.params;
        if (!mongoose_1.default.Types.ObjectId.isValid(id)) {
            return res.status(400).json({ message: "Invalid designer ID" });
        }
        const result = await Designer_1.default.findByIdAndDelete(id);
        if (!result) {
            return res.status(404).json({ message: "Designer not found" });
        }
        res.json({ success: true, message: "Designer deleted" });
    }
    catch (error) {
        console.error("Error deleting designer:", error);
        res.status(500).json({ message: "Failed to delete designer" });
    }
});
// ==========================================
// GET /api/admin/received-designs
// Get all designer uploads (received designs)
// ==========================================
router.get("/received-designs", async (req, res) => {
    try {
        const uploads = await DesignerUpload_1.default.find({})
            .populate("designRequestId", "businessName adType uploaderName uploaderPhone status email phoneNumber")
            .sort({ createdAt: -1 });
        // Map to the format the admin frontend expects
        const designs = uploads.map((u) => {
            const dr = u.designRequestId || {};
            return {
                id: u._id,
                _id: u._id,
                designRequestId: dr._id || u.designRequestId,
                designerName: u.designerName,
                designerId: u.designerId,
                uploaderName: dr.uploaderName || "Unknown",
                uploaderPhone: dr.uploaderPhone || "",
                businessName: dr.businessName || "",
                adType: dr.adType || "image",
                status: u.status === "uploaded"
                    ? "new"
                    : u.status === "approved"
                        ? "user-approved"
                        : u.status === "rejected"
                            ? "changes-requested"
                            : u.status,
                designFiles: (u.filesS3 || []).map((f) => ({
                    url: f.url,
                    type: f.contentType?.startsWith("video/") ? "video" : "image",
                    name: f.filename || "file",
                })),
                designerNotes: u.notes,
                adminFeedback: u.adminNotes,
                userFeedback: u.userFeedback || "",
                uploadedAt: u.createdAt,
                createdAt: u.createdAt,
            };
        });
        res.json({ success: true, designs, uploads });
    }
    catch (error) {
        console.error("Error fetching received designs:", error);
        res.status(500).json({ message: "Failed to fetch received designs" });
    }
});
// ==========================================
// POST /api/admin/received-designs/:id/send-to-user
// Approve a designer upload and mark as sent to user
// ==========================================
router.post("/received-designs/:id/send-to-user", async (req, res) => {
    try {
        const { id } = req.params;
        if (!mongoose_1.default.Types.ObjectId.isValid(id)) {
            return res.status(400).json({ message: "Invalid upload ID" });
        }
        const designerUpload = await DesignerUpload_1.default.findById(id);
        if (!designerUpload) {
            return res.status(404).json({ message: "Designer upload not found" });
        }
        designerUpload.status = "sent-to-user";
        if (req.body.adminNotes) {
            designerUpload.adminNotes = req.body.adminNotes;
        }
        await designerUpload.save();
        // Also update the design request status
        const designRequest = await DesignRequest_1.default.findById(designerUpload.designRequestId);
        if (designRequest) {
            designRequest.status = "completed";
            await designRequest.save();
        }
        res.json({
            success: true,
            message: "Design sent to user successfully",
            upload: designerUpload,
        });
    }
    catch (error) {
        console.error("Error sending design to user:", error);
        res.status(500).json({ message: "Failed to send design to user" });
    }
});
// ============================================
// ADMIN VOUCHER MANAGEMENT
// ============================================
const Voucher_1 = __importDefault(require("../models/Voucher"));
const uuid_1 = require("uuid");
/**
 * POST /api/admin/vouchers
 * Create a new voucher template (admin panel)
 */
router.post("/vouchers", adminAuth, async (req, res) => {
    try {
        const { companyLogo, companyName, phoneNumber, address, amount, discountPercentage, validity, voucherImage, description, expiryDate, isPublished = false, } = req.body;
        console.log(`ðŸ“ Admin creating voucher template`);
        // Validation
        if (!companyName || !amount) {
            return res.status(400).json({
                success: false,
                message: "Company name and amount are required",
            });
        }
        // Generate unique voucher number
        const voucherNumber = (0, uuid_1.v4)().replace(/-/g, "").slice(0, 12).toUpperCase();
        // Set expiry date (default to 1 year from now if not provided)
        const expiryDateFinal = expiryDate ? new Date(expiryDate) : new Date();
        if (!expiryDate) {
            expiryDateFinal.setFullYear(expiryDateFinal.getFullYear() + 1);
        }
        // Create voucher template (without userId - will be assigned when published to users)
        const voucher = await Voucher_1.default.create({
            voucherNumber,
            companyLogo,
            companyName,
            phoneNumber,
            address,
            amount: amount || 1200,
            MRP: amount || 1200,
            discountPercentage: discountPercentage || 0,
            validity: validity ||
                `Valid till ${expiryDateFinal.toLocaleDateString("en-US", { month: "long", day: "numeric", year: "numeric" })}`,
            voucherImage,
            description,
            expiryDate: expiryDateFinal,
            issueDate: new Date(),
            isPublished,
            publishedAt: isPublished ? new Date() : null,
            createdByAdmin: null,
            source: "admin",
            redeemedStatus: "unredeemed",
        });
        console.log(`âœ… Voucher template created: ${voucher.voucherNumber}`);
        res.status(201).json({
            success: true,
            message: "Voucher template created successfully",
            voucher: {
                id: voucher._id,
                voucherNumber: voucher.voucherNumber,
                companyName: voucher.companyName,
                amount: voucher.amount,
                isPublished: voucher.isPublished,
            },
        });
    }
    catch (error) {
        console.error("âŒ Error creating voucher template:", error);
        res.status(500).json({
            success: false,
            message: "Failed to create voucher template",
        });
    }
});
/**
 * GET /api/admin/vouchers
 * List all voucher templates
 */
router.get("/vouchers", adminAuth, async (req, res) => {
    try {
        const { isPublished, limit = 50, skip = 0 } = req.query;
        const filter = { source: "admin" };
        if (isPublished !== undefined) {
            filter.isPublished = isPublished === "true";
        }
        const vouchers = await Voucher_1.default.find(filter)
            .sort({ createdAt: -1 })
            .skip(Number(skip))
            .limit(Number(limit))
            .lean();
        const total = await Voucher_1.default.countDocuments(filter);
        res.json({
            success: true,
            vouchers: vouchers.map((v) => ({
                _id: v._id,
                voucherNumber: v.voucherNumber,
                companyName: v.companyName,
                companyLogo: v.companyLogo,
                phoneNumber: v.phoneNumber,
                address: v.address,
                amount: v.amount,
                discountPercentage: v.discountPercentage,
                validity: v.validity,
                voucherImage: v.voucherImage,
                description: v.description,
                isPublished: v.isPublished,
                publishedAt: v.publishedAt,
                expiryDate: v.expiryDate,
                createdAt: v.createdAt,
            })),
            pagination: {
                total,
                skip: Number(skip),
                limit: Number(limit),
            },
        });
    }
    catch (error) {
        console.error("âŒ Error fetching vouchers:", error);
        res.status(500).json({
            success: false,
            message: "Failed to fetch vouchers",
        });
    }
});
/**
 * PUT /api/admin/vouchers/:id
 * Update voucher template
 */
router.put("/vouchers/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const updateData = req.body;
        console.log(`ðŸ“ Admin updating voucher ${id}`);
        const voucher = await Voucher_1.default.findByIdAndUpdate(id, { $set: updateData }, { new: true });
        if (!voucher) {
            return res.status(404).json({
                success: false,
                message: "Voucher not found",
            });
        }
        res.json({
            success: true,
            message: "Voucher updated successfully",
            voucher,
        });
    }
    catch (error) {
        console.error("âŒ Error updating voucher:", error);
        res.status(500).json({
            success: false,
            message: "Failed to update voucher",
        });
    }
});
/**
 * POST /api/admin/vouchers/:id/publish
 * Publish voucher to all users
 */
router.post("/vouchers/:id/publish", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { userIds } = req.body; // Optional: specific user IDs, or empty to publish to all
        console.log(`ðŸ“¢ Admin publishing voucher ${id}`);
        const voucherTemplate = await Voucher_1.default.findById(id);
        if (!voucherTemplate) {
            return res.status(404).json({
                success: false,
                message: "Voucher template not found",
            });
        }
        // Mark template as published
        voucherTemplate.isPublished = true;
        voucherTemplate.publishedAt = new Date();
        await voucherTemplate.save();
        // If userIds provided, create copies for specific users
        // If not, the template itself will be visible to all users
        let assignedCount = 0;
        if (userIds && Array.isArray(userIds) && userIds.length > 0) {
            // Create individual voucher copies for each user
            const voucherCopies = userIds.map((userId) => ({
                ...voucherTemplate.toObject(),
                _id: new mongoose_1.default.Types.ObjectId(),
                userId: userId,
                originalOwner: userId,
                voucherNumber: (0, uuid_1.v4)().replace(/-/g, "").slice(0, 12).toUpperCase(),
                creditId: null,
            }));
            await Voucher_1.default.insertMany(voucherCopies);
            assignedCount = voucherCopies.length;
        }
        res.json({
            success: true,
            message: userIds && userIds.length > 0
                ? `Voucher published and assigned to ${assignedCount} users`
                : "Voucher template published successfully",
            voucher: {
                id: voucherTemplate._id,
                voucherNumber: voucherTemplate.voucherNumber,
                isPublished: voucherTemplate.isPublished,
                assignedToUsers: assignedCount,
            },
        });
    }
    catch (error) {
        console.error("âŒ Error publishing voucher:", error);
        res.status(500).json({
            success: false,
            message: "Failed to publish voucher",
        });
    }
});
/**
 * DELETE /api/admin/vouchers/:id
 * Delete voucher template
 */
router.delete("/vouchers/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        console.log(`ðŸ—‘ï¸ Admin deleting voucher ${id}`);
        const voucher = await Voucher_1.default.findByIdAndDelete(id);
        if (!voucher) {
            return res.status(404).json({
                success: false,
                message: "Voucher not found",
            });
        }
        res.json({
            success: true,
            message: "Voucher deleted successfully",
        });
    }
    catch (error) {
        console.error("âŒ Error deleting voucher:", error);
        res.status(500).json({
            success: false,
            message: "Failed to delete voucher",
        });
    }
});
exports.default = router;
