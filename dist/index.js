"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// index.ts
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
// Load .env from the project root
dotenv_1.default.config({ path: path_1.default.join(__dirname, "../.env") });
const express_1 = __importDefault(require("express"));
const cors_1 = __importDefault(require("cors"));
const compression_1 = __importDefault(require("compression"));
const fs_1 = __importDefault(require("fs"));
const mongoose_1 = __importDefault(require("mongoose"));
const http_1 = require("http");
const socket_io_1 = require("socket.io");
const db_1 = require("./db");
const auth_1 = __importDefault(require("./routes/auth"));
const otp_1 = __importDefault(require("./routes/otp"));
const cards_1 = __importDefault(require("./routes/cards"));
const contacts_1 = __importDefault(require("./routes/contacts"));
const notifications_1 = __importDefault(require("./routes/notifications"));
const messages_1 = __importStar(require("./routes/messages"));
const groups_1 = __importDefault(require("./routes/groups"));
const chats_1 = __importDefault(require("./routes/chats"));
const admin_1 = __importDefault(require("./routes/admin"));
const adminAuth_1 = __importDefault(require("./routes/adminAuth"));
const groupSharing_1 = __importDefault(require("./routes/groupSharing"));
const ads_1 = __importDefault(require("./routes/ads"));
const channelPartnerAds_1 = __importDefault(require("./routes/channelPartnerAds"));
const credits_1 = __importDefault(require("./routes/credits"));
const feedback_1 = __importDefault(require("./routes/feedback"));
const quiz_1 = __importDefault(require("./routes/quiz"));
const businessPromotion_1 = __importDefault(require("./routes/businessPromotion"));
const mlm_1 = __importDefault(require("./routes/mlm"));
const business_listing_1 = __importDefault(require("./routes/business-listing"));
const designer_1 = __importDefault(require("./routes/designer"));
const categories_1 = __importDefault(require("./routes/categories"));
const socketService_1 = require("./services/socketService");
const gridfsService_1 = require("./services/gridfsService");
const imageCache_1 = require("./services/imageCache");
const scheduler_1 = require("./services/mlm/scheduler");
const app = (0, express_1.default)();
const server = (0, http_1.createServer)(app);
// Socket.IO setup with CORS - Optimized for speed and memory
const io = new socket_io_1.Server(server, {
    cors: {
        origin: "*",
        methods: ["GET", "POST"],
        credentials: true,
    },
    pingTimeout: 20000, // Reduced from 60s to 20s to detect disconnects faster
    pingInterval: 10000, // Reduced from 25s to 10s for faster keepalive
    transports: ["websocket", "polling"], // WebSocket first for speed
    allowEIO3: true,
    upgradeTimeout: 10000, // Reduced from 30s to 10s
    maxHttpBufferSize: 2e6, // Reduced to 2MB (was 5MB) to save memory on Render 512MB limit
    connectTimeout: 10000, // Add connection timeout
    perMessageDeflate: false, // Disable compression for speed
});
// CORS Configuration - Allow requests from Vercel and admin dashboards
const defaultAllowed = [
    "https://api.instantllycards.com",
    "https://api-test.instantllycards.com",
    "https://instantlly-ads.vercel.app",
    "https://instantlly-admin.vercel.app",
    "https://instantllychannelpatneradmin.vercel.app", // Channel Partner Admin
    "https://instantllychannelpatner.vercel.app", // Channel Partner Main
    "https://www.instantllycards.com", // InstantllyCards main website
    "https://instantllycards.com", // InstantllyCards main website (no www)
    "http://localhost:3000",
    "http://localhost:3001",
    "http://localhost:5500",
];
const adminOrigin = process.env.ADMIN_WEB_ORIGIN; // e.g. https://instantllychannelpatneradmin.vercel.app
const allowedOrigins = adminOrigin
    ? [...defaultAllowed, adminOrigin]
    : [...defaultAllowed];
app.use((0, cors_1.default)({
    origin: function (origin, callback) {
        // Allow requests with no origin (mobile apps, Postman, server-to-server)
        if (!origin)
            return callback(null, true);
        // Allow if origin in allowedOrigins
        if (allowedOrigins.includes(origin))
            return callback(null, true);
        // For development convenience, allow localhost and 127.0.0.1 origins
        if (origin.startsWith("http://localhost") ||
            origin.startsWith("http://127.0.0.1"))
            return callback(null, true);
        // Otherwise deny
        return callback(null, false);
    },
    credentials: true,
    methods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
    allowedHeaders: [
        "Content-Type",
        "Authorization",
        "x-admin-key",
        "Cache-Control",
        "Pragma",
        "Expires",
        "Range",
    ],
    exposedHeaders: [
        "Content-Length",
        "Content-Type",
        "Content-Range",
        "Accept-Ranges",
    ],
    maxAge: 86400, // 24 hours
}));
app.use((0, compression_1.default)()); // Enable gzip compression for faster responses
// Parse JSON bodies - with type checking for various content-types
app.use(express_1.default.json({
    limit: "150mb", // Increased to 150MB to support video ads
    type: ["application/json", "application/*+json", "text/plain"], // Accept various JSON content types
}));
// Parse URL-encoded bodies (for form submissions)
app.use(express_1.default.urlencoded({ extended: true, limit: "150mb" }));
// Request logging middleware for debugging
app.use((req, res, next) => {
    if (req.path.includes("/login") || req.path.includes("/signup")) {
        console.log(`📥 ${req.method} ${req.path} - Content-Type: ${req.get("content-type")}`);
        console.log(`📦 Body keys: ${req.body ? Object.keys(req.body).join(", ") : "no body"}`);
    }
    next();
});
// Additional CORS headers middleware (belt and suspenders approach)
// Explicit CORS headers middleware (backup layer)
app.use((req, res, next) => {
    // Only set origin if it's in our allowed list
    const origin = req.headers.origin;
    if (origin &&
        (allowedOrigins.includes(origin) ||
            origin.startsWith("http://localhost") ||
            origin.startsWith("http://127.0.0.1"))) {
        res.header("Access-Control-Allow-Origin", origin);
        res.header("Access-Control-Allow-Credentials", "true");
    }
    res.header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
    res.header("Access-Control-Allow-Headers", "Content-Type, Authorization, x-admin-key, Cache-Control, Pragma, Expires, Range");
    res.header("Access-Control-Expose-Headers", "Content-Length, Content-Type, Content-Range, Accept-Ranges");
    // Handle preflight requests
    if (req.method === "OPTIONS") {
        return res.status(204).send();
    }
    next();
});
// Create uploads directories if they don't exist
const profilesDir = path_1.default.join(__dirname, "../uploads/profiles");
const cardsDir = path_1.default.join(__dirname, "../uploads/cards");
if (!fs_1.default.existsSync(profilesDir)) {
    fs_1.default.mkdirSync(profilesDir, { recursive: true });
}
if (!fs_1.default.existsSync(cardsDir)) {
    fs_1.default.mkdirSync(cardsDir, { recursive: true });
}
// Serve static files from uploads directory
app.use("/uploads", express_1.default.static(path_1.default.join(__dirname, "../uploads")));
// Favicon handler (prevents 404 errors in browser console)
app.get("/favicon.ico", (_req, res) => {
    res.status(204).end(); // No content
});
// Root health check for Render's health probe (responds to GET / or /health)
app.get("/", (_req, res) => {
    res.status(200).json({
        status: "ok",
        service: "Instantlly Cards Backend",
        version: "1.5",
        timestamp: Date.now(),
    });
});
app.get("/health", (_req, res) => {
    res.status(200).json({
        status: "ok",
        service: "Instantlly Cards Backend",
        timestamp: Date.now(),
    });
});
// Detailed health route
app.get("/api/health", async (_req, res) => {
    try {
        // Check database connection
        const dbStatus = mongoose_1.default.connection.readyState === 1 ? "connected" : "disconnected";
        let dbPing = "failed";
        if (mongoose_1.default.connection.readyState === 1 && mongoose_1.default.connection.db) {
            try {
                await mongoose_1.default.connection.db.admin().ping();
                dbPing = "ok";
            }
            catch (pingError) {
                console.error("DB ping failed:", pingError);
            }
        }
        const memUsage = process.memoryUsage();
        const cacheStats = imageCache_1.imageCache.getStats();
        res.status(200).json({
            ok: true,
            database: "mongodb",
            dbStatus: dbStatus,
            dbPing: dbPing,
            ts: Date.now(),
            version: "1.5", // Simple signup - only name, phone, password
            hasJwtSecret: !!process.env.JWT_SECRET,
            hasMongoUri: !!process.env.MONGODB_URI,
            memory: {
                heapUsedMB: (memUsage.heapUsed / 1024 / 1024).toFixed(2),
                heapTotalMB: (memUsage.heapTotal / 1024 / 1024).toFixed(2),
                rssMB: (memUsage.rss / 1024 / 1024).toFixed(2),
            },
            cache: cacheStats,
        });
    }
    catch (error) {
        console.error("Health check failed:", error);
        res.status(500).json({
            ok: false,
            error: "Health check failed",
            ts: Date.now(),
        });
    }
});
// Debug endpoint to check environment variables
app.get("/api/debug", (_req, res) => {
    res.status(200).json({
        hasJwtSecret: !!process.env.JWT_SECRET,
        jwtSecretLength: process.env.JWT_SECRET ? process.env.JWT_SECRET.length : 0,
        hasMongoUri: !!process.env.MONGODB_URI,
        hasExpoToken: !!process.env.EXPO_ACCESS_TOKEN,
        nodeEnv: process.env.NODE_ENV || "not set",
        port: process.env.PORT || "not set",
    });
});
// Start server with database connection
const port = Number(process.env.PORT) || 8080;
async function startServer() {
    try {
        console.log("🔄 Connecting to MongoDB...");
        await (0, db_1.connectDB)();
        console.log("✅ MongoDB connected successfully!");
        // Initialize GridFS for ad image storage
        gridfsService_1.gridfsService.initialize();
        console.log("✅ GridFS initialized for ad images");
        // Add routes after DB connection
        // Last updated: 2025-01-10 - Removed Firebase authentication, using only Fast2SMS OTP
        app.use("/api/auth", auth_1.default);
        app.use("/api/auth", otp_1.default); // OTP endpoints (send-otp, verify-otp) - Fast2SMS only
        app.use("/api/users", auth_1.default); // Mount auth router at /api/users for search-by-phone endpoint
        console.log("✅ Mounted /api/users route for search-by-phone endpoint");
        console.log("✅ Mounted /api/auth OTP routes (send-otp, verify-otp) - Fast2SMS only");
        app.use("/api/cards", cards_1.default);
        app.use("/api/contacts", contacts_1.default);
        app.use("/api/notifications", notifications_1.default);
        app.use("/api/messages", messages_1.default);
        app.use("/api/groups", groups_1.default);
        app.use("/api/chats", chats_1.default);
        app.use("/api/admin", admin_1.default);
        console.log("✅ Mounted /api/admin routes (dashboard stats, user management)");
        app.use("/api/admin-auth", adminAuth_1.default);
        console.log("✅ Mounted /api/admin-auth routes (admin login/verification)");
        app.use("/api/group-sharing", groupSharing_1.default);
        console.log("✅ Mounted /api/group-sharing routes (7 endpoints)");
        app.use("/api/ads", ads_1.default);
        console.log("✅ Mounted /api/ads routes (advertisement management)");
        app.use("/api/channel-partner/ads", channelPartnerAds_1.default);
        console.log("✅ Mounted /api/channel-partner/ads routes (channel partner ad upload/management)");
        app.use("/api/credits", credits_1.default);
        console.log("✅ Mounted /api/credits routes (credits system - balance, transfer, transactions)");
        app.use("/api/feedback", feedback_1.default);
        console.log("✅ Mounted /api/feedback routes (user feedback system)");
        app.use("/api/quiz", quiz_1.default);
        console.log("✅ Mounted /api/quiz routes (quiz progress, answer submission)");
        app.use("/api/business-promotion", businessPromotion_1.default);
        console.log("✅ Mounted /api/business-promotion routes (business promotion forms)");
        app.use("/api/mlm", mlm_1.default);
        console.log("✅ Mounted /api/mlm routes (MLM credits, vouchers, commissions)");
        app.use("/api/business-listings", business_listing_1.default);
        console.log("✅ Mounted /api/business-listing routes (business listing search)");
        app.use("/api/designer", designer_1.default);
        console.log("✅ Mounted /api/designer routes (designer login, requests, uploads)");
        app.use("/api/categories", categories_1.default);
        console.log("✅ Mounted /api/categories routes (categories, custom services management)");
        // Initialize Socket.IO service
        const socketService = new socketService_1.SocketService(io);
        console.log("🔌 Socket.IO service initialized");
        // Inject Socket.IO instance into messages router
        (0, messages_1.setSocketIO)(io);
        console.log("🔗 Socket.IO instance injected into messages router");
        // Initialize Erlang Gateway Integration only when configured (avoid crashes in hosted envs)
        let hybridMessaging = null;
        const erlangUrl = process.env.ERLANG_GATEWAY_URL || null;
        if (erlangUrl) {
            try {
                const HybridMessagingService = require("./services/hybridMessagingService").default;
                hybridMessaging = new HybridMessagingService(io);
                console.log("🚀 Erlang Gateway integration initialized");
                global.hybridMessaging = hybridMessaging;
            }
            catch (err) {
                console.error("❌ Failed to initialize HybridMessagingService:", err);
                hybridMessaging = null;
            }
        }
        else {
            console.log("⚠️ ERLANG_GATEWAY_URL not set - skipping Erlang gateway initialization");
        }
        (0, scheduler_1.startMlmScheduler)();
        console.log("✅ MLM scheduler started");
        // Start the server
        const port = process.env.PORT || 3001;
        server.listen(port, () => {
            console.log(`🚀 API server listening on 0.0.0.0:${port}`);
            console.log(`📡 Health check: http://0.0.0.0:${port}/api/health`);
            console.log(`🔌 Socket.IO server is running`);
            console.log(`👥 Real-time chat system is ready`);
        });
    }
    catch (error) {
        console.error("❌ Failed to start server:", error);
        process.exit(1);
    }
}
startServer();
// Deployment trigger - Tue Feb 17 17:35:53 IST 2026
