// index.ts
import dotenv from "dotenv";
import path from "path";
// Load .env from the project root
dotenv.config({ path: path.join(__dirname, "../.env") });

import express, { Request, Response } from "express";
import cors from "cors";
import compression from "compression";
import fs from "fs";
import mongoose from "mongoose";
import { createServer } from "http";
import { Server } from "socket.io";
import { connectDB } from "./db";
import authRouter from "./routes/auth";
import otpRouter from "./routes/otp";
import cardsRouter from "./routes/cards";
import contactsRouter from "./routes/contacts";
import notificationsRouter from "./routes/notifications";
import messagesRouter, { setSocketIO } from "./routes/messages";
import groupsRouter from "./routes/groups";
import chatsRouter from "./routes/chats";
import adminRouter from "./routes/admin";
import adminAuthRouter from "./routes/adminAuth";
import groupSharingRouter from "./routes/groupSharing";
import adsRouter from "./routes/ads";
import channelPartnerAdsRouter from "./routes/channelPartnerAds";
import creditsRouter from "./routes/credits";
import feedbackRouter from "./routes/feedback";
import quizRouter from "./routes/quiz";
import businessPromotionRouter from "./routes/businessPromotion";
import mlmRouter from "./routes/mlm";
import businessListing from "./routes/business-listing";
import designerRouter from "./routes/designer";
import { SocketService } from "./services/socketService";
import { gridfsService } from "./services/gridfsService";
import { imageCache } from "./services/imageCache";
import { startMlmScheduler } from "./services/mlm/scheduler";

const app = express();
const server = createServer(app);

// Socket.IO setup with CORS - Optimized for speed and memory
const io = new Server(server, {
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

app.use(
  cors({
    origin: function (origin, callback) {
      // Allow requests with no origin (mobile apps, Postman, server-to-server)
      if (!origin) return callback(null, true);

      // Allow if origin in allowedOrigins
      if (allowedOrigins.includes(origin)) return callback(null, true);

      // For development convenience, allow localhost and 127.0.0.1 origins
      if (
        origin.startsWith("http://localhost") ||
        origin.startsWith("http://127.0.0.1")
      )
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
  }),
);

app.use(compression()); // Enable gzip compression for faster responses

// Parse JSON bodies - with type checking for various content-types
app.use(
  express.json({
    limit: "150mb", // Increased to 150MB to support video ads
    type: ["application/json", "application/*+json", "text/plain"], // Accept various JSON content types
  }),
);

// Parse URL-encoded bodies (for form submissions)
app.use(express.urlencoded({ extended: true, limit: "150mb" }));

// Request logging middleware for debugging
app.use((req, res, next) => {
  if (req.path.includes("/login") || req.path.includes("/signup")) {
    console.log(
      `📥 ${req.method} ${req.path} - Content-Type: ${req.get("content-type")}`,
    );
    console.log(
      `📦 Body keys: ${req.body ? Object.keys(req.body).join(", ") : "no body"}`,
    );
  }
  next();
});

// Additional CORS headers middleware (belt and suspenders approach)
// Explicit CORS headers middleware (backup layer)
app.use((req, res, next) => {
  // Only set origin if it's in our allowed list
  const origin = req.headers.origin;
  if (
    origin &&
    (allowedOrigins.includes(origin) ||
      origin.startsWith("http://localhost") ||
      origin.startsWith("http://127.0.0.1"))
  ) {
    res.header("Access-Control-Allow-Origin", origin);
    res.header("Access-Control-Allow-Credentials", "true");
  }

  res.header("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
  res.header(
    "Access-Control-Allow-Headers",
    "Content-Type, Authorization, x-admin-key, Cache-Control, Pragma, Expires, Range",
  );
  res.header(
    "Access-Control-Expose-Headers",
    "Content-Length, Content-Type, Content-Range, Accept-Ranges",
  );

  // Handle preflight requests
  if (req.method === "OPTIONS") {
    return res.status(204).send();
  }

  next();
});

// Create uploads directories if they don't exist
const profilesDir = path.join(__dirname, "../uploads/profiles");
const cardsDir = path.join(__dirname, "../uploads/cards");

if (!fs.existsSync(profilesDir)) {
  fs.mkdirSync(profilesDir, { recursive: true });
}
if (!fs.existsSync(cardsDir)) {
  fs.mkdirSync(cardsDir, { recursive: true });
}

// Serve static files from uploads directory
app.use("/uploads", express.static(path.join(__dirname, "../uploads")));

// Favicon handler (prevents 404 errors in browser console)
app.get("/favicon.ico", (_req: Request, res: Response) => {
  res.status(204).end(); // No content
});

// Root health check for Render's health probe (responds to GET / or /health)
app.get("/", (_req: Request, res: Response) => {
  res.status(200).json({
    status: "ok",
    service: "Instantlly Cards Backend",
    version: "1.5",
    timestamp: Date.now(),
  });
});

app.get("/health", (_req: Request, res: Response) => {
  res.status(200).json({
    status: "ok",
    service: "Instantlly Cards Backend",
    timestamp: Date.now(),
  });
});

// Detailed health route
app.get("/api/health", async (_req: Request, res: Response) => {
  try {
    // Check database connection
    const dbStatus =
      mongoose.connection.readyState === 1 ? "connected" : "disconnected";
    let dbPing = "failed";

    if (mongoose.connection.readyState === 1 && mongoose.connection.db) {
      try {
        await mongoose.connection.db.admin().ping();
        dbPing = "ok";
      } catch (pingError) {
        console.error("DB ping failed:", pingError);
      }
    }

    const memUsage = process.memoryUsage();
    const cacheStats = imageCache.getStats();

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
  } catch (error) {
    console.error("Health check failed:", error);
    res.status(500).json({
      ok: false,
      error: "Health check failed",
      ts: Date.now(),
    });
  }
});

// Debug endpoint to check environment variables
app.get("/api/debug", (_req: Request, res: Response) => {
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
    await connectDB();
    console.log("✅ MongoDB connected successfully!");

    // Initialize GridFS for ad image storage
    gridfsService.initialize();
    console.log("✅ GridFS initialized for ad images");

    // Add routes after DB connection
    // Last updated: 2025-01-10 - Removed Firebase authentication, using only Fast2SMS OTP
    app.use("/api/auth", authRouter);
    app.use("/api/auth", otpRouter); // OTP endpoints (send-otp, verify-otp) - Fast2SMS only
    app.use("/api/users", authRouter); // Mount auth router at /api/users for search-by-phone endpoint
    console.log("✅ Mounted /api/users route for search-by-phone endpoint");
    console.log(
      "✅ Mounted /api/auth OTP routes (send-otp, verify-otp) - Fast2SMS only",
    );
    app.use("/api/cards", cardsRouter);
    app.use("/api/contacts", contactsRouter);
    app.use("/api/notifications", notificationsRouter);
    app.use("/api/messages", messagesRouter);
    app.use("/api/groups", groupsRouter);
    app.use("/api/chats", chatsRouter);
    app.use("/api/admin", adminRouter);
    console.log(
      "✅ Mounted /api/admin routes (dashboard stats, user management)",
    );
    app.use("/api/admin-auth", adminAuthRouter);
    console.log("✅ Mounted /api/admin-auth routes (admin login/verification)");
    app.use("/api/group-sharing", groupSharingRouter);
    console.log("✅ Mounted /api/group-sharing routes (7 endpoints)");
    app.use("/api/ads", adsRouter);
    console.log("✅ Mounted /api/ads routes (advertisement management)");
    app.use("/api/channel-partner/ads", channelPartnerAdsRouter);
    console.log(
      "✅ Mounted /api/channel-partner/ads routes (channel partner ad upload/management)",
    );
    app.use("/api/credits", creditsRouter);
    console.log(
      "✅ Mounted /api/credits routes (credits system - balance, transfer, transactions)",
    );
    app.use("/api/feedback", feedbackRouter);
    console.log("✅ Mounted /api/feedback routes (user feedback system)");
    app.use("/api/quiz", quizRouter);
    console.log(
      "✅ Mounted /api/quiz routes (quiz progress, answer submission)",
    );
    app.use("/api/business-promotion", businessPromotionRouter);
    console.log(
      "✅ Mounted /api/business-promotion routes (business promotion forms)",
    );
    app.use("/api/mlm", mlmRouter);
    console.log(
      "✅ Mounted /api/mlm routes (MLM credits, vouchers, commissions)",
    );
    app.use("/api/business-listings", businessListing);
    console.log(
      "✅ Mounted /api/business-listing routes (business listing search)",
    );
    app.use("/api/designer", designerRouter);
    console.log(
      "✅ Mounted /api/designer routes (designer login, requests, uploads)",
    );

    // Initialize Socket.IO service
    const socketService = new SocketService(io);
    console.log("🔌 Socket.IO service initialized");

    // Inject Socket.IO instance into messages router
    setSocketIO(io);
    console.log("🔗 Socket.IO instance injected into messages router");

    // Initialize Erlang Gateway Integration only when configured (avoid crashes in hosted envs)
    let hybridMessaging: any = null;
    const erlangUrl = process.env.ERLANG_GATEWAY_URL || null;
    if (erlangUrl) {
      try {
        const HybridMessagingService =
          require("./services/hybridMessagingService").default;
        hybridMessaging = new HybridMessagingService(io);
        console.log("🚀 Erlang Gateway integration initialized");
        (global as any).hybridMessaging = hybridMessaging;
      } catch (err) {
        console.error("❌ Failed to initialize HybridMessagingService:", err);
        hybridMessaging = null;
      }
    } else {
      console.log(
        "⚠️ ERLANG_GATEWAY_URL not set - skipping Erlang gateway initialization",
      );
    }

    startMlmScheduler();
    console.log("✅ MLM scheduler started");

    // Start the server
    const port = process.env.PORT || 3001;
    server.listen(port, () => {
      console.log(`🚀 API server listening on 0.0.0.0:${port}`);
      console.log(`📡 Health check: http://0.0.0.0:${port}/api/health`);
      console.log(`🔌 Socket.IO server is running`);
      console.log(`👥 Real-time chat system is ready`);
    });
  } catch (error) {
    console.error("❌ Failed to start server:", error);
    process.exit(1);
  }
}

startServer();
// Deployment trigger - Tue Feb 17 17:35:53 IST 2026
