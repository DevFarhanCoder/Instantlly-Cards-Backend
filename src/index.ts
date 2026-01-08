// index.ts
import dotenv from "dotenv";
import path from "path";
// Load .env from the project root
dotenv.config({ path: path.join(__dirname, '../.env') });

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
import firebaseAuthRouter from "./routes/firebaseAuth";
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
import { SocketService } from "./services/socketService";
import { gridfsService } from "./services/gridfsService";
import { imageCache } from "./services/imageCache";

// ==========================================
// CRITICAL: Global Error Handlers
// Prevents exit status 134 crashes
// ==========================================
process.on('uncaughtException', (error: Error) => {
  console.error('❌ UNCAUGHT EXCEPTION - This would have crashed the app!');
  console.error('Error:', error.message);
  console.error('Stack:', error.stack);
  // Don't exit - log and continue
});

process.on('unhandledRejection', (reason: any, promise: Promise<any>) => {
  console.error('❌ UNHANDLED REJECTION - This would have crashed the app!');
  console.error('Reason:', reason);
  console.error('Promise:', promise);
  // Don't exit - log and continue
});

process.on('SIGTERM', async () => {
  console.log('📴 SIGTERM received, starting graceful shutdown...');
  await gracefulShutdown();
});

process.on('SIGINT', async () => {
  console.log('📴 SIGINT received, starting graceful shutdown...');
  await gracefulShutdown();
});

let isShuttingDown = false;

async function gracefulShutdown() {
  if (isShuttingDown) return;
  isShuttingDown = true;
  
  try {
    console.log('🔄 Closing server...');
    server.close(() => {
      console.log('✅ HTTP server closed');
    });
    
    console.log('🔄 Closing database connection...');
    await mongoose.connection.close();
    console.log('✅ MongoDB connection closed');
    
    console.log('✅ Graceful shutdown complete');
    process.exit(0);
  } catch (error) {
    console.error('❌ Error during graceful shutdown:', error);
    process.exit(1);
  }
}
// ==========================================

const app = express();
const server = createServer(app);

// Socket.IO setup with CORS - Optimized for 4 instances with 512MB RAM each
const io = new Server(server, {
  cors: {
    origin: "*",
    methods: ["GET", "POST"],
    credentials: true
  },
  pingTimeout: 15000, // Reduced to 15s for faster disconnect detection
  pingInterval: 8000, // Reduced to 8s for faster keepalive
  transports: ["websocket", "polling"], // WebSocket first for speed
  allowEIO3: true,
  upgradeTimeout: 8000, // Reduced to 8s
  maxHttpBufferSize: 1e6, // Reduced to 1MB (from 2MB) for 4 instances
  connectTimeout: 8000, // Reduced connection timeout
  perMessageDeflate: false, // Disable compression for speed
  // CRITICAL: Prevent memory buildup with multiple instances
  connectionStateRecovery: {
    maxDisconnectionDuration: 2 * 60 * 1000, // 2 minutes
    skipMiddlewares: true,
  },
});

// CORS Configuration - Allow requests from Vercel and admin dashboards
const defaultAllowed = [
  'https://instantlly-ads.vercel.app',
  'https://instantlly-admin.vercel.app',
  'http://localhost:3000',
  'http://localhost:3001'
];

const adminOrigin = process.env.ADMIN_WEB_ORIGIN; // e.g. https://instantllychannelpatneradmin.vercel.app
const allowedOrigins = adminOrigin ? [...defaultAllowed, adminOrigin] : [...defaultAllowed];

app.use(cors({
  origin: function(origin, callback) {
    // Allow requests with no origin (mobile apps, Postman, server-to-server)
    if (!origin) return callback(null, true);

    // Allow if origin in allowedOrigins
    if (allowedOrigins.includes(origin)) return callback(null, true);

    // For development convenience, allow localhost origins
    if (origin.startsWith('http://localhost')) return callback(null, true);

    // Otherwise deny
    return callback(new Error('Not allowed by CORS'));
  },
  credentials: true,
  methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
  allowedHeaders: ['Content-Type', 'Authorization', 'x-admin-key', 'Cache-Control', 'Pragma', 'Expires'],
  exposedHeaders: ['Content-Length', 'Content-Type'],
  maxAge: 86400 // 24 hours
}));

app.use(compression()); // Enable gzip compression for faster responses

// Parse JSON bodies - with type checking for various content-types
// REDUCED LIMITS: Prevent memory spikes with 4 instances
app.use(express.json({ 
  limit: "5mb", // Reduced from 10mb to prevent OOM
  type: ['application/json', 'application/*+json', 'text/plain'] // Accept various JSON content types
})); 

// Parse URL-encoded bodies (for form submissions)
app.use(express.urlencoded({ extended: true, limit: "5mb" })); // Reduced from 10mb

// Request logging middleware for debugging
app.use((req, res, next) => {
  if (req.path.includes('/login') || req.path.includes('/signup')) {
    console.log(`📥 ${req.method} ${req.path} - Content-Type: ${req.get('content-type')}`);
    console.log(`📦 Body keys: ${req.body ? Object.keys(req.body).join(', ') : 'no body'}`);
  }
  next()
});

// Additional CORS headers middleware (belt and suspenders approach)
app.use((req, res, next) => {
  res.header('Access-Control-Allow-Origin', req.headers.origin || '*');
  res.header('Access-Control-Allow-Credentials', 'true');
  res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.header('Access-Control-Allow-Headers', 'Content-Type, Authorization, x-admin-key, Cache-Control, Pragma, Expires');
  res.header('Access-Control-Expose-Headers', 'Content-Length, Content-Type');
  
  // Handle preflight requests
  if (req.method === 'OPTIONS') {
    return res.status(204).send();
  }
  
  next();
});

// Create uploads directories if they don't exist
const profilesDir = path.join(__dirname, '../uploads/profiles');
const cardsDir = path.join(__dirname, '../uploads/cards');

if (!fs.existsSync(profilesDir)) {
  fs.mkdirSync(profilesDir, { recursive: true });
}
if (!fs.existsSync(cardsDir)) {
  fs.mkdirSync(cardsDir, { recursive: true });
}

// Serve static files from uploads directory
app.use('/uploads', express.static(path.join(__dirname, '../uploads')));

// Favicon handler (prevents 404 errors in browser console)
app.get('/favicon.ico', (_req: Request, res: Response) => {
  res.status(204).end(); // No content
});

// Root health check for Render's health probe (responds to GET / or /health)
app.get("/", (_req: Request, res: Response) => {
  res.status(200).json({ 
    status: "ok",
    service: "Instantlly Cards Backend",
    version: "1.5",
    timestamp: Date.now()
  });
});

app.get("/health", (_req: Request, res: Response) => {
  res.status(200).json({ 
    status: "ok",
    service: "Instantlly Cards Backend",
    timestamp: Date.now()
  });
});

// Detailed health route
app.get("/api/health", async (_req: Request, res: Response) => {
  try {
    // Check database connection
    const dbStatus = mongoose.connection.readyState === 1 ? "connected" : "disconnected";
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
        rssMB: (memUsage.rss / 1024 / 1024).toFixed(2)
      },
      cache: cacheStats
    });
  } catch (error) {
    console.error("Health check failed:", error);
    res.status(500).json({ 
      ok: false, 
      error: "Health check failed",
      ts: Date.now()
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
    nodeEnv: process.env.NODE_ENV || 'not set',
    port: process.env.PORT || 'not set'
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
    // Last updated: 2025-11-19 - Added Firebase authentication
    app.use("/api/auth", authRouter);
    app.use("/api/auth", otpRouter); // OTP endpoints (send-otp, verify-otp)
    app.use("/api/auth", firebaseAuthRouter); // Firebase authentication (firebase-auth)
    app.use("/api/users", authRouter); // Mount auth router at /api/users for search-by-phone endpoint
    console.log("✅ Mounted /api/users route for search-by-phone endpoint");
    console.log("✅ Mounted /api/auth OTP routes (send-otp, verify-otp)");
    console.log("✅ Mounted /api/auth Firebase routes (firebase-auth)");
    app.use("/api/cards", cardsRouter);          
    app.use("/api/contacts", contactsRouter);    
    app.use("/api/notifications", notificationsRouter);    
    app.use("/api/messages", messagesRouter);    
    app.use("/api/groups", groupsRouter);    
    app.use("/api/chats", chatsRouter);
    app.use("/api/admin", adminRouter);
    console.log("✅ Mounted /api/admin routes (dashboard stats, user management)");
    app.use("/api/admin-auth", adminAuthRouter);
    console.log("✅ Mounted /api/admin-auth routes (admin login/verification)");
    app.use("/api/group-sharing", groupSharingRouter);
    console.log("✅ Mounted /api/group-sharing routes (7 endpoints)");    
    app.use("/api/ads", adsRouter);
    console.log("✅ Mounted /api/ads routes (advertisement management)");
    app.use("/api/channel-partner/ads", channelPartnerAdsRouter);
    console.log("✅ Mounted /api/channel-partner/ads routes (channel partner ad upload/management)");
    app.use("/api/credits", creditsRouter);
    console.log("✅ Mounted /api/credits routes (credits system - balance, transfer, transactions)");
    app.use("/api/feedback", feedbackRouter);
    console.log("✅ Mounted /api/feedback routes (user feedback system)");
    app.use("/api/quiz", quizRouter);
    console.log("✅ Mounted /api/quiz routes (quiz progress, answer submission)");
    app.use("/api/business-promotion", businessPromotionRouter);
    console.log("✅ Mounted /api/business-promotion routes (business promotion forms)");

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
        const HybridMessagingService = require("./services/hybridMessagingService").default;
        hybridMessaging = new HybridMessagingService(io);
        console.log("🚀 Erlang Gateway integration initialized");
        (global as any).hybridMessaging = hybridMessaging;
      } catch (err) {
        console.error('❌ Failed to initialize HybridMessagingService:', err);
        hybridMessaging = null;
      }
    } else {
      console.log('⚠️ ERLANG_GATEWAY_URL not set - skipping Erlang gateway initialization');
    }

    // Global error handling middleware (must be last)
    app.use((err: any, req: Request, res: Response, next: any) => {
      console.error('❌ Global error handler caught:', err.message);
      console.error('Stack:', err.stack);
      
      // Send error response
      res.status(err.status || 500).json({
        success: false,
        message: err.message || 'Internal server error',
        ...(process.env.NODE_ENV === 'development' && { stack: err.stack })
      });
    });

    // Start the server
    const port = process.env.PORT || 3001;
    server.listen(port, () => {
      console.log(`🚀 API server listening on 0.0.0.0:${port}`);
      console.log(`📡 Health check: http://0.0.0.0:${port}/api/health`);
      console.log(`🔌 Socket.IO server is running`);
      console.log(`👥 Real-time chat system is ready`);
      console.log(`💾 AGGRESSIVE memory optimization active (Exit 134 prevention)`);
      console.log(`🛡️  Global error handlers active`);
      
      // Log memory usage every 10 minutes
      setInterval(() => {
        const mem = process.memoryUsage();
        const heapUsedMB = mem.heapUsed / 1024 / 1024;
        const heapTotalMB = mem.heapTotal / 1024 / 1024;
        const rssMB = mem.rss / 1024 / 1024;
        console.log(`📊 [MEMORY] Heap: ${heapUsedMB.toFixed(0)}MB / ${heapTotalMB.toFixed(0)}MB | RSS: ${rssMB.toFixed(0)}MB`);
        
        // Warning if approaching limits
        if (heapUsedMB > 350) {
          console.log(`🚨 [WARNING] Memory usage high - approaching 512MB limit!`);
        }
      }, 10 * 60 * 1000);
    });

  } catch (error) {
    console.error("❌ Failed to start server:", error);
    // Don't exit immediately - allow restart
    setTimeout(() => process.exit(1), 1000);
  }
}

startServer();