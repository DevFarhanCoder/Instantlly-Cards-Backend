// index.ts
import dotenv from "dotenv";
dotenv.config();

import express, { Request, Response } from "express";
import cors from "cors";
import path from "path";
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
import { SocketService } from "./services/socketService";

const app = express();
const server = createServer(app);

// Socket.IO setup with CORS
const io = new Server(server, {
  cors: {
    origin: "*",
    methods: ["GET", "POST"],
    credentials: true
  },
  pingTimeout: 60000,
  pingInterval: 25000,
  transports: ["polling", "websocket"], // Start with polling first
  allowEIO3: true, // Support older Socket.IO versions
  upgradeTimeout: 30000, // Allow more time for websocket upgrade
  maxHttpBufferSize: 1e6 // 1MB buffer size
});

app.use(cors());
app.use(express.json({ limit: "10mb" })); // instead of default

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

// single health route
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
    
    res.status(200).json({ 
      ok: true, 
      database: "mongodb", 
      dbStatus: dbStatus,
      dbPing: dbPing,
      ts: Date.now(), 
      version: "1.5", // Simple signup - only name, phone, password
      hasJwtSecret: !!process.env.JWT_SECRET,
      hasMongoUri: !!process.env.MONGODB_URI
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

    // Add routes after DB connection
    // Last updated: 2025-10-17 - Added OTP endpoints for phone verification
    app.use("/api/auth", authRouter);
    app.use("/api/auth", otpRouter); // OTP endpoints (send-otp, verify-otp)
    app.use("/api/users", authRouter); // Mount auth router at /api/users for search-by-phone endpoint
    console.log("✅ Mounted /api/users route for search-by-phone endpoint");
    console.log("✅ Mounted /api/auth OTP routes (send-otp, verify-otp)");
    app.use("/api/cards", cardsRouter);          
    app.use("/api/contacts", contactsRouter);    
    app.use("/api/notifications", notificationsRouter);    
    app.use("/api/messages", messagesRouter);    
    app.use("/api/groups", groupsRouter);    
    app.use("/api/chats", chatsRouter);
    app.use("/api/admin", adminRouter);
    app.use("/api/admin-auth", adminAuthRouter);
    console.log("✅ Mounted /api/admin-auth routes (admin login/verification)");
    app.use("/api/group-sharing", groupSharingRouter);
    console.log("✅ Mounted /api/group-sharing routes (7 endpoints)");    
    app.use("/api/ads", adsRouter);
    console.log("✅ Mounted /api/ads routes (advertisement management)");

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