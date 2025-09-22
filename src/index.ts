// index.ts
import dotenv from "dotenv";
dotenv.config();

import express from "express";
import cors from "cors";
import path from "path";
import fs from "fs";
import mongoose from "mongoose";
import { connectDB } from "./db";
import authRouter from "./routes/auth";
import cardsRouter from "./routes/cards";
import contactsRouter from "./routes/contacts";
import notificationsRouter from "./routes/notifications";
import messagesRouter from "./routes/messages";
import groupsRouter from "./routes/groups";

const app = express();
app.use(cors());
app.use(express.json({ limit: "10mb" })); // instead of default

// Create uploads directory if it doesn't exist
const uploadsDir = path.join(__dirname, '../uploads/profiles');
if (!fs.existsSync(uploadsDir)) {
  fs.mkdirSync(uploadsDir, { recursive: true });
}

// Serve static files from uploads directory
app.use('/uploads', express.static(path.join(__dirname, '../uploads')));

// single health route
app.get("/api/health", async (_req, res) => {
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
      version: "1.4", // Clean signup rebuild
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
app.get("/api/debug", (_req, res) => {
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
    app.use("/api/auth", authRouter);            
    app.use("/api/cards", cardsRouter);          
    app.use("/api/contacts", contactsRouter);    
    app.use("/api/notifications", notificationsRouter);    
    app.use("/api/messages", messagesRouter);    
    app.use("/api/groups", groupsRouter);    

    // Start the server
    const server = app.listen(port, '0.0.0.0', () => {
      console.log(`🚀 API server listening on 0.0.0.0:${port}`);
      console.log(`📡 Health check: http://0.0.0.0:${port}/api/health`);
    });

  } catch (error) {
    console.error("❌ Failed to start server:", error);
    process.exit(1);
  }
}

startServer();