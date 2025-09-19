// index.ts
import dotenv from "dotenv";
dotenv.config();

import express from "express";
import cors from "cors";
import mongoose from "mongoose";
import path from "path";
import fs from "fs";
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
app.get("/api/health", (_req, res) => {
  const mongoUp = mongoose.connection?.readyState === 1; // 1=connected
  res.status(200).json({ ok: true, mongo: mongoUp ? "up" : "down", ts: Date.now() });
});

// Start server immediately
const port = Number(process.env.PORT) || 8080;

// Add routes first
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

// Connect to MongoDB in background
(async () => {
  try {
    console.log("🔄 Connecting to MongoDB...");
    await connectDB();                           
    console.log("✅ MongoDB connected successfully!");
  } catch (err) {
    console.error("❌ MongoDB connection failed:", err instanceof Error ? err.message : String(err));
    console.log("⚠️  Server running without database connection");
  }
})();
