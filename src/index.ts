// index.ts
import dotenv from "dotenv";
dotenv.config();

import express from "express";
import cors from "cors";
import mongoose from "mongoose";
import { connectDB } from "./db";
import authRouter from "./routes/auth";
import cardsRouter from "./routes/cards";
import contactsRouter from "./routes/contacts";

const app = express();
app.use(cors());
app.use(express.json({ limit: "10mb" })); // instead of default

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

// Start the server
const server = app.listen(port, '127.0.0.1', () => {
  console.log(`🚀 API server listening on 127.0.0.1:${port}`);
  console.log(`📡 Health check: http://127.0.0.1:${port}/api/health`);
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
