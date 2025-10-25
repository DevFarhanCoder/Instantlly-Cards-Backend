// src/db.ts
import mongoose from "mongoose";

export async function connectDB() {
  if (mongoose.connection.readyState === 1) return; // already connected
  const uri = process.env.MONGODB_URI;
  if (!uri) throw new Error("MONGODB_URI is not set");

  console.log("Attempting MongoDB connection...");
  
  await mongoose.connect(uri, {
    serverSelectionTimeoutMS: 10000, // Increased from 5s to 10s for better reliability
    socketTimeoutMS: 15000, // Increased from 10s to 15s
    connectTimeoutMS: 10000, // Increased from 5s to 10s
    maxPoolSize: 10, // Maintain up to 10 socket connections
    minPoolSize: 5,  // Maintain minimum 5 socket connections
    maxIdleTimeMS: 30000, // Close connections after 30 seconds of inactivity
    retryWrites: true,
    retryReads: true
  });

  const c = mongoose.connection;
  c.on("connected", () => console.log("✅ MongoDB connected"));
  c.on("error", (e) => console.error("❌ MongoDB error:", e));
  c.on("disconnected", () => console.warn("⚠️ MongoDB disconnected"));
  c.on("reconnected", () => console.log("🔄 MongoDB reconnected"));
  
  console.log("MongoDB connection established");
}
