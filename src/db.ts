// src/db.ts
import mongoose from "mongoose";

export async function connectDB() {
  if (mongoose.connection.readyState === 1) return; // already connected
  const uri = process.env.MONGODB_URI;
  if (!uri) throw new Error("MONGODB_URI is not set");

  console.log("Attempting MongoDB connection...");
  
  await mongoose.connect(uri, {
    serverSelectionTimeoutMS: 30000, // Increased to 30s for slow network
    socketTimeoutMS: 45000, // Increased to 45s for large file operations (GridFS)
    connectTimeoutMS: 30000, // Increased to 30s for initial connection
    maxPoolSize: 50, // Increased pool size for concurrent requests
    minPoolSize: 10, // Maintain minimum 10 socket connections
    maxIdleTimeMS: 60000, // Close connections after 60 seconds of inactivity
    retryWrites: true,
    retryReads: true,
    heartbeatFrequencyMS: 10000, // Check connection health every 10s
    family: 4 // Force IPv4 (faster DNS resolution)
  });

  const c = mongoose.connection;
  c.on("connected", () => console.log("✅ MongoDB connected"));
  c.on("error", (e) => console.error("❌ MongoDB error:", e));
  c.on("disconnected", () => console.warn("⚠️ MongoDB disconnected"));
  c.on("reconnected", () => console.log("🔄 MongoDB reconnected"));
  
  console.log("MongoDB connection established");
}
