// src/db.ts
import mongoose from "mongoose";
import { connectToDocumentDB, getDatabaseInfo } from "./config/documentdb";

/**
 * Enhanced database connection with AWS DocumentDB support
 * Priority: DocumentDB first, then MongoDB Atlas fallback
 */
export async function connectDB() {
  if (mongoose.connection.readyState === 1) {
    console.log("Database already connected");
    return;
  }

  try {
    // Try DocumentDB first (if DOCUMENTDB_URI is available)
    if (process.env.DOCUMENTDB_URI) {
      await connectToDocumentDB();
      return;
    }

    // Fallback to original MongoDB Atlas connection
    console.log("🔄 DOCUMENTDB_URI not found, using MongoDB Atlas...");
    await connectToMongoAtlas();
    
  } catch (error) {
    console.error("❌ Database connection failed:", error);
    throw error;
  }
}

/**
 * Original MongoDB Atlas connection (kept as fallback)
 */
async function connectToMongoAtlas() {
  const uri = process.env.MONGODB_URI;
  if (!uri) throw new Error("MONGODB_URI is not set");

  console.log("🔄 Connecting to MongoDB Atlas...");
  
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
  c.on("connected", () => console.log("✅ MongoDB Atlas connected"));
  c.on("error", (e) => console.error("❌ MongoDB Atlas error:", e));
  c.on("disconnected", () => console.warn("⚠️ MongoDB Atlas disconnected"));
  c.on("reconnected", () => console.log("🔄 MongoDB Atlas reconnected"));
  
  console.log("✅ MongoDB Atlas connection established");
}

/**
 * Get enhanced database information for health checks
 */
export function getDBInfo() {
  return getDatabaseInfo();
}
