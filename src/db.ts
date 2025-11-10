// src/db.ts
import mongoose from "mongoose";

let isConnected = false;
let connectionAttempts = 0;
const MAX_RETRY_ATTEMPTS = 3;

export async function connectDB() {
  if (mongoose.connection.readyState === 1) {
    isConnected = true;
    return; // already connected
  }
  
  const uri = process.env.MONGODB_URI;
  if (!uri) throw new Error("MONGODB_URI is not set");

  console.log("Attempting MongoDB connection...");
  
  try {
    await mongoose.connect(uri, {
      serverSelectionTimeoutMS: 30000,
      socketTimeoutMS: 45000,
      connectTimeoutMS: 30000,
      maxPoolSize: 50,
      minPoolSize: 10,
      maxIdleTimeMS: 60000,
      retryWrites: true,
      retryReads: true,
      heartbeatFrequencyMS: 10000,
      family: 4
    });

    const c = mongoose.connection;
    
    c.on("connected", () => {
      console.log("✅ MongoDB connected successfully");
      isConnected = true;
      connectionAttempts = 0;
    });
    
    c.on("error", (e) => {
      console.error("❌ MongoDB error:", e.message);
      isConnected = false;
      
      // Check for authentication errors
      if (e.message.includes("Authentication failed") || 
          e.message.includes("auth failed") ||
          e.message.includes("bad auth")) {
        console.error("🔐 CRITICAL: MongoDB authentication failed - Check MONGODB_URI password!");
        console.error("💡 Update password in Render.com environment variables");
      }
    });
    
    c.on("disconnected", () => {
      console.warn("⚠️ MongoDB disconnected");
      isConnected = false;
    });
    
    c.on("reconnected", () => {
      console.log("🔄 MongoDB reconnected");
      isConnected = true;
      connectionAttempts = 0;
    });
    
    console.log("MongoDB connection established");
  } catch (error: any) {
    connectionAttempts++;
    console.error(`❌ MongoDB connection failed (attempt ${connectionAttempts}/${MAX_RETRY_ATTEMPTS}):`, error.message);
    
    if (error.message.includes("Authentication failed") || 
        error.message.includes("auth failed") ||
        error.message.includes("bad auth")) {
      console.error("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
      console.error("🔐 AUTHENTICATION ERROR - IMMEDIATE ACTION REQUIRED!");
      console.error("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
      console.error("1. Go to Render.com dashboard");
      console.error("2. Select 'Instantlly-Cards-Backend' service");
      console.error("3. Click 'Environment' tab");
      console.error("4. Update MONGODB_URI with new password");
      console.error("5. Click 'Manual Deploy' to restart");
      console.error("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    }
    
    if (connectionAttempts < MAX_RETRY_ATTEMPTS) {
      console.log(`⏳ Retrying connection in 5 seconds...`);
      await new Promise(resolve => setTimeout(resolve, 5000));
      return connectDB(); // Retry
    }
    
    throw error; // Give up after max attempts
  }
}

export function isDBConnected(): boolean {
  return isConnected && mongoose.connection.readyState === 1;
}
