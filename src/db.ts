// src/db.ts
import mongoose from "mongoose";

let isConnected = false;
let connectionAttempts = 0;
const MAX_RETRY_ATTEMPTS = 5; // Increased from 3
const RETRY_DELAY_MS = 3000; // 3 seconds

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
      retryWrites: true,
      retryReads: true,
      serverSelectionTimeoutMS: 10000, // 10 seconds
      socketTimeoutMS: 45000, // 45 seconds
      family: 4, // Use IPv4, skip trying IPv6
    });

    // Set connected flag immediately after successful connection
    isConnected = true;
    connectionAttempts = 0;
    console.log("✅ MongoDB connection established");

    const c = mongoose.connection;
    
    c.on("connected", () => {
      console.log("✅ MongoDB connected event fired");
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
      
      // DON'T throw - just log
    });
    
    c.on("disconnected", () => {
      console.warn("⚠️ MongoDB disconnected - will attempt reconnection");
      isConnected = false;
    });
    
    c.on("reconnected", () => {
      console.log("🔄 MongoDB reconnected");
      isConnected = true;
      connectionAttempts = 0;
    });
    
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
      console.log(`⏳ Retrying connection in ${RETRY_DELAY_MS/1000} seconds...`);
      await new Promise(resolve => setTimeout(resolve, RETRY_DELAY_MS));
      return connectDB(); // Retry
    }
    
    // After max attempts, throw but don't crash the process
    console.error("❌ Max connection attempts reached - service may be degraded");
    throw error;
  }
}

export function isDBConnected(): boolean {
  return isConnected && mongoose.connection.readyState === 1;
}
