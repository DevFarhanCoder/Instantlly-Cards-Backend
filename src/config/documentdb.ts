// src/config/documentdb.ts
import mongoose from "mongoose";
import path from "path";
import fs from "fs";

interface DocumentDBConfig {
  uri: string;
  options: mongoose.ConnectOptions;
}

/**
 * AWS DocumentDB Connection Configuration
 * Features:
 * - SSL/TLS encryption required
 * - Connection pooling optimized for DocumentDB
 * - Retry logic for better reliability
 * - Automatic fallback to MongoDB Atlas
 */
export function getDocumentDBConfig(): DocumentDBConfig {
  // Get DocumentDB URI from environment
  const documentDbUri = process.env.DOCUMENTDB_URI;
  
  if (!documentDbUri) {
    throw new Error("DOCUMENTDB_URI environment variable is not set");
  }

  // SSL Certificate path (relative to backend root)
  const sslCertPath = path.join(__dirname, "../../global-bundle.pem");
  
  // Verify SSL certificate exists
  if (!fs.existsSync(sslCertPath)) {
    throw new Error(`DocumentDB SSL certificate not found at ${sslCertPath}`);
  }

  const config: DocumentDBConfig = {
    uri: documentDbUri,
    options: {
      // SSL Configuration (required for DocumentDB)
      tls: true,
      tlsCAFile: sslCertPath,
      
      // Connection Timeouts (optimized for DocumentDB)
      serverSelectionTimeoutMS: 30000, // 30 seconds for DocumentDB
      socketTimeoutMS: 45000, // 45 seconds socket timeout
      connectTimeoutMS: 30000, // 30 seconds connect timeout
      
      // Connection Pool Settings
      maxPoolSize: 10, // Maximum number of connections
      minPoolSize: 2,  // Minimum number of connections
      maxIdleTimeMS: 60000, // Close connections after 60 seconds of inactivity
      
      // DocumentDB Specific Settings
      retryWrites: false, // DocumentDB doesn't support retryable writes
      
      // Retry Logic
      retryReads: true,
      
      // Heartbeat
      heartbeatFrequencyMS: 10000, // 10 seconds heartbeat
    }
  };

  return config;
}

/**
 * Connect to AWS DocumentDB with automatic fallback to MongoDB Atlas
 */
export async function connectToDocumentDB(): Promise<void> {
  try {
    console.log("🔄 Connecting to AWS DocumentDB...");
    
    const config = getDocumentDBConfig();
    
    // Attempt DocumentDB connection
    await mongoose.connect(config.uri, config.options);
    
    console.log("✅ Connected to AWS DocumentDB successfully!");
    console.log("🌐 Database: AWS DocumentDB");
    console.log("🔒 SSL: Enabled");
    console.log("🔄 Connection Pool: Active");
    
    // Setup connection event listeners
    setupDocumentDBEventListeners();
    
  } catch (error) {
    console.error("❌ DocumentDB connection failed:", error);
    console.log("🔄 Attempting fallback to MongoDB Atlas...");
    
    // Fallback to MongoDB Atlas
    await fallbackToMongoAtlas();
  }
}

/**
 * Fallback connection to MongoDB Atlas
 */
async function fallbackToMongoAtlas(): Promise<void> {
  const atlasUri = process.env.MONGODB_URI;
  
  if (!atlasUri) {
    throw new Error("Neither DOCUMENTDB_URI nor MONGODB_URI is available");
  }

  console.log("🔄 Connecting to MongoDB Atlas (fallback)...");
  
  await mongoose.connect(atlasUri, {
    serverSelectionTimeoutMS: 10000,
    socketTimeoutMS: 15000,
    connectTimeoutMS: 10000,
    maxPoolSize: 10,
    minPoolSize: 5,
    maxIdleTimeMS: 30000,
    retryWrites: true,
    retryReads: true
  });

  console.log("✅ Fallback: Connected to MongoDB Atlas");
  console.log("🌐 Database: MongoDB Atlas");
  
  // Setup Atlas event listeners
  setupAtlasEventListeners();
}

/**
 * Setup DocumentDB-specific event listeners
 */
function setupDocumentDBEventListeners(): void {
  const connection = mongoose.connection;
  
  connection.on("connected", () => {
    console.log("🔗 DocumentDB: Connection established");
  });
  
  connection.on("error", (error) => {
    console.error("❌ DocumentDB error:", error);
  });
  
  connection.on("disconnected", () => {
    console.warn("⚠️ DocumentDB: Connection lost");
  });
  
  connection.on("reconnected", () => {
    console.log("🔄 DocumentDB: Reconnected successfully");
  });

  connection.on("close", () => {
    console.log("🔒 DocumentDB: Connection closed");
  });
}

/**
 * Setup MongoDB Atlas event listeners
 */
function setupAtlasEventListeners(): void {
  const connection = mongoose.connection;
  
  connection.on("connected", () => {
    console.log("🔗 MongoDB Atlas: Connection established");
  });
  
  connection.on("error", (error) => {
    console.error("❌ MongoDB Atlas error:", error);
  });
  
  connection.on("disconnected", () => {
    console.warn("⚠️ MongoDB Atlas: Connection lost");
  });
  
  connection.on("reconnected", () => {
    console.log("🔄 MongoDB Atlas: Reconnected successfully");
  });
}

/**
 * Get current database connection info for health checks
 */
export function getDatabaseInfo(): { type: string; status: string; host?: string } {
  const connection = mongoose.connection;
  const state = connection.readyState;
  
  let status: string;
  switch (state) {
    case 0: status = "disconnected"; break;
    case 1: status = "connected"; break;
    case 2: status = "connecting"; break;
    case 3: status = "disconnecting"; break;
    default: status = "unknown";
  }

  // Determine database type based on connection string
  const host = connection.host;
  const isDocumentDB = host?.includes("docdb.amazonaws.com");
  const type = isDocumentDB ? "AWS DocumentDB" : "MongoDB Atlas";
  
  return {
    type,
    status,
    host: host || "unknown"
  };
}
