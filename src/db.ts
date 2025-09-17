// src/db.ts
import mongoose from "mongoose";

export async function connectDB() {
  if (mongoose.connection.readyState === 1) return; // already connected
  const uri = process.env.MONGODB_URI;
  if (!uri) throw new Error("MONGODB_URI is not set");

  console.log("Attempting MongoDB connection...");
  
  await mongoose.connect(uri, {
    serverSelectionTimeoutMS: 5000, // wait up to 5s for a node (reduced from 15s)
    socketTimeoutMS: 10000, // reduced timeout
    connectTimeoutMS: 5000, // connection timeout
  });

  const c = mongoose.connection;
  c.on("connected", () => console.log("Mongo connected"));
  c.on("error", (e) => console.error("Mongo error", e));
  c.on("disconnected", () => console.warn("Mongo disconnected"));
  
  console.log("MongoDB connection established");
}
