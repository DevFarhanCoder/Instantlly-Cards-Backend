// src/db.ts
import mongoose from "mongoose";

export async function connectDB() {
  if (mongoose.connection.readyState === 1) return; // already connected
  const uri = process.env.MONGODB_URI;
  if (!uri) throw new Error("MONGODB_URI is not set");

  await mongoose.connect(uri, {
    serverSelectionTimeoutMS: 15000, // wait up to 15s for a node
    socketTimeoutMS: 45000,
    // keepAlive: true, // optional
  });

  const c = mongoose.connection;
  c.on("connected", () => console.log("Mongo connected"));
  c.on("error", (e) => console.error("Mongo error", e));
  c.on("disconnected", () => console.warn("Mongo disconnected"));
}
