// index.ts
import express from "express";
import cors from "cors";
import mongoose from "mongoose";
import { connectDB } from "./db";
import authRouter from "./routes/auth";
import cardsRouter from "./routes/cards";

const app = express();
app.use(cors());
app.use(express.json({ limit: "10mb" })); // instead of default

// single health route
app.get("/api/health", (_req, res) => {
  const mongoUp = mongoose.connection?.readyState === 1; // 1=connected
  res.status(200).json({ ok: true, mongo: mongoUp ? "up" : "down", ts: Date.now() });
});

(async () => {
  try {
    await connectDB();                           // wait for Mongo before routes

    app.use("/api/auth", authRouter);            // << IMPORTANT
    app.use("/api/cards", cardsRouter);          // if you have cards router

    const port = process.env.PORT || 8080;
    app.listen(port, () => console.log(`API listening on :${port}`));
  } catch (err) {
    console.error("FATAL: DB connect failed", err);
    process.exit(1);
  }
})();
