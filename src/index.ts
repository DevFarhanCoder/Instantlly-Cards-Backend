// index.ts
import express from "express";
import cors from "cors";
import { connectDB } from "./db";
import authRouter from "./routes/auth";
import cardsRouter from "./routes/cards"; // keep only if you have it
import mongoose from "mongoose";

const app = express();
app.set("trust proxy", 1);
app.use(cors());
app.use(express.json());

// Health (1 route only)
app.get("/api/health", (_req, res) => {
  const dbReady = mongoose.connection?.readyState === 1; // 1 = connected
  res.status(200).json({
    ok: true,
    uptime: process.uptime(),
    mongo: dbReady ? "up" : "down",
    ts: Date.now(),
  });
});

(async () => {
  try {
    await connectDB();

    app.use("/api", authRouter);
    if (cardsRouter) app.use("/api", cardsRouter);

    const port = process.env.PORT || 8080;
    app.listen(port, () => console.log(`API listening on :${port}`));
  } catch (err) {
    console.error("FATAL: DB connect failed", err);
    process.exit(1);
  }
})();
