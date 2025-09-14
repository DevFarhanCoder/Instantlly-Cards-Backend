import express from "express";
import cors from "cors";
import { connectDB } from "./db";
import authRouter from "./routes/auth";
import cardsRouter from "./routes/cards"; // if you have one

const app = express();
app.use(cors());
app.use(express.json());

// health
app.get("/api/health", (_req, res) => {
  const ready = (require("mongoose") as any).connection?.readyState === 1;
  res.json({ ok: true, mongo: ready ? "up" : "down", ts: Date.now() });
});

(async () => {
  try {
    await connectDB();                 // ⬅️ wait for Mongo first
    app.use("/api", authRouter);
    app.use("/api", cardsRouter);      // mount others AFTER connect

    const port = process.env.PORT || 8080;
    app.listen(port, () => console.log(`API listening on :${port}`));
  } catch (err) {
    console.error("FATAL: DB connect failed", err);
    process.exit(1);
  }
})();
