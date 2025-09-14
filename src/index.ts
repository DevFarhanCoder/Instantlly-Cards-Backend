import express from "express";
import cors from "cors";
import authRouter from "./routes/auth";

const app = express();

app.use(cors());
app.use(express.json()); // <-- IMPORTANT

// health (for wakeups & checks)
app.get("/api/health", (_req, res) => res.json({ ok: true, ts: Date.now() }));

// mount routers under /api
app.use("/api", authRouter);

const port = process.env.PORT || 8080;
app.listen(port, () => console.log(`API listening on :${port}`));
