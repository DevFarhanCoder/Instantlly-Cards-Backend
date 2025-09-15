import { Router } from "express";
import Card from "../models/Card";
import { AuthReq, requireAuth } from "../middleware/auth";

const r = Router();

// Public feed (no auth) – latest 50 cards
r.get("/feed/public", async (_req, res) => {
  const items = await Card.find({}).sort({ createdAt: -1 }).limit(50).lean();
  res.json({ data: items });
});

// Require auth for everything below
r.use(requireAuth);

// CREATE (expects flat body)
r.post("/", async (req: AuthReq, res) => {
  try {
    const doc = await Card.create({ ...req.body, userId: req.userId! });
    res.status(201).json({ data: doc });
  } catch (err: any) {
    console.error("CREATE CARD ERROR", err);
    res.status(400).json({ message: err?.message || "Bad request" });
  }
});

// LIST own cards
r.get("/", async (req: AuthReq, res) => {
  const items = await Card.find({ userId: req.userId! }).sort({ createdAt: -1 }).lean();
  res.json({ data: items });
});

// UPDATE
r.put("/:id", async (req: AuthReq, res) => {
  try {
    const doc = await Card.findOneAndUpdate(
      { _id: req.params.id, userId: req.userId! },
      req.body,
      { new: true }
    );
    if (!doc) return res.status(404).json({ message: "Not found" });
    res.json({ data: doc });
  } catch (err) {
    console.error("UPDATE CARD ERROR", err);
    res.status(400).json({ message: "Bad request" });
  }
});

// DELETE
r.delete("/:id", async (req: AuthReq, res) => {
  try {
    const doc = await Card.findOneAndDelete({ _id: req.params.id, userId: req.userId! });
    if (!doc) return res.status(404).json({ message: "Not found" });
    res.json({ ok: true });
  } catch (err) {
    console.error("DELETE CARD ERROR", err);
    res.status(400).json({ message: "Bad request" });
  }
});

export default r;
