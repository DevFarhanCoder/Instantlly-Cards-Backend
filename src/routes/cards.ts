import { Router } from 'express';
import Card from '../models/Card';
import { AuthReq } from '../middleware/auth';

const r = Router();

// CREATE
r.post('/', async (req: AuthReq, res) => {
  const doc = await Card.create({ ...req.body, userId: req!.userId! });
  res.status(201).json({ data: doc });
});

// LIST (own cards) with simple pagination
r.get('/', async (req: AuthReq, res) => {
  const page = Math.max(1, parseInt((req.query.page as string) || '1', 10));
  const limit = Math.min(50, Math.max(1, parseInt((req.query.limit as string) || '20', 10)));
  const skip = (page - 1) * limit;
  const [data, total] = await Promise.all([
    Card.find({ userId: req!.userId! }).sort({ createdAt: -1 }).skip(skip).limit(limit),
    Card.countDocuments({ userId: req!.userId! })
  ]);
  res.json({ data, page, total });
});

// READ
r.get('/:id', async (req: AuthReq, res) => {
  const doc = await Card.findOne({ _id: req.params.id, userId: req!.userId! });
  if (!doc) return res.status(404).json({ message: 'Not found' });
  res.json({ data: doc });
});

// UPDATE
r.put('/:id', async (req: AuthReq, res) => {
  const doc = await Card.findOneAndUpdate({ _id: req.params.id, userId: req!.userId! }, req.body, { new: true });
  if (!doc) return res.status(404).json({ message: 'Not found' });
  res.json({ data: doc });
});

// DELETE
r.delete('/:id', async (req: AuthReq, res) => {
  const doc = await Card.findOneAndDelete({ _id: req.params.id, userId: req!.userId! });
  if (!doc) return res.status(404).json({ message: 'Not found' });
  res.json({ ok: true });
});

export default r;
