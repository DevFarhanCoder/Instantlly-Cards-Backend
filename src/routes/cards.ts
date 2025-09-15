import { Router } from 'express';
import Card from '../models/Card';
import { AuthReq, requireAuth } from '../middleware/auth';

const r = Router();

// ✅ protect all card routes
r.use(requireAuth);

// CREATE
r.post('/', async (req: AuthReq, res) => {
  try {
    const doc = await Card.create({ ...req.body, userId: req.userId! });
    res.status(201).json({ data: doc });
  } catch (err: any) {
    if (err?.name === 'ValidationError') {
      return res.status(400).json({ message: err.message });
    }
    console.error('CREATE CARD ERROR', err);
    res.status(500).json({ message: 'Server error' });
  }
});

// LIST (own cards) with simple pagination
r.get('/', async (req: AuthReq, res) => {
  try {
    const page = Math.max(1, parseInt((req.query.page as string) || '1', 10));
    const limit = Math.min(50, Math.max(1, parseInt((req.query.limit as string) || '20', 10)));
    const q = { userId: req.userId! };
    const [items, total] = await Promise.all([
      Card.find(q).sort({ createdAt: -1 }).skip((page - 1) * limit).limit(limit).lean(),
      Card.countDocuments(q),
    ]);
    res.json({ data: items, page, limit, total });
  } catch (err) {
    console.error('LIST CARDS ERROR', err);
    res.status(500).json({ message: 'Server error' });
  }
});

// UPDATE
r.put('/:id', async (req: AuthReq, res) => {
  try {
    const doc = await Card.findOneAndUpdate(
      { _id: req.params.id, userId: req.userId! },
      req.body,
      { new: true }
    );
    if (!doc) return res.status(404).json({ message: 'Not found' });
    res.json({ data: doc });
  } catch (err) {
    console.error('UPDATE CARD ERROR', err);
    res.status(500).json({ message: 'Server error' });
  }
});

// DELETE
r.delete('/:id', async (req: AuthReq, res) => {
  try {
    const doc = await Card.findOneAndDelete({ _id: req.params.id, userId: req.userId! });
    if (!doc) return res.status(404).json({ message: 'Not found' });
    res.json({ ok: true });
  } catch (err) {
    console.error('DELETE CARD ERROR', err);
    res.status(500).json({ message: 'Server error' });
  }
});

export default r;
