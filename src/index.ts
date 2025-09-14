import 'dotenv/config';
import express from 'express';
import cors from 'cors';
import mongoose from 'mongoose';
import authRouter from './routes/auth';
import cardsRouter from './routes/cards';
import { requireAuth } from './middleware/auth';

const app = express();

// CORS: allow mobile + web (adjust origins as you wish)
app.use(cors({ origin: true, credentials: false }));
app.use(express.json({ limit: '1mb' }));

app.get('/health', (_req, res) => res.json({ ok: true }));

app.use('/api/auth', authRouter);
app.use('/api/cards', requireAuth, cardsRouter); // protected

// 404 handler
app.use((_req, res) => res.status(404).json({ message: 'Not Found' }));

const PORT = process.env.PORT || 8080;
const MONGODB_URI = process.env.MONGODB_URI as string;

async function start() {
  try {
    await mongoose.connect(MONGODB_URI);
    app.listen(PORT, () => {
      console.log(`API listening on :${PORT}`);
    });
  } catch (e) {
    console.error('Failed to start server', e);
    process.exit(1);
  }
}

start();
