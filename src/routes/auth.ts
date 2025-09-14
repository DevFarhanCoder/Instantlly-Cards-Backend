import { Router } from 'express';
import bcrypt from 'bcryptjs';
import jwt from 'jsonwebtoken';
import User from '../models/User';

const r = Router();

r.post('/signup', async (req, res) => {
  const { email, password, name } = req.body || {};
  if (!email || !password) return res.status(400).json({ message: 'email & password required' });

  const exists = await User.findOne({ email });
  if (exists) return res.status(409).json({ message: 'email in use' });

  const passwordHash = await bcrypt.hash(password, 10);
  const user = await User.create({ email, passwordHash, name });
  const token = jwt.sign({ sub: user.id }, process.env.JWT_SECRET as string, { expiresIn: '30d' });

  res.json({ token, user: { id: user.id, email: user.email, name: user.name } });
});

r.post('/login', async (req, res) => {
  const { email, password } = req.body || {};
  const user = await User.findOne({ email });
  if (!user) return res.status(401).json({ message: 'invalid credentials' });

  const ok = await bcrypt.compare(password, user.passwordHash);
  if (!ok) return res.status(401).json({ message: 'invalid credentials' });

  const token = jwt.sign({ sub: user.id }, process.env.JWT_SECRET as string, { expiresIn: '30d' });
  res.json({ token, user: { id: user.id, email: user.email, name: user.name } });
});

export default r;
