import { NextFunction, Request, Response } from 'express';
import jwt from 'jsonwebtoken';

export interface AuthReq extends Request {
  userId?: string;
}

export function requireAuth(req: AuthReq, res: Response, next: NextFunction) {
  const h = req.header('authorization') || '';
  const token = h.startsWith('Bearer ') ? h.slice(7) : undefined;
  if (!token) return res.status(401).json({ message: 'Missing token' });

  try {
    const payload = jwt.verify(token, process.env.JWT_SECRET as string) as { sub: string };
    req.userId = payload.sub;
    next();
  } catch {
    res.status(401).json({ message: 'Invalid token' });
  }
}
