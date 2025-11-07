import { NextFunction, Request, Response } from 'express';
import jwt from 'jsonwebtoken';
import mongoose from 'mongoose';

export interface AuthReq extends Request {
  userId?: string;
}

export function requireAuth(req: AuthReq, res: Response, next: NextFunction) {
  const h = req.header('authorization') || '';
  const token = h.startsWith('Bearer ') ? h.slice(7) : undefined;
  if (!token) return res.status(401).json({ message: 'Missing token' });

  try {
    const payload = jwt.verify(token, process.env.JWT_SECRET as string) as { sub: string };
    
    // Validate that the userId is a proper ObjectId format
    if (!mongoose.Types.ObjectId.isValid(payload.sub)) {
      console.log(`❌ Invalid ObjectId format in token: ${payload.sub}`);
      return res.status(401).json({ message: 'Invalid user ID format in token' });
    }
    
    req.userId = payload.sub;
    next();
  } catch (error) {
    console.log(`❌ Token verification failed:`, error);
    res.status(401).json({ message: 'Invalid token' });
  }
}
