import { NextFunction, Request, Response } from 'express';
import jwt from 'jsonwebtoken';

export interface AuthReq extends Request {
  userId?: string;
}

export function requireAuth(req: AuthReq, res: Response, next: NextFunction) {
  const timestamp = new Date().toISOString();
  const path = req.path;
  const method = req.method;
  
  console.log(`\n🔐 [AUTH CHECK] ${timestamp}`);
  console.log(`📍 ${method} ${path}`);
  console.log(`🌐 IP: ${req.ip || 'unknown'}`);
  console.log(`📱 User Agent: ${req.headers['user-agent']?.substring(0, 50) || 'unknown'}`);
  
  const h = req.header('authorization') || '';
  console.log(`🔑 Authorization header: ${h ? 'Bearer ***' + h.slice(-10) : 'MISSING'}`);
  
  const token = h.startsWith('Bearer ') ? h.slice(7) : undefined;
  if (!token) {
    console.log(`❌ [AUTH FAILED] No token provided`);
    console.log(`🔐 [AUTH CHECK END] Rejecting request\n`);
    return res.status(401).json({ message: 'Missing token' });
  }

  try {
    console.log(`🔍 Verifying JWT token...`);
    const payload = jwt.verify(token, process.env.JWT_SECRET as string) as { sub: string };
    req.userId = payload.sub;
    console.log(`✅ [AUTH SUCCESS] User ID: ${payload.sub}`);
    console.log(`🔐 [AUTH CHECK END] Proceeding to endpoint\n`);
    next();
  } catch (error) {
    console.log(`❌ [AUTH FAILED] Invalid token: ${error instanceof Error ? error.message : 'Unknown error'}`);
    console.log(`🔐 [AUTH CHECK END] Rejecting request\n`);
    res.status(401).json({ message: 'Invalid token' });
  }
}
