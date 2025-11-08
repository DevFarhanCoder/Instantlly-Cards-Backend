import { NextFunction, Request, Response } from 'express';
import jwt from 'jsonwebtoken';

export interface AdminAuthReq extends Request {
  adminId?: string;
  adminUsername?: string;
  adminRole?: string;
}

const JWT_SECRET = process.env.JWT_SECRET || 'your-secret-key-change-in-production';

export function requireAdminAuth(req: AdminAuthReq, res: Response, next: NextFunction) {
  const authHeader = req.header('authorization') || '';
  const token = authHeader.startsWith('Bearer ') ? authHeader.slice(7) : undefined;
  
  if (!token) {
    return res.status(401).json({ 
      success: false,
      message: 'Missing authentication token' 
    });
  }

  try {
    const payload = jwt.verify(token, JWT_SECRET) as { 
      id: string;
      username: string;
      role: string;
    };
    
    // Set admin info on request
    req.adminId = payload.id;
    req.adminUsername = payload.username;
    req.adminRole = payload.role;
    
    next();
  } catch (error) {
    console.log(`❌ Admin token verification failed:`, error);
    res.status(401).json({ 
      success: false,
      message: 'Invalid or expired token' 
    });
  }
}
