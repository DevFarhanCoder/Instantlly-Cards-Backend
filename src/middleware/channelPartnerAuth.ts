import { Request, Response, NextFunction } from 'express';
import jwt from 'jsonwebtoken';
import mongoose from 'mongoose';

// JWT Secret for channel partner tokens (should match channel partner backend)
const JWT_SECRET = process.env.CHANNEL_PARTNER_JWT_SECRET || 'inst@ntly_c@rds_pr0ducti0n_s3cr3t_k3y_2024_v1.0';

interface ChannelPartnerPayload {
  userId: string;
  phone: string;
}

// Extend Request to include channel partner info
declare global {
  namespace Express {
    interface Request {
      channelPartnerId?: string;
      channelPartnerPhone?: string;
    }
  }
}

/**
 * Middleware to authenticate channel partner JWT tokens
 * Token format: { userId, phone }
 */
export const authenticateChannelPartner = async (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  try {
    const authHeader = req.headers.authorization;

    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      console.log('❌ No authorization header or invalid format');
      return res.status(401).json({ message: 'No token provided' });
    }

    const token = authHeader.substring(7); // Remove 'Bearer ' prefix

    if (!token) {
      console.log('❌ Empty token');
      return res.status(401).json({ message: 'No token provided' });
    }

    // Verify token
    const payload = jwt.verify(token, JWT_SECRET) as ChannelPartnerPayload;

    if (!payload.userId || !payload.phone) {
      console.log('❌ Invalid token payload:', payload);
      return res.status(401).json({ message: 'Invalid token format' });
    }

    // Validate ObjectId format
    if (!mongoose.Types.ObjectId.isValid(payload.userId)) {
      console.log(`❌ Invalid userId format in channel partner token: ${payload.userId}`);
      return res.status(401).json({ message: 'Invalid user ID format in token' });
    }

    // Attach channel partner info to request
    req.channelPartnerId = payload.userId;
    req.channelPartnerPhone = payload.phone;

    console.log(`✅ Channel partner authenticated: ${payload.phone} (ID: ${payload.userId})`);

    next();
  } catch (error) {
    if (error instanceof jwt.JsonWebTokenError) {
      console.log('❌ Invalid token:', error.message);
      return res.status(401).json({ message: 'Invalid token' });
    }
    if (error instanceof jwt.TokenExpiredError) {
      console.log('❌ Token expired');
      return res.status(401).json({ message: 'Token expired' });
    }

    console.error('❌ Channel partner auth error:', error);
    return res.status(500).json({ message: 'Authentication error' });
  }
};
