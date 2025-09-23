import { Server as SocketIOServer, Socket } from 'socket.io';
import jwt from 'jsonwebtoken';
import mongoose from 'mongoose';
import Message from '../models/Message';
import Chat from '../models/Chat';
import Group from '../models/Group';
import User from '../models/User';

interface SocketUser {
  userId: string;
  socketId: string;
  username: string;
  profilePicture?: string;
  lastSeen: Date;
}

interface OnlineUser {
  userId: string;
  socketId: string;
  username: string;
  isOnline: boolean;
  lastSeen: Date;
}

interface TypingUser {
  userId: string;
  username: string;
  conversationId?: string;
  groupId?: string;
}

interface MessageData {
  senderId: string;
  receiverId?: string;
  groupId?: string;
  content: string;
  messageType: 'text' | 'image' | 'file' | 'location';
  localMessageId?: string;
  metadata?: {
    fileName?: string;
    fileSize?: number;
    fileUrl?: string;
    latitude?: number;
    longitude?: number;
  };
}

export class SocketService {
  private io: SocketIOServer;
  private onlineUsers: Map<string, SocketUser> = new Map();
  private typingUsers: Map<string, TypingUser> = new Map();

  constructor(io: SocketIOServer) {
    this.io = io;
    this.setupSocketAuthentication();
    this.setupSocketEvents();
  }

  private setupSocketAuthentication() {
    this.io.use(async (socket: any, next: any) => {
      try {
        const token = socket.handshake.auth.token || socket.handshake.headers.authorization?.replace('Bearer ', '');
        
        if (!token) {
          return next(new Error('Authentication error: No token provided'));
        }

        const decoded = jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key') as any;
        const user = await User.findById(decoded.userId).select('name email profilePicture');

        if (!user) {
          return next(new Error('Authentication error: User not found'));
        }

        socket.userId = user._id.toString();
        socket.user = user;
        next();
      } catch (error) {
        console.error('Socket authentication error:', error);
        next(new Error('Authentication error: Invalid token'));
      }
    });
  }

  private setupSocketEvents() {
    this.io.on('connection', (socket: any) => {
      console.log(`User ${socket.user.name} connected with socket ${socket.id}`);
      
      // Add user to online users
      this.handleUserJoin(socket);

      // Handle private message sending
      socket.on('send_message', (data: MessageData) => {
        this.handlePrivateMessage(socket, data);
      });

      // Handle group message sending
      socket.on('send_group_message', (data: MessageData) => {
        this.handleGroupMessage(socket, data);
      });

      // Handle message read status
      socket.on('message_read', (data: { messageId: string; conversationId?: string; groupId?: string }) => {
        this.handleMessageRead(socket, data);
      });

      // Handle typing indicators
      socket.on('typing_start', (data: { receiverId?: string; groupId?: string }) => {
        this.handleTypingStart(socket, data);
      });

      socket.on('typing_stop', (data: { receiverId?: string; groupId?: string }) => {
        this.handleTypingStop(socket, data);
      });

      // Handle joining conversation/group rooms
      socket.on('join_conversation', (data: { conversationId?: string; groupId?: string }) => {
        this.handleJoinRoom(socket, data);
      });

      socket.on('leave_conversation', (data: { conversationId?: string; groupId?: string }) => {
        this.handleLeaveRoom(socket, data);
      });

      // Handle online status
      socket.on('get_online_users', () => {
        this.handleGetOnlineUsers(socket);
      });

      // Handle user status updates
      socket.on('update_status', (data: { status: 'online' | 'away' | 'busy' }) => {
        this.handleStatusUpdate(socket, data);
      });

      // Handle disconnection
      socket.on('disconnect', () => {
        this.handleUserDisconnect(socket);
      });
    });
  }

  private handleUserJoin(socket: any) {
    const user: SocketUser = {
      userId: socket.userId,
      socketId: socket.id,
      username: socket.user.name,
      profilePicture: socket.user.profilePicture,
      lastSeen: new Date()
    };

    this.onlineUsers.set(socket.userId, user);

    // Notify all connected users about online status
    socket.broadcast.emit('user_online', {
      userId: socket.userId,
      username: socket.user.name,
      profilePicture: socket.user.profilePicture
    });

    // Send current online users to newly connected user
    const onlineUsersList = Array.from(this.onlineUsers.values()).map(u => ({
      userId: u.userId,
      username: u.username,
      profilePicture: u.profilePicture,
      isOnline: true
    }));

    socket.emit('online_users', onlineUsersList);
  }

  private async handlePrivateMessage(socket: any, data: MessageData) {
    try {
      if (!data.receiverId) {
        socket.emit('error', { message: 'Receiver ID is required for private messages' });
        return;
      }

      // Find or create conversation
      const chat = await Chat.findOrCreateConversation(data.senderId, data.receiverId);
      
      // Create message
      const message = new Message({
        sender: data.senderId,
        receiver: data.receiverId,
        content: data.content,
        messageType: data.messageType,
        conversationId: chat._id?.toString(),
        localMessageId: data.localMessageId,
        metadata: data.metadata,
        isDelivered: false,
        isPendingDelivery: true
      });

      await message.save();
      await Chat.updateLastMessage(chat._id?.toString() || '', message._id?.toString() || '');

      // Populate sender info
      await message.populate('sender', 'name profilePicture email');

      // Send to receiver if online
      const receiverSocket = this.onlineUsers.get(data.receiverId);
      if (receiverSocket) {
        this.io.to(receiverSocket.socketId).emit('new_message', {
          ...message.toObject(),
          conversationId: chat._id?.toString()
        });

        // Mark as delivered
        message.isDelivered = true;
        message.deliveredAt = new Date();
        message.isPendingDelivery = false;
        await message.save();
      }

      // Send confirmation to sender
      socket.emit('message_sent', {
        ...message.toObject(),
        conversationId: chat._id?.toString(),
        localMessageId: data.localMessageId
      });

      // Increment unread count for receiver
      if (chat.incrementUnreadCount) {
        await chat.incrementUnreadCount(data.receiverId);
      }

    } catch (error) {
      console.error('Error handling private message:', error);
      socket.emit('error', { message: 'Failed to send message' });
    }
  }

  private async handleGroupMessage(socket: any, data: MessageData) {
    try {
      if (!data.groupId) {
        socket.emit('error', { message: 'Group ID is required for group messages' });
        return;
      }

      // Verify user is member of group
      const group = await Group.findById(data.groupId);
      if (!group || !group.members.includes(new mongoose.Types.ObjectId(data.senderId))) {
        socket.emit('error', { message: 'You are not a member of this group' });
        return;
      }

      // Create message
      const message = new Message({
        sender: data.senderId,
        groupId: data.groupId,
        content: data.content,
        messageType: data.messageType,
        localMessageId: data.localMessageId,
        metadata: data.metadata
      });

      await message.save();
      if (group.updateLastMessage) {
        await group.updateLastMessage(message._id?.toString() || '');
      }

      // Populate sender info
      await message.populate('sender', 'name profilePicture email');

      // Send to all group members who are online
      const messageData = {
        ...message.toObject(),
        groupId: data.groupId
      };

      for (const memberId of group.members) {
        const memberIdStr = memberId.toString();
        if (memberIdStr !== data.senderId) { // Don't send to sender
          const memberSocket = this.onlineUsers.get(memberIdStr);
          if (memberSocket) {
            this.io.to(memberSocket.socketId).emit('new_group_message', messageData);
          }
        }
      }

      // Send confirmation to sender
      socket.emit('group_message_sent', {
        ...messageData,
        localMessageId: data.localMessageId
      });

    } catch (error) {
      console.error('Error handling group message:', error);
      socket.emit('error', { message: 'Failed to send group message' });
    }
  }

  private async handleMessageRead(socket: any, data: { messageId: string; conversationId?: string; groupId?: string }) {
    try {
      const message = await Message.findById(data.messageId);
      if (!message) {
        return;
      }

      if (message.markAsRead) {
        await message.markAsRead(socket.userId);
      }

      // Notify sender about read status
      const senderSocket = this.onlineUsers.get(message.sender.toString());
      if (senderSocket && senderSocket.socketId !== socket.id) {
        this.io.to(senderSocket.socketId).emit('message_read', {
          messageId: data.messageId,
          readBy: socket.userId,
          conversationId: data.conversationId,
          groupId: data.groupId
        });
      }

      // Update chat unread count
      if (data.conversationId) {
        await Chat.markAsRead(data.conversationId, socket.userId);
      }

    } catch (error) {
      console.error('Error handling message read:', error);
    }
  }

  private handleTypingStart(socket: any, data: { receiverId?: string; groupId?: string }) {
    const typingUser: TypingUser = {
      userId: socket.userId,
      username: socket.user.name,
      conversationId: data.receiverId,
      groupId: data.groupId
    };

    const typingKey = data.groupId ? `group_${data.groupId}` : `user_${data.receiverId}`;
    this.typingUsers.set(`${socket.userId}_${typingKey}`, typingUser);

    if (data.receiverId) {
      // Private conversation typing
      const receiverSocket = this.onlineUsers.get(data.receiverId);
      if (receiverSocket) {
        this.io.to(receiverSocket.socketId).emit('user_typing', {
          userId: socket.userId,
          username: socket.user.name,
          isTyping: true
        });
      }
    } else if (data.groupId) {
      // Group typing - notify all group members
      socket.to(`group_${data.groupId}`).emit('user_typing_group', {
        userId: socket.userId,
        username: socket.user.name,
        groupId: data.groupId,
        isTyping: true
      });
    }
  }

  private handleTypingStop(socket: any, data: { receiverId?: string; groupId?: string }) {
    const typingKey = data.groupId ? `group_${data.groupId}` : `user_${data.receiverId}`;
    this.typingUsers.delete(`${socket.userId}_${typingKey}`);

    if (data.receiverId) {
      // Private conversation typing
      const receiverSocket = this.onlineUsers.get(data.receiverId);
      if (receiverSocket) {
        this.io.to(receiverSocket.socketId).emit('user_typing', {
          userId: socket.userId,
          username: socket.user.name,
          isTyping: false
        });
      }
    } else if (data.groupId) {
      // Group typing
      socket.to(`group_${data.groupId}`).emit('user_typing_group', {
        userId: socket.userId,
        username: socket.user.name,
        groupId: data.groupId,
        isTyping: false
      });
    }
  }

  private handleJoinRoom(socket: any, data: { conversationId?: string; groupId?: string }) {
    if (data.conversationId) {
      socket.join(`conversation_${data.conversationId}`);
    } else if (data.groupId) {
      socket.join(`group_${data.groupId}`);
    }
  }

  private handleLeaveRoom(socket: any, data: { conversationId?: string; groupId?: string }) {
    if (data.conversationId) {
      socket.leave(`conversation_${data.conversationId}`);
    } else if (data.groupId) {
      socket.leave(`group_${data.groupId}`);
    }
  }

  private handleGetOnlineUsers(socket: any) {
    const onlineUsersList = Array.from(this.onlineUsers.values()).map(u => ({
      userId: u.userId,
      username: u.username,
      profilePicture: u.profilePicture,
      isOnline: true,
      lastSeen: u.lastSeen
    }));

    socket.emit('online_users', onlineUsersList);
  }

  private handleStatusUpdate(socket: any, data: { status: 'online' | 'away' | 'busy' }) {
    const user = this.onlineUsers.get(socket.userId);
    if (user) {
      socket.broadcast.emit('user_status_changed', {
        userId: socket.userId,
        status: data.status
      });
    }
  }

  private handleUserDisconnect(socket: any) {
    console.log(`User ${socket.user?.name} disconnected`);
    
    // Remove from online users
    this.onlineUsers.delete(socket.userId);

    // Remove from typing users
    for (const [key, typingUser] of this.typingUsers.entries()) {
      if (typingUser.userId === socket.userId) {
        this.typingUsers.delete(key);
      }
    }

    // Notify other users about offline status
    socket.broadcast.emit('user_offline', {
      userId: socket.userId,
      lastSeen: new Date()
    });
  }

  // Public method to send push notification
  public async sendPushNotification(userId: string, message: any, type: 'message' | 'group_message') {
    // Implementation for push notifications
    // This would integrate with your existing push notification service
    console.log(`Sending push notification to ${userId}:`, { message, type });
  }

  // Public method to get online users count
  public getOnlineUsersCount(): number {
    return this.onlineUsers.size;
  }

  // Public method to check if user is online
  public isUserOnline(userId: string): boolean {
    return this.onlineUsers.has(userId);
  }
}