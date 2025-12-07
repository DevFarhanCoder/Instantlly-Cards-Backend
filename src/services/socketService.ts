import { Server as SocketIOServer, Socket } from 'socket.io';
import jwt from 'jsonwebtoken';
import mongoose from 'mongoose';
import Message from '../models/Message';
import Chat from '../models/Chat';
import Group from '../models/Group';
import GroupCall from '../models/GroupCall';
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
  messageType: 'text' | 'image' | 'file' | 'location' | 'system';
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
        console.log('🔐 Socket authentication attempt:', {
          socketId: socket.id,
          authToken: socket.handshake.auth.token ? 'Present' : 'Missing',
          authHeader: socket.handshake.headers.authorization ? 'Present' : 'Missing'
        });

        const token = socket.handshake.auth.token || socket.handshake.headers.authorization?.replace('Bearer ', '');

        if (!token) {
          console.error('❌ Socket auth failed: No token provided');
          return next(new Error('Authentication error: No token provided'));
        }

        console.log('🔑 Verifying JWT token for socket connection...');
        const jwtSecret = process.env.JWT_SECRET || 'your-secret-key';
        console.log('🔑 JWT Secret available:', !!jwtSecret, 'Length:', jwtSecret.length);

        const decoded = jwt.verify(token, jwtSecret) as any;
        console.log('✅ JWT token decoded successfully:', { sub: decoded.sub, userId: decoded.userId });

        // Handle both 'sub' and 'userId' field formats
        const userId = decoded.sub || decoded.userId;
        if (!userId) {
          console.error('❌ Socket auth failed: No user ID found in token');
          return next(new Error('Authentication error: Invalid token format'));
        }

        const user = await User.findById(userId).select('name email profilePicture');

        if (!user) {
          console.error('❌ Socket auth failed: User not found for ID:', userId);
          return next(new Error('Authentication error: User not found'));
        }

        console.log('✅ Socket authentication successful for user:', user.name);
        socket.userId = user._id.toString();
        socket.user = user;
        next();
      } catch (error) {
        console.error('❌ Socket authentication error:', error);
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

      // Handle group calling events
      socket.on('initiate_group_call', (data: { groupId: string; callType: 'audio' | 'video' }) => {
        this.handleInitiateGroupCall(socket, data);
      });

      socket.on('join_group_call', (data: { groupId: string; callId: string }) => {
        this.handleJoinGroupCall(socket, data);
      });

      socket.on('leave_group_call', (data: { groupId: string; callId: string }) => {
        this.handleLeaveGroupCall(socket, data);
      });

      socket.on('end_group_call', (data: { groupId: string; callId: string }) => {
        this.handleEndGroupCall(socket, data);
      });

      socket.on('group_call_signal', (data: { groupId: string; callId: string; signal: any; targetUserId?: string }) => {
        this.handleGroupCallSignal(socket, data);
      });

      // Handle disconnection
      socket.on('disconnect', () => {
        this.handleUserDisconnect(socket);
      });
    });
  }

  private async handleUserJoin(socket: any) {
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

    // Deliver pending messages that were sent while user was offline
    try {
      const pendingMessages = await Message.find({
        receiver: socket.userId,
        isDelivered: false,
        isPendingDelivery: true
      }).populate('sender', 'name profilePicture email').sort({ createdAt: 1 });

      console.log(`📬 Found ${pendingMessages.length} pending messages for user ${socket.user.name}`);

      for (const message of pendingMessages) {
        // Send message to user
        socket.emit('new_message', {
          ...message.toObject(),
          conversationId: message.conversationId
        });

        // Mark as delivered
        message.isDelivered = true;
        message.deliveredAt = new Date();
        message.isPendingDelivery = false;
        await message.save();

        // Notify sender about delivery
        const senderId = message.sender && typeof message.sender === 'object' ? message.sender._id : message.sender;
        const senderSocket = this.onlineUsers.get(senderId?.toString() || '');
        if (senderSocket) {
          this.io.to(senderSocket.socketId).emit('message_delivered', {
            messageId: message._id?.toString() || '',
            localMessageId: message.localMessageId,
            status: 'delivered',
            deliveredAt: message.deliveredAt
          });
        }

        console.log(`✅ Delivered pending message ${message._id} to ${socket.user.name}`);
      }
    } catch (error) {
      console.error('Error delivering pending messages:', error);
    }
  }

  private async handlePrivateMessage(socket: any, data: MessageData) {
    try {
      if (!data.receiverId) {
        socket.emit('error', { message: 'Receiver ID is required for private messages' });
        return;
      }

      // Find or create conversation
      const senderId = socket.userId;
      const receiverId = data.receiverId;
      const chat = await Chat.findOrCreateConversation(senderId, data.receiverId);

      // Create message
      const message = new Message({
        sender: senderId,
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

        // Emit delivered status to sender
        this.io.to(socket.id).emit('message_delivered', {
          messageId: message._id?.toString(),
          localMessageId: data.localMessageId,
          status: 'delivered',
          deliveredAt: message.deliveredAt
        });
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
      if (!group) {
        socket.emit('error', { message: 'Group not found' });
        return;
      }
      const senderId = socket.userId;
      const isMember = Array.isArray(group.members) && group.members.some(
        (m: any) => m.toString() === senderId
      );
      if (!isMember) {
        socket.emit('error', { message: 'You are not a member of this group' });
        return;
      }

      // Create message
      const message = new Message({
        sender: senderId,
        groupId: data.groupId,
        content: data.content,
        messageType: data.messageType,
        localMessageId: data.localMessageId,
        metadata: data.metadata
      });

      await message.save();

      if ((group as any).updateLastMessage) {
        await (group as any).updateLastMessage(message._id?.toString() || '');
      }
      // Agar static hai to aise:
      // if ((Group as any).updateLastMessage) {
      //   await (Group as any).updateLastMessage(group._id.toString(), message._id.toString());
      // }

      // Populate sender info
      await message.populate('sender', 'name profilePicture email');

      // Send to all group members who are online
      const messageData = {
        ...message.toObject(),
        groupId: data.groupId
      };

      for (const memberId of group.members) {
        const memberIdStr = memberId.toString();
        if (memberIdStr !== senderId) { // Don't send to sender
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

      // Update group unread counts for offline members
      const offlineMembers = group.members.filter(memberId =>
        memberId.toString() !== senderId && !this.onlineUsers.has(memberId.toString())
      );

      if (offlineMembers.length > 0) {
        // Increment unread count for offline members
        try {
          await Promise.all(offlineMembers.map(async (memberId) => {
            // Here you would update unread count in database for the user
            // This depends on how you're tracking unread counts
            console.log(`Member ${memberId} is offline, should increment unread count`);
          }));
        } catch (error) {
          console.error('Error updating unread counts for offline members:', error);
        }
      }

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
  public async sendPushNotification(userId: string, message: any, type: 'message' | 'new_group_message') {
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

  // Group Calling Methods
  private async handleInitiateGroupCall(socket: any, data: { groupId: string; callType: 'audio' | 'video' }) {
    try {
      console.log(`📞 ${socket.user.name} initiating ${data.callType} call in group ${data.groupId}`);

      // Verify user is member of group
      const group = await Group.findById(data.groupId);
      if (!group || !group.members.includes(new mongoose.Types.ObjectId(socket.userId))) {
        socket.emit('error', { message: 'You are not a member of this group' });
        return;
      }

      // Generate call session
      const callId = new mongoose.Types.ObjectId().toString();
      const callSession = {
        callId: callId,
        groupId: data.groupId,
        initiatorId: socket.userId,
        initiatorName: socket.user.name,
        initiatorAvatar: socket.user.profilePicture,
        callType: data.callType,
        startTime: new Date(),
        participants: [
          {
            userId: socket.userId,
            name: socket.user.name,
            profilePicture: socket.user.profilePicture,
            joinedAt: new Date(),
            isInitiator: true
          }
        ],
        status: 'ringing'
      };

      // Join call room
      socket.join(`call_${callId}`);

      // Notify all group members about incoming call
      for (const memberId of group.members) {
        const memberIdStr = memberId.toString();
        if (memberIdStr !== socket.userId) {
          const memberSocket = this.onlineUsers.get(memberIdStr);
          if (memberSocket) {
            this.io.to(memberSocket.socketId).emit('incoming_group_call', {
              callId: callId,
              groupId: data.groupId,
              groupName: group.name,
              initiatorId: socket.userId,
              initiatorName: socket.user.name,
              initiatorAvatar: socket.user.profilePicture,
              callType: data.callType,
              startTime: callSession.startTime
            });
          }
        }
      }

      // Send confirmation to initiator
      socket.emit('group_call_initiated', callSession);

    } catch (error) {
      console.error('Error initiating group call:', error);
      socket.emit('error', { message: 'Failed to initiate group call' });
    }
  }

  private async handleJoinGroupCall(socket: any, data: { groupId: string; callId: string }) {
    try {
      console.log(`📞 ${socket.user.name} joining group call ${data.callId}`);

      // Join call room
      socket.join(`call_${data.callId}`);

      const participant = {
        userId: socket.userId,
        name: socket.user.name,
        profilePicture: socket.user.profilePicture,
        joinedAt: new Date(),
        isInitiator: false
      };

      // Notify all participants about new joiner
      socket.to(`call_${data.callId}`).emit('participant_joined', {
        callId: data.callId,
        participant: participant
      });

      // Send confirmation to joiner
      socket.emit('joined_group_call', {
        callId: data.callId,
        groupId: data.groupId,
        participant: participant
      });

    } catch (error) {
      console.error('Error joining group call:', error);
      socket.emit('error', { message: 'Failed to join group call' });
    }
  }

  private async handleLeaveGroupCall(socket: any, data: { groupId: string; callId: string }) {
    try {
      console.log(`📞 ${socket.user.name} leaving group call ${data.callId}`);

      // Leave call room
      socket.leave(`call_${data.callId}`);

      // Notify remaining participants
      socket.to(`call_${data.callId}`).emit('participant_left', {
        callId: data.callId,
        userId: socket.userId,
        userName: socket.user.name,
        leftAt: new Date()
      });

      // Send confirmation to leaver
      socket.emit('left_group_call', {
        callId: data.callId,
        leftAt: new Date()
      });

    } catch (error) {
      console.error('Error leaving group call:', error);
      socket.emit('error', { message: 'Failed to leave group call' });
    }
  }

  private async handleEndGroupCall(socket: any, data: { groupId: string; callId: string }) {
    try {
      console.log(`📞 ${socket.user.name} ending group call ${data.callId}`);

      // Verify user has permission to end call (admin or initiator check would go here)

      // Notify all participants that call is ending
      this.io.to(`call_${data.callId}`).emit('group_call_ended', {
        callId: data.callId,
        endedBy: socket.userId,
        endedByName: socket.user.name,
        endedAt: new Date()
      });

      // Remove all participants from call room
      const room = this.io.sockets.adapter.rooms.get(`call_${data.callId}`);
      if (room) {
        for (const socketId of room) {
          this.io.sockets.sockets.get(socketId)?.leave(`call_${data.callId}`);
        }
      }

    } catch (error) {
      console.error('Error ending group call:', error);
      socket.emit('error', { message: 'Failed to end group call' });
    }
  }

  private handleGroupCallSignal(socket: any, data: { groupId: string; callId: string; signal: any; targetUserId?: string }) {
    try {
      // Handle WebRTC signaling between participants
      if (data.targetUserId) {
        // Direct signaling to specific participant
        const targetSocket = this.onlineUsers.get(data.targetUserId);
        if (targetSocket) {
          this.io.to(targetSocket.socketId).emit('group_call_signal', {
            callId: data.callId,
            fromUserId: socket.userId,
            fromUserName: socket.user.name,
            signal: data.signal
          });
        }
      } else {
        // Broadcast to all call participants
        socket.to(`call_${data.callId}`).emit('group_call_signal', {
          callId: data.callId,
          fromUserId: socket.userId,
          fromUserName: socket.user.name,
          signal: data.signal
        });
      }
    } catch (error) {
      console.error('Error handling group call signal:', error);
    }
  }
}