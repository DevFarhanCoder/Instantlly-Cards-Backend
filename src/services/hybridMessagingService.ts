/**
 * Hybrid Messaging Service
 * Bridges Socket.IO (existing) with Erlang Gateway (new)
 * Allows gradual migration: Socket.IO → Erlang
 */

import { Server as SocketIOServer } from 'socket.io';
import ErlangGatewayClient from './erlangGatewayClient';

class HybridMessagingService {
  private io: SocketIOServer;
  private erlangGateway: ErlangGatewayClient | null = null;
  private useErlang: boolean = false; // Feature flag

  constructor(io: SocketIOServer) {
    this.io = io;
    // Only initialize Erlang gateway if configured via env
    const gatewayUrl = process.env.ERLANG_GATEWAY_URL || null;
    if (gatewayUrl) {
      this.erlangGateway = new ErlangGatewayClient(gatewayUrl);
      this.initializeErlangGateway();
    } else {
      console.log('⚠️ ERLANG_GATEWAY_URL not set - running in Socket.IO fallback mode');
      this.erlangGateway = null;
    }
  }

  private initializeErlangGateway(): void {
    if (!this.erlangGateway) return;

    // Try to connect to Erlang gateway
    this.erlangGateway.on('connected', () => {
      console.log('✅ Erlang Gateway connected - Enhanced mode enabled');
      this.useErlang = true;
    });

    this.erlangGateway.on('disconnected', () => {
      console.log('⚠️  Erlang Gateway disconnected - Fallback to Socket.IO');
      this.useErlang = false;
    });

    this.erlangGateway.on('message', (message) => {
      // Forward messages from Erlang to Socket.IO clients
      this.forwardToSocketIO(message);
    });

    // Connect to Erlang gateway
    try {
      this.erlangGateway.connect();
    } catch (err) {
      console.error('❌ Failed to start Erlang gateway client:', err);
      this.useErlang = false;
    }
  }

  sendMessage(data: any): void {
    if (this.useErlang && this.erlangGateway) {
      // Use Erlang gateway (high performance)
      console.log('📨 Sending via Erlang Gateway');
      this.erlangGateway.sendMessage(data);
    } else {
      // Fallback to Socket.IO (existing behavior)
      console.log('📨 Sending via Socket.IO (fallback)');
      this.sendViaSocketIO(data);
    }
  }

  private sendViaSocketIO(data: any): void {
    // Your existing Socket.IO logic
    if (data.receiverId) {
      this.io.to(data.receiverId).emit('new_message', data);
    }
  }

  private forwardToSocketIO(message: any): void {
    // Forward Erlang messages to Socket.IO clients (for backward compatibility)
    if (message.receiverId) {
      this.io.to(message.receiverId).emit('new_message', message);
    }
  }

  getStatus(): { mode: string; erlangConnected: boolean } {
    return {
      mode: this.useErlang ? 'erlang' : 'socketio',
      erlangConnected: this.useErlang
    };
  }

  // Enable Erlang gateway (for gradual rollout)
  enableErlang(): void {
    this.useErlang = true;
    console.log('✅ Erlang Gateway ENABLED');
  }

  // Disable Erlang gateway (rollback)
  disableErlang(): void {
    this.useErlang = false;
    console.log('⚠️  Erlang Gateway DISABLED - Using Socket.IO');
  }
}

export default HybridMessagingService;
