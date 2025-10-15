/**
 * Hybrid Messaging Service
 * Bridges Socket.IO (existing) with Erlang Gateway (new)
 * Allows gradual migration: Socket.IO → Erlang
 */

import { Server as SocketIOServer } from 'socket.io';
import ErlangGatewayClient from './erlangGatewayClient';

class HybridMessagingService {
  private io: SocketIOServer;
  private erlangGateway: ErlangGatewayClient;
  private useErlang: boolean = false; // Feature flag

  constructor(io: SocketIOServer) {
    this.io = io;
    this.erlangGateway = new ErlangGatewayClient('ws://localhost:8080/ws');
    this.initializeErlangGateway();
  }

  private initializeErlangGateway(): void {
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
    this.erlangGateway.connect();
  }

  sendMessage(data: any): void {
    if (this.useErlang) {
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
