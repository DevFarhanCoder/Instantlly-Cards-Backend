/**
 * Erlang Gateway Client for Node.js Backend
 * Connects your existing Socket.IO backend to the Erlang messaging gateway
 */

import WebSocket from 'ws';
import { EventEmitter } from 'events';

interface Message {
  messageId: string;
  senderId: string;
  receiverId: string;
  content: string;
  messageType: string;
  timestamp: number;
}

class ErlangGatewayClient extends EventEmitter {
  private ws: WebSocket | null = null;
  private gatewayUrl: string | null;
  private reconnectInterval: number = 5000;
  private isConnected: boolean = false;

  constructor(gatewayUrl?: string) {
    super();
    // Prefer explicit constructor arg, then env var, otherwise disable gateway
    const envUrl = process.env.ERLANG_GATEWAY_URL;
    this.gatewayUrl = gatewayUrl || envUrl || null;

    // Prevent unhandled 'error' events bubbling up
    this.on('error', (err) => {
      console.error('ErlangGatewayClient emitted error:', err?.message || err);
    });
  }

  connect(): void {
    if (!this.gatewayUrl) {
      console.log('⚠️ Erlang Gateway URL not configured - skipping connection');
      return;
    }

    console.log('🔌 Connecting to Erlang Gateway:', this.gatewayUrl);

    this.ws = new WebSocket(this.gatewayUrl);

    this.ws.on('open', () => {
      console.log('✅ Connected to Erlang Gateway');
      this.isConnected = true;
      
      // Authenticate as backend service
      this.send({
        type: 'auth',
        userId: 'nodejs_backend',
        deviceId: 'backend_service'
      });

      this.emit('connected');
    });

    this.ws.on('message', (data: WebSocket.Data) => {
      try {
        const message = JSON.parse(data.toString());
        this.handleMessage(message);
      } catch (error) {
        console.error('Failed to parse message:', error);
      }
    });

    this.ws.on('close', () => {
      console.log('❌ Disconnected from Erlang Gateway');
      this.isConnected = false;
      this.emit('disconnected');
      
      // Auto-reconnect
      setTimeout(() => this.connect(), this.reconnectInterval);
    });

    this.ws.on('error', (error: Error) => {
      console.error('WebSocket error:', error);
      this.emit('error', error);
    });
  }

  private handleMessage(message: any): void {
    switch (message.type) {
      case 'ack':
        // Erlang gateway acknowledgment
        console.log('✅ Message acknowledged by Erlang Gateway');
        this.emit('ack', message);
        break;

      case 'auth_success':
        console.log('✅ Backend authenticated with Erlang Gateway');
        this.emit('authenticated');
        break;

      case 'new_message':
        // Forward to Socket.IO clients
        this.emit('message', message);
        break;

      case 'presence_update':
        this.emit('presence', message);
        break;

      case 'message_ack':
        this.emit('ack', message);
        break;

      default:
        console.log('Unknown message type:', message.type);
    }
  }

  sendMessage(message: Message): void {
    if (!this.isConnected || !this.ws) {
      console.error('Not connected to Erlang Gateway');
      return;
    }

    this.send({
      type: 'send_message',
      messageId: message.messageId,
      receiverId: message.receiverId,
      content: message.content,
      messageType: message.messageType
    });
  }

  updatePresence(userId: string, status: 'online' | 'offline'): void {
    // This will be handled by the gateway when users connect/disconnect
    console.log(`User ${userId} is now ${status}`);
  }

  private send(data: any): void {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(data));
    }
  }

  disconnect(): void {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }
}

export default ErlangGatewayClient;
