"use strict";
/**
 * Erlang Gateway Client for Node.js Backend
 * Connects your existing Socket.IO backend to the Erlang messaging gateway
 */
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const ws_1 = __importDefault(require("ws"));
const events_1 = require("events");
class ErlangGatewayClient extends events_1.EventEmitter {
    constructor(gatewayUrl) {
        super();
        this.ws = null;
        this.reconnectInterval = 5000;
        this.isConnected = false;
        // Prefer explicit constructor arg, then env var, otherwise disable gateway
        const envUrl = process.env.ERLANG_GATEWAY_URL;
        this.gatewayUrl = gatewayUrl || envUrl || null;
        // Prevent unhandled 'error' events bubbling up
        this.on('error', (err) => {
            console.error('ErlangGatewayClient emitted error:', err?.message || err);
        });
    }
    connect() {
        if (!this.gatewayUrl) {
            console.log('⚠️ Erlang Gateway URL not configured - skipping connection');
            return;
        }
        console.log('🔌 Connecting to Erlang Gateway:', this.gatewayUrl);
        this.ws = new ws_1.default(this.gatewayUrl);
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
        this.ws.on('message', (data) => {
            try {
                const message = JSON.parse(data.toString());
                this.handleMessage(message);
            }
            catch (error) {
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
        this.ws.on('error', (error) => {
            console.error('WebSocket error:', error);
            this.emit('error', error);
        });
    }
    handleMessage(message) {
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
    sendMessage(message) {
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
    updatePresence(userId, status) {
        // This will be handled by the gateway when users connect/disconnect
        console.log(`User ${userId} is now ${status}`);
    }
    send(data) {
        if (this.ws && this.ws.readyState === ws_1.default.OPEN) {
            this.ws.send(JSON.stringify(data));
        }
    }
    disconnect() {
        if (this.ws) {
            this.ws.close();
            this.ws = null;
        }
    }
}
exports.default = ErlangGatewayClient;
