"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
const tempMessageSchema = new mongoose_1.Schema({
    senderId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'User',
        required: true,
        index: true
    },
    receiverId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'User',
        index: true
    },
    groupId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'Group',
        index: true
    },
    text: {
        type: String,
        required: true,
        maxlength: 1000 // Limit message size for notifications
    },
    messageId: {
        type: String,
        required: true,
        unique: true,
        index: true
    },
    isDelivered: {
        type: Boolean,
        default: false,
        index: true
    },
    deliveredAt: {
        type: Date
    },
    createdAt: {
        type: Date,
        default: Date.now,
        index: true
    },
    expiresAt: {
        type: Date,
        default: () => new Date(Date.now() + 15 * 24 * 60 * 60 * 1000), // 15 days from now
        index: { expireAfterSeconds: 0 } // MongoDB TTL index for automatic deletion
    }
});
// Compound indexes for efficient queries
tempMessageSchema.index({ receiverId: 1, isDelivered: 1, createdAt: -1 });
tempMessageSchema.index({ groupId: 1, isDelivered: 1, createdAt: -1 });
tempMessageSchema.index({ senderId: 1, createdAt: -1 });
// Validation: message must have either receiverId OR groupId, not both
tempMessageSchema.pre('save', function (next) {
    const hasReceiver = !!this.receiverId;
    const hasGroup = !!this.groupId;
    if (hasReceiver && hasGroup) {
        next(new Error('Message cannot have both receiverId and groupId'));
    }
    else if (!hasReceiver && !hasGroup) {
        next(new Error('Message must have either receiverId or groupId'));
    }
    else {
        next();
    }
});
exports.default = (0, mongoose_1.model)('TempMessage', tempMessageSchema);
