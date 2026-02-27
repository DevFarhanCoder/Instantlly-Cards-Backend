"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const groupSharedCardSchema = new mongoose_1.default.Schema({
    // Card being shared
    cardId: {
        type: mongoose_1.default.Schema.Types.ObjectId,
        ref: 'Card',
        required: true
    },
    // Who sent the card (supports both ObjectId and string userIds)
    senderId: {
        type: String, // Changed from ObjectId to String to support temporary userIds
        required: true
    },
    // Group where the card was shared
    groupId: {
        type: mongoose_1.default.Schema.Types.ObjectId,
        ref: 'Group',
        required: true
    },
    // Optional message with the card
    message: {
        type: String,
        maxlength: 500,
        default: ""
    },
    // When the card was shared
    sentAt: {
        type: Date,
        default: Date.now
    },
    // Cache some card info for quick queries
    cardTitle: {
        type: String,
        required: true
    },
    // Cache sender info
    senderName: {
        type: String,
        required: true
    },
    // Cache group info
    groupName: {
        type: String,
        required: true
    },
}, {
    timestamps: true // Adds createdAt and updatedAt
});
// Indexes for efficient queries
groupSharedCardSchema.index({ senderId: 1, sentAt: -1 }); // For sent cards query
groupSharedCardSchema.index({ groupId: 1, sentAt: -1 }); // For group cards query
groupSharedCardSchema.index({ groupId: 1, senderId: 1, sentAt: -1 }); // Composite index for group + sender queries
groupSharedCardSchema.index({ cardId: 1 }); // For card lookup
const GroupSharedCard = mongoose_1.default.model('GroupSharedCard', groupSharedCardSchema);
exports.default = GroupSharedCard;
