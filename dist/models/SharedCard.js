"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const sharedCardSchema = new mongoose_1.default.Schema({
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
    // Who received the card (supports both ObjectId and string userIds)
    recipientId: {
        type: String, // Changed from ObjectId to String to support temporary userIds
        required: true
    },
    // Optional message with the card
    message: {
        type: String,
        maxlength: 500,
        default: ""
    },
    // Delivery status
    status: {
        type: String,
        enum: ['sent', 'delivered', 'viewed'],
        default: 'sent'
    },
    // When the card was shared
    sentAt: {
        type: Date,
        default: Date.now
    },
    // When the recipient viewed the card
    viewedAt: {
        type: Date
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
    // Cache recipient info
    recipientName: {
        type: String,
        required: true
    },
    // PERFORMANCE OPTIMIZATION: Store photos directly (ultra denormalization)
    // This eliminates need for additional queries to Card and User collections
    cardPhoto: {
        type: String,
        default: ""
    },
    senderProfilePicture: {
        type: String,
        default: ""
    },
    recipientProfilePicture: {
        type: String,
        default: ""
    },
}, {
    timestamps: true // Adds createdAt and updatedAt
});
// Indexes for efficient queries with cursor-based pagination
sharedCardSchema.index({ senderId: 1, _id: -1 }); // For sent cards with cursor pagination
sharedCardSchema.index({ recipientId: 1, _id: -1 }); // For received cards with cursor pagination
sharedCardSchema.index({ senderId: 1, sentAt: -1 }); // For sent cards time-based sorting
sharedCardSchema.index({ recipientId: 1, sentAt: -1 }); // For received cards time-based sorting
sharedCardSchema.index({ recipientId: 1, status: 1, _id: -1 }); // For unviewed cards with cursor
sharedCardSchema.index({ cardId: 1 }); // For card lookup
sharedCardSchema.index({ status: 1 }); // For status filtering
// Search indexes for fuzzy search functionality
sharedCardSchema.index({
    cardTitle: 'text',
    senderName: 'text',
    message: 'text'
}, {
    name: 'search_index',
    weights: {
        cardTitle: 10, // Higher weight for card titles
        senderName: 5, // Medium weight for sender names
        message: 1 // Lower weight for messages
    }
});
// Additional search indexes for case-insensitive queries
sharedCardSchema.index({ recipientId: 1, cardTitle: 1 }); // For recipient + card title search
sharedCardSchema.index({ recipientId: 1, senderName: 1 }); // For recipient + sender name search
const SharedCard = mongoose_1.default.model('SharedCard', sharedCardSchema);
exports.default = SharedCard;
