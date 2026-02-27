"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importStar(require("mongoose"));
const GroupCallSchema = new mongoose_1.Schema({
    callId: {
        type: String,
        required: true,
        unique: true,
        index: true
    },
    groupId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'Group',
        required: true,
        index: true
    },
    initiatorId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'User',
        required: true
    },
    callType: {
        type: String,
        enum: ['audio', 'video'],
        required: true,
        default: 'audio'
    },
    startTime: {
        type: Date,
        required: true,
        default: Date.now
    },
    endTime: {
        type: Date
    },
    status: {
        type: String,
        enum: ['ringing', 'active', 'ended'],
        required: true,
        default: 'ringing'
    },
    participants: [{
            userId: {
                type: mongoose_1.Schema.Types.ObjectId,
                ref: 'User',
                required: true
            },
            joinedAt: {
                type: Date,
                required: true,
                default: Date.now
            },
            leftAt: {
                type: Date
            },
            isInitiator: {
                type: Boolean,
                required: true,
                default: false
            }
        }],
    duration: {
        type: Number // Duration in seconds
    }
}, {
    timestamps: true,
    collection: 'group_calls'
});
// Indexes for performance
GroupCallSchema.index({ groupId: 1, createdAt: -1 });
GroupCallSchema.index({ initiatorId: 1, createdAt: -1 });
GroupCallSchema.index({ status: 1, createdAt: -1 });
GroupCallSchema.index({ 'participants.userId': 1 });
// Methods
GroupCallSchema.methods.addParticipant = function (userId) {
    const existingParticipant = this.participants.find((p) => p.userId.toString() === userId);
    if (!existingParticipant) {
        this.participants.push({
            userId: new mongoose_1.default.Types.ObjectId(userId),
            joinedAt: new Date(),
            isInitiator: false
        });
    }
    return this.save();
};
GroupCallSchema.methods.removeParticipant = function (userId) {
    const participant = this.participants.find((p) => p.userId.toString() === userId && !p.leftAt);
    if (participant) {
        participant.leftAt = new Date();
    }
    return this.save();
};
GroupCallSchema.methods.endCall = function () {
    this.status = 'ended';
    this.endTime = new Date();
    // Mark all active participants as left
    this.participants.forEach((p) => {
        if (!p.leftAt) {
            p.leftAt = this.endTime;
        }
    });
    // Calculate duration
    if (this.startTime && this.endTime) {
        this.duration = Math.floor((this.endTime.getTime() - this.startTime.getTime()) / 1000);
    }
    return this.save();
};
// Static methods
GroupCallSchema.statics.getActiveCallForGroup = function (groupId) {
    return this.findOne({
        groupId: new mongoose_1.default.Types.ObjectId(groupId),
        status: { $in: ['ringing', 'active'] }
    }).populate('initiatorId', 'name profilePicture')
        .populate('participants.userId', 'name profilePicture');
};
GroupCallSchema.statics.getCallHistory = function (groupId, limit = 10) {
    return this.find({
        groupId: new mongoose_1.default.Types.ObjectId(groupId),
        status: 'ended'
    })
        .populate('initiatorId', 'name profilePicture')
        .populate('participants.userId', 'name profilePicture')
        .sort({ createdAt: -1 })
        .limit(limit);
};
const GroupCall = mongoose_1.default.model('GroupCall', GroupCallSchema);
exports.default = GroupCall;
