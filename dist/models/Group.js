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
exports.GroupModel = void 0;
const mongoose_1 = __importStar(require("mongoose"));
const GroupSchema = new mongoose_1.Schema({
    name: {
        type: String,
        required: true,
        trim: true,
        maxlength: 100
    },
    description: {
        type: String,
        trim: true,
        maxlength: 500
    },
    icon: {
        type: String,
        trim: true
    },
    members: [{
            type: mongoose_1.Schema.Types.ObjectId,
            ref: "User",
            required: true
        }],
    admin: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "User",
        required: true
    },
    joinCode: {
        type: String,
        required: true,
        unique: true,
        sparse: true, // Ignore null values for uniqueness
        minlength: 6,
        maxlength: 6
    },
    lastMessage: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "Message"
    },
    lastMessageTime: {
        type: Date,
        default: Date.now
    },
    isActive: {
        type: Boolean,
        default: true
    },
    mutedBy: [{
            type: mongoose_1.Schema.Types.ObjectId,
            ref: "User"
        }],
    memberRoles: {
        type: Map,
        of: String,
        enum: ['admin', 'member'],
        default: new Map()
    },
    chatId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: "Chat"
    },
    adminTransferInfo: {
        previousAdmin: {
            type: mongoose_1.Schema.Types.ObjectId,
            ref: "User"
        },
        transferredAt: {
            type: Date
        },
        seen: {
            type: Boolean,
            default: false
        }
    }
}, {
    timestamps: true
});
// Indexes for better performance
GroupSchema.index({ members: 1 });
GroupSchema.index({ admin: 1 });
GroupSchema.index({ lastMessageTime: -1 });
GroupSchema.index({ isActive: 1 });
// Pre-save hook to ensure joinCode and admin role are set
GroupSchema.pre('save', async function (next) {
    // Generate joinCode if not present
    if (!this.joinCode || this.joinCode === '') {
        try {
            console.log('🔍 Generating joinCode in pre-save hook...');
            this.joinCode = await this.constructor.generateInviteCode();
            console.log('✅ Generated joinCode:', this.joinCode);
        }
        catch (error) {
            console.error('Error generating joinCode in pre-save hook:', error);
            // Fallback to timestamp-based code if generation fails
            const timestamp = Date.now().toString(36).toUpperCase();
            this.joinCode = timestamp.substring(timestamp.length - 6);
            console.log('✅ Fallback joinCode:', this.joinCode);
        }
    }
    // Set admin role in memberRoles
    if (this.admin && (!this.memberRoles.has(this.admin.toString()) || this.memberRoles.get(this.admin.toString()) !== 'admin')) {
        this.memberRoles.set(this.admin.toString(), 'admin');
    }
    // Set default member roles for all members
    if (this.members) {
        this.members.forEach(memberId => {
            if (!this.memberRoles.has(memberId.toString())) {
                this.memberRoles.set(memberId.toString(), 'member');
            }
        });
    }
    next();
});
// Generate a unique 6-character invite code
GroupSchema.statics.generateInviteCode = async function () {
    let joinCode;
    let attempts = 0;
    const maxAttempts = 10;
    while (attempts < maxAttempts) {
        // Generate 6-character alphanumeric code
        joinCode = Math.random().toString(36).substring(2, 8).toUpperCase();
        // Ensure it's exactly 6 characters by padding if necessary
        while (joinCode.length < 6) {
            joinCode += Math.random().toString(36).substring(2, 3).toUpperCase();
        }
        joinCode = joinCode.substring(0, 6);
        try {
            // Check if this code already exists
            const existingGroup = await this.findOne({ joinCode });
            if (!existingGroup) {
                return joinCode;
            }
        }
        catch (error) {
            console.error('Error checking existing joinCode:', error);
        }
        attempts++;
    }
    // Fallback: if we couldn't generate a unique code after maxAttempts, 
    // use timestamp-based code
    const timestamp = Date.now().toString(36).toUpperCase();
    return timestamp.substring(timestamp.length - 6);
};
// Find group by join code
GroupSchema.statics.findByJoinCode = function (joinCode) {
    return this.findOne({ joinCode: joinCode.toUpperCase(), isActive: true });
};
// Get all groups for a user
GroupSchema.statics.getUserGroups = function (userId) {
    return this.find({
        members: new mongoose_1.default.Types.ObjectId(userId),
        isActive: true
    })
        .populate('members', 'name profilePicture email')
        .populate('admin', 'name profilePicture email')
        .populate('lastMessage')
        .sort({ lastMessageTime: -1 });
};
// Instance method to add member
GroupSchema.methods.addMember = function (userId) {
    const userObjectId = new mongoose_1.default.Types.ObjectId(userId);
    if (!this.members.includes(userObjectId)) {
        this.members.push(userObjectId);
        this.memberRoles.set(userId, 'member');
    }
    return this.save();
};
// Instance method to remove member
GroupSchema.methods.removeMember = function (userId) {
    this.members = this.members.filter((memberId) => memberId.toString() !== userId);
    this.memberRoles.delete(userId);
    this.mutedBy = this.mutedBy.filter((mutedId) => mutedId.toString() !== userId);
    return this.save();
};
// Instance method to update last message
GroupSchema.methods.updateLastMessage = function (messageId) {
    this.lastMessage = new mongoose_1.default.Types.ObjectId(messageId);
    this.lastMessageTime = new Date();
    return this.save();
};
const GroupModel = mongoose_1.default.model("Group", GroupSchema);
exports.GroupModel = GroupModel;
exports.default = GroupModel;
