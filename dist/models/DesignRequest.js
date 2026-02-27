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
const DesignRequestSchema = new mongoose_1.Schema({
    businessName: {
        type: String,
        default: '',
    },
    email: {
        type: String,
        default: '',
    },
    webLinks: {
        type: [String],
        default: [],
    },
    phoneNumber: {
        type: String,
        default: '',
    },
    adText: {
        type: String,
        default: '',
    },
    businessAddress: {
        type: String,
        default: '',
    },
    adType: {
        type: String,
        enum: ['image', 'video'],
        required: true,
    },
    channelType: {
        type: String,
        enum: ['withChannel', 'withoutChannel'],
        default: 'withoutChannel',
    },
    referenceImagesGridFS: {
        type: [mongoose_1.Schema.Types.ObjectId],
        default: [],
    },
    referenceVideosGridFS: {
        type: [mongoose_1.Schema.Types.ObjectId],
        default: [],
    },
    referenceImagesS3: {
        type: [{
                url: { type: String, required: true },
                key: { type: String, required: true },
            }],
        default: [],
    },
    referenceVideosS3: {
        type: [{
                url: { type: String, required: true },
                key: { type: String, required: true },
            }],
        default: [],
    },
    uploaderPhone: {
        type: String,
        required: true,
    },
    uploaderName: {
        type: String,
        default: 'Mobile User',
    },
    userId: {
        type: String,
        default: '',
    },
    status: {
        type: String,
        enum: ['pending', 'in-progress', 'completed', 'cancelled'],
        default: 'pending',
    },
    adminNotes: {
        type: String,
        default: '',
    },
    assignedDesignerId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'Designer',
        default: null,
    },
    assignedDesignerName: {
        type: String,
        default: '',
    },
    assignedAt: {
        type: Date,
        default: null,
    },
    completedAdId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'Ad',
        default: null,
    },
}, {
    timestamps: true,
});
// Indexes for efficient queries
DesignRequestSchema.index({ uploaderPhone: 1 });
DesignRequestSchema.index({ status: 1 });
DesignRequestSchema.index({ createdAt: -1 });
const DesignRequest = mongoose_1.default.model('DesignRequest', DesignRequestSchema);
exports.default = DesignRequest;
