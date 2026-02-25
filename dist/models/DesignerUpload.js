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
const DesignerUploadSchema = new mongoose_1.Schema({
    designRequestId: { type: mongoose_1.Schema.Types.ObjectId, ref: 'DesignRequest', required: true },
    designerId: { type: mongoose_1.Schema.Types.ObjectId, ref: 'Designer', required: true },
    designerName: { type: String, required: true },
    filesS3: [{
            url: { type: String, required: true },
            key: { type: String, required: true },
            filename: { type: String },
            contentType: { type: String }
        }],
    notes: { type: String, default: '' },
    status: {
        type: String,
        enum: ['uploaded', 'approved', 'rejected', 'sent-to-user'],
        default: 'uploaded'
    },
    adminNotes: { type: String, default: '' },
    userFeedback: { type: String, default: '' }
}, { timestamps: true });
// Index for fast lookups
DesignerUploadSchema.index({ designerId: 1, createdAt: -1 });
DesignerUploadSchema.index({ designRequestId: 1 });
DesignerUploadSchema.index({ status: 1 });
const DesignerUpload = mongoose_1.default.model('DesignerUpload', DesignerUploadSchema);
exports.default = DesignerUpload;
