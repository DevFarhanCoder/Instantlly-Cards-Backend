"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
// ─────────────────────────────────────────────────────────────
// Category schema — N-level hierarchy
// Each document is one node; parent_id = null → root category
// ─────────────────────────────────────────────────────────────
const categorySchema = new mongoose_1.default.Schema({
    name: { type: String, required: true, trim: true },
    icon: { type: String, default: "📁" },
    // Hierarchy fields
    parent_id: {
        type: mongoose_1.default.Schema.Types.ObjectId,
        ref: "Category",
        default: null,
    },
    level: { type: Number, default: 0 }, // 0 = root, 1 = sub, 2 = sub-sub, …
    // Legacy flat subcategories array — kept for backwards compatibility
    subcategories: [{ type: String, trim: true }],
    isActive: { type: Boolean, default: true },
    order: { type: Number, default: 0 },
}, { timestamps: true });
categorySchema.index({ parent_id: 1, order: 1 });
categorySchema.index({ isActive: 1, order: 1 });
categorySchema.index({ parent_id: 1, name: 1 }, { unique: true });
exports.default = mongoose_1.default.model("Category", categorySchema);
