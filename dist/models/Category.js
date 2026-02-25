"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
// Category schema - stores the master list of categories and subcategories
const categorySchema = new mongoose_1.default.Schema({
    name: { type: String, required: true, trim: true },
    icon: { type: String, default: "📁" }, // emoji icon for the category
    subcategories: [{ type: String, trim: true }], // array of subcategory names
    isActive: { type: Boolean, default: true },
    order: { type: Number, default: 0 }, // for display ordering
}, { timestamps: true });
categorySchema.index({ name: 1 }, { unique: true });
categorySchema.index({ isActive: 1, order: 1 });
exports.default = mongoose_1.default.model("Category", categorySchema);
