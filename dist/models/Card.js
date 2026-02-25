"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const schema = new mongoose_1.default.Schema({
    userId: { type: String, index: true, required: true },
    isDefault: { type: Boolean, default: false }, // Flag for default card (only 1 per user)
    // Personal
    name: { type: String, required: true, trim: true },
    gender: { type: String, default: "", enum: ["", "Male", "Female"] },
    birthdate: { type: String, default: "" }, // ISO date string format: YYYY-MM-DDTHH:mm:ss.sssZ
    anniversary: { type: String, default: "" }, // ISO date string format: YYYY-MM-DDTHH:mm:ss.sssZ
    personalCountryCode: { type: String, default: "", match: [/^\d*$/, "Digits only"] },
    personalPhone: { type: String, default: "", match: [/^\d*$/, "Digits only"] },
    email: { type: String, default: "" },
    location: { type: String, default: "" },
    mapsLink: { type: String, default: "" },
    // Business
    companyName: { type: String, default: "" },
    designation: { type: String, default: "" },
    companyCountryCode: { type: String, default: "", match: [/^\d*$/, "Digits only"] },
    companyPhone: { type: String, default: "", match: [/^\d*$/, "Digits only"] },
    companyPhones: {
        type: [{
                countryCode: { type: String, default: "91" },
                phone: { type: String, default: "" }
            }],
        default: []
    },
    companyEmail: { type: String, default: "" },
    companyWebsite: { type: String, default: "" },
    companyAddress: { type: String, default: "" },
    companyMapsLink: { type: String, default: "" },
    message: { type: String, default: "" },
    companyPhoto: { type: String, default: "" }, // data URI or CDN URL
    businessHours: { type: String, default: "" },
    servicesOffered: { type: String, default: "" },
    establishedYear: { type: String, default: "" },
    aboutBusiness: { type: String, default: "" },
    // Social
    linkedin: { type: String, default: "" },
    twitter: { type: String, default: "" },
    instagram: { type: String, default: "" },
    facebook: { type: String, default: "" },
    youtube: { type: String, default: "" },
    whatsapp: { type: String, default: "" },
    telegram: { type: String, default: "" },
    // Search
    keywords: { type: String, default: "" },
}, { timestamps: true });
// Add indexes for faster querying
schema.index({ userId: 1, createdAt: -1 }); // Composite index for user's cards sorted by date
schema.index({ createdAt: -1 }); // Index for sorting by creation date
schema.index({ companyName: 'text', name: 'text' }); // Text search index
// CRITICAL: Unique constraint to prevent multiple default cards per user
// This enforces database-level atomicity for default card creation
schema.index({ userId: 1, isDefault: 1 }, {
    unique: true,
    partialFilterExpression: { isDefault: true },
    name: 'unique_default_card_per_user'
});
exports.default = mongoose_1.default.model("Card", schema);
