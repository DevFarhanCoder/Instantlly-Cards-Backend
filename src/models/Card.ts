import mongoose from "mongoose";

const schema = new mongoose.Schema(
  {
    userId: { type: String, index: true, required: true },

    // Personal
    name: { type: String, required: true, trim: true },
    gender: { type: String, default: "", enum: ["", "Male", "Female"] },
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
    companyEmail: { type: String, default: "" },
    companyWebsite: { type: String, default: "" },
    companyAddress: { type: String, default: "" },
    companyMapsLink: { type: String, default: "" },
    message: { type: String, default: "" },
    companyPhoto: { type: String, default: "" }, // data URI or CDN URL

    // Social
    linkedin: { type: String, default: "" },
    twitter: { type: String, default: "" },
    instagram: { type: String, default: "" },
    facebook: { type: String, default: "" },
    youtube: { type: String, default: "" },
    whatsapp: { type: String, default: "" },
    telegram: { type: String, default: "" },
  },
  { timestamps: true }
);

// Add indexes for faster querying
schema.index({ userId: 1, createdAt: -1 }); // Composite index for user's cards sorted by date
schema.index({ createdAt: -1 }); // Index for sorting by creation date
schema.index({ companyName: 'text', name: 'text' }); // Text search index

export default mongoose.model("Card", schema);
