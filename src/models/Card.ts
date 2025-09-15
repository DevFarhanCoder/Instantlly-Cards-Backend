import mongoose from "mongoose";

const schema = new mongoose.Schema(
  {
    userId: { type: String, index: true, required: true },

    // Personal
    name: { type: String, required: true, trim: true },
    designation: { type: String, default: "" },
    personalCountryCode: { type: String, default: "", match: [/^\d*$/, "Digits only"] },
    personalPhone: { type: String, default: "", match: [/^\d*$/, "Digits only"] },
    email: { type: String, default: "" },
    website: { type: String, default: "" },
    location: { type: String, default: "" },
    mapsLink: { type: String, default: "" },

    // Business
    companyName: { type: String, default: "" },
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

export default mongoose.model("Card", schema);
