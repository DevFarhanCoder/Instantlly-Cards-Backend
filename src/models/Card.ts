import mongoose from 'mongoose';

const schema = new mongoose.Schema({
  userId: { type: String, index: true, required: true },

  // Personal
  name: String,
  designation: String,
  contact: String,
  email: String,
  website: String,
  location: String,
  mapsLink: String,

  // Business
  companyName: String,
  companyContact: String,
  companyEmail: String,
  companyWebsite: String,
  companyAddress: String,
  companyMapsLink: String,
  message: String,

  // Social
  linkedin: String,
  twitter: String,
  instagram: String,
  facebook: String,
  youtube: String,
  whatsapp: String,
  telegram: String
}, { timestamps: true });

export default mongoose.model('Card', schema);
