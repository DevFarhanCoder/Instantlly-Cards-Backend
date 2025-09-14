import mongoose from 'mongoose';
const schema = new mongoose.Schema({
  email: { type: String, unique: true, required: true, index: true },
  passwordHash: { type: String, required: true },
  name: { type: String }
}, { timestamps: true });
export default mongoose.model('User', schema);
