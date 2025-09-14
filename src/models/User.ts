import { Schema, model } from "mongoose";

const UserSchema = new Schema(
  {
    email: { type: String, required: true, unique: true, index: true },
    // frequently marked select:false for safety, so we must .select("+passwordHash") in queries
    passwordHash: { type: String, required: true, select: false },
    name: String,
  },
  { timestamps: true }
);

export default model("User", UserSchema);
