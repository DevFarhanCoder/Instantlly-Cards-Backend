import { Schema, model, models } from "mongoose";

const UserSchema = new Schema(
  {
    name: { type: String, required: true },
    phone: { 
      type: String, 
      required: true, 
      unique: true, 
      index: true,
      validate: {
        validator: function(v: string) {
          return /^\+?[\d\s\-\(\)]{10,15}$/.test(v);
        },
        message: 'Phone number must be between 10-15 digits'
      }
    },
    password: { type: String, required: true, select: false }, // <- key
    email: { 
      type: String, 
      default: undefined,
      sparse: true, // This allows multiple null/undefined values
      unique: true  // But still enforces uniqueness for non-null values
    }, // Made fully optional for phone-based auth
    profilePicture: { type: String, default: "" },
    about: { type: String, default: "Available" },
    pushToken: { type: String },
    platform: { type: String },
    pushTokenUpdatedAt: { type: Date },
  },
  { timestamps: true }
);

export default models.User || model("User", UserSchema);
