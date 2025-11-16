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
    password: { type: String, required: true, select: false },
    // Email is completely optional - no constraints at all
    email: { type: String },
    profilePicture: { type: String, default: "" },
    about: { type: String, default: "Available" },
    pushToken: { type: String },
    platform: { type: String },
    pushTokenUpdatedAt: { type: Date },
    // Credits system - 5 lac (500,000) credits on signup, valid for 1 month
    credits: { type: Number, default: 500000 },
    creditsExpiryDate: { type: Date }, // Credits expire 1 month after signup
    referralCode: { type: String, unique: true, sparse: true, index: true },
    referredBy: { type: Schema.Types.ObjectId, ref: 'User' },
  },
  { timestamps: true }
);

export default models.User || model("User", UserSchema);
