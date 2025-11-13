import { Schema, model, models } from "mongoose";

const FeedbackSchema = new Schema(
  {
    userId: { 
      type: Schema.Types.ObjectId, 
      ref: "User", 
      required: true,
      index: true
    },
    name: { type: String, required: true },
    phone: { type: String, required: true },
    email: { type: String },
    subject: { type: String, required: true },
    message: { type: String, required: true },
    rating: { 
      type: Number, 
      min: 1, 
      max: 5,
      default: null
    },
    status: {
      type: String,
      enum: ["pending", "in-progress", "resolved", "closed"],
      default: "pending"
    },
    adminResponse: { type: String },
    respondedAt: { type: Date },
  },
  { timestamps: true }
);

export default models.Feedback || model("Feedback", FeedbackSchema);
