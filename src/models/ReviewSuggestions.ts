import { Schema, model, models } from "mongoose";

const ReviewSuggestionsSchema = new Schema(
  {
    rating: {
      type: Number,
      required: true,
      min: 1,
      max: 5,
      unique: true,
      index: true
    },
    category: String, // optional - for category-specific suggestions

    suggestions: [
      {
        text: String,
        emoji: String,
        weight: Number // 0-1 for ML ranking
      }
    ],

    prompt: String, // "What went wrong?", "What did you like?", etc.
    emoji: String, // 😠, 😞, 😐, 😊, 🤩
    label: String, // Terrible, Bad, Average, Good, Excellent

    createdAt: { type: Date, default: Date.now },
    updatedAt: { type: Date, default: Date.now }
  },
  { timestamps: true }
);

export default models.ReviewSuggestions || model("ReviewSuggestions", ReviewSuggestionsSchema);
