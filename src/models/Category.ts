import mongoose from "mongoose";

// Category schema - stores the master list of categories and subcategories
const categorySchema = new mongoose.Schema(
  {
    name: { type: String, required: true, trim: true },
    icon: { type: String, default: "📁" }, // emoji icon for the category
    subcategories: [{ type: String, trim: true }], // array of subcategory names
    isActive: { type: Boolean, default: true },
    order: { type: Number, default: 0 }, // for display ordering
  },
  { timestamps: true }
);

categorySchema.index({ name: 1 }, { unique: true });
categorySchema.index({ isActive: 1, order: 1 });

export default mongoose.model("Category", categorySchema);
