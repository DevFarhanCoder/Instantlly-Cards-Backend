import mongoose from "mongoose";

// ─────────────────────────────────────────────────────────────
// Category schema — N-level hierarchy
// Each document is one node; parent_id = null → root category
// ─────────────────────────────────────────────────────────────
const categorySchema = new mongoose.Schema(
  {
    name: { type: String, required: true, trim: true },
    icon: { type: String, default: "📁" },

    // Hierarchy fields
    parent_id: {
      type: mongoose.Schema.Types.ObjectId,
      ref: "Category",
      default: null,
    },
    level: { type: Number, default: 0 }, // 0 = root, 1 = sub, 2 = sub-sub, …

    // Legacy flat subcategories array — kept for backwards compatibility
    subcategories: [{ type: String, trim: true }],

    isActive: { type: Boolean, default: true },
    order: { type: Number, default: 0 },
  },
  { timestamps: true }
);

categorySchema.index({ parent_id: 1, order: 1 });
categorySchema.index({ isActive: 1, order: 1 });
categorySchema.index({ parent_id: 1, name: 1 }, { unique: true });

export default mongoose.model("Category", categorySchema);
