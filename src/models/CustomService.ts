import mongoose from "mongoose";

// CustomService schema - tracks custom services added by users that don't match existing categories
const customServiceSchema = new mongoose.Schema(
  {
    serviceName: { type: String, required: true, trim: true },
    addedBy: { type: String, required: true }, // userId who added this custom service
    userName: { type: String, default: "" }, // display name of the user
    cardId: { type: String, default: "" }, // card where this was added
    parentCategory: { type: String, default: "" }, // if user was inside a category when adding
    status: {
      type: String,
      enum: ["pending", "approved", "rejected"],
      default: "pending",
    },
    // When admin approves, record where it was added
    approvedAs: {
      type: { type: String, enum: ["category", "subcategory", ""], default: "" },
      categoryName: { type: String, default: "" }, // if added as subcategory, which parent category
    },
    approvedAt: { type: Date },
    approvedByAdmin: { type: String, default: "" },
  },
  { timestamps: true }
);

customServiceSchema.index({ status: 1, createdAt: -1 });
customServiceSchema.index({ serviceName: 1 });
customServiceSchema.index({ addedBy: 1 });

export default mongoose.model("CustomService", customServiceSchema);
