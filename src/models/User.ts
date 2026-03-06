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
        validator: function (v: string) {
          return /^\+?[\d\s\-\(\)]{10,15}$/.test(v);
        },
        message: "Phone number must be between 10-15 digits",
      },
    },
    password: { type: String, required: false, select: false }, // Optional - not currently used
    // Email is completely optional - no constraints at all
    email: { type: String },
    profilePicture: { type: String, default: "" },
    about: { type: String, default: "Available" },
    gender: { type: String }, // male, female, other
    birthdate: { type: Date },
    anniversary: { type: Date },
    pushToken: { type: String },
    platform: { type: String },
    pushTokenUpdatedAt: { type: Date },
    // Credits system - 5 lac (500,000) credits on signup, valid for 1 month
    credits: { type: Number, default: 200 },
    creditsExpiryDate: { type: Date }, // Credits expire 1 month after signup
    referralCode: { type: String, unique: true, sparse: true, index: true },
    referredBy: { type: Schema.Types.ObjectId, ref: "User" },
    // Service type - Selected during first-time account setup
    serviceType: {
      type: String,
      enum: ["home-based", "business-visiting"],
      default: null,
    },
    // Quiz progress tracking - supports all 30 questions
    quizProgress: {
      completed: { type: Boolean, default: false },
      currentQuestionIndex: { type: Number, default: 0 },
      answeredQuestions: {
        type: [String],
        default: [],
        validate: {
          validator: function (v: string[]) {
            return v.length <= 30; // Maximum 30 questions
          },
          message: "Cannot have more than 30 answered questions",
        },
      }, // Array of question keys (e.g., ['married', 'haveBike'])
      answers: {
        type: Map,
        of: String,
        default: {},
      }, // Map of questionKey -> answer (stores all 30 Q&A pairs)
      creditsEarned: { type: Number, default: 0 },
      creditsRecordedInTransactions: { type: Number, default: 0 }, // Credits already saved in transactions
      startedAt: { type: Date },
      completedAt: { type: Date },
    },
    parentId: { type: Schema.Types.ObjectId, ref: "User", index: true },
    level: { type: Number, default: 0, min: 0 },
    directCount: { type: Number, default: 0, min: 0 },
    downlineCount: { type: Number, default: 0, min: 0 }, // Total descendants (all levels)

    // Special Credits System - "Sales Target at Special Discount"
    // Separate from regular MLM credits
    specialCredits: {
      balance: { type: Number, default: 0 }, // Current balance of special credits
      totalReceived: { type: Number, default: 0 }, // Lifetime received
      totalSent: { type: Number, default: 0 }, // Lifetime sent
      availableSlots: { type: Number, default: 0 }, // Number of slots (10 for admin, 5 for others)
      usedSlots: { type: Number, default: 0 }, // Number of used slots
    },

    // Voucher admin flag
    isVoucherAdmin: { type: Boolean, default: false },

    // Global voucher balance (legacy / admin-level)
    voucherBalance: { type: Number, default: 0 },

    // Per-voucher balance map: { [voucherId]: count }
    // Allows each admin-created voucher to have its own independent balance
    voucherBalances: { type: Map, of: Number, default: {} },

    // ── V2 fields ──────────────────────────────────────────────────────────

    // Ordered ancestor chain from root down to this user's parent.
    // Cached to avoid $graphLookup on every read. Updated on link/unlink.
    ancestors: {
      type: [{ type: Schema.Types.ObjectId, ref: "User" }],
      default: [],
    },

    // Explicit role to drive NetworkRules.maxDirectByRole lookups.
    // Derived from isVoucherAdmin but stored independently so rule checks
    // don't have to join the voucher-admin flag.
    role: {
      type: String,
      enum: ["admin", "user"],
      default: "user",
    },

    // Soft-delete support: deleted users are hidden from the tree but their
    // historical records (NetworkEvent, SlotEvent, etc.) are preserved.
    isDeleted: { type: Boolean, default: false },
    deletedAt: { type: Date, default: null },
  },
  { timestamps: true },
);

// V2 indexes
UserSchema.index({ ancestors: 1 });
UserSchema.index({ role: 1, parentId: 1 });
UserSchema.index({ isDeleted: 1, parentId: 1 });

export default models.User || model("User", UserSchema);
