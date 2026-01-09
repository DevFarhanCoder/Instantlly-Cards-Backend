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
    password: { type: String, required: false, select: false }, // Optional for Firebase users
    firebaseUid: { type: String, sparse: true, unique: true, index: true }, // Firebase user ID
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
    referredBy: { type: Schema.Types.ObjectId, ref: 'User' },
    // Service type - Selected during first-time account setup
    serviceType: { 
      type: String, 
      enum: ['home-based', 'business-visiting'],
      default: null 
    },
    // Quiz progress tracking
    quizProgress: {
      completed: { type: Boolean, default: false },
      currentQuestionIndex: { type: Number, default: 0 },
      answeredQuestions: [{ type: String }], // Array of question keys (e.g., ['married', 'haveBike'])
      answers: {
        type: Map,
        of: String,
        default: {}
      }, // Map of questionKey -> answer
      creditsEarned: { type: Number, default: 0 },
      creditsRecordedInTransactions: { type: Number, default: 0 }, // Credits already saved in transactions
      startedAt: { type: Date },
      completedAt: { type: Date }
    }
  },
  { timestamps: true }
);

export default models.User || model("User", UserSchema);
