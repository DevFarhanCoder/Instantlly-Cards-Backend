import { Schema, model, models } from "mongoose";

const BusinessPromotionSchema = new Schema(
  {
    userId: {
      type: Schema.Types.ObjectId,
      ref: 'User',
      required: true,
      index: true
    },
    // Step 1: Business Information
    businessName: { type: String, required: true },
    ownerName: { type: String, required: true },
    description: { type: String, default: '' },

    // Step 2: Category Information
    category: [{ type: String }], // Array of selected categories/subcategories

    // Step 3: Contact Details
    email: { type: String, default: '' },
    phone: { type: String, default: '' }, // Can be comma-separated multiple numbers
    whatsapp: { type: String, default: '' },
    website: { type: String, default: '' },

    // Business Hours
    businessHours: {
      type: Map,
      of: {
        open: { type: Boolean, default: false },
        openTime: { type: String, default: '09:00' },
        closeTime: { type: String, default: '18:00' }
      },
      default: {
        Sunday: { open: false, openTime: '09:00', closeTime: '18:00' },
        Monday: { open: false, openTime: '09:00', closeTime: '18:00' },
        Tuesday: { open: false, openTime: '09:00', closeTime: '18:00' },
        Wednesday: { open: false, openTime: '09:00', closeTime: '18:00' },
        Thursday: { open: false, openTime: '09:00', closeTime: '18:00' },
        Friday: { open: false, openTime: '09:00', closeTime: '18:00' },
        Saturday: { open: false, openTime: '09:00', closeTime: '18:00' },
      }
    },

    // Step 4: Location & Credentials
    area: { type: String, default: '' },
    pincode: { type: String, default: '' },
    plotNo: { type: String, default: '' },
    buildingName: { type: String, default: '' },
    streetName: { type: String, default: '' },
    landmark: { type: String, default: '' },
    city: { type: String, default: '' },
    state: { type: String, default: '' },
    gstNumber: { type: String, default: '' },
    panNumber: { type: String, default: '' },

    listingType: {
      type: String,
      enum: ['free', 'promoted'],
      default: 'free',
      index: true
    },


    // Form completion status
    status: {
      type: String,
      enum: ['draft', 'submitted', 'active', 'inactive', 'expired'],
      default: 'draft'
    },

    currentStep: {
      type: String,
      enum: ['business', 'category', 'contact', 'location'],
      default: 'business'
    },
    progress: { type: Number, default: 0 },
    stepIndex: { type: Number, default: 1 },


    // Pricing/Payment (will be populated after pricing page)
    // selectedPlan: { type: String, default: null }, // e.g., 'basic', 'premium', 'enterprise'
    plan: {
      name: { type: String, default: null },  // basic, plus, max
      price: { type: Number, default: 0 },
      durationDays: { type: Number, default: 0 },
      activatedAt: { type: Date, default: null }
    },




    paymentStatus: {
      type: String,
      enum: ['not_required', 'pending', 'paid', 'expired', 'failed'],
      default: 'not_required'
    },

    paymentId: { type: String, default: null },

    visibility: {
      priorityScore: { type: Number, default: 10 },
      impressions: { type: Number, default: 0 },
      clicks: { type: Number, default: 0 },
      leads: { type: Number, default: 0 },
      callClicks: { type: Number, default: 0 },
      whatsappClicks: { type: Number, default: 0 }
    },


    media: [
      {
        url: String,
        publicId: String, // if using cloud storage
        uploadedAt: {
          type: Date,
          default: Date.now
        }
      }
    ],




    // Promotion visibility
    isActive: { type: Boolean, default: false },
    expiryDate: { type: Date, default: null },
  },
  { timestamps: true }
);

// Index for efficient querying
BusinessPromotionSchema.index({ userId: 1, status: 1 });
BusinessPromotionSchema.index({ isActive: 1, expiryDate: 1 });
BusinessPromotionSchema.index({ city: 1, category: 1, listingType: 1 });
BusinessPromotionSchema.index({ 'visibility.priorityScore': -1 });


export default models.BusinessPromotion || model("BusinessPromotion", BusinessPromotionSchema);


// draft → form in progress

// submitted → free submitted OR paid waiting payment

// active → visible in listing

// inactive → admin disabled / user paused

// expired → paid plan ended