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
    
    // Form completion status
    status: { 
      type: String, 
      enum: ['draft', 'completed', 'published'], 
      default: 'draft' 
    },
    currentStep: { 
      type: String, 
      enum: ['business', 'category', 'contact', 'location'], 
      default: 'business' 
    },
    
    // Pricing/Payment (will be populated after pricing page)
    selectedPlan: { type: String, default: null }, // e.g., 'basic', 'premium', 'enterprise'
    paymentStatus: { 
      type: String, 
      enum: ['pending', 'paid', 'failed'], 
      default: 'pending' 
    },
    paymentId: { type: String, default: null },
    
    // Promotion visibility
    isActive: { type: Boolean, default: false },
    expiryDate: { type: Date, default: null },
  },
  { timestamps: true }
);

// Index for efficient querying
BusinessPromotionSchema.index({ userId: 1, status: 1 });
BusinessPromotionSchema.index({ isActive: 1, expiryDate: 1 });

export default models.BusinessPromotion || model("BusinessPromotion", BusinessPromotionSchema);
