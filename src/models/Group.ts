import mongoose, { Schema, Document, Model } from "mongoose";

export interface IGroup extends Document {
  name: string;
  description?: string;
  icon?: string;
  members: mongoose.Types.ObjectId[];
  admin: mongoose.Types.ObjectId;
  joinCode: string; // 6-character alphanumeric code for joining
  createdAt: Date;
  updatedAt: Date;
}

export interface IGroupModel extends Model<IGroup> {
  generateInviteCode(): Promise<string>;
}

const GroupSchema = new Schema<IGroup, IGroupModel>({
  name: {
    type: String,
    required: true,
    trim: true,
    maxlength: 100
  },
  description: {
    type: String,
    trim: true,
    maxlength: 500
  },
  icon: {
    type: String,
    trim: true
  },
  members: [{
    type: Schema.Types.ObjectId,
    ref: "User",
    required: true
  }],
  admin: {
    type: Schema.Types.ObjectId,
    ref: "User",
    required: true
  },
  joinCode: {
    type: String,
    required: true,
    unique: true,
    sparse: true, // Ignore null values for uniqueness
    minlength: 6,
    maxlength: 6
  }
}, {
  timestamps: true
});

// Pre-save hook to ensure joinCode is always set
GroupSchema.pre('save', async function(next) {
  if (!this.joinCode) {
    try {
      this.joinCode = await (this.constructor as any).generateInviteCode();
    } catch (error) {
      console.error('Error generating joinCode in pre-save hook:', error);
      // Fallback to timestamp-based code if generation fails
      const timestamp = Date.now().toString(36).toUpperCase();
      this.joinCode = timestamp.substring(timestamp.length - 6);
    }
  }
  next();
});

// Generate a unique 6-character invite code
GroupSchema.statics.generateInviteCode = async function(): Promise<string> {
  let joinCode: string;
  let attempts = 0;
  const maxAttempts = 10;
  
  while (attempts < maxAttempts) {
    // Generate 6-character alphanumeric code
    joinCode = Math.random().toString(36).substring(2, 8).toUpperCase();
    
    // Ensure it's exactly 6 characters by padding if necessary
    while (joinCode.length < 6) {
      joinCode += Math.random().toString(36).substring(2, 3).toUpperCase();
    }
    joinCode = joinCode.substring(0, 6);
    
    try {
      // Check if this code already exists
      const existingGroup = await this.findOne({ joinCode });
      if (!existingGroup) {
        return joinCode;
      }
    } catch (error) {
      console.error('Error checking existing joinCode:', error);
    }
    
    attempts++;
  }
  
  // Fallback: if we couldn't generate a unique code after maxAttempts, 
  // use timestamp-based code
  const timestamp = Date.now().toString(36).toUpperCase();
  return timestamp.substring(timestamp.length - 6);
};

export default mongoose.model<IGroup, IGroupModel>("Group", GroupSchema);