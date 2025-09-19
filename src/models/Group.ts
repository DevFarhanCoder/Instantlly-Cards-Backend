import mongoose, { Schema, Document } from "mongoose";

export interface IGroup extends Document {
  name: string;
  description?: string;
  icon?: string;
  members: mongoose.Types.ObjectId[];
  admin: mongoose.Types.ObjectId;
  joinCode: string; // 6-digit code for joining
  createdAt: Date;
  updatedAt: Date;
}

const GroupSchema = new Schema<IGroup>({
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
    length: 6,
    index: true
  }
}, {
  timestamps: true
});

// Ensure join codes are unique
GroupSchema.index({ joinCode: 1 }, { unique: true });

// Generate a unique 6-digit join code
GroupSchema.statics.generateJoinCode = async function(): Promise<string> {
  let joinCode: string;
  let isUnique = false;
  
  while (!isUnique) {
    // Generate 6-digit random number
    joinCode = Math.floor(100000 + Math.random() * 900000).toString();
    
    // Check if this code already exists
    const existingGroup = await this.findOne({ joinCode });
    if (!existingGroup) {
      isUnique = true;
    }
  }
  
  return joinCode!;
};

export default mongoose.model<IGroup>("Group", GroupSchema);