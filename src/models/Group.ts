import mongoose, { Schema, Document } from "mongoose";

export interface IGroup extends Document {
  name: string;
  description?: string;
  icon?: string;
  members: mongoose.Types.ObjectId[];
  admin: mongoose.Types.ObjectId;
  inviteCode: string; // 6-character alphanumeric code for joining
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
  inviteCode: {
    type: String,
    required: true,
    unique: true,
    length: 6
  }
}, {
  timestamps: true
});

// Generate a unique 6-character invite code
GroupSchema.statics.generateInviteCode = async function(): Promise<string> {
  let inviteCode: string;
  let isUnique = false;
  
  while (!isUnique) {
    // Generate 6-character alphanumeric code
    inviteCode = Math.random().toString(36).substring(2, 8).toUpperCase();
    
    // Check if this code already exists
    const existingGroup = await this.findOne({ inviteCode });
    if (!existingGroup) {
      isUnique = true;
    }
  }
  
  return inviteCode!;
};

export default mongoose.model<IGroup>("Group", GroupSchema);