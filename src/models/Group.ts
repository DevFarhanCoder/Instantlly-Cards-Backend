import mongoose, { Schema, Document, Model } from "mongoose";

export interface IGroup extends Document {
  name: string;
  description?: string;
  icon?: string;
  members: mongoose.Types.ObjectId[];
  admin: mongoose.Types.ObjectId;
  joinCode: string; // 6-character alphanumeric code for joining
  lastMessage?: mongoose.Types.ObjectId;
  lastMessageTime: Date;
  isActive: boolean;
  mutedBy: mongoose.Types.ObjectId[];
  memberRoles: Map<string, 'admin' | 'member'>;
  chatId?: mongoose.Types.ObjectId; // Reference to chat document
  createdAt: Date;
  updatedAt: Date;
  addMember(userId: string): Promise<IGroup>;
  removeMember(userId: string): Promise<IGroup>;
  updateLastMessage(messageId: string): Promise<IGroup>;
}

export interface IGroupModel extends Model<IGroup> {
  generateInviteCode(): Promise<string>;
  findByJoinCode(joinCode: string): Promise<IGroup | null>;
  getUserGroups(userId: string): Promise<IGroup[]>;
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
  },
  lastMessage: {
    type: Schema.Types.ObjectId,
    ref: "Message"
  },
  lastMessageTime: {
    type: Date,
    default: Date.now
  },
  isActive: {
    type: Boolean,
    default: true
  },
  mutedBy: [{
    type: Schema.Types.ObjectId,
    ref: "User"
  }],
  memberRoles: {
    type: Map,
    of: String,
    enum: ['admin', 'member'],
    default: new Map()
  },
  chatId: {
    type: Schema.Types.ObjectId,
    ref: "Chat"
  }
}, {
  timestamps: true
});

// Indexes for better performance
GroupSchema.index({ members: 1 });
GroupSchema.index({ admin: 1 });
GroupSchema.index({ lastMessageTime: -1 });
GroupSchema.index({ isActive: 1 });

// Pre-save hook to ensure joinCode and admin role are set
GroupSchema.pre('save', async function(next) {
  // Generate joinCode if not present
  if (!this.joinCode || this.joinCode === '') {
    try {
      console.log('🔍 Generating joinCode in pre-save hook...');
      this.joinCode = await (this.constructor as any).generateInviteCode();
      console.log('✅ Generated joinCode:', this.joinCode);
    } catch (error) {
      console.error('Error generating joinCode in pre-save hook:', error);
      // Fallback to timestamp-based code if generation fails
      const timestamp = Date.now().toString(36).toUpperCase();
      this.joinCode = timestamp.substring(timestamp.length - 6);
      console.log('✅ Fallback joinCode:', this.joinCode);
    }
  }
  
  // Set admin role in memberRoles
  if (this.admin && (!this.memberRoles.has(this.admin.toString()) || this.memberRoles.get(this.admin.toString()) !== 'admin')) {
    this.memberRoles.set(this.admin.toString(), 'admin');
  }
  
  // Set default member roles for all members
  if (this.members) {
    this.members.forEach(memberId => {
      if (!this.memberRoles.has(memberId.toString())) {
        this.memberRoles.set(memberId.toString(), 'member');
      }
    });
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

// Find group by join code
GroupSchema.statics.findByJoinCode = function(joinCode: string) {
  return this.findOne({ joinCode: joinCode.toUpperCase(), isActive: true });
};

// Get all groups for a user
GroupSchema.statics.getUserGroups = function(userId: string) {
  return this.find({
    members: new mongoose.Types.ObjectId(userId),
    isActive: true
  })
  .populate('members', 'name profilePicture email')
  .populate('admin', 'name profilePicture email')
  .populate('lastMessage')
  .sort({ lastMessageTime: -1 });
};

// Instance method to add member
GroupSchema.methods.addMember = function(userId: string) {
  const userObjectId = new mongoose.Types.ObjectId(userId);
  
  if (!this.members.includes(userObjectId)) {
    this.members.push(userObjectId);
    this.memberRoles.set(userId, 'member');
  }
  
  return this.save();
};

// Instance method to remove member
GroupSchema.methods.removeMember = function(userId: string) {
  this.members = this.members.filter((memberId: any) => memberId.toString() !== userId);
  this.memberRoles.delete(userId);
  this.mutedBy = this.mutedBy.filter((mutedId: any) => mutedId.toString() !== userId);
  
  return this.save();
};

// Instance method to update last message
GroupSchema.methods.updateLastMessage = function(messageId: string) {
  this.lastMessage = new mongoose.Types.ObjectId(messageId);
  this.lastMessageTime = new Date();
  return this.save();
};

export default mongoose.model<IGroup, IGroupModel>("Group", GroupSchema);