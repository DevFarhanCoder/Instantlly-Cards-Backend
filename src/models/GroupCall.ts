import mongoose, { Document, Schema } from 'mongoose';

export interface IGroupCall extends Document {
  callId: string;
  groupId: mongoose.Types.ObjectId;
  initiatorId: mongoose.Types.ObjectId;
  callType: 'audio' | 'video';
  startTime: Date;
  endTime?: Date;
  status: 'ringing' | 'active' | 'ended';
  participants: Array<{
    userId: mongoose.Types.ObjectId;
    joinedAt: Date;
    leftAt?: Date;
    isInitiator: boolean;
  }>;
  duration?: number; // in seconds
  createdAt: Date;
  updatedAt: Date;
}

const GroupCallSchema = new Schema<IGroupCall>({
  callId: {
    type: String,
    required: true,
    unique: true,
    index: true
  },
  groupId: {
    type: Schema.Types.ObjectId,
    ref: 'Group',
    required: true,
    index: true
  },
  initiatorId: {
    type: Schema.Types.ObjectId,
    ref: 'User',
    required: true
  },
  callType: {
    type: String,
    enum: ['audio', 'video'],
    required: true,
    default: 'audio'
  },
  startTime: {
    type: Date,
    required: true,
    default: Date.now
  },
  endTime: {
    type: Date
  },
  status: {
    type: String,
    enum: ['ringing', 'active', 'ended'],
    required: true,
    default: 'ringing'
  },
  participants: [{
    userId: {
      type: Schema.Types.ObjectId,
      ref: 'User',
      required: true
    },
    joinedAt: {
      type: Date,
      required: true,
      default: Date.now
    },
    leftAt: {
      type: Date
    },
    isInitiator: {
      type: Boolean,
      required: true,
      default: false
    }
  }],
  duration: {
    type: Number // Duration in seconds
  }
}, {
  timestamps: true,
  collection: 'group_calls'
});

// Indexes for performance
GroupCallSchema.index({ groupId: 1, createdAt: -1 });
GroupCallSchema.index({ initiatorId: 1, createdAt: -1 });
GroupCallSchema.index({ status: 1, createdAt: -1 });
GroupCallSchema.index({ 'participants.userId': 1 });

// Methods
GroupCallSchema.methods.addParticipant = function(userId: string) {
  const existingParticipant = this.participants.find(
    (p: any) => p.userId.toString() === userId
  );
  
  if (!existingParticipant) {
    this.participants.push({
      userId: new mongoose.Types.ObjectId(userId),
      joinedAt: new Date(),
      isInitiator: false
    });
  }
  
  return this.save();
};

GroupCallSchema.methods.removeParticipant = function(userId: string) {
  const participant = this.participants.find(
    (p: any) => p.userId.toString() === userId && !p.leftAt
  );
  
  if (participant) {
    participant.leftAt = new Date();
  }
  
  return this.save();
};

GroupCallSchema.methods.endCall = function() {
  this.status = 'ended';
  this.endTime = new Date();
  
  // Mark all active participants as left
  this.participants.forEach((p: any) => {
    if (!p.leftAt) {
      p.leftAt = this.endTime;
    }
  });
  
  // Calculate duration
  if (this.startTime && this.endTime) {
    this.duration = Math.floor((this.endTime.getTime() - this.startTime.getTime()) / 1000);
  }
  
  return this.save();
};

// Static methods
GroupCallSchema.statics.getActiveCallForGroup = function(groupId: string) {
  return this.findOne({
    groupId: new mongoose.Types.ObjectId(groupId),
    status: { $in: ['ringing', 'active'] }
  }).populate('initiatorId', 'name profilePicture')
    .populate('participants.userId', 'name profilePicture');
};

GroupCallSchema.statics.getCallHistory = function(groupId: string, limit: number = 10) {
  return this.find({
    groupId: new mongoose.Types.ObjectId(groupId),
    status: 'ended'
  })
  .populate('initiatorId', 'name profilePicture')
  .populate('participants.userId', 'name profilePicture')
  .sort({ createdAt: -1 })
  .limit(limit);
};

const GroupCall = mongoose.model<IGroupCall>('GroupCall', GroupCallSchema);

export default GroupCall;