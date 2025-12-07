import { Router, Request, Response } from 'express';
import { Types } from 'mongoose';
import Group from '../models/Group';
import User from '../models/User';
import GroupCall from '../models/GroupCall';
import { requireAuth, AuthReq } from '../middleware/auth';
import { sendMessageNotification, sendGroupInviteNotification } from '../services/pushNotifications';

const router = Router();

// GET /api/groups - Get all groups for the current user
router.get('/', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }
    
    const groups = await Group.find({
      members: new Types.ObjectId(userId)
    }).populate('members', 'name phone profilePicture')
      .populate('admin', 'name phone')
      .populate('adminTransferInfo.previousAdmin', 'name');

    // Add transfer info for new admins
    const groupsWithTransferInfo = groups.map(group => {
      const groupObj: any = group.toObject();
      
      // If current user is the admin and there's unseen transfer info
      if (groupObj.admin?._id && groupObj.admin._id.toString() === userId && 
          groupObj.adminTransferInfo && !groupObj.adminTransferInfo.seen) {
        const previousAdmin: any = groupObj.adminTransferInfo.previousAdmin;
        groupObj.adminTransferredBy = previousAdmin?.name || 'Someone';
        groupObj.showAdminTransfer = true;
      }
      
      return groupObj;
    });

    res.json({
      success: true,
      groups: groupsWithTransferInfo
    });
  } catch (error) {
    console.error('Get groups error:', error);
    res.status(500).json({ error: 'Failed to fetch groups' });
  }
});

// POST /api/groups - Create a new group
router.post('/', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    console.log('🔍 Group creation started - Request body:', JSON.stringify(req.body, null, 2));
    const { name, description, memberIds, members, icon } = req.body;
    const adminId = req.userId;

    console.log('🔍 Admin ID:', adminId);

    // Check if icon is a local file path and ignore it
    let groupIcon = '';
    if (icon && !icon.startsWith('file://') && !icon.startsWith('/data/')) {
      groupIcon = icon;
    }
    console.log('🔍 Group icon:', groupIcon || 'No icon (local path ignored)');

    if (!adminId) {
      console.log('❌ No admin ID found');
      return res.status(401).json({ error: 'User not authenticated' });
    }

    // Support both 'memberIds' and 'members' field names for compatibility
    const memberList = memberIds || members;
    console.log('🔍 Member list:', memberList);

    if (!name || !memberList || !Array.isArray(memberList)) {
      console.log('❌ Validation failed - name:', name, 'memberList:', memberList);
      return res.status(400).json({ 
        error: 'Group name and member IDs are required' 
      });
    }

    console.log('🔍 Converting member IDs to ObjectIds...');
    // Convert to ObjectIds
    const memberObjectIds = memberList.map((id: string) => new Types.ObjectId(id));
    const adminObjectId = new Types.ObjectId(adminId);
    const allMemberIds = [...memberObjectIds, adminObjectId];

    console.log('🔍 All member IDs:', allMemberIds.map(id => id.toString()));

    // Verify all member IDs exist
    console.log('🔍 Verifying members exist in database...');
    const foundUsers = await User.find({ _id: { $in: allMemberIds } });

    console.log('🔍 Found users:', foundUsers.length, 'Expected:', allMemberIds.length);
    
    if (foundUsers.length !== allMemberIds.length) {
      console.log('❌ Some member IDs are invalid');
      console.log('Expected IDs:', allMemberIds.map(id => id.toString()));
      console.log('Found users:', foundUsers.map(user => user._id.toString()));
      return res.status(400).json({ 
        error: 'Some member IDs are invalid' 
      });
    }

    console.log('🔍 Generating invite code...');

    // Generate unique invite code using the model's static method
    let joinCode;
    try {
      joinCode = await Group.generateInviteCode();
      console.log('✅ Generated invite code:', joinCode);
    } catch (error) {
      console.error('❌ Error generating invite code:', error);
      // Fallback code generation
      const timestamp = Date.now().toString(36).toUpperCase();
      joinCode = timestamp.substring(timestamp.length - 6);
      console.log('✅ Fallback invite code:', joinCode);
    }

    console.log('🔍 Creating group with data:', {
      name: name.trim(),
      description: description?.trim() || '',
      icon: groupIcon,
      members: allMemberIds.map(id => id.toString()),
      admin: adminObjectId.toString(),
      joinCode: joinCode
    });

    const group = await Group.create({
      name: name.trim(),
      description: description?.trim() || '',
      icon: groupIcon,
      members: allMemberIds,
      admin: adminObjectId,
      joinCode: joinCode
    });

    console.log('✅ Group created successfully with ID:', group._id);
    console.log('✅ Group joinCode after creation:', group.joinCode);

    // Populate the group data
    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phone profilePicture')
      .populate('admin', 'name phone');



      // Send notifications to all members except admin (don't let this fail group creation)
    if (memberList.length > 0) {
      // Run notifications in background - don't await
      setImmediate(async () => {
        try {
          const admin = await User.findById(adminId);
          const adminName = admin?.name || 'Someone';
          
          for (const memberId of memberList) {
            try {
              const member = await User.findById(new Types.ObjectId(memberId));
              if (member?.pushToken && member.pushToken !== 'expo-go-local-mode') {
                await sendGroupInviteNotification(
                  member.pushToken,
                  adminName,
                  group.name,
                  (group._id as any).toString(),
                  adminId
                );
                console.log(`📱 Group invite notification sent to ${member.name}`);
              } else if (member?.pushToken === 'expo-go-local-mode') {
                console.log(`📱 Member ${member.name} is using Expo Go - notification will be handled locally`);
              } else {
                console.log(`📱 No push token for member ${member.name}, skipping push notification`);
              }
            } catch (memberError) {
              console.error(`Failed to send notification to member ${memberId}:`, memberError);
            }
          }
        } catch (notificationError) {
          console.error('Failed to send group notifications:', notificationError);
        }
      });
    }

    res.status(201).json({
      success: true,
      group: populatedGroup,
      joinCode: populatedGroup?.joinCode || joinCode // Ensure joinCode is always returned
    });
  } catch (error) {
    console.error('❌ Create group error:', error);
    console.error('❌ Error stack:', error instanceof Error ? error.stack : 'No stack trace');
    res.status(500).json({ error: 'Failed to create group' });
  }
});

// POST /api/groups/join - Join a group using invite code
router.post('/join', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { inviteCode, joinCode } = req.body;
    const userId = req.userId;

    // Support both field names for backward compatibility
    const code = inviteCode || joinCode;

    if (!code) {
      return res.status(400).json({ error: 'Invite code is required' });
    }

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    const group = await Group.findOne({ joinCode: code.toUpperCase() });
    if (!group) {
      return res.status(404).json({ error: 'Invalid invite code' });
    }

    if (group.members.includes(new Types.ObjectId(userId))) {
      return res.status(400).json({ error: 'You are already a member of this group' });
    }

    // Add user to group
    const updatedGroup = await Group.findByIdAndUpdate(
      group._id, 
      {
        $push: { members: new Types.ObjectId(userId) },
        updatedAt: new Date()
      },
      { new: true }
    ).populate('members', 'name phone profilePicture')
     .populate('admin', 'name phone');

    if (!updatedGroup) {
      return res.status(500).json({ error: 'Failed to join group' });
    }

    res.json({
      success: true,
      group: updatedGroup,
      message: 'Successfully joined the group'
    });
  } catch (error) {
    console.error('Join group error:', error);
    res.status(500).json({ error: 'Failed to join group' });
  }
});

// PUT /api/groups/:id/transfer-admin - Transfer admin rights to another member
router.put('/:id/transfer-admin', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;
    const { newAdminId } = req.body;

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    if (!newAdminId) {
      return res.status(400).json({ error: 'New admin ID is required' });
    }

    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    // Check if current user is the admin
    if (group.admin.toString() !== userId) {
      return res.status(403).json({ error: 'Only group admin can transfer admin rights' });
    }

    // Check if new admin is a member of the group
    const isMember = group.members.some(memberId => memberId.toString() === newAdminId);
    if (!isMember) {
      return res.status(400).json({ error: 'New admin must be a member of the group' });
    }

    // Transfer admin rights and store transfer info
    group.adminTransferInfo = {
      previousAdmin: new Types.ObjectId(userId),
      transferredAt: new Date(),
      seen: false
    };
    group.admin = new Types.ObjectId(newAdminId);
    group.updatedAt = new Date();
    await group.save();

    // Get user details for notification
    const newAdmin = await User.findById(newAdminId).select('name');
    const currentAdmin = await User.findById(userId).select('name');

    console.log(`✅ Admin transferred from ${currentAdmin?.name} to ${newAdmin?.name} in group ${group.name}`);

    // Send Socket.IO notification to new admin
    const io = (req as any).io;
    console.log('🔌 Socket.IO instance available:', !!io);
    console.log('📤 Attempting to send notification to user ID:', newAdminId);
    
    if (io && newAdmin && group._id) {
      const notification = {
        type: 'admin_transfer',
        groupId: String(group._id),
        groupName: group.name,
        fromUser: currentAdmin?.name || 'Unknown',
        message: `You are now the admin of "${group.name}"`,
        timestamp: new Date().toISOString()
      };
      
      // Send to specific user room
      io.to(newAdminId).emit('admin_transferred', notification);
      console.log(`🔔 Sent admin_transferred event to room: ${newAdminId}`);
      console.log(`📋 Notification payload:`, notification);
      
      // Also broadcast to all connected clients for debugging
      io.emit('admin_transferred_debug', { ...notification, targetUserId: newAdminId });
      console.log(`🔔 Also sent broadcast for debugging`);
    } else {
      console.warn('⚠️ Cannot send notification - missing requirements:', {
        hasIo: !!io,
        hasNewAdmin: !!newAdmin,
        hasGroupId: !!group._id
      });
    }

    res.json({
      success: true,
      message: `Admin rights transferred to ${newAdmin?.name}`,
      transferredBy: currentAdmin?.name || 'Unknown',
      isNewAdmin: userId !== newAdminId, // true if the requester is NOT the new admin
      group: await Group.findById(groupId)
        .populate('members', 'name phone profilePicture')
        .populate('admin', 'name phone')
    });
  } catch (error) {
    console.error('Transfer admin error:', error);
    res.status(500).json({ error: 'Failed to transfer admin rights' });
  }
});

// PUT /api/groups/:id/mark-transfer-seen - Mark admin transfer notification as seen
router.put('/:id/mark-transfer-seen', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    // Only the current admin can mark the transfer as seen
    if (group.admin.toString() === userId && group.adminTransferInfo) {
      group.adminTransferInfo.seen = true;
      await group.save();
      console.log(`✅ Marked admin transfer as seen for group: ${group.name}`);
    }

    res.json({ success: true });
  } catch (error) {
    console.error('Mark transfer seen error:', error);
    res.status(500).json({ error: 'Failed to mark transfer as seen' });
  }
});

// DELETE /api/groups/:id - Delete or leave a group
router.delete('/:id', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    // Check if user is a member of the group
    if (!group.members.includes(new Types.ObjectId(userId))) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    // If user is admin and there are other members, transfer admin or delete group
    if (group.admin.toString() === userId) {
      const otherMembers = group.members.filter(memberId => memberId.toString() !== userId);
      
      if (otherMembers.length > 0) {
        // Transfer admin to first remaining member
        await Group.findByIdAndUpdate(groupId, {
          admin: otherMembers[0],
          members: otherMembers,
          updatedAt: new Date()
        });

        return res.json({
          success: true,
          message: 'You left the group and admin was transferred',
          action: 'left'
        });
      } else {
        // Delete the group if admin is the only member
        await Group.findByIdAndDelete(groupId);
        
        return res.json({
          success: true,
          message: 'Group deleted successfully',
          action: 'deleted'
        });
      }
    } else {
      // Regular member leaving
      await Group.findByIdAndUpdate(groupId, {
        $pull: { members: new Types.ObjectId(userId) },
        updatedAt: new Date()
      });

      return res.json({
        success: true,
        message: 'You left the group successfully',
        action: 'left'
      });
    }
  } catch (error) {
    console.error('Delete group error:', error);
    res.status(500).json({ error: 'Failed to delete/leave group' });
  }
});

// DELETE /api/groups - Clear all groups (for cleanup)
router.delete('/', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }
    
    // Get all groups where user is a member
    const userGroups = await Group.find({ members: new Types.ObjectId(userId) });
    
    for (const group of userGroups) {
      if (group.admin.toString() === userId) {
        // If user is admin, delete the group entirely
        await Group.findByIdAndDelete(group._id);
      } else {
        // If user is just a member, remove them from the group
        await Group.findByIdAndUpdate(group._id, {
          $pull: { members: new Types.ObjectId(userId) },
          updatedAt: new Date()
        });
      }
    }

    res.json({
      success: true,
      message: 'All groups cleared successfully'
    });
  } catch (error) {
    console.error('Clear groups error:', error);
    res.status(500).json({ error: 'Failed to clear groups' });
  }
});

// Group Calling Endpoints

// POST /api/groups/:id/call/initiate - Initiate a group call
router.post('/:id/call/initiate', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;
    const { callType } = req.body; // 'audio' or 'video'

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    // Verify group exists and user is a member
    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    const isMember = group.members.some(memberId => memberId.toString() === userId);
    if (!isMember) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    // Get user details
    const caller = await User.findById(userId).select('name profilePicture');
    if (!caller) {
      return res.status(404).json({ error: 'User not found' });
    }

    // Generate call session data
    const callSession = {
      callId: new Types.ObjectId().toString(),
      groupId: groupId,
      initiatorId: userId,
      initiatorName: caller.name,
      callType: callType || 'audio',
      startTime: new Date(),
      participants: [],
      status: 'ringing'
    };

    // For now, we'll emit the call initiation via Socket.IO
    // In a production system, you'd want to store active calls in Redis or MongoDB
    
    console.log(`📞 Group call initiated: ${caller.name} → ${group.name} (${callType})`);

    res.json({
      success: true,
      message: 'Group call initiated successfully',
      callSession: callSession
    });
  } catch (error) {
    console.error('Initiate group call error:', error);
    res.status(500).json({ error: 'Failed to initiate group call' });
  }
});

// POST /api/groups/:id/call/join - Join an ongoing group call
router.post('/:id/call/join', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;
    const { callId } = req.body;

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    // Verify group exists and user is a member
    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    const isMember = group.members.some(memberId => memberId.toString() === userId);
    if (!isMember) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    // Get user details
    const user = await User.findById(userId).select('name profilePicture');
    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    console.log(`📞 User ${user.name} joining group call in ${group.name}`);

    res.json({
      success: true,
      message: 'Successfully joined group call',
      participant: {
        userId: userId,
        name: user.name,
        profilePicture: user.profilePicture,
        joinedAt: new Date()
      }
    });
  } catch (error) {
    console.error('Join group call error:', error);
    res.status(500).json({ error: 'Failed to join group call' });
  }
});

// POST /api/groups/:id/call/leave - Leave an ongoing group call
router.post('/:id/call/leave', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;
    const { callId } = req.body;

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    // Get user details for logging
    const user = await User.findById(userId).select('name');
    console.log(`📞 User ${user?.name} leaving group call`);

    res.json({
      success: true,
      message: 'Successfully left group call',
      leftAt: new Date()
    });
  } catch (error) {
    console.error('Leave group call error:', error);
    res.status(500).json({ error: 'Failed to leave group call' });
  }
});

// POST /api/groups/:id/call/end - End a group call (only initiator or admin)
router.post('/:id/call/end', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;
    const { callId } = req.body;

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    // Verify group exists and user has permission to end call
    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    const isAdmin = group.admin.toString() === userId;
    // In a real system, you'd also check if user is the call initiator
    
    if (!isAdmin) {
      return res.status(403).json({ error: 'Only group admin can end calls' });
    }

    console.log(`📞 Group call ended in ${group.name}`);

    res.json({
      success: true,
      message: 'Group call ended successfully',
      endedAt: new Date()
    });
  } catch (error) {
    console.error('End group call error:', error);
    res.status(500).json({ error: 'Failed to end group call' });
  }
});

// GET /api/groups/:id/call/status - Get current call status for a group
router.get('/:id/call/status', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const groupId = req.params.id;
    const userId = req.userId;

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    // Verify group exists and user is a member
    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    const isMember = group.members.some(memberId => memberId.toString() === userId);
    if (!isMember) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    // In a real system, you'd check Redis or active call storage
    // For now, return no active call
    res.json({
      success: true,
      hasActiveCall: false,
      callSession: null
    });
  } catch (error) {
    console.error('Get call status error:', error);
    res.status(500).json({ error: 'Failed to get call status' });
  }
});

export default router;