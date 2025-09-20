import { Router, Request, Response } from 'express';
import mongoose from 'mongoose';
import Group from '../models/Group';
import User from '../models/User';
import { requireAuth, AuthReq } from '../middleware/auth';
import { sendMessageNotification } from '../services/pushNotifications';

const router = Router();

// GET /api/groups - Get all groups for the current user
router.get('/', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    const groups = await Group.find({
      members: userId
    }).populate('members', 'name phone profilePicture')
      .populate('admin', 'name phone')
      .sort({ updatedAt: -1 });

    res.json({
      success: true,
      groups
    });
  } catch (error) {
    console.error('Get groups error:', error);
    res.status(500).json({ error: 'Failed to fetch groups' });
  }
});

// POST /api/groups - Create a new group
router.post('/', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { name, description, memberIds, icon } = req.body;
    const adminId = req.userId;

    if (!name || !memberIds || !Array.isArray(memberIds)) {
      return res.status(400).json({ 
        error: 'Group name and member IDs are required' 
      });
    }

    // Verify all member IDs exist
    const members = await User.find({
      _id: { $in: [...memberIds, adminId] }
    });

    if (members.length !== memberIds.length + 1) {
      return res.status(400).json({ 
        error: 'Some member IDs are invalid' 
      });
    }

    // Generate unique invite code
    let inviteCode: string;
    let isUnique = false;
    
    while (!isUnique) {
      inviteCode = Math.random().toString(36).substring(2, 8).toUpperCase();
      const existingGroup = await Group.findOne({ inviteCode });
      if (!existingGroup) {
        isUnique = true;
      }
    }

    const group = await Group.create({
      name: name.trim(),
      description: description?.trim() || '',
      icon: icon || '',
      members: [...memberIds, adminId],
      admin: adminId,
      inviteCode: inviteCode!,
      createdAt: new Date(),
      updatedAt: new Date()
    });

    // Populate the group data
    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phone profilePicture')
      .populate('admin', 'name phone');

    // Send notifications to all members except admin
    if (populatedGroup && memberIds.length > 0) {
      try {
        const adminData = populatedGroup.admin as any;
        const adminName = (adminData && adminData.name) ? String(adminData.name) : 'Someone';
        
        for (const memberId of memberIds) {
          try {
            const member = await User.findById(memberId);
            if (member?.pushToken && member.pushToken !== 'expo-go-local-mode') {
              await sendMessageNotification(
                member.pushToken,
                `New Group: ${group.name}`,
                `${adminName} added you to a new group`,
                JSON.stringify({ 
                  type: 'group_invite',
                  groupId: group._id?.toString() || '',
                  groupName: group.name,
                  adminId: adminId?.toString() || ''
                })
              );
            }
          } catch (memberError) {
            console.error(`Failed to send notification to member ${memberId}:`, memberError);
          }
        }
      } catch (notificationError) {
        console.error('Failed to send group notifications:', notificationError);
      }
    }

    res.status(201).json({
      success: true,
      group: populatedGroup,
      inviteCode: inviteCode!
    });
  } catch (error) {
    console.error('Create group error:', error);
    res.status(500).json({ error: 'Failed to create group' });
  }
});

// POST /api/groups/join - Join a group using invite code
router.post('/join', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const { inviteCode } = req.body;
    const userId = req.userId;

    if (!inviteCode) {
      return res.status(400).json({ error: 'Invite code is required' });
    }

    if (!userId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    const group = await Group.findOne({ inviteCode: inviteCode.toUpperCase() });
    if (!group) {
      return res.status(404).json({ error: 'Invalid invite code' });
    }

    // Convert userId to ObjectId for comparison
    const userObjectId = new mongoose.Types.ObjectId(userId);
    
    if (group.members.some(memberId => memberId.equals(userObjectId))) {
      return res.status(400).json({ error: 'You are already a member of this group' });
    }

    // Add user to group
    group.members.push(userObjectId);
    group.updatedAt = new Date();
    await group.save();

    // Populate the updated group
    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phone profilePicture')
      .populate('admin', 'name phone');

    res.json({
      success: true,
      group: populatedGroup,
      message: 'Successfully joined the group'
    });
  } catch (error) {
    console.error('Join group error:', error);
    res.status(500).json({ error: 'Failed to join group' });
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

    const userObjectId = new mongoose.Types.ObjectId(userId);
    
    // Check if user is a member of the group
    if (!group.members.some(memberId => memberId.equals(userObjectId))) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    // If user is admin and there are other members, transfer admin or delete group
    if (group.admin.equals(userObjectId)) {
      const otherMembers = group.members.filter(memberId => !memberId.equals(userObjectId));
      
      if (otherMembers.length > 0) {
        // Transfer admin to first remaining member
        group.admin = otherMembers[0];
        group.members = otherMembers;
        group.updatedAt = new Date();
        await group.save();

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
      group.members = group.members.filter(memberId => !memberId.equals(userObjectId));
      group.updatedAt = new Date();
      await group.save();

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
    
    // Remove user from all groups they're a member of
    await Group.updateMany(
      { members: userId },
      { $pull: { members: userId } }
    );
    
    // Delete groups where user was admin and now has no members
    await Group.deleteMany({
      admin: userId,
      members: { $size: 0 }
    });
    
    // Transfer admin for groups where user was admin but has other members
    const adminGroups = await Group.find({
      admin: userId,
      members: { $exists: true, $not: { $size: 0 } }
    });
    
    for (const group of adminGroups) {
      if (group.members.length > 0) {
        group.admin = group.members[0];
        await group.save();
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

export default router;