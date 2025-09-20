import { Router, Request, Response } from 'express';
import { Types } from 'mongoose';
import Group from '../models/Group';
import User from '../models/User';
import { requireAuth, AuthReq } from '../middleware/auth';
import { sendMessageNotification } from '../services/pushNotifications';

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
      .populate('admin', 'name phone');

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

    if (!adminId) {
      return res.status(401).json({ error: 'User not authenticated' });
    }

    if (!name || !memberIds || !Array.isArray(memberIds)) {
      return res.status(400).json({ 
        error: 'Group name and member IDs are required' 
      });
    }

    // Convert to ObjectIds
    const memberObjectIds = memberIds.map((id: string) => new Types.ObjectId(id));
    const adminObjectId = new Types.ObjectId(adminId);
    const allMemberIds = [...memberObjectIds, adminObjectId];

    // Verify all member IDs exist
    const members = await User.find({ _id: { $in: allMemberIds } });

    if (members.length !== allMemberIds.length) {
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
      members: allMemberIds,
      admin: adminObjectId,
      inviteCode: inviteCode!
    });

    // Populate the group data
    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phone profilePicture')
      .populate('admin', 'name phone');

    // Send notifications to all members except admin
    if (memberIds.length > 0) {
      try {
        const admin = await User.findById(adminId);
        const adminName = admin?.name || 'Someone';
        
        for (const memberId of memberIds) {
          try {
            const member = await User.findById(new Types.ObjectId(memberId));
            if (member?.pushToken && member.pushToken !== 'expo-go-local-mode') {
              await sendMessageNotification(
                member.pushToken,
                `New Group: ${group.name}`,
                `${adminName} added you to a new group`,
                JSON.stringify({ 
                  type: 'group_invite',
                  groupId: group._id,
                  groupName: group.name,
                  adminId: adminId
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

export default router;