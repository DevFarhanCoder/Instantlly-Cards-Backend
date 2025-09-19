import express from "express";
import Group, { IGroup } from "../models/Group";
import User from "../models/User";
import Notification from "../models/Notification";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = express.Router();

// Create a new group
router.post("/create", requireAuth, async (req: AuthReq, res) => {
  try {
    const { name, description, icon, members } = req.body;
    const adminId = req.userId;

    if (!adminId) {
      return res.status(401).json({ message: "Unauthorized" });
    }

    if (!name || !name.trim()) {
      return res.status(400).json({ message: "Group name is required" });
    }

    // Generate unique join code
    const joinCode = await (Group as any).generateJoinCode();

    // Create the group
    const group = new Group({
      name: name.trim(),
      description: description?.trim(),
      icon,
      members: [adminId, ...(members || [])], // Admin is always a member
      admin: adminId,
      joinCode
    });

    await group.save();

    // Send notifications to added members (excluding admin)
    const memberIds = members || [];
    if (memberIds.length > 0) {
      const admin = await User.findById(adminId);
      const adminName = admin?.name || "Someone";

      const notifications = memberIds.map((memberId: string) => ({
        userId: memberId,
        type: 'GENERAL',
        title: 'Added to Group',
        message: `${adminName} added you to the group "${name}"`,
        data: {
          groupId: group._id,
          groupName: name,
          adminId: adminId,
          adminName: adminName
        }
      }));

      await Notification.insertMany(notifications);
    }

    // Populate the group data for response
    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phoneNumber profilePicture')
      .populate('admin', 'name phoneNumber profilePicture');

    res.status(201).json({
      message: "Group created successfully",
      group: populatedGroup
    });

  } catch (error) {
    console.error("Error creating group:", error);
    res.status(500).json({ message: "Failed to create group" });
  }
});

// Join group by code
router.post("/join", requireAuth, async (req: AuthReq, res) => {
  try {
    const { joinCode } = req.body;
    const userId = req.userId;

    if (!userId) {
      return res.status(401).json({ message: "Unauthorized" });
    }

    if (!joinCode || joinCode.length !== 6) {
      return res.status(400).json({ message: "Invalid join code" });
    }

    // Find group by join code
    const group = await Group.findOne({ joinCode });
    if (!group) {
      return res.status(404).json({ message: "Group not found with this code" });
    }

    // Check if user is already a member
    if (group.members.includes(userId as any)) {
      return res.status(400).json({ message: "You are already a member of this group" });
    }

    // Add user to group
    group.members.push(userId as any);
    await group.save();

    // Get user info for notification
    const user = await User.findById(userId);
    const userName = user?.name || "Someone";

    // Notify group admin
    await Notification.create({
      userId: group.admin,
      type: 'GENERAL',
      title: 'New Group Member',
      message: `${userName} joined your group "${group.name}"`,
      data: {
        groupId: group._id,
        groupName: group.name,
        newMemberId: userId,
        newMemberName: userName
      }
    });

    // Populate the group data for response
    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phoneNumber profilePicture')
      .populate('admin', 'name phoneNumber profilePicture');

    res.status(200).json({
      message: "Successfully joined group",
      group: populatedGroup
    });

  } catch (error) {
    console.error("Error joining group:", error);
    res.status(500).json({ message: "Failed to join group" });
  }
});

// Get user's groups
router.get("/my-groups", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;

    if (!userId) {
      return res.status(401).json({ message: "Unauthorized" });
    }

    const groups = await Group.find({ members: userId })
      .populate('members', 'name phoneNumber profilePicture')
      .populate('admin', 'name phoneNumber profilePicture')
      .sort({ updatedAt: -1 });

    res.status(200).json({
      message: "Groups retrieved successfully",
      groups
    });

  } catch (error) {
    console.error("Error retrieving groups:", error);
    res.status(500).json({ message: "Failed to retrieve groups" });
  }
});

// Get group details
router.get("/:groupId", requireAuth, async (req: AuthReq, res) => {
  try {
    const { groupId } = req.params;
    const userId = req.userId;

    if (!userId) {
      return res.status(401).json({ message: "Unauthorized" });
    }

    const group = await Group.findById(groupId)
      .populate('members', 'name phoneNumber profilePicture')
      .populate('admin', 'name phoneNumber profilePicture');

    if (!group) {
      return res.status(404).json({ message: "Group not found" });
    }

    // Check if user is a member
    if (!group.members.some((member: any) => member._id.toString() === userId)) {
      return res.status(403).json({ message: "You are not a member of this group" });
    }

    res.status(200).json({
      message: "Group details retrieved successfully",
      group
    });

  } catch (error) {
    console.error("Error retrieving group details:", error);
    res.status(500).json({ message: "Failed to retrieve group details" });
  }
});

// Update group (admin only)
router.put("/:groupId", requireAuth, async (req: AuthReq, res) => {
  try {
    const { groupId } = req.params;
    const { name, description, icon } = req.body;
    const userId = req.userId;

    if (!userId) {
      return res.status(401).json({ message: "Unauthorized" });
    }

    const group = await Group.findById(groupId);
    if (!group) {
      return res.status(404).json({ message: "Group not found" });
    }

    // Check if user is admin
    if (group.admin.toString() !== userId) {
      return res.status(403).json({ message: "Only group admin can update group details" });
    }

    // Update group details
    if (name && name.trim()) group.name = name.trim();
    if (description !== undefined) group.description = description?.trim();
    if (icon !== undefined) group.icon = icon;

    await group.save();

    const populatedGroup = await Group.findById(group._id)
      .populate('members', 'name phoneNumber profilePicture')
      .populate('admin', 'name phoneNumber profilePicture');

    res.status(200).json({
      message: "Group updated successfully",
      group: populatedGroup
    });

  } catch (error) {
    console.error("Error updating group:", error);
    res.status(500).json({ message: "Failed to update group" });
  }
});

export default router;