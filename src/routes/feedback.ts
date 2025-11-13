import { Router } from "express";
import Feedback from "../models/Feedback";
import User from "../models/User";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

// Submit feedback
router.post("/", requireAuth, async (req: AuthReq, res) => {
  try {
    const { subject, message, rating } = req.body;
    const userId = req.userId;

    // Validate required fields
    if (!subject || !message) {
      return res.status(400).json({ 
        success: false, 
        message: "Subject and message are required" 
      });
    }

    // Get user details
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ 
        success: false, 
        message: "User not found" 
      });
    }

    // Create feedback
    const feedback = new Feedback({
      userId,
      name: user.name,
      phone: user.phone,
      email: user.email || "",
      subject,
      message,
      rating: rating || null,
      status: "pending"
    });

    await feedback.save();

    res.status(201).json({
      success: true,
      message: "Feedback submitted successfully",
      data: feedback
    });
  } catch (error) {
    console.error("Error submitting feedback:", error);
    res.status(500).json({ 
      success: false, 
      message: "Failed to submit feedback" 
    });
  }
});

// Get user's feedback history
router.get("/my-feedback", requireAuth, async (req: AuthReq, res) => {
  try {
    const userId = req.userId;

    const feedbacks = await Feedback.find({ userId })
      .sort({ createdAt: -1 })
      .select("-__v");

    res.json({
      success: true,
      data: feedbacks
    });
  } catch (error) {
    console.error("Error fetching feedback:", error);
    res.status(500).json({ 
      success: false, 
      message: "Failed to fetch feedback" 
    });
  }
});

// Get all feedback (Admin only - you can add admin auth middleware later)
router.get("/all", requireAuth, async (req: AuthReq, res) => {
  try {
    const { status, page = 1, limit = 20 } = req.query;

    const query: any = {};
    if (status) {
      query.status = status;
    }

    const skip = (Number(page) - 1) * Number(limit);

    const feedbacks = await Feedback.find(query)
      .sort({ createdAt: -1 })
      .skip(skip)
      .limit(Number(limit))
      .populate("userId", "name phone email")
      .select("-__v");

    const total = await Feedback.countDocuments(query);

    res.json({
      success: true,
      data: feedbacks,
      pagination: {
        total,
        page: Number(page),
        limit: Number(limit),
        pages: Math.ceil(total / Number(limit))
      }
    });
  } catch (error) {
    console.error("Error fetching all feedback:", error);
    res.status(500).json({ 
      success: false, 
      message: "Failed to fetch feedback" 
    });
  }
});

// Update feedback status (Admin only)
router.patch("/:id/status", requireAuth, async (req: AuthReq, res) => {
  try {
    const { id } = req.params;
    const { status, adminResponse } = req.body;

    const validStatuses = ["pending", "in-progress", "resolved", "closed"];
    if (status && !validStatuses.includes(status)) {
      return res.status(400).json({ 
        success: false, 
        message: "Invalid status" 
      });
    }

    const updateData: any = {};
    if (status) updateData.status = status;
    if (adminResponse) {
      updateData.adminResponse = adminResponse;
      updateData.respondedAt = new Date();
    }

    const feedback = await Feedback.findByIdAndUpdate(
      id,
      updateData,
      { new: true }
    );

    if (!feedback) {
      return res.status(404).json({ 
        success: false, 
        message: "Feedback not found" 
      });
    }

    res.json({
      success: true,
      message: "Feedback updated successfully",
      data: feedback
    });
  } catch (error) {
    console.error("Error updating feedback:", error);
    res.status(500).json({ 
      success: false, 
      message: "Failed to update feedback" 
    });
  }
});

// Delete feedback
router.delete("/:id", requireAuth, async (req: AuthReq, res) => {
  try {
    const { id } = req.params;
    const userId = req.userId;

    const feedback = await Feedback.findOne({ _id: id, userId });

    if (!feedback) {
      return res.status(404).json({ 
        success: false, 
        message: "Feedback not found or unauthorized" 
      });
    }

    await Feedback.findByIdAndDelete(id);

    res.json({
      success: true,
      message: "Feedback deleted successfully"
    });
  } catch (error) {
    console.error("Error deleting feedback:", error);
    res.status(500).json({ 
      success: false, 
      message: "Failed to delete feedback" 
    });
  }
});

export default router;
