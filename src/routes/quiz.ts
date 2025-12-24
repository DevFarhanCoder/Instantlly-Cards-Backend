import { Router, Response } from 'express';
import User from '../models/User';
import Transaction from '../models/Transaction';
import { requireAuth, AuthReq } from '../middleware/auth';

const router = Router();

// Get quiz progress for current user
router.get('/progress', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ success: false, message: 'User not found' });
    }

    // Return quiz progress
    res.json({
      success: true,
      data: {
        completed: user.quizProgress?.completed || false,
        currentQuestionIndex: user.quizProgress?.currentQuestionIndex || 0,
        answeredQuestions: user.quizProgress?.answeredQuestions || [],
        answers: user.quizProgress?.answers ? Object.fromEntries(user.quizProgress.answers) : {},
        creditsEarned: user.quizProgress?.creditsEarned || 0,
        totalCredits: user.credits
      }
    });
  } catch (error) {
    console.error('Error fetching quiz progress:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// Submit answer for a question
router.post('/answer', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    const { questionKey, answer, questionIndex } = req.body;

    if (!questionKey || !answer) {
      return res.status(400).json({ success: false, message: 'Question key and answer are required' });
    }

    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ success: false, message: 'User not found' });
    }

    // Check if quiz is already completed
    if (user.quizProgress?.completed) {
      return res.status(400).json({ 
        success: false, 
        message: 'Quiz already completed' 
      });
    }

    // Initialize quizProgress if it doesn't exist
    if (!user.quizProgress) {
      user.quizProgress = {
        completed: false,
        currentQuestionIndex: 0,
        answeredQuestions: [],
        answers: new Map(),
        creditsEarned: 0,
        startedAt: new Date()
      };
    }

    // Check if question was already answered
    const alreadyAnswered = user.quizProgress.answeredQuestions.includes(questionKey);
    
    if (alreadyAnswered) {
      return res.status(400).json({
        success: false,
        message: 'Question already answered',
        data: {
          creditsEarned: 0,
          totalCredits: user.credits,
          answeredQuestions: user.quizProgress.answeredQuestions
        }
      });
    }

    // Add answer
    user.quizProgress.answeredQuestions.push(questionKey);
    user.quizProgress.answers.set(questionKey, answer);
    
    // Award credits (10 per question)
    const CREDITS_PER_QUESTION = 10;
    const balanceBefore = user.credits;
    user.quizProgress.creditsEarned += CREDITS_PER_QUESTION;
    user.credits += CREDITS_PER_QUESTION;
    const balanceAfter = user.credits;

    // Update current question index
    if (questionIndex !== undefined) {
      user.quizProgress.currentQuestionIndex = questionIndex + 1;
    }

    // Check if all 10 questions are answered
    const isCompleted = user.quizProgress.answeredQuestions.length >= 10;
    if (isCompleted) {
      user.quizProgress.completed = true;
      user.quizProgress.completedAt = new Date();
    }

    await user.save();

    // Create transaction record
    await Transaction.create({
      type: 'quiz_bonus',
      toUser: userId,
      amount: CREDITS_PER_QUESTION,
      description: isCompleted ? 'Quiz completion bonus' : `Quiz answer bonus - Question ${user.quizProgress.answeredQuestions.length}`,
      balanceBefore,
      balanceAfter,
      status: 'completed'
    });

    res.json({
      success: true,
      message: 'Answer saved successfully',
      data: {
        creditsEarned: CREDITS_PER_QUESTION,
        totalCreditsFromQuiz: user.quizProgress.creditsEarned,
        totalCredits: user.credits,
        answeredQuestions: user.quizProgress.answeredQuestions,
        completed: user.quizProgress.completed
      }
    });
  } catch (error) {
    console.error('Error saving answer:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// Reset quiz progress (for testing/admin purposes)
router.post('/reset', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ success: false, message: 'User not found' });
    }

    // Deduct quiz credits if any were earned
    if (user.quizProgress?.creditsEarned) {
      user.credits = Math.max(0, user.credits - user.quizProgress.creditsEarned);
    }

    // Reset quiz progress
    user.quizProgress = {
      completed: false,
      currentQuestionIndex: 0,
      answeredQuestions: [],
      answers: new Map(),
      creditsEarned: 0,
      startedAt: undefined,
      completedAt: undefined
    };

    await user.save();

    res.json({
      success: true,
      message: 'Quiz progress reset successfully',
      data: {
        totalCredits: user.credits
      }
    });
  } catch (error) {
    console.error('Error resetting quiz:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

export default router;
