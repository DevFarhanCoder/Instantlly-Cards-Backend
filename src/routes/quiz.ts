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

    // Check if all 30 questions are already answered (true completion)
    const totalQuestionsAnswered = user.quizProgress?.answeredQuestions?.length || 0;
    
    // AUTO-FIX: If marked as completed but not all 30 questions answered, reset completion status
    if (user.quizProgress?.completed && totalQuestionsAnswered < 30) {
      console.log(`⚠️ Auto-fixing quiz completion: User has ${totalQuestionsAnswered}/30 questions but marked as completed`);
      user.quizProgress.completed = false;
      await user.save();
    }
    
    // Only block if truly completed (all 30 questions answered)
    if (user.quizProgress?.completed && totalQuestionsAnswered >= 30) {
      return res.status(400).json({ 
        success: false, 
        message: 'Quiz already completed - all 30 questions answered' 
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
    user.quizProgress.creditsEarned += CREDITS_PER_QUESTION;
    user.credits += CREDITS_PER_QUESTION;

    // Update current question index
    if (questionIndex !== undefined) {
      user.quizProgress.currentQuestionIndex = questionIndex + 1;
    }

    // Check if all 30 questions are answered
    const isCompleted = user.quizProgress.answeredQuestions.length >= 30;
    if (isCompleted) {
      user.quizProgress.completed = true;
      user.quizProgress.completedAt = new Date();
    }

    // Log quiz progress for debugging
    console.log(`📝 Quiz Progress: Question ${user.quizProgress.answeredQuestions.length}/30 answered`);
    console.log(`   Question Key: ${questionKey}`);
    console.log(`   Total Answered: ${user.quizProgress.answeredQuestions.length}`);
    console.log(`   Is Completed: ${isCompleted}`);

    // Save with explicit validation bypass for large arrays
    await user.save({ validateBeforeSave: true });

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

// Save quiz session progress - creates transaction for accumulated credits
router.post('/save-progress', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ success: false, message: 'User not found' });
    }

    if (!user.quizProgress) {
      return res.status(400).json({ 
        success: false, 
        message: 'No quiz progress found' 
      });
    }

    // Initialize creditsRecordedInTransactions if not set
    if (!user.quizProgress.creditsRecordedInTransactions) {
      user.quizProgress.creditsRecordedInTransactions = 0;
    }

    // Calculate credits earned in this session (not yet recorded)
    const creditsToRecord = user.quizProgress.creditsEarned - user.quizProgress.creditsRecordedInTransactions;

    if (creditsToRecord <= 0) {
      return res.json({
        success: true,
        message: 'No new credits to record',
        data: {
          creditsRecorded: 0,
          totalCredits: user.credits
        }
      });
    }

    // Create transaction for this session's credits
    const balanceBefore = user.credits - creditsToRecord;
    const balanceAfter = user.credits;
    const questionsInThisSession = creditsToRecord / 10; // Each question is worth 10 credits

    await Transaction.create({
      type: 'quiz_bonus',
      toUser: userId,
      amount: creditsToRecord,
      description: `Quiz bonus - ${questionsInThisSession} question${questionsInThisSession > 1 ? 's' : ''} answered (+${creditsToRecord} credits)`,
      balanceBefore,
      balanceAfter,
      status: 'completed'
    });

    // Update recorded amount
    user.quizProgress.creditsRecordedInTransactions = user.quizProgress.creditsEarned;
    await user.save();

    res.json({
      success: true,
      message: 'Quiz progress saved and transaction created',
      data: {
        creditsRecorded: creditsToRecord,
        totalCredits: user.credits,
        completed: user.quizProgress.completed
      }
    });
  } catch (error) {
    console.error('Error saving quiz progress:', error);
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

// Sync quiz transactions for users who completed quiz before transaction tracking
router.post('/sync-transactions', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ success: false, message: 'User not found' });
    }

    // Check if quiz is completed and has credits earned
    if (!user.quizProgress?.completed || !user.quizProgress?.creditsEarned) {
      return res.status(400).json({ 
        success: false, 
        message: 'No completed quiz found' 
      });
    }

    // Delete all existing quiz transactions for this user
    await Transaction.deleteMany({
      toUser: userId,
      type: 'quiz_bonus'
    });

    // Create single transaction for completed quiz
    const creditsEarned = user.quizProgress.creditsEarned;
    const completedAt = user.quizProgress.completedAt || new Date();
    
    await Transaction.create({
      type: 'quiz_bonus',
      toUser: userId,
      amount: creditsEarned,
      description: `Quiz completion bonus (${creditsEarned} credits)`,
      balanceBefore: user.credits - creditsEarned,
      balanceAfter: user.credits,
      status: 'completed',
      createdAt: completedAt,
      updatedAt: completedAt
    });

    res.json({
      success: true,
      message: 'Quiz transactions consolidated successfully',
      data: {
        creditsEarned,
        totalCredits: user.credits
      }
    });
  } catch (error) {
    console.error('Error syncing quiz transactions:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// Fix quiz completion status for users stuck with old logic
router.post('/fix-completion', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ success: false, message: 'User not found' });
    }

    const answeredCount = user.quizProgress?.answeredQuestions?.length || 0;
    
    // If user has less than 30 questions answered but marked as completed, fix it
    if (user.quizProgress?.completed && answeredCount < 30) {
      user.quizProgress.completed = false;
      await user.save();
      
      return res.json({
        success: true,
        message: 'Quiz completion status fixed',
        data: {
          answeredQuestions: answeredCount,
          completed: false,
          canContinue: true
        }
      });
    }

    res.json({
      success: true,
      message: 'No fix needed',
      data: {
        answeredQuestions: answeredCount,
        completed: user.quizProgress?.completed || false,
        canContinue: answeredCount < 30
      }
    });
  } catch (error) {
    console.error('Error fixing quiz completion:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

// DEBUG: Get detailed quiz stats (for debugging storage issues)
router.get('/debug-stats', requireAuth, async (req: AuthReq, res: Response) => {
  try {
    const userId = req.userId;
    
    const user = await User.findById(userId);
    if (!user) {
      return res.status(404).json({ success: false, message: 'User not found' });
    }

    const answeredCount = user.quizProgress?.answeredQuestions?.length || 0;
    const answersMapSize = user.quizProgress?.answers?.size || 0;
    
    res.json({
      success: true,
      debug: {
        answeredQuestionsCount: answeredCount,
        answeredQuestionsList: user.quizProgress?.answeredQuestions || [],
        answersMapSize: answersMapSize,
        answersKeys: user.quizProgress?.answers ? Array.from(user.quizProgress.answers.keys()) : [],
        creditsEarned: user.quizProgress?.creditsEarned || 0,
        completed: user.quizProgress?.completed || false,
        currentQuestionIndex: user.quizProgress?.currentQuestionIndex || 0,
        expectedCredits: answeredCount * 10,
        mismatch: {
          questionsVsAnswers: answeredCount !== answersMapSize,
          creditsVsQuestions: (user.quizProgress?.creditsEarned || 0) !== (answeredCount * 10)
        }
      }
    });
  } catch (error) {
    console.error('Error getting quiz debug stats:', error);
    res.status(500).json({ success: false, message: 'Server error' });
  }
});

export default router;
