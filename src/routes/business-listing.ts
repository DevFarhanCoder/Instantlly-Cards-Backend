import { Router } from "express";
import BusinessPromotion from "../models/BusinessPromotion";
import { requireAuth, AuthReq } from "../middleware/auth";

const router = Router();

router.get('/', async (req, res) => {
  try {
    const { subcategory, city } = req.query;
    const now = new Date();

    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━');
    console.log('📥 Incoming Query Params:');
    console.log('   subcategory:', subcategory);
    console.log('   city:', city);

    const query: any = {
      isActive: true,
      status: 'active',
      $or: [
        { expiryDate: null },
        { expiryDate: { $gt: now } },
      ]
    };

    if (subcategory) {
      query.category = {
        $elemMatch: {
          $regex: subcategory,
          $options: 'i'
        }
      };
    }

    if (city) {
      query.city = city;
    }

    console.log('🧠 Final Mongo Query:');
    console.log(JSON.stringify(query, null, 2));

    const data = await BusinessPromotion.find(query)
      .sort({
        'visibility.priorityScore': -1,
        createdAt: -1
      })
      .lean();

    console.log('📊 Results Found:', data.length);
    console.log('📄 Result Categories:', data.map(d => d.category));
    console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━');

    res.json({ success: true, data });

  } catch (error) {
    console.error("❌ Business listings error:", error);
    res.status(500).json({
      success: false,
      message: "Server error fetching listings"
    });
  }
});


router.post('/:id/impression', async (req, res) => {
    await BusinessPromotion.findByIdAndUpdate(
        req.params.id,
        { $inc: { 'visibility.impressions': 1 } }
    );
    res.json({ success: true });
});

router.post('/:id/click', async (req, res) => {
    await BusinessPromotion.findByIdAndUpdate(
        req.params.id,
        { $inc: { 'visibility.clicks': 1 } }
    );
    res.json({ success: true });
});



export default router;