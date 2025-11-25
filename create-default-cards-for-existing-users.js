// Script to create a default card for all users who do not have any cards
// Usage: node create-default-cards-for-existing-users.js

const mongoose = require('mongoose');
const User = require('./src/models/User').default || require('./src/models/User');
const Card = require('./src/models/Card').default || require('./src/models/Card');
require('dotenv').config();

async function main() {
  await mongoose.connect(process.env.MONGODB_URI, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
  });
  console.log('Connected to MongoDB');

  const users = await User.find({});
  let createdCount = 0;

  for (const user of users) {
    const cardCount = await Card.countDocuments({ userId: user._id.toString() });
    if (cardCount === 0) {
      await Card.create({
        userId: user._id.toString(),
        name: user.name,
        personalPhone: user.phone.replace(/\D/g, ""),
      });
      createdCount++;
      console.log(`🆕 Default card created for user: ${user.phone}`);
    }
  }

  console.log(`Done! Default cards created for ${createdCount} users.`);
  await mongoose.disconnect();
}

main().catch(err => {
  console.error('Error running script:', err);
  process.exit(1);
});
