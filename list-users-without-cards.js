// Script to list all users who do NOT have any cards
// Usage: node list-users-without-cards.js

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
  let noCardCount = 0;

  for (const user of users) {
    const cardCount = await Card.countDocuments({ userId: user._id.toString() });
    if (cardCount === 0) {
      noCardCount++;
      console.log(`User with NO card: ${user.name} (${user.phone}) [${user._id}]`);
    }
  }

  console.log(`\nTotal users without cards: ${noCardCount}`);
  await mongoose.disconnect();
}

main().catch(err => {
  console.error('Error running script:', err);
  process.exit(1);
});
