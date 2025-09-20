// Script to clean up groups with null joinCode values
const mongoose = require('mongoose');
require('dotenv').config();

// MongoDB connection string from environment
const MONGODB_URI = process.env.MONGODB_URI;

// Group schema (simplified for cleanup)
const GroupSchema = new mongoose.Schema({
  name: String,
  description: String,
  icon: String,
  members: [{ type: mongoose.Schema.Types.ObjectId, ref: "User" }],
  admin: { type: mongoose.Schema.Types.ObjectId, ref: "User" },
  joinCode: String
}, { timestamps: true });

const Group = mongoose.model('Group', GroupSchema);

// Function to generate a 6-character join code
function generateJoinCode() {
  let code = Math.random().toString(36).substring(2, 8).toUpperCase();
  // Ensure it's exactly 6 characters
  while (code.length < 6) {
    code += Math.random().toString(36).substring(2, 3).toUpperCase();
  }
  return code.substring(0, 6);
}

async function cleanupGroups() {
  try {
    await mongoose.connect(MONGODB_URI);
    console.log('Connected to MongoDB');

    // Find all groups with null or missing joinCode
    const groupsWithoutJoinCode = await Group.find({
      $or: [
        { joinCode: null },
        { joinCode: { $exists: false } },
        { joinCode: '' }
      ]
    });

    console.log(`Found ${groupsWithoutJoinCode.length} groups without joinCode`);

    if (groupsWithoutJoinCode.length === 0) {
      console.log('No groups need cleanup');
      return;
    }

    // Update each group with a unique joinCode
    for (const group of groupsWithoutJoinCode) {
      let joinCode;
      let isUnique = false;
      let attempts = 0;

      // Generate a unique join code
      while (!isUnique && attempts < 20) {
        joinCode = generateJoinCode();
        const existing = await Group.findOne({ joinCode });
        if (!existing) {
          isUnique = true;
        }
        attempts++;
      }

      if (!isUnique) {
        // Fallback: use timestamp + random
        const timestamp = Date.now().toString(36).toUpperCase();
        joinCode = (timestamp + Math.random().toString(36).substring(2, 4)).substring(-6);
      }

      await Group.updateOne(
        { _id: group._id },
        { $set: { joinCode } }
      );

      console.log(`Updated group "${group.name}" with joinCode: ${joinCode}`);
    }

    console.log('Cleanup completed successfully');

  } catch (error) {
    console.error('Cleanup failed:', error);
  } finally {
    await mongoose.disconnect();
    console.log('Disconnected from MongoDB');
  }
}

// Run the cleanup
cleanupGroups()
  .then(() => {
    console.log('Script completed');
    process.exit(0);
  })
  .catch((error) => {
    console.error('Script failed:', error);
    process.exit(1);
  });