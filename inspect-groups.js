// Script to check current group data in database
const mongoose = require('mongoose');
require('dotenv').config();

// MongoDB connection string from environment
const MONGODB_URI = process.env.MONGODB_URI;

// Group schema (simplified for inspection)
const GroupSchema = new mongoose.Schema({
  name: String,
  description: String,
  icon: String,
  members: [{ type: mongoose.Schema.Types.ObjectId, ref: "User" }],
  admin: { type: mongoose.Schema.Types.ObjectId, ref: "User" },
  joinCode: String,
  createdAt: Date,
  updatedAt: Date
}, { timestamps: true });

const Group = mongoose.model('Group', GroupSchema);

async function inspectGroups() {
  try {
    await mongoose.connect(MONGODB_URI);
    console.log('Connected to MongoDB');

    // Find all groups without populating references first
    const groups = await Group.find({});
    
    console.log(`\nFound ${groups.length} groups:`);
    
    groups.forEach((group, index) => {
      console.log(`\n--- Group ${index + 1} ---`);
      console.log(`Name: ${group.name}`);
      console.log(`Description: ${group.description || 'N/A'}`);
      console.log(`Join Code: ${group.joinCode || 'N/A'}`);
      console.log(`Members Count: ${group.members ? group.members.length : 0}`);
      console.log(`Members IDs:`, group.members ? group.members.map(m => m.toString()) : []);
      console.log(`Admin ID:`, group.admin ? group.admin.toString() : 'N/A');
      console.log(`Created At: ${group.createdAt}`);
      console.log(`Updated At: ${group.updatedAt}`);
      console.log(`Created At (raw): ${typeof group.createdAt} - ${group.createdAt}`);
      console.log(`Created At (ISO): ${group.createdAt ? group.createdAt.toISOString() : 'N/A'}`);
    });

  } catch (error) {
    console.error('Inspection failed:', error);
  } finally {
    await mongoose.disconnect();
    console.log('\nDisconnected from MongoDB');
  }
}

// Run the inspection
inspectGroups()
  .then(() => {
    console.log('\nScript completed');
    process.exit(0);
  })
  .catch((error) => {
    console.error('Script failed:', error);
    process.exit(1);
  });