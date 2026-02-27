const mongoose = require("mongoose");
require("dotenv").config();

async function run() {
  await mongoose.connect(process.env.MONGODB_URI);
  const db = mongoose.connection.db;
  
  const result = await db.collection("users").updateOne(
    { name: /Rajesh Modi/i },
    { $set: { credits: mongoose.mongo.Long.fromNumber(14648436000) } }
  );
  
  console.log("Matched:", result.matchedCount, "Modified:", result.modifiedCount);
  
  // Verify
  const user = await db.collection("users").findOne({ name: /Rajesh Modi/i }, { projection: { name: 1, credits: 1 } });
  console.log("Verified - Name:", user.name, "Credits:", user.credits.toString());
  
  await mongoose.disconnect();
  process.exit(0);
}

run().catch(e => { console.error(e); process.exit(1); });
