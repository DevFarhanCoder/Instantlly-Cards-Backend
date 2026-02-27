/**
 * Set passwords for Farhan and Muskan (recreated by admin-setup.js without passwords)
 */

const mongoose = require("mongoose");
const bcrypt = require("bcryptjs");
require("dotenv").config();

const USERS = [
  { phone: "+919867969445", name: "Mohammad Farhan", password: "farhan123" },
  { phone: "+918828188930", name: "Muskan", password: "muskan123" },
];

const UserSchema = new mongoose.Schema(
  {},
  { strict: false, collection: "users" },
);
const User = mongoose.model("User", UserSchema);

async function setPasswords() {
  await mongoose.connect(process.env.MONGODB_URI);
  console.log("✅ Connected\n");

  for (const u of USERS) {
    const user = await User.findOne({ phone: u.phone });
    if (!user) {
      console.log(`❌ Not found: ${u.name} (${u.phone})`);
      continue;
    }
    const hashed = await bcrypt.hash(u.password, 12);
    await User.updateOne({ _id: user._id }, { $set: { password: hashed } });
    console.log(`✅ Password set for ${u.name} → "${u.password}"`);
  }

  await mongoose.disconnect();
  console.log("\nDone. Users can now log in.");
}

setPasswords().catch((e) => {
  console.error(e);
  process.exit(1);
});
