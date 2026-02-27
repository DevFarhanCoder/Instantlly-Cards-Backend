const mongoose = require("mongoose");
require("dotenv").config();

const User = mongoose.model(
  "User",
  new mongoose.Schema({}, { strict: false, timestamps: true }),
);
const MlmWallet = mongoose.model(
  "MlmWallet",
  new mongoose.Schema({}, { strict: false, timestamps: true }),
);
const SpecialCredit = mongoose.model(
  "SpecialCredit",
  new mongoose.Schema({}, { strict: false, timestamps: true }),
);

const ADMIN_PHONE = "+919867477227";
const FARHAN_PHONE = "+919867969445";

async function run() {
  await mongoose.connect(process.env.MONGODB_URI);
  console.log("Connected\n");

  const admin = await User.findOne({ phone: ADMIN_PHONE });
  const farhan = await User.findOne({ phone: FARHAN_PHONE });

  if (!admin) throw new Error("Admin not found");
  console.log(`Admin: ${admin.name} (${admin._id})`);
  console.log(
    `Farhan: ${farhan ? farhan.name + " " + farhan._id : "not found"}\n`,
  );

  // 1. Zero Farhan's credits and vouchers
  if (farhan) {
    await MlmWallet.updateOne(
      { userId: farhan._id },
      { $set: { creditBalance: 0 } },
      { upsert: true },
    );
    await User.updateOne({ _id: farhan._id }, { $set: { voucherBalance: 0 } });
    console.log("✅ Farhan credits and voucherBalance zeroed");
  }

  // 2. Show current direct buyers of admin
  const directBuyers = await User.find(
    { parentId: admin._id },
    "_id name phone",
  );
  console.log(`Direct buyers of admin (${directBuyers.length}):`);
  directBuyers.forEach((u) => console.log(`  - ${u.name} (${u.phone})`));

  // 3. Detach all direct buyers from admin
  if (directBuyers.length > 0) {
    await User.updateMany(
      { parentId: admin._id },
      { $set: { parentId: null } },
    );
    console.log(
      `✅ Detached ${directBuyers.length} direct buyer(s) from admin`,
    );
  }

  // 4. Reset admin direct/downline counters
  await User.updateOne(
    { _id: admin._id },
    { $set: { directCount: 0, downlineCount: 0 } },
  );
  console.log("✅ Admin directCount & downlineCount reset to 0");

  // 5. Reset all SpecialCredit slots back to available
  const scResult = await SpecialCredit.updateMany(
    { ownerId: admin._id },
    {
      $set: {
        status: "available",
        recipientId: null,
        recipientName: null,
        recipientPhone: null,
        sentAt: null,
      },
    },
  );
  await User.updateOne(
    { _id: admin._id },
    {
      $set: {
        "specialCredits.usedSlots": 0,
        "specialCredits.availableSlots": 10,
      },
    },
  );
  console.log(
    `✅ Reset ${scResult.modifiedCount} SpecialCredit slots → available`,
  );

  console.log("\nDone.");
  await mongoose.disconnect();
}

run().catch((e) => {
  console.error("Error:", e.message);
  process.exit(1);
});
