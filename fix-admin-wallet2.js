const mongoose = require("mongoose");
require("dotenv").config();
const W = mongoose.model(
  "W",
  new mongoose.Schema({}, { strict: false, collection: "mlmwallets" }),
);
const U = mongoose.model(
  "U",
  new mongoose.Schema({}, { strict: false, collection: "users" }),
);

mongoose.connect(process.env.MONGODB_URI).then(async () => {
  const admin = await U.findOne({ phone: "+919867477227" }).lean();
  const allWallets = await W.find({ userId: admin._id }).lean();
  console.log("All admin wallets:", allWallets.length);
  allWallets.forEach((w) =>
    console.log("  id:", w._id, "balance:", w.creditBalance),
  );

  await W.updateOne(
    { userId: admin._id },
    { $set: { creditBalance: 146484360000 } },
    { upsert: true },
  );
  const check = await W.findOne({ userId: admin._id }).lean();
  console.log(
    "\n✅ Admin wallet set to:",
    check?.creditBalance?.toLocaleString(),
  );
  await mongoose.disconnect();
});
