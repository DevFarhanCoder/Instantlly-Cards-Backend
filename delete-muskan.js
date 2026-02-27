const mongoose = require("mongoose");
require("dotenv").config();
const User = mongoose.model("User", new mongoose.Schema({}, { strict: false }));
const Voucher = mongoose.model(
  "Voucher",
  new mongoose.Schema({}, { strict: false }),
);
const MlmWallet = mongoose.model(
  "MlmWallet",
  new mongoose.Schema({}, { strict: false }),
);
const SC = mongoose.model(
  "SpecialCredit",
  new mongoose.Schema({}, { strict: false }),
);

mongoose.connect(process.env.MONGODB_URI).then(async () => {
  const u = await User.findOne({ phone: "+918828188930" });
  if (u) {
    await Voucher.deleteMany({ userId: u._id });
    await MlmWallet.deleteMany({ userId: u._id });
    await SC.deleteMany({ $or: [{ ownerId: u._id }, { recipientId: u._id }] });
    await User.deleteOne({ _id: u._id });
    console.log("✅ Deleted:", u.name);
  } else {
    // Search by name
    const byName = await User.find({ name: /muskan/i });
    console.log(
      "Found by name:",
      byName.map((x) => `${x.name} ${x.phone}`),
    );
    for (const u2 of byName) {
      await Voucher.deleteMany({ userId: u2._id });
      await MlmWallet.deleteMany({ userId: u2._id });
      await SC.deleteMany({
        $or: [{ ownerId: u2._id }, { recipientId: u2._id }],
      });
      await User.deleteOne({ _id: u2._id });
      console.log("✅ Deleted:", u2.name, u2.phone);
    }
  }
  await mongoose.disconnect();
});
