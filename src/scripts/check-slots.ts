import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import SpecialCredit from "../models/SpecialCredit";
import Voucher from "../models/Voucher";

(async () => {
  await mongoose.connect(process.env.MONGODB_URI!);

  const distinct = await SpecialCredit.distinct("voucherId");
  console.log("voucherIds in existing slots:", JSON.stringify(distinct));

  const total = await SpecialCredit.countDocuments();
  console.log("total SpecialCredit docs:", total);

  // Find the correct published Instantlly template
  const template = await Voucher.findOne({
    $or: [{ userId: { $exists: false } }, { userId: null }],
    isPublished: true,
    companyName: "Instantlly",
  }).lean();
  console.log(
    "target voucher template:",
    template ? (template as any)._id.toString() : "NOT FOUND",
  );

  await mongoose.connection.close();
})().catch(console.error);
