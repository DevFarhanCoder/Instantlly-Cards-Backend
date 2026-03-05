import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import Voucher from "../models/Voucher";

(async () => {
  await mongoose.connect(process.env.MONGODB_URI!);

  const result = await Voucher.findOneAndUpdate(
    {
      $or: [{ userId: { $exists: false } }, { userId: null }],
      companyName: "Instantlly",
      isPublished: true,
    },
    {
      $set: {
        expiryDate: new Date("2026-08-30"),
        // Sync all other fields to match the canonical hardcoded values
        amount: 100,
        MRP: 100,
        discountPercentage: 70,
        phoneNumber: "+91 98674 77227",
        address: "Jogeshwari, Mumbai",
        title: "Sales Target at Special Discount",
        minVouchersRequired: 5,
      },
    },
    { new: true },
  ).lean();

  if (result) {
    console.log(`✅ Updated template _id: ${(result as any)._id}`);
    console.log(`   expiryDate → ${(result as any).expiryDate}`);
  } else {
    console.log("❌ No admin template found.");
  }

  await mongoose.connection.close();
})().catch(console.error);
