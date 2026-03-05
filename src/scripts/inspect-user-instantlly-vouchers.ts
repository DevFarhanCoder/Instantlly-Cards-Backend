import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import Voucher from "../models/Voucher";

(async () => {
  await mongoose.connect(process.env.MONGODB_URI!);

  // Find instantlly vouchers that belong to users (have userId)
  const vouchers = await Voucher.find({
    companyName: "Instantlly",
    userId: { $exists: true, $ne: null },
  }).lean();

  console.log(`Found ${vouchers.length} user-owned Instantlly voucher(s):\n`);
  vouchers.forEach((v: any) => {
    console.log(
      JSON.stringify(
        {
          _id: v._id,
          userId: v.userId,
          voucherNumber: v.voucherNumber,
          companyName: v.companyName,
          title: v.title,
          amount: v.amount,
          discountPercentage: v.discountPercentage,
          MRP: v.MRP,
          phoneNumber: v.phoneNumber,
          address: v.address,
          description: v.description,
          expiryDate: v.expiryDate,
          isPublished: v.isPublished,
          companyLogo: v.companyLogo,
          voucherImage: v.voucherImage,
          minVouchersRequired: v.minVouchersRequired,
          source: v.source,
          redeemedStatus: v.redeemedStatus,
        },
        null,
        2,
      ),
    );
    console.log("---");
  });

  await mongoose.connection.close();
})().catch(console.error);
