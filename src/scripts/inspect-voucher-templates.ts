import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import Voucher from "../models/Voucher";

(async () => {
  await mongoose.connect(process.env.MONGODB_URI!);

  // All admin-level templates (no userId)
  const templates = await Voucher.find({
    $or: [{ userId: { $exists: false } }, { userId: null }],
  }).lean();

  console.log(`Found ${templates.length} admin template(s):\n`);
  templates.forEach((t: any) => {
    console.log(
      JSON.stringify(
        {
          _id: t._id,
          voucherNumber: t.voucherNumber,
          companyName: t.companyName,
          title: t.title,
          amount: t.amount,
          discountPercentage: t.discountPercentage,
          MRP: t.MRP,
          phoneNumber: t.phoneNumber,
          address: t.address,
          description: t.description,
          expiryDate: t.expiryDate,
          isPublished: t.isPublished,
          companyLogo: t.companyLogo,
          voucherImage: t.voucherImage,
          minVouchersRequired: t.minVouchersRequired,
          source: t.source,
        },
        null,
        2,
      ),
    );
    console.log("---");
  });

  await mongoose.connection.close();
})().catch(console.error);
