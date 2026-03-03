import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import Voucher from "../models/Voucher";

(async () => {
  await mongoose.connect(process.env.MONGODB_URI!);
  // Delete admin-level template that has the wrong values (₹500, 0% off)
  const result = await (Voucher as any).deleteMany({
    $or: [{ userId: { $exists: false } }, { userId: null }],
    amount: 500,
    discountPercentage: 0,
  });
  console.log(`✅ Deleted ${result.deletedCount} duplicate voucher(s)`);
  await mongoose.connection.close();
})();
