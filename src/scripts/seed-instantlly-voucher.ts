// Seed the hardcoded "Instantlly" MLM voucher into the DB as a published admin template.
// Safe to run multiple times — it upserts by voucherNumber.
import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import Voucher from "../models/Voucher";

async function seed() {
  const mongoUri = process.env.MONGODB_URI || process.env.MONGO_URI;
  if (!mongoUri) throw new Error("MONGODB_URI not set in .env");

  console.log("🔌 Connecting to MongoDB...");
  await mongoose.connect(mongoUri);
  console.log("✅ Connected\n");

  const template = {
    // No userId → marks it as an admin-level template (not tied to any user)
    userId: undefined,
    voucherNumber: "INSTANTLLY-SPECIAL",
    companyName: "Instantlly",
    title: "Sales Target at Special Discount",
    phoneNumber: "+91 98674 77227",
    address: "Jogeshwari, Mumbai",
    description: "",
    MRP: 100,
    amount: 100,
    discountPercentage: 70,
    minVouchersRequired: 5,
    issueDate: new Date(),
    expiryDate: new Date("2027-02-24"),
    redeemedStatus: "unredeemed",
    source: "admin",
    isPublished: true,
    publishedAt: new Date(),
  };

  const existing = await Voucher.findOne({
    voucherNumber: "INSTANTLLY-SPECIAL",
    userId: { $exists: false },
  });

  if (existing) {
    // Update fields in case anything changed
    await Voucher.updateOne(
      { _id: existing._id },
      {
        $set: {
          companyName: template.companyName,
          title: template.title,
          phoneNumber: template.phoneNumber,
          address: template.address,
          description: template.description,
          MRP: template.MRP,
          amount: template.amount,
          discountPercentage: template.discountPercentage,
          minVouchersRequired: template.minVouchersRequired,
          expiryDate: template.expiryDate,
          isPublished: template.isPublished,
          publishedAt: existing.publishedAt ?? template.publishedAt,
          source: template.source,
        },
      },
    );
    console.log(`✅ Updated existing template  _id: ${existing._id}`);
  } else {
    const created = await Voucher.create(template);
    console.log(`✅ Created new template  _id: ${created._id}`);
  }

  console.log("\n🎉 Done. The Instantlly voucher now lives in the DB.");
  await mongoose.connection.close();
}

seed().catch((err) => {
  console.error("❌ Error:", err);
  process.exit(1);
});
