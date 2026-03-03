// Remove duplicate slot numbers per (ownerId, voucherId).
// Keeps the slot with the higher status priority (sent > available) or higher _id.
import dotenv from "dotenv";
import path from "path";
dotenv.config({ path: path.join(__dirname, "../../.env") });

import mongoose from "mongoose";
import SpecialCredit from "../models/SpecialCredit";

(async () => {
  await mongoose.connect(process.env.MONGODB_URI!);

  // Get all distinct (ownerId, voucherId) combos
  const groups = await SpecialCredit.aggregate([
    { $group: { _id: { ownerId: "$ownerId", voucherId: "$voucherId" } } },
  ]);

  let totalDeleted = 0;

  for (const g of groups) {
    const { ownerId, voucherId } = g._id;

    // Get all slots for this owner+voucher sorted by slotNumber, then prefer "sent" status
    const slots = await SpecialCredit.find({ ownerId, voucherId })
      .sort({ slotNumber: 1, status: -1 }) // "sent" > "available" alphabetically desc
      .lean();

    const seen = new Map<number, mongoose.Types.ObjectId>();
    const toDelete: mongoose.Types.ObjectId[] = [];

    for (const slot of slots) {
      const num = (slot as any).slotNumber;
      const id = (slot as any)._id;
      if (seen.has(num)) {
        // Prefer "sent" over "available"; if same status keep the older one
        const existingId = seen.get(num)!;
        const existingSlot = slots.find((s) =>
          (s as any)._id.equals(existingId),
        )!;
        if (
          (slot as any).status === "sent" &&
          (existingSlot as any).status !== "sent"
        ) {
          // New one is sent, delete the old available one
          toDelete.push(existingId);
          seen.set(num, id);
        } else {
          // Keep existing, delete this one
          toDelete.push(id);
        }
      } else {
        seen.set(num, id);
      }
    }

    if (toDelete.length > 0) {
      await SpecialCredit.deleteMany({ _id: { $in: toDelete } });
      totalDeleted += toDelete.length;
      console.log(
        `ownerId=${ownerId} voucherId=${voucherId}: deleted ${toDelete.length} duplicates`,
      );
    }
  }

  console.log(`\n✅ Done. Removed ${totalDeleted} duplicate slot(s).`);
  const remaining = await SpecialCredit.countDocuments();
  console.log(`Total slots remaining: ${remaining}`);

  await mongoose.connection.close();
})().catch((err) => {
  console.error("❌", err);
  process.exit(1);
});
