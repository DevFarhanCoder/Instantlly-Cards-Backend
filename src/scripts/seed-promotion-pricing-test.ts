import dotenv from "dotenv";
import path from "path";
import mongoose from "mongoose";
import { connectDB } from "../db";
import PromotionPricingPlan from "../models/PromotionPricingPlan";

dotenv.config({ path: path.join(__dirname, "../../.env") });

type AreaType = "pincode" | "tehsil" | "district";

const AREA_TYPES: AreaType[] = ["pincode", "tehsil", "district"];
const RANKS = [21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1];

function getRankLabel(rank: number): string {
  return rank === 21 ? "No Rank" : `Rank ${rank}`;
}

function priorityFromRank(rank: number): number {
  if (rank === 21) return 10;
  return Math.max(20, 100 - rank * 3);
}

async function seedTestPricing(): Promise<void> {
  await connectDB();

  const operations = RANKS.flatMap((rank) =>
    AREA_TYPES.map((areaType) => {
      const code = `BL_${areaType.toUpperCase()}_R${rank}_30D`;
      return {
        updateOne: {
          filter: { code },
          update: {
            $set: {
              code,
              areaType,
              rank,
              rankLabel: getRankLabel(rank),
              amount: 1,
              currency: "INR",
              durationDays: 30,
              priorityScore: priorityFromRank(rank),
              isActive: true,
            },
          },
          upsert: true,
        },
      };
    }),
  );

  const result = await PromotionPricingPlan.bulkWrite(operations);

  console.log("✅ Test promotion pricing seeded successfully");
  console.log(`Plans targeted: ${operations.length}`);
  console.log(`Inserted: ${result.upsertedCount}, Modified: ${result.modifiedCount}`);
}

seedTestPricing()
  .then(async () => {
    await mongoose.disconnect();
    process.exit(0);
  })
  .catch(async (error) => {
    console.error("❌ Failed to seed test promotion pricing:", error);
    await mongoose.disconnect();
    process.exit(1);
  });
