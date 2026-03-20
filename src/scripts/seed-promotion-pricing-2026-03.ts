import dotenv from "dotenv";
import path from "path";
import mongoose from "mongoose";
import { connectDB } from "../db";
import PromotionPricingPlan from "../models/PromotionPricingPlan";

dotenv.config({ path: path.join(__dirname, "../../.env") });

type AreaType =
  | "pincode"
  | "tehsil"
  | "district"
  | "division"
  | "state"
  | "zone"
  | "india";

const AREA_TYPES: AreaType[] = [
  "pincode",
  "tehsil",
  "district",
  "division",
  "state",
  "zone",
  "india",
];

const RANKS = [21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1];

function getRankLabel(rank: number): string {
  return rank === 21 ? "No Rank" : `Rank ${rank}`;
}

function priorityFromRank(rank: number): number {
  if (rank === 21) return 10;
  return Math.max(20, 100 - rank * 3);
}

function getPincodeForRank(rank: number): number {
  if (rank === 21) return 600;
  return 2600 - (rank - 1) * 100;
}

function getAmountForArea(rank: number, areaType: AreaType): number {
  const pincode = getPincodeForRank(rank);
  const multipliers: Record<AreaType, number> = {
    pincode: 1,
    tehsil: 6,
    district: 36,
    division: 216,
    state: 1296,
    zone: 7776,
    india: 46656,
  };
  return pincode * multipliers[areaType];
}

async function seedPricing(): Promise<void> {
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
              amount: getAmountForArea(rank, areaType),
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

  console.log("✅ Promotion pricing updated");
  console.log(`Plans targeted: ${operations.length}`);
  console.log(`Inserted: ${result.upsertedCount}, Modified: ${result.modifiedCount}`);
}

seedPricing()
  .then(async () => {
    await mongoose.disconnect();
    process.exit(0);
  })
  .catch(async (error) => {
    console.error("❌ Failed to seed promotion pricing:", error);
    await mongoose.disconnect();
    process.exit(1);
  });
