"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
const mongoose_1 = __importDefault(require("mongoose"));
const db_1 = require("../db");
const PromotionPricingPlan_1 = __importDefault(require("../models/PromotionPricingPlan"));
dotenv_1.default.config({ path: path_1.default.join(__dirname, "../../.env") });
const AREA_TYPES = ["pincode", "tehsil", "district"];
const RANKS = [21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1];
function getRankLabel(rank) {
    return rank === 21 ? "No Rank" : `Rank ${rank}`;
}
function priorityFromRank(rank) {
    if (rank === 21)
        return 10;
    return Math.max(20, 100 - rank * 3);
}
async function seedTestPricing() {
    await (0, db_1.connectDB)();
    const operations = RANKS.flatMap((rank) => AREA_TYPES.map((areaType) => {
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
    }));
    const result = await PromotionPricingPlan_1.default.bulkWrite(operations);
    console.log("✅ Test promotion pricing seeded successfully");
    console.log(`Plans targeted: ${operations.length}`);
    console.log(`Inserted: ${result.upsertedCount}, Modified: ${result.modifiedCount}`);
}
seedTestPricing()
    .then(async () => {
    await mongoose_1.default.disconnect();
    process.exit(0);
})
    .catch(async (error) => {
    console.error("❌ Failed to seed test promotion pricing:", error);
    await mongoose_1.default.disconnect();
    process.exit(1);
});
