"use strict";
/**
 * Migration Script: Convert Base64 Ad Images to GridFS
 *
 * This script migrates all existing ads from storing images as base64 strings
 * in MongoDB documents to using GridFS for efficient large file storage.
 *
 * Benefits:
 * - Reduces document size from ~66MB to <100KB per ad
 * - Eliminates 502 gateway timeout errors
 * - Enables streaming large images
 * - Better for images >16MB (MongoDB document limit)
 *
 * Usage:
 * npm run migrate:gridfs
 */
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const dotenv_1 = __importDefault(require("dotenv"));
dotenv_1.default.config(); // Load environment variables
const mongoose_1 = __importDefault(require("mongoose"));
const Ad_1 = __importDefault(require("../models/Ad"));
const gridfsService_1 = require("../services/gridfsService");
const MONGODB_URI = process.env.MONGODB_URI || "";
if (!MONGODB_URI) {
    console.error("❌ MONGODB_URI not found in environment variables");
    console.error("Please set MONGODB_URI in your .env file");
    process.exit(1);
}
async function migrateAdsToGridFS() {
    try {
        console.log("🚀 Starting migration: Base64 → GridFS");
        console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        // Connect to MongoDB
        await mongoose_1.default.connect(MONGODB_URI);
        console.log("✅ Connected to MongoDB");
        // Initialize GridFS
        gridfsService_1.gridfsService.initialize();
        console.log("✅ GridFS initialized");
        // Get all ads with base64 images (not yet migrated)
        const ads = await Ad_1.default.find({
            $or: [
                { bottomImageGridFS: { $exists: false } },
                { bottomImageGridFS: null }
            ]
        });
        console.log(`\n📊 Found ${ads.length} ads to migrate\n`);
        if (ads.length === 0) {
            console.log("✅ No ads to migrate. All ads already using GridFS!");
            process.exit(0);
        }
        let successCount = 0;
        let errorCount = 0;
        for (let i = 0; i < ads.length; i++) {
            const ad = ads[i];
            console.log(`\n[${i + 1}/${ads.length}] Migrating: ${ad.title}`);
            console.log(`  ID: ${ad._id}`);
            try {
                // Migrate bottom image
                let bottomImageId = null;
                if (ad.bottomImage && ad.bottomImage.length > 0) {
                    console.log(`  📤 Uploading bottom image (${(ad.bottomImage.length / 1024 / 1024).toFixed(2)}MB)...`);
                    bottomImageId = await gridfsService_1.gridfsService.uploadBase64(ad.bottomImage, `${ad._id}_bottom.jpg`, {
                        adId: ad._id.toString(),
                        type: "bottom",
                        title: ad.title
                    });
                    console.log(`  ✅ Bottom image uploaded: ${bottomImageId}`);
                }
                // Migrate fullscreen image
                let fullscreenImageId = null;
                if (ad.fullscreenImage && ad.fullscreenImage.length > 0) {
                    console.log(`  📤 Uploading fullscreen image (${(ad.fullscreenImage.length / 1024 / 1024).toFixed(2)}MB)...`);
                    fullscreenImageId = await gridfsService_1.gridfsService.uploadBase64(ad.fullscreenImage, `${ad._id}_fullscreen.jpg`, {
                        adId: ad._id.toString(),
                        type: "fullscreen",
                        title: ad.title
                    });
                    console.log(`  ✅ Fullscreen image uploaded: ${fullscreenImageId}`);
                }
                // Update ad document with GridFS references
                await Ad_1.default.findByIdAndUpdate(ad._id, {
                    bottomImageGridFS: bottomImageId,
                    fullscreenImageGridFS: fullscreenImageId,
                    // Keep base64 as backup during migration (will remove later)
                    // bottomImage: "", // Uncomment to clear base64 data
                    // fullscreenImage: "" // Uncomment to clear base64 data
                });
                console.log(`  ✅ Ad updated with GridFS references`);
                successCount++;
            }
            catch (error) {
                console.error(`  ❌ Error migrating ad ${ad._id}:`, error);
                errorCount++;
            }
        }
        console.log("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        console.log("📊 Migration Complete!");
        console.log(`  ✅ Success: ${successCount} ads`);
        console.log(`  ❌ Failed: ${errorCount} ads`);
        console.log("\n⚠️  IMPORTANT NEXT STEPS:");
        console.log("  1. Test the new /api/ads/active endpoint");
        console.log("  2. Verify mobile app can load images");
        console.log("  3. Run cleanup script to remove base64 data:");
        console.log("     npm run migrate:gridfs:cleanup");
        console.log("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
        process.exit(0);
    }
    catch (error) {
        console.error("❌ Migration failed:", error);
        process.exit(1);
    }
}
// Cleanup script - removes base64 data after successful migration
async function cleanupBase64Data() {
    try {
        console.log("🧹 Starting cleanup: Removing base64 data from ads");
        await mongoose_1.default.connect(MONGODB_URI);
        console.log("✅ Connected to MongoDB");
        const result = await Ad_1.default.updateMany({
            bottomImageGridFS: { $exists: true, $ne: null }
        }, {
            $set: {
                bottomImage: "", // Clear base64 - GridFS is now source of truth
                fullscreenImage: ""
            }
        });
        console.log(`✅ Cleaned up ${result.modifiedCount} ads`);
        console.log("💾 Estimated space saved: ~60-70MB per ad");
        process.exit(0);
    }
    catch (error) {
        console.error("❌ Cleanup failed:", error);
        process.exit(1);
    }
}
// Run migration or cleanup based on argument
const command = process.argv[2];
if (command === "cleanup") {
    cleanupBase64Data();
}
else {
    migrateAdsToGridFS();
}
