"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// Remove duplicate slot numbers per (ownerId, voucherId).
// Keeps the slot with the higher status priority (sent > available) or higher _id.
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
dotenv_1.default.config({ path: path_1.default.join(__dirname, "../../.env") });
const mongoose_1 = __importDefault(require("mongoose"));
const SpecialCredit_1 = __importDefault(require("../models/SpecialCredit"));
(async () => {
    await mongoose_1.default.connect(process.env.MONGODB_URI);
    // Get all distinct (ownerId, voucherId) combos
    const groups = await SpecialCredit_1.default.aggregate([
        { $group: { _id: { ownerId: "$ownerId", voucherId: "$voucherId" } } },
    ]);
    let totalDeleted = 0;
    for (const g of groups) {
        const { ownerId, voucherId } = g._id;
        // Get all slots for this owner+voucher sorted by slotNumber, then prefer "sent" status
        const slots = await SpecialCredit_1.default.find({ ownerId, voucherId })
            .sort({ slotNumber: 1, status: -1 }) // "sent" > "available" alphabetically desc
            .lean();
        const seen = new Map();
        const toDelete = [];
        for (const slot of slots) {
            const num = slot.slotNumber;
            const id = slot._id;
            if (seen.has(num)) {
                // Prefer "sent" over "available"; if same status keep the older one
                const existingId = seen.get(num);
                const existingSlot = slots.find((s) => s._id.equals(existingId));
                if (slot.status === "sent" &&
                    existingSlot.status !== "sent") {
                    // New one is sent, delete the old available one
                    toDelete.push(existingId);
                    seen.set(num, id);
                }
                else {
                    // Keep existing, delete this one
                    toDelete.push(id);
                }
            }
            else {
                seen.set(num, id);
            }
        }
        if (toDelete.length > 0) {
            await SpecialCredit_1.default.deleteMany({ _id: { $in: toDelete } });
            totalDeleted += toDelete.length;
            console.log(`ownerId=${ownerId} voucherId=${voucherId}: deleted ${toDelete.length} duplicates`);
        }
    }
    console.log(`\n✅ Done. Removed ${totalDeleted} duplicate slot(s).`);
    const remaining = await SpecialCredit_1.default.countDocuments();
    console.log(`Total slots remaining: ${remaining}`);
    await mongoose_1.default.connection.close();
})().catch((err) => {
    console.error("❌", err);
    process.exit(1);
});
