"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// Migrate all SpecialCredit slots that have null or old voucherId
// to point to the current published Instantlly admin template.
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
dotenv_1.default.config({ path: path_1.default.join(__dirname, "../../.env") });
const mongoose_1 = __importDefault(require("mongoose"));
const SpecialCredit_1 = __importDefault(require("../models/SpecialCredit"));
const Voucher_1 = __importDefault(require("../models/Voucher"));
(async () => {
    await mongoose_1.default.connect(process.env.MONGODB_URI);
    // Find the canonical published Instantlly template (no userId = admin template)
    const template = await Voucher_1.default.findOne({
        $or: [{ userId: { $exists: false } }, { userId: null }],
        isPublished: true,
        companyName: "Instantlly",
    }).lean();
    if (!template) {
        console.error("❌ No published Instantlly admin template found in DB. Run seed:instantlly-voucher first.");
        process.exit(1);
    }
    const newId = template._id;
    console.log(`Target template: ${newId}`);
    // Update slots that have no voucherId
    const r1 = await SpecialCredit_1.default.updateMany({ $or: [{ voucherId: { $exists: false } }, { voucherId: null }] }, { $set: { voucherId: newId } });
    console.log(`✅ Updated ${r1.modifiedCount} slot(s) with null voucherId → ${newId}`);
    // Update slots that point to any OTHER voucher (old/deleted templates)
    const r2 = await SpecialCredit_1.default.updateMany({ voucherId: { $ne: newId } }, { $set: { voucherId: newId } });
    console.log(`✅ Updated ${r2.modifiedCount} slot(s) with old voucherId → ${newId}`);
    const total = await SpecialCredit_1.default.countDocuments({ voucherId: newId });
    console.log(`\n🎉 Done. ${total} total slots now point to ${newId}`);
    await mongoose_1.default.connection.close();
})().catch((err) => {
    console.error("❌", err);
    process.exit(1);
});
