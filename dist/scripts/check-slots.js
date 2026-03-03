"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
dotenv_1.default.config({ path: path_1.default.join(__dirname, "../../.env") });
const mongoose_1 = __importDefault(require("mongoose"));
const SpecialCredit_1 = __importDefault(require("../models/SpecialCredit"));
const Voucher_1 = __importDefault(require("../models/Voucher"));
(async () => {
    await mongoose_1.default.connect(process.env.MONGODB_URI);
    const distinct = await SpecialCredit_1.default.distinct("voucherId");
    console.log("voucherIds in existing slots:", JSON.stringify(distinct));
    const total = await SpecialCredit_1.default.countDocuments();
    console.log("total SpecialCredit docs:", total);
    // Find the correct published Instantlly template
    const template = await Voucher_1.default.findOne({
        $or: [{ userId: { $exists: false } }, { userId: null }],
        isPublished: true,
        companyName: "Instantlly",
    }).lean();
    console.log("target voucher template:", template ? template._id.toString() : "NOT FOUND");
    await mongoose_1.default.connection.close();
})().catch(console.error);
