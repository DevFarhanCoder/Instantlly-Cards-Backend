"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
dotenv_1.default.config({ path: path_1.default.join(__dirname, "../../.env") });
const mongoose_1 = __importDefault(require("mongoose"));
const Voucher_1 = __importDefault(require("../models/Voucher"));
(async () => {
    await mongoose_1.default.connect(process.env.MONGODB_URI);
    // Delete admin-level template that has the wrong values (₹500, 0% off)
    const result = await Voucher_1.default.deleteMany({
        $or: [{ userId: { $exists: false } }, { userId: null }],
        amount: 500,
        discountPercentage: 0,
    });
    console.log(`✅ Deleted ${result.deletedCount} duplicate voucher(s)`);
    await mongoose_1.default.connection.close();
})();
