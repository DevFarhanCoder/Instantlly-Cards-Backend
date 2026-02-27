"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const dotenv_1 = __importDefault(require("dotenv"));
const mongoose_1 = __importDefault(require("mongoose"));
const User_1 = __importDefault(require("../models/User"));
dotenv_1.default.config();
async function main() {
    const uri = process.env.MONGODB_URI;
    if (!uri) {
        console.error('Please set MONGODB_URI in your environment or .env');
        process.exit(2);
    }
    const phoneArg = process.argv[2];
    if (!phoneArg) {
        console.error('Usage: npx ts-node src/scripts/check-user.ts <phone>');
        process.exit(2);
    }
    const normalized = phoneArg.replace(/[\s\-()]/g, '');
    const variants = [
        normalized,
        normalized.startsWith('+') ? normalized : ('+' + normalized),
        normalized.replace(/^\+/, ''),
        normalized.replace(/^\+91/, ''),
    ].filter(Boolean);
    console.log('Connecting to MongoDB...');
    await mongoose_1.default.connect(uri, {});
    console.log('Looking for user using variants:', variants);
    const user = (await User_1.default.findOne({ phone: { $in: variants } }).lean());
    if (!user) {
        console.log('User not found for any variant.');
        process.exit(0);
    }
    console.log('User found:');
    console.log({ _id: user._id, phone: user.phone, hasPassword: !!user.password });
    await mongoose_1.default.disconnect();
}
main().catch(err => {
    console.error('ERROR', err);
    process.exit(1);
});
