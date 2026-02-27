"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const dotenv_1 = __importDefault(require("dotenv"));
const mongoose_1 = __importDefault(require("mongoose"));
const bcryptjs_1 = __importDefault(require("bcryptjs"));
const User_1 = __importDefault(require("../models/User"));
dotenv_1.default.config();
async function main() {
    const uri = process.env.MONGODB_URI;
    if (!uri) {
        console.error('Please set MONGODB_URI in your environment or .env');
        process.exit(2);
    }
    const phoneArg = process.argv[2];
    const newPass = process.argv[3];
    if (!phoneArg || !newPass) {
        console.error('Usage: npx ts-node src/scripts/reset-password.ts <phone> <newPassword>');
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
    const user = await User_1.default.findOne({ phone: { $in: variants } });
    if (!user) {
        console.log('User not found for any variant.');
        process.exit(0);
    }
    const hashed = await bcryptjs_1.default.hash(newPass, 12);
    user.password = hashed;
    await user.save();
    console.log('Password reset successfully for user:', user._id.toString());
    await mongoose_1.default.disconnect();
}
main().catch(err => {
    console.error('ERROR', err);
    process.exit(1);
});
