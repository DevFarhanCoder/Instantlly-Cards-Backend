"use strict";
// ✅ DOWNLINE TRACKING SERVICE
// Updates downlineCount for all ancestors when a new member joins
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.updateAncestorDownlineCounts = updateAncestorDownlineCounts;
exports.calculateDownlineCount = calculateDownlineCount;
exports.recalculateAllDownlineCounts = recalculateAllDownlineCounts;
const User_1 = __importDefault(require("../../models/User"));
/**
 * Update downline count for all ancestors when a new user joins
 * @param userId - New user who just joined
 */
async function updateAncestorDownlineCounts(userId) {
    const user = await User_1.default.findById(userId);
    if (!user || !user.parentId) {
        return; // Root user or no parent
    }
    // Traverse up the tree and increment downlineCount for all ancestors
    let currentParentId = user.parentId;
    const updatedAncestors = [];
    while (currentParentId) {
        await User_1.default.findByIdAndUpdate(currentParentId, {
            $inc: { downlineCount: 1 },
        });
        updatedAncestors.push(currentParentId.toString());
        // Get next parent
        const parent = await User_1.default.findById(currentParentId);
        if (!parent || !parent.parentId) {
            break;
        }
        currentParentId = parent.parentId;
    }
    console.log(`✅ Updated downlineCount for ${updatedAncestors.length} ancestors`);
    return updatedAncestors;
}
/**
 * Calculate total downline count for a user (recursive count of all descendants)
 * Use this for one-time fixes or validation
 * @param userId - User ID to calculate for
 * @returns Total descendant count
 */
async function calculateDownlineCount(userId) {
    const directChildren = await User_1.default.find({ parentId: userId }).lean();
    if (directChildren.length === 0) {
        return 0;
    }
    let total = directChildren.length;
    for (const child of directChildren) {
        const childDownline = await calculateDownlineCount(child._id.toString());
        total += childDownline;
    }
    return total;
}
/**
 * Recalculate and update downlineCount for all users (admin utility)
 * Use this if downlineCount gets out of sync
 */
async function recalculateAllDownlineCounts() {
    const allUsers = await User_1.default.find({}).lean();
    for (const user of allUsers) {
        const downlineCount = await calculateDownlineCount(user._id.toString());
        await User_1.default.findByIdAndUpdate(user._id, { downlineCount });
        console.log(`✅ Updated ${user.name}: ${downlineCount} downline members`);
    }
    console.log(`✅ Recalculated downlineCount for ${allUsers.length} users`);
}
