"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDirectBuyers = getDirectBuyers;
const mongoose_1 = __importDefault(require("mongoose"));
const User_1 = __importDefault(require("../../models/User"));
const SpecialCredit_1 = __importDefault(require("../../models/SpecialCredit"));
async function getDirectBuyers(parentId, limit, skip, voucherId) {
    let userFilter = { parentId };
    if (voucherId) {
        // Find users who received a SpecialCredit slot from this owner for this voucher
        const sentSlots = await SpecialCredit_1.default.find({
            ownerId: new mongoose_1.default.Types.ObjectId(parentId),
            voucherId: new mongoose_1.default.Types.ObjectId(voucherId),
            status: "sent",
        })
            .select("recipientId")
            .lean();
        const buyerIds = sentSlots
            .map((s) => s.recipientId?.toString())
            .filter(Boolean)
            .map((id) => new mongoose_1.default.Types.ObjectId(id));
        // If voucherId provided but no buyers yet, return empty immediately
        if (buyerIds.length === 0)
            return [];
        userFilter = { parentId, _id: { $in: buyerIds } };
    }
    const directUsers = await User_1.default.find(userFilter)
        .select("name phone level createdAt")
        .sort({ createdAt: -1 })
        .skip(skip)
        .limit(limit)
        .lean();
    if (directUsers.length === 0)
        return [];
    const directIds = directUsers.map((user) => user._id);
    const lookup = await User_1.default.aggregate([
        { $match: { _id: { $in: directIds } } },
        {
            $graphLookup: {
                from: "users",
                startWith: "$_id",
                connectFromField: "_id",
                connectToField: "parentId",
                as: "team",
                maxDepth: 9,
            },
        },
        { $project: { _id: 1, teamSize: { $size: "$team" } } },
    ]);
    const teamMap = new Map();
    lookup.forEach((entry) => teamMap.set(entry._id.toString(), entry.teamSize));
    return directUsers.map((user) => ({
        id: user._id.toString(),
        name: user.name,
        phone: user.phone,
        level: user.level || 0,
        teamSize: teamMap.get(user._id.toString()) || 0,
        joinedDate: user.createdAt,
    }));
}
