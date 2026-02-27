"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDirectBuyers = getDirectBuyers;
const User_1 = __importDefault(require("../../models/User"));
async function getDirectBuyers(parentId, limit, skip) {
    const directUsers = await User_1.default.find({ parentId })
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
