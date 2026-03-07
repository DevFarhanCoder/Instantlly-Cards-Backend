"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.buildNetworkTree = buildNetworkTree;
const mongoose_1 = __importDefault(require("mongoose"));
const User_1 = __importDefault(require("../../models/User"));
const SpecialCredit_1 = __importDefault(require("../../models/SpecialCredit"));
const mlm_1 = require("../../utils/mlm");
async function buildNetworkTree(rootId, depth, perParentLimit, voucherId) {
    const rootUser = await User_1.default.findById(rootId).select("name phone parentId level directCount createdAt");
    if (!rootUser)
        return null;
    const root = {
        id: rootUser._id.toString(),
        name: rootUser.name,
        phone: rootUser.phone,
        level: rootUser.level || 0,
        directCount: rootUser.directCount || 0,
        joinedDate: rootUser.createdAt,
        structuralCreditPool: (0, mlm_1.getStructuralCreditPool)(rootUser.level || 1),
        directChildren: [],
    };
    if (depth <= 0)
        return root;
    let currentParents = [rootId];
    const nodeMap = new Map();
    nodeMap.set(rootId, root);
    for (let d = 0; d < depth; d += 1) {
        let children = [];
        if (voucherId && mongoose_1.default.isValidObjectId(voucherId)) {
            const voucherObjectId = new mongoose_1.default.Types.ObjectId(voucherId);
            const ownerObjectIds = currentParents.map((id) => new mongoose_1.default.Types.ObjectId(id));
            // Voucher-aware tree: only include child links that came from sent slots
            // for the selected voucher template.
            const sentSlots = await SpecialCredit_1.default.find({
                ownerId: { $in: ownerObjectIds },
                voucherId: voucherObjectId,
                status: "sent",
                recipientId: { $exists: true, $ne: null },
            })
                .select("ownerId recipientId sentAt")
                .sort({ sentAt: 1, createdAt: 1 })
                .lean();
            const allowedRecipientIds = [
                ...new Set(sentSlots
                    .map((slot) => slot.recipientId?.toString())
                    .filter(Boolean)),
            ].map((id) => new mongoose_1.default.Types.ObjectId(id));
            if (allowedRecipientIds.length > 0) {
                children = (await User_1.default.find({
                    parentId: { $in: ownerObjectIds },
                    _id: { $in: allowedRecipientIds },
                })
                    .select("name phone parentId level directCount createdAt")
                    .sort({ createdAt: 1 })
                    .lean());
            }
        }
        else {
            children = (await User_1.default.find({ parentId: { $in: currentParents } })
                .select("name phone parentId level directCount createdAt")
                .sort({ createdAt: 1 })
                .lean());
        }
        if (children.length === 0)
            break;
        const grouped = {};
        for (const child of children) {
            const parentKey = child.parentId.toString();
            if (!grouped[parentKey])
                grouped[parentKey] = [];
            grouped[parentKey].push(child);
        }
        const nextParents = [];
        for (const parentId of currentParents) {
            const parentNode = nodeMap.get(parentId);
            if (!parentNode)
                continue;
            const group = grouped[parentId] || [];
            const limitedChildren = group.slice(0, perParentLimit);
            if (voucherId) {
                parentNode.directCount = limitedChildren.length;
            }
            parentNode.directChildren = limitedChildren.map((child) => {
                const node = {
                    id: child._id.toString(),
                    name: child.name,
                    phone: child.phone,
                    level: child.level || 0,
                    directCount: voucherId ? 0 : child.directCount || 0,
                    joinedDate: child.createdAt,
                    structuralCreditPool: (0, mlm_1.getStructuralCreditPool)(child.level || 1),
                    directChildren: [],
                };
                nodeMap.set(node.id, node);
                nextParents.push(node.id);
                return node;
            });
        }
        currentParents = nextParents;
    }
    return root;
}
