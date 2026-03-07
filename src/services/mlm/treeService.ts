import mongoose from "mongoose";
import User from "../../models/User";
import SpecialCredit from "../../models/SpecialCredit";
import { getStructuralCreditPool } from "../../utils/mlm";

export async function buildNetworkTree(
  rootId: string,
  depth: number,
  perParentLimit: number,
  voucherId?: string,
) {
  const rootUser = await User.findById(rootId).select(
    "name phone parentId level directCount createdAt",
  );
  if (!rootUser) return null;

  const root: any = {
    id: rootUser._id.toString(),
    name: rootUser.name,
    phone: rootUser.phone,
    level: rootUser.level || 0,
    directCount: rootUser.directCount || 0,
    joinedDate: rootUser.createdAt,
    structuralCreditPool: getStructuralCreditPool(rootUser.level || 1),
    directChildren: [] as any[],
  };

  if (depth <= 0) return root;

  let currentParents = [rootId];
  const nodeMap = new Map<string, any>();
  nodeMap.set(rootId, root);

  for (let d = 0; d < depth; d += 1) {
    let children: any[] = [];
    if (voucherId && mongoose.isValidObjectId(voucherId)) {
      const voucherObjectId = new mongoose.Types.ObjectId(voucherId);
      const ownerObjectIds = currentParents.map(
        (id) => new mongoose.Types.ObjectId(id),
      );

      // Voucher-aware tree: only include child links that came from sent slots
      // for the selected voucher template.
      const sentSlots = await SpecialCredit.find({
        ownerId: { $in: ownerObjectIds },
        voucherId: voucherObjectId,
        status: "sent",
        recipientId: { $exists: true, $ne: null },
      })
        .select("ownerId recipientId sentAt")
        .sort({ sentAt: 1, createdAt: 1 })
        .lean();

      const allowedRecipientIds = [
        ...new Set(
          sentSlots
            .map((slot: any) => slot.recipientId?.toString())
            .filter(Boolean),
        ),
      ].map((id) => new mongoose.Types.ObjectId(id));

      if (allowedRecipientIds.length > 0) {
        children = (await User.find({
          parentId: { $in: ownerObjectIds },
          _id: { $in: allowedRecipientIds },
        })
          .select("name phone parentId level directCount createdAt")
          .sort({ createdAt: 1 })
          .lean()) as any[];
      }
    } else {
      children = (await User.find({ parentId: { $in: currentParents } })
        .select("name phone parentId level directCount createdAt")
        .sort({ createdAt: 1 })
        .lean()) as any[];
    }

    if (children.length === 0) break;

    const grouped: Record<string, any[]> = {};
    for (const child of children) {
      const parentKey = (child.parentId as any).toString();
      if (!grouped[parentKey]) grouped[parentKey] = [];
      grouped[parentKey].push(child);
    }

    const nextParents: string[] = [];

    for (const parentId of currentParents) {
      const parentNode = nodeMap.get(parentId);
      if (!parentNode) continue;

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
          structuralCreditPool: getStructuralCreditPool(child.level || 1),
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
