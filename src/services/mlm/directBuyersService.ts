import mongoose from "mongoose";
import User from "../../models/User";
import Voucher from "../../models/Voucher";

export async function getDirectBuyers(
  parentId: string,
  limit: number,
  skip: number,
  voucherId?: string,
) {
  let userFilter: Record<string, any> = { parentId };

  if (voucherId) {
    // Find user IDs who own a voucher with this creditId (voucher package)
    const matchingVouchers = await Voucher.find({
      creditId: new mongoose.Types.ObjectId(voucherId),
    })
      .select("userId")
      .lean();

    const buyerIds = [
      ...new Set(
        matchingVouchers
          .map((v) => (v.userId as any)?.toString())
          .filter(Boolean),
      ),
    ].map((id) => new mongoose.Types.ObjectId(id));

    userFilter = { parentId, _id: { $in: buyerIds } };
  }

  const directUsers = await User.find(userFilter)
    .select("name phone level createdAt")
    .sort({ createdAt: -1 })
    .skip(skip)
    .limit(limit)
    .lean();

  if (directUsers.length === 0) return [];

  const directIds = directUsers.map((user) => user._id);
  const lookup = await User.aggregate([
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

  const teamMap = new Map<string, number>();
  lookup.forEach((entry) => teamMap.set(entry._id.toString(), entry.teamSize));

  return (directUsers as any[]).map((user: any) => ({
    id: (user._id as any).toString(),
    name: user.name,
    phone: user.phone,
    level: user.level || 0,
    teamSize: teamMap.get((user._id as any).toString()) || 0,
    joinedDate: user.createdAt,
  }));
}
