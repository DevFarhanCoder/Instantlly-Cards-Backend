import MlmCredit from "../../models/MlmCredit";

export async function getCreditDashboard(userId: string) {
  const [received, transferred, activeCredits, recentTransfers] =
    await Promise.all([
      MlmCredit.countDocuments({ receiverId: userId }),
      MlmCredit.countDocuments({ senderId: userId }),
      MlmCredit.find({
        receiverId: userId,
        status: { $in: ["pending", "active"] },
      })
        .sort({ createdAt: -1 })
        .limit(10)
        .lean(),
      MlmCredit.find({ senderId: userId })
        .sort({ createdAt: -1 })
        .limit(10)
        .populate("receiverId", "name")
        .lean(),
    ]);

  const timers = (activeCredits as any[]).map((credit) => ({
    creditId: credit._id.toString(),
    status: credit.status,
    paymentStatus: credit.paymentStatus,
    expiresAt: credit.expiresAt,
    transferExpiresAt: credit.transferExpiresAt,
    remainingTransfers: Math.max(
      0,
      (credit.quantity || 0) - (credit.transferredCount || 0),
    ),
  }));

  return {
    totalCreditsReceived: received,
    totalCreditsTransferred: transferred,
    activeCredits: activeCredits.filter((credit) => credit.status === "active")
      .length,
    timers,
    recentTransfers: recentTransfers.map((credit: any) => {
      let status = "completed";
      if (credit.status === "pending") status = "pending";
      if (credit.status === "expired" || credit.status === "reverted") {
        status = "returned";
      }

      return {
        id: credit._id.toString(),
        recipientName: credit.receiverId?.name || "User",
        recipientId: credit.receiverId?._id?.toString() || "",
        amount: 1,
        date: credit.createdAt,
        status,
      };
    }),
  };
}
