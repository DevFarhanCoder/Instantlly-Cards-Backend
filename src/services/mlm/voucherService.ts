import { v4 as uuidv4 } from "uuid";
import Voucher from "../../models/Voucher";

const DEFAULT_VOUCHER_MRP = 1200;
const VOUCHER_EXPIRY_DAYS = 365;

export async function generateVouchers(
  userId: string,
  creditId: string,
  quantity: number,
) {
  const now = new Date();
  const expiry = new Date(now);
  expiry.setDate(expiry.getDate() + VOUCHER_EXPIRY_DAYS);

  const vouchers = Array.from({ length: quantity }).map(() => ({
    userId,
    creditId,
    voucherNumber: uuidv4().replace(/-/g, "").slice(0, 12).toUpperCase(),
    MRP: DEFAULT_VOUCHER_MRP,
    issueDate: now,
    expiryDate: expiry,
  }));

  return Voucher.insertMany(vouchers);
}
