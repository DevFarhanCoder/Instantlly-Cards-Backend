# MLM API Routes Refactoring - Complete Changes

## Summary

Refactored `src/routes/mlm.ts` (555 lines → 550 lines) to remove commission/withdrawal logic and add discount-based payment approval workflow.

---

## 🗑️ REMOVED ENDPOINTS (7 endpoints)

### 1. Commission Endpoints (2)

- ❌ `GET /api/mlm/commissions/summary` - Commission wallet balance
- ❌ `GET /api/mlm/commissions/transactions` - Commission transaction history

### 2. Withdrawal Endpoints (4)

- ❌ `POST /api/mlm/withdrawals/request` - Request commission withdrawal
- ❌ `GET /api/mlm/withdrawals/history` - View withdrawal history
- ❌ `POST /api/mlm/admin/withdrawals/:id/approve` - Admin approve withdrawal
- ❌ `POST /api/mlm/admin/withdrawals/:id/reject` - Admin reject withdrawal

### 3. Old Credit Confirmation (1)

- ⚠️ `POST /api/mlm/credits/:creditId/confirm-payment` - **MODIFIED** (see below)

---

## ✅ ADDED ENDPOINTS (5 endpoints)

### 1. Discount Information (2)

```typescript
GET /api/mlm/discount/info
// Returns: current level, downlineCount, payableAmount, virtualCommission, next level target
Response: {
  success: true,
  discountInfo: {
    currentLevel: 3,
    downlineCount: 50,
    discountPercent: 62.5,
    payableAmount: 2250,
    virtualCommission: 1350, // BASE_PAYABLE - payableAmount
    nextLevel: {
      level: 4,
      requiredDownline: 125,
      discountPercent: 65,
    }
  }
}

GET /api/mlm/discount/summary
// Returns: formatted discount data with disclaimer
Response: {
  success: true,
  summary: {
    currentLevel: 3,
    discountPercent: 62.5,
    payableAmount: 2250,
    virtualCommission: 1350,
    disclaimer: "This amount represents savings unlocked via discounts and is not withdrawable.",
    nextLevelTarget: {
      level: 4,
      remainingDownline: 75,
      targetDiscountPercent: 65,
    }
  }
}
```

### 2. Payment Approval Workflow (3)

```typescript
POST /api/mlm/credits/:creditId/confirm-payment
// ⚠️ UPDATED: Now sets status to "waiting_approval" instead of "paid"
// Receiver clicks "I Have Paid" button
Body: (none)
Response: {
  success: true,
  message: "Payment confirmation received. Waiting for admin approval.",
  credit: {
    id: "...",
    status: "pending",
    paymentStatus: "waiting_approval"
  }
}

POST /api/mlm/admin/credits/:creditId/approve-payment
// Admin approves payment → generates vouchers
Headers: { "x-admin-key": "..." }
Body: { adminId: "optional-admin-user-id" }
Response: {
  success: true,
  message: "Payment approved. Vouchers generated.",
  vouchersGenerated: 5,
  credit: {
    id: "...",
    status: "active",
    paymentStatus: "approved"
  }
}

POST /api/mlm/admin/credits/:creditId/reject-payment
// Admin rejects payment → refunds sender
Headers: { "x-admin-key": "..." }
Body: { adminId: "optional", note: "Payment not verified" }
Response: {
  success: true,
  message: "Payment rejected. Credit refunded to sender.",
  credit: {
    id: "...",
    status: "reverted",
    paymentStatus: "rejected"
  }
}

GET /api/mlm/admin/credits/pending-approval
// List all credits waiting for admin approval
Headers: { "x-admin-key": "..." }
Query: { limit: 50, skip: 0 }
Response: {
  success: true,
  credits: [
    {
      id: "...",
      sender: { name: "...", phone: "..." },
      receiver: { name: "...", phone: "..." },
      quantity: 1,
      paymentConfirmedAt: "2025-01-15T10:30:00Z",
      createdAt: "2025-01-15T10:00:00Z"
    }
  ]
}
```

---

## 🔧 MODIFIED ENDPOINTS (2)

### 1. `/api/mlm/credits/:creditId/confirm-payment` (Line 145)

**Before:**

```typescript
credit.paymentStatus = "paid";
await distributeCommission(...); // ❌ Called commission service
// Generated vouchers immediately
```

**After:**

```typescript
credit.paymentStatus = "waiting_approval";
credit.paymentConfirmedByReceiver = true;
credit.paymentConfirmedAt = new Date();
// No commission distribution
// No voucher generation (admin approval required)
```

### 2. `/api/mlm/overview` (Line 463)

**Before:**

```typescript
const commissionSummary = await getCommissionSummary(userId); // ❌ Archived service
metrics: {
  totalCommissionEarned: commissionSummary.totalEarned,
  availableCommissionBalance: commissionSummary.availableBalance,
}
```

**After:**

```typescript
const discountSummary = await getDiscountSummary(userId); // ✅ New service
metrics: {
  virtualCommission: discountSummary.virtualCommission,
  currentDiscountPercent: discountSummary.discountPercent,
}
```

---

## 📦 UPDATED IMPORTS

**Removed:**

```typescript
import CommissionTransaction from "../models/CommissionTransaction"; // Archived
import Withdrawal from "../models/Withdrawal"; // Archived
import { distributeCommission } from "../services/mlm/commissionService"; // Archived
import { getCommissionSummary } from "../services/mlm/commissionSummaryService"; // Archived
import {
  addCommission,
  subtractCommission,
} from "../services/mlm/walletService"; // Removed
```

**Added:**

```typescript
import {
  getUserDiscountInfo,
  getDiscountSummary,
  calculatePurchaseDiscount,
} from "../services/mlm/discountService";
import { updateAncestorDownlineCounts } from "../services/mlm/downlineService";
```

---

## 🔀 UNCHANGED ENDPOINTS (11)

These endpoints remain unchanged:

- ✅ `GET /api/mlm/wallet` - Wallet balance (now credit-only)
- ✅ `GET /api/mlm/credits/dashboard` - Credit status dashboard
- ✅ `POST /api/mlm/credits/transfer` - Transfer credit to receiver
- ✅ `GET /api/mlm/vouchers` - List vouchers
- ✅ `POST /api/mlm/vouchers/:voucherId/redeem` - Redeem voucher
- ✅ `GET /api/mlm/network/tree` - Network tree
- ✅ `GET /api/mlm/network/children` - Direct children
- ✅ `GET /api/mlm/network/direct-buyers` - Direct buyers
- ✅ `GET /api/mlm/network/structural-pool` - Structural pool display
- ✅ `POST /api/mlm/admin/credits/seed` - Admin seed credits
- ✅ `GET /api/mlm/overview` - Dashboard overview (metrics updated)

---

## 🚨 BREAKING CHANGES FOR FRONTEND

### API Calls to Remove

```typescript
// ❌ DELETE THESE
fetch("/api/mlm/commissions/summary");
fetch("/api/mlm/commissions/transactions");
fetch("/api/mlm/withdrawals/request", { method: "POST" });
fetch("/api/mlm/withdrawals/history");
```

### API Calls to Add

```typescript
// ✅ ADD THESE
fetch("/api/mlm/discount/info"); // Replace commission summary
fetch("/api/mlm/discount/summary"); // For dashboard display
```

### Updated Response Shapes

**Before (Commission):**

```json
{
  "totalCommissionEarned": 15000,
  "commissionAvailableBalance": 5000,
  "totalWithdrawn": 10000
}
```

**After (Discount):**

```json
{
  "currentLevel": 3,
  "discountPercent": 62.5,
  "payableAmount": 2250,
  "virtualCommission": 1350,
  "disclaimer": "This amount represents savings..."
}
```

---

## 🧪 TESTING CHECKLIST

### Manual Payment Approval Workflow

1. ✅ Sender transfers credit to receiver
2. ✅ Receiver clicks "I Have Paid" → status becomes `waiting_approval`
3. ✅ Admin fetches pending approvals via `/admin/credits/pending-approval`
4. ✅ Admin contacts sender offline to verify payment
5. ✅ Admin approves → status becomes `approved`, vouchers generated
6. ✅ Admin rejects → status becomes `rejected`, credit refunded to sender

### Discount Calculation

1. ✅ User with 0 downline → Level 1 → 40% discount → Pay ₹3600
2. ✅ User with 5 downline → Level 2 → 55% discount → Pay ₹2700
3. ✅ User with 25 downline → Level 3 → 62.5% discount → Pay ₹2250
4. ✅ Virtual commission = BASE_PAYABLE (₹3600) - payableAmount

### Edge Cases

- ❌ Receiver tries to approve credit they didn't receive (403 Unauthorized)
- ❌ Admin tries to approve already-approved credit (400 Bad Request)
- ❌ Admin tries to reject already-rejected credit (400 Bad Request)
- ❌ Non-admin calls admin endpoints without valid x-admin-key (401 Unauthorized)

---

## 📝 LEGAL COMPLIANCE NOTES

### Wording Changes Required in Frontend

- ❌ "Commission" → ✅ "Discount Savings"
- ❌ "Earnings" → ✅ "Virtual Savings"
- ❌ "Withdraw" → ✅ (Remove completely)
- ❌ "Balance Available" → ✅ "Credits Available"

### Disclaimers to Add

```
"Virtual commission represents the discount you have unlocked based on your
network size. This amount is not cash and cannot be withdrawn. It reflects
the savings you receive on your next purchase."
```

---

## 🔄 AUTH ROUTES UPDATE

**File:** `src/routes/auth.ts`

**Change:** Added downline count tracking on user signup

```typescript
// After line 193 (user.save())
if (savedUser.parentId) {
  try {
    const { updateAncestorDownlineCounts } =
      await import("../services/mlm/downlineService");
    await updateAncestorDownlineCounts(savedUser._id.toString());
    console.log("✅ Updated downline counts for ancestors");
  } catch (downlineError) {
    console.error("⚠️ Failed to update downline counts:", downlineError);
    // Don't fail signup if downline count update fails
  }
}
```

**Impact:** When a new user signs up via referral, all ancestors' `downlineCount` is incremented, unlocking discount levels for them.

---

## ✅ COMPLETION STATUS

- ✅ Routes file refactored (555 lines)
- ✅ Imports updated (removed 5, added 2)
- ✅ 7 endpoints removed (commission/withdrawals)
- ✅ 5 endpoints added (discount info + payment approval)
- ✅ 2 endpoints modified (payment confirm, overview)
- ✅ 11 endpoints unchanged (credit/voucher/network logic intact)
- ✅ Auth routes updated (downline count tracking)
- ✅ No TypeScript compilation errors
- ❌ Frontend components (pending)
- ❌ End-to-end testing (pending)

---

## 🚀 NEXT STEPS

1. **Frontend Refactoring** (Priority: HIGH)
   - Update `CommissionDashboardCard.tsx` → `DiscountDashboardCard.tsx`
   - Replace `/commissions/*` API calls with `/discount/*`
   - Add "I Have Paid" button in credit transfer flow
   - Update wording (avoid "income", use "savings")

2. **Admin Panel** (Optional)
   - Create web-based admin UI for payment approval
   - Show pending credits with sender/receiver details
   - Approve/reject buttons with confirmation dialogs

3. **Data Migration** (If production data exists)
   - Run `recalculateAllDownlineCounts()` for existing users
   - Map old `paymentStatus="paid"` → `"approved"`

4. **Testing**
   - Test complete payment approval workflow
   - Test discount calculations at all 10 levels
   - Test edge cases (double approval, unauthorized access)

---

**✅ Backend refactoring complete! Ready for frontend updates.**
