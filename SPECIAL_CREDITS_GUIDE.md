# Special Credits System - "Sales Target at Special Discount"

## Overview

This is a separate MLM credits system specifically for voucher admins and their network. It operates independently from the regular MLM credits system.

## Key Features

- **Admin**: 10 credit slots, each with 14,648,436,000 credits
- **Regular Users**: 5 credit slots with credits divided by 5 at each level
- **Vouchers Figure for Admin**: 122,070,300
- **Label**: "Sales Target at Special Discount"

## Credit Distribution Chain

Each level divides credits by 5:

| Level     | Credits Per Slot | Slots |
| --------- | ---------------- | ----- |
| 0 (Admin) | 14,648,436,000   | 10    |
| 1         | 2,929,686,000    | 5     |
| 2         | 585,936,000      | 5     |
| 3         | 117,186,000      | 5     |
| 4         | 23,436,000       | 5     |
| 5         | 4,686,000        | 5     |
| 6         | 936,000          | 5     |
| 7         | 186,000          | 5     |
| 8         | 36,000           | 5     |
| 9         | 6,000            | 5     |

## API Endpoints

### 1. Initialize Admin Special Credits

**POST** `/api/mlm/special-credits/admin/initialize`

Initializes the admin's 10 special credit slots.

**Headers:**

```
x-admin-key: your-secure-admin-key-here
```

**Body:**

```json
{
  "adminUserId": "676c4b9e123abc456def7890"
}
```

**Response:**

```json
{
  "success": true,
  "message": "Admin special credit slots initialized",
  "admin": {
    "id": "676c4b9e123abc456def7890",
    "name": "Rajesh Modi",
    "phone": "+91 9867477227",
    "isVoucherAdmin": true
  },
  "slots": 10,
  "creditPerSlot": 14648436000
}
```

---

### 2. Get User's Special Credit Slots

**GET** `/api/mlm/special-credits/slots`

Gets the current user's special credit slots (both used and available).

**Headers:**

```
Authorization: Bearer <token>
```

**Response:**

```json
{
  "success": true,
  "user": {
    "id": "676c4b9e123abc456def7890",
    "name": "Rajesh Modi",
    "phone": "+91 9867477227",
    "level": 0,
    "isVoucherAdmin": true
  },
  "slots": [
    {
      "slotNumber": 1,
      "status": "sent",
      "creditAmount": 14648436000,
      "recipientName": "Gori Farhan",
      "recipientPhone": "+91 9867969445",
      "recipientId": "676c4b9e123abc456def7891",
      "sentAt": "2026-02-23T10:30:00.000Z",
      "isAvailable": false
    },
    {
      "slotNumber": 2,
      "status": "available",
      "creditAmount": 14648436000,
      "recipientName": null,
      "recipientPhone": null,
      "recipientId": null,
      "sentAt": null,
      "isAvailable": true
    }
    // ... 8 more slots
  ],
  "summary": {
    "totalSlots": 10,
    "availableSlots": 9,
    "usedSlots": 1,
    "creditPerSlot": 14648436000
  }
}
```

---

### 3. Send Special Credits to User

**POST** `/api/mlm/special-credits/send`

Send special credits from a specific slot to a user by phone number.

**Headers:**

```
Authorization: Bearer <token>
```

**Body:**

```json
{
  "recipientPhone": "+91 9867969445",
  "slotNumber": 1
}
```

**Response:**

```json
{
  "success": true,
  "message": "Successfully sent 14,648,436,000 special credits to Gori Farhan",
  "transfer": {
    "sender": {
      "id": "676c4b9e123abc456def7890",
      "name": "Rajesh Modi",
      "phone": "+91 9867477227"
    },
    "recipient": {
      "id": "676c4b9e123abc456def7891",
      "name": "Gori Farhan",
      "phone": "+91 9867969445",
      "level": 1
    },
    "slot": {
      "slotNumber": 1,
      "creditAmount": 14648436000,
      "sentAt": "2026-02-23T10:30:00.000Z"
    },
    "recipientSlotsCreated": 5,
    "recipientCreditPerSlot": 2929686000
  }
}
```

---

### 4. Get Special Credits Dashboard

**GET** `/api/mlm/special-credits/dashboard`

Gets comprehensive dashboard data for special credits.

**Headers:**

```
Authorization: Bearer <token>
```

**Response:**

```json
{
  "success": true,
  "user": {
    "id": "676c4b9e123abc456def7890",
    "name": "Rajesh Modi",
    "phone": "+91 9867477227",
    "level": 0,
    "isVoucherAdmin": true
  },
  "dashboard": {
    "vouchersFigure": 122070300,
    "specialCredits": {
      "balance": 0,
      "totalReceived": 0,
      "totalSent": 14648436000,
      "creditPerSlot": 14648436000
    },
    "slots": {
      "total": 10,
      "available": 9,
      "used": 1
    },
    "label": "Sales Target at Special Discount"
  },
  "networkUsers": [
    {
      "id": "676c4b9e123abc456def7891",
      "name": "Gori Farhan",
      "phone": "+91 9867969445",
      "level": 1,
      "creditsReceived": 14648436000
    }
  ]
}
```

---

### 5. Get Network Users with Placeholders

**GET** `/api/mlm/special-credits/network`

Gets network users including empty placeholders for UI display.

**Headers:**

```
Authorization: Bearer <token>
```

**Response:**

```json
{
  "success": true,
  "networkUsers": [
    {
      "slotNumber": 1,
      "name": "Gori Farhan",
      "phone": "+91 9867969445",
      "credits": 14648436000,
      "sentAt": "2026-02-23T10:30:00.000Z",
      "recipientLevel": 1,
      "isPlaceholder": false
    },
    {
      "slotNumber": 2,
      "name": "",
      "phone": "",
      "credits": 14648436000,
      "sentAt": null,
      "recipientLevel": null,
      "isPlaceholder": true
    }
    // ... 8 more slots
  ],
  "summary": {
    "totalSlots": 10,
    "usedSlots": 1,
    "availableSlots": 9,
    "creditPerSlot": 14648436000
  }
}
```

---

## Database Schema

### SpecialCredit Model

```typescript
{
  ownerId: ObjectId,          // User who owns this slot
  slotNumber: Number,          // 1-10 for admin, 1-5 for users
  creditAmount: Number,        // Credits in this slot
  status: String,              // "available" | "sent" | "expired"
  recipientId: ObjectId,       // User who received the credits
  recipientName: String,       // Recipient's name
  recipientPhone: String,      // Recipient's phone
  sentAt: Date,                // When credits were sent
  level: Number,               // Level in the chain (0-9)
  sourceSlotId: ObjectId,      // Parent slot that created this
  createdAt: Date,
  updatedAt: Date
}
```

### User Model (New Fields)

```typescript
{
  // ... existing fields ...

  specialCredits: {
    balance: Number,           // Current special credits balance
    totalReceived: Number,     // Lifetime received
    totalSent: Number,         // Lifetime sent
    availableSlots: Number,    // Number of slots (10 for admin, 5 for others)
    usedSlots: Number          // Number of used slots
  },

  isVoucherAdmin: Boolean      // True for root admin
}
```

---

## Usage Flow

### Step 1: Initialize Admin

```bash
curl -X POST http://localhost:3000/api/mlm/special-credits/admin/initialize \
  -H "x-admin-key: your-secure-admin-key-here" \
  -H "Content-Type: application/json" \
  -d '{
    "adminUserId": "676c4b9e123abc456def7890"
  }'
```

### Step 2: Admin Views Dashboard

```bash
curl -X GET http://localhost:3000/api/mlm/special-credits/dashboard \
  -H "Authorization: Bearer <admin-token>"
```

### Step 3: Admin Sends Credits to User

```bash
curl -X POST http://localhost:3000/api/mlm/special-credits/send \
  -H "Authorization: Bearer <admin-token>" \
  -H "Content-Type: application/json" \
  -d '{
    "recipientPhone": "+91 9867969445",
    "slotNumber": 1
  }'
```

### Step 4: Recipient Views Their Slots

```bash
curl -X GET http://localhost:3000/api/mlm/special-credits/slots \
  -H "Authorization: Bearer <recipient-token>"
```

### Step 5: Recipient Sends to Another User

```bash
curl -X POST http://localhost:3000/api/mlm/special-credits/send \
  -H "Authorization: Bearer <recipient-token>" \
  -H "Content-Type: application/json" \
  -d '{
    "recipientPhone": "+91 1234567890",
    "slotNumber": 1
  }'
```

---

## UI Display Guidelines

### Admin Dashboard

- **Vouchers Figure**: 122,070,300 (display prominently)
- **Label**: "Sales Target at Special Discount"
- **Network Section**: 10 rows
  - Empty slots show: Blank name/phone + "Send" button
  - Filled slots show: Actual name/phone + "Call" button
  - Each row displays: 14,648,436,000 credits

### Regular User Dashboard

- **Label**: "Sales Target at Special Discount"
- **Network Section**: 5 rows
  - Empty slots show: Blank name/phone + "Send" button
  - Filled slots show: Actual name/phone + "Call" button
  - Each row displays: Credits based on level (e.g., 2,929,686,000 for Level 1)

---

## Example Use Case

1. **Admin (Rajesh Modi)** has 10 slots × 14,648,436,000 credits
2. Admin sends slot 1 to **Gori Farhan** (+91 9867969445)
3. Farhan automatically gets 5 slots × 2,929,686,000 credits
4. Farhan sends slot 1 to **User A**
5. User A gets 5 slots × 585,936,000 credits
6. This continues down the chain up to 10 levels

---

## Security Notes

- Only authenticated users can access their own slots
- Admin initialization requires admin key
- Phone number validation ensures users exist before sending
- Slots can only be used once (cannot override sent credits)
- Parent-child relationships are automatically maintained

---

## Testing

### Test Admin Credentials

- **Phone**: +91 9867477227
- **Name**: Rajesh Modi

### Test User Credentials

- **Phone**: +91 9867969445
- **Name**: Gori Farhan

---

## Frontend Integration Notes

1. **Dashboard Screen**: Call `/special-credits/dashboard` to get vouchers figure and stats
2. **Network Tab**: Call `/special-credits/network` to get all slots with placeholders
3. **Send Action**: Call `/special-credits/send` with phone number and slot number
4. **Auto-refresh**: After sending, refresh the network view to show updated data
5. **Call Button**: For sent slots, show "Call" button that opens phone dialer with recipient's phone number

---

## Credits Calculation Reference

```javascript
const SPECIAL_CREDIT_CHAIN = [
  14648436000, // Level 0 (Admin)
  2929686000, // Level 1 (÷5)
  585936000, // Level 2 (÷5)
  117186000, // Level 3 (÷5)
  23436000, // Level 4 (÷5)
  4686000, // Level 5 (÷5)
  936000, // Level 6 (÷5)
  186000, // Level 7 (÷5)
  36000, // Level 8 (÷5)
  6000, // Level 9 (÷5)
];
```
