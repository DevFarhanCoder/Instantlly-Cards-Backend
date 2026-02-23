# Quick API Testing Guide - Special Credits System

## Prerequisites

1. Have Postman or similar API client installed
2. Backend server running on `http://localhost:3000` (or your server URL)
3. Admin and test user accounts exist in the database

## Test Data

**Admin Account:**

- Name: Rajesh Modi
- Phone: +91 9867477227

**Test User:**

- Name: Gori Farhan
- Phone: +91 9867969445

---

## Step-by-Step Testing

### Step 1: Initialize Admin Special Credits

This only needs to be run once to set up the admin.

**Option A: Using Node Script (Recommended)**

```bash
cd Instantlly-Cards-Backend
node init-admin-special-credits.js
```

**Option B: Using API Endpoint**

```
POST http://localhost:3000/api/mlm/special-credits/admin/initialize
Headers:
  x-admin-key: your-secure-admin-key-here
  Content-Type: application/json

Body:
{
  "adminUserId": "<ADMIN_USER_ID_HERE>"
}
```

**Expected Response:**

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

### Step 2: Admin Login & Get Token

```
POST http://localhost:3000/api/auth/login
Content-Type: application/json

Body:
{
  "phone": "+91 9867477227"
}
```

**Expected Response:**

```json
{
  "success": true,
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "user": {
    "id": "676c4b9e123abc456def7890",
    "name": "Rajesh Modi",
    "phone": "+91 9867477227"
  }
}
```

**💡 Copy the token for next steps**

---

### Step 3: View Admin Dashboard

```
GET http://localhost:3000/api/mlm/special-credits/dashboard
Headers:
  Authorization: Bearer <ADMIN_TOKEN>
```

**Expected Response:**

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
      "totalSent": 0,
      "creditPerSlot": 14648436000
    },
    "slots": {
      "total": 10,
      "available": 10,
      "used": 0
    },
    "label": "Sales Target at Special Discount"
  },
  "networkUsers": []
}
```

---

### Step 4: View Admin's Network (with Placeholders)

```
GET http://localhost:3000/api/mlm/special-credits/network
Headers:
  Authorization: Bearer <ADMIN_TOKEN>
```

**Expected Response:**

```json
{
  "success": true,
  "networkUsers": [
    {
      "slotNumber": 1,
      "name": "",
      "phone": "",
      "credits": 14648436000,
      "sentAt": null,
      "recipientLevel": null,
      "isPlaceholder": true
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
    // ... 8 more placeholder slots
  ],
  "summary": {
    "totalSlots": 10,
    "usedSlots": 0,
    "availableSlots": 10,
    "creditPerSlot": 14648436000
  }
}
```

---

### Step 5: Admin Sends Credits to Farhan

```
POST http://localhost:3000/api/mlm/special-credits/send
Headers:
  Authorization: Bearer <ADMIN_TOKEN>
  Content-Type: application/json

Body:
{
  "recipientPhone": "+91 9867969445",
  "slotNumber": 1
}
```

**Expected Response:**

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

### Step 6: View Updated Network (Slot 1 Now Shows Farhan)

```
GET http://localhost:3000/api/mlm/special-credits/network
Headers:
  Authorization: Bearer <ADMIN_TOKEN>
```

**Expected Response:**

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
    // ... 8 more placeholder slots
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

### Step 7: Farhan Login & Get Token

```
POST http://localhost:3000/api/auth/login
Content-Type: application/json

Body:
{
  "phone": "+91 9867969445"
}
```

**💡 Copy Farhan's token**

---

### Step 8: View Farhan's Dashboard

```
GET http://localhost:3000/api/mlm/special-credits/dashboard
Headers:
  Authorization: Bearer <FARHAN_TOKEN>
```

**Expected Response:**

```json
{
  "success": true,
  "user": {
    "id": "676c4b9e123abc456def7891",
    "name": "Gori Farhan",
    "phone": "+91 9867969445",
    "level": 1,
    "isVoucherAdmin": false
  },
  "dashboard": {
    "vouchersFigure": 0,
    "specialCredits": {
      "balance": 14648436000,
      "totalReceived": 14648436000,
      "totalSent": 0,
      "creditPerSlot": 2929686000
    },
    "slots": {
      "total": 5,
      "available": 5,
      "used": 0
    },
    "label": "Sales Target at Special Discount"
  },
  "networkUsers": []
}
```

**Notice:**

- Farhan has **5 slots** (not 10)
- Each slot has **2,929,686,000 credits** (divided by 5)
- No vouchers figure (only admin has this)

---

### Step 9: View Farhan's Slots

```
GET http://localhost:3000/api/mlm/special-credits/slots
Headers:
  Authorization: Bearer <FARHAN_TOKEN>
```

**Expected Response:**

```json
{
  "success": true,
  "user": {
    "id": "676c4b9e123abc456def7891",
    "name": "Gori Farhan",
    "phone": "+91 9867969445",
    "level": 1,
    "isVoucherAdmin": false
  },
  "slots": [
    {
      "slotNumber": 1,
      "status": "available",
      "creditAmount": 2929686000,
      "recipientName": null,
      "recipientPhone": null,
      "recipientId": null,
      "sentAt": null,
      "isAvailable": true
    }
    // ... 4 more slots
  ],
  "summary": {
    "totalSlots": 5,
    "availableSlots": 5,
    "usedSlots": 0,
    "creditPerSlot": 2929686000
  }
}
```

---

### Step 10: Farhan Sends to Another User

```
POST http://localhost:3000/api/mlm/special-credits/send
Headers:
  Authorization: Bearer <FARHAN_TOKEN>
  Content-Type: application/json

Body:
{
  "recipientPhone": "+91 1234567890",
  "slotNumber": 1
}
```

**Expected Response:**

```json
{
  "success": true,
  "message": "Successfully sent 2,929,686,000 special credits to User Name",
  "transfer": {
    "sender": {
      "id": "676c4b9e123abc456def7891",
      "name": "Gori Farhan",
      "phone": "+91 9867969445"
    },
    "recipient": {
      "id": "676c4b9e123abc456def7892",
      "name": "User Name",
      "phone": "+91 1234567890",
      "level": 2
    },
    "slot": {
      "slotNumber": 1,
      "creditAmount": 2929686000,
      "sentAt": "2026-02-23T11:00:00.000Z"
    },
    "recipientSlotsCreated": 5,
    "recipientCreditPerSlot": 585936000
  }
}
```

**Notice:**

- Recipient gets **585,936,000 credits per slot** (2,929,686,000 ÷ 5)

---

## Error Cases to Test

### 1. Send to Non-Existent Phone Number

```
POST http://localhost:3000/api/mlm/special-credits/send
Body: { "recipientPhone": "+91 9999999999", "slotNumber": 1 }

Expected Error:
{
  "success": false,
  "message": "No user found with phone number: +91 9999999999"
}
```

### 2. Send to Same Slot Twice

```
POST http://localhost:3000/api/mlm/special-credits/send
Body: { "recipientPhone": "+91 9867969445", "slotNumber": 1 }

Expected Error (second time):
{
  "success": false,
  "message": "Slot 1 already used. Sent to Gori Farhan",
  "slot": {
    "recipientName": "Gori Farhan",
    "recipientPhone": "+91 9867969445",
    "sentAt": "2026-02-23T10:30:00.000Z"
  }
}
```

### 3. Invalid Slot Number

```
POST http://localhost:3000/api/mlm/special-credits/send
Body: { "recipientPhone": "+91 9867969445", "slotNumber": 11 }

Expected Error:
{
  "success": false,
  "message": "Invalid slot number. Maximum slots: 10"
}
```

### 4. Send to Self

```
POST http://localhost:3000/api/mlm/special-credits/send
Body: { "recipientPhone": "+91 9867477227", "slotNumber": 2 }

Expected Error:
{
  "success": false,
  "message": "Cannot send special credits to yourself"
}
```

---

## Verification Checklist

✅ Admin has 10 slots with 14,648,436,000 credits each  
✅ Admin shows vouchers figure: 122,070,300  
✅ After sending, slot shows recipient name/phone  
✅ "Send" button should change to "Call" button (frontend)  
✅ Recipient gets 5 slots with credits ÷ 5  
✅ Credits cascade correctly through levels  
✅ Special credits are separate from regular MLM credits  
✅ Label shows "Sales Target at Special Discount"

---

## Frontend Display Mock

### Admin Dashboard

```
┌──────────────────────────────────────────────────────────┐
│ Vouchers Dashboard                                        │
├──────────────────────────────────────────────────────────┤
│ Vouchers: 122,070,300                                     │
│                                                           │
│ Sales Target at Special Discount                         │
│                                                           │
│ Network Users                                            │
│ ┌────────────────────────────────────────────────────┐  │
│ │ 1. Gori Farhan      +91 9867969445  [Call] 14.6B  │  │
│ │ 2. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 3. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 4. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 5. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 6. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 7. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 8. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 9. [Name]           [Phone]         [Send] 14.6B  │  │
│ │ 10. [Name]          [Phone]         [Send] 14.6B  │  │
│ └────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────┘
```

### Regular User (Farhan) Dashboard

```
┌──────────────────────────────────────────────────────────┐
│ Vouchers Dashboard                                        │
├──────────────────────────────────────────────────────────┤
│ Sales Target at Special Discount                         │
│                                                           │
│ Network Users                                            │
│ ┌────────────────────────────────────────────────────┐  │
│ │ 1. [Name]           [Phone]         [Send] 2.9B   │  │
│ │ 2. [Name]           [Phone]         [Send] 2.9B   │  │
│ │ 3. [Name]           [Phone]         [Send] 2.9B   │  │
│ │ 4. [Name]           [Phone]         [Send] 2.9B   │  │
│ │ 5. [Name]           [Phone]         [Send] 2.9B   │  │
│ └────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────┘
```

---

## Quick Run Commands

```bash
# 1. Start backend server
cd Instantlly-Cards-Backend
npm run dev

# 2. Initialize admin (run once)
node init-admin-special-credits.js

# 3. Test with curl (example)
curl -X GET http://localhost:3000/api/mlm/special-credits/dashboard \
  -H "Authorization: Bearer YOUR_TOKEN_HERE"
```
