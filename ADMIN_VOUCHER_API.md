# Admin Voucher Management API

Complete API documentation for creating and managing vouchers from the admin panel.

---

## 📋 Overview

Admins can create beautiful voucher cards that will be displayed in the mobile app's **Vouchers Tab**. Each voucher includes:

- Company Logo
- Company Name
- Phone Number
- Address
- Amount/Value
- Discount Percentage
- Validity Period
- Voucher Detail Image

---

## 🔐 Authentication

All admin endpoints require authentication via `requireAdminAuth` middleware:

```http
Authorization: Bearer <admin_jwt_token>
```

The JWT token should contain:

- `id`: Admin user ID
- `username`: Admin username
- `role`: "admin" or "super_admin"

---

## 📍 API Endpoints

### 1. Create Voucher Template

**POST** `/api/admin/vouchers`

Creates a new voucher template that can be published to users.

**Request Body:**

```json
{
  "companyLogo": "https://example.com/logo.png",
  "companyName": "Instantlly",
  "phoneNumber": "+91 9820329571",
  "address": "Jogeshwari, Mumbai",
  "amount": 3600,
  "discountPercentage": 40,
  "validity": "Valid till August 30th, 2026",
  "voucherImage": "https://example.com/voucher-detail.jpg",
  "description": "Premium voucher with exclusive benefits",
  "expiryDate": "2026-08-30T00:00:00.000Z",
  "isPublished": false
}
```

**Response:**

```json
{
  "success": true,
  "message": "Voucher template created successfully",
  "voucher": {
    "id": "63f8a1b2c3d4e5f6g7h8i9j0",
    "voucherNumber": "A1B2C3D4E5F6",
    "companyName": "Instantlly",
    "amount": 3600,
    "isPublished": false
  }
}
```

---

### 2. List All Vouchers

**GET** `/api/admin/vouchers`

Retrieves all voucher templates with optional filtering.

**Query Parameters:**

- `isPublished` (optional): `true` or `false` to filter published/unpublished vouchers
- `limit` (optional): Number of results per page (default: 50)
- `skip` (optional): Number of results to skip for pagination (default: 0)

**Example Request:**

```http
GET /api/admin/vouchers?isPublished=true&limit=20&skip=0
```

**Response:**

```json
{
  "success": true,
  "vouchers": [
    {
      "id": "63f8a1b2c3d4e5f6g7h8i9j0",
      "voucherNumber": "A1B2C3D4E5F6",
      "companyName": "Instantlly",
      "companyLogo": "https://example.com/logo.png",
      "phoneNumber": "+91 9820329571",
      "address": "Jogeshwari, Mumbai",
      "amount": 3600,
      "discountPercentage": 40,
      "validity": "Valid till August 30th, 2026",
      "voucherImage": "https://example.com/voucher-detail.jpg",
      "description": "Premium voucher",
      "isPublished": true,
      "publishedAt": "2026-02-16T10:30:00.000Z",
      "expiryDate": "2026-08-30T00:00:00.000Z",
      "createdAt": "2026-02-15T08:20:00.000Z"
    }
  ],
  "pagination": {
    "total": 15,
    "skip": 0,
    "limit": 20
  }
}
```

---

### 3. Update Voucher Template

**PUT** `/api/admin/vouchers/:id`

Updates an existing voucher template.

**Request Body:**

```json
{
  "companyName": "Updated Company Name",
  "amount": 4000,
  "discountPercentage": 50,
  "isPublished": true
}
```

**Response:**

```json
{
  "success": true,
  "message": "Voucher updated successfully",
  "voucher": {
    // Updated voucher object
  }
}
```

---

### 4. Publish Voucher

**POST** `/api/admin/vouchers/:id/publish`

Publishes a voucher template to make it visible to users in the mobile app.

**Request Body (Optional):**

```json
{
  "userIds": ["user_id_1", "user_id_2"]
}
```

- If `userIds` is provided: Creates individual voucher copies for specific users
- If `userIds` is empty/null: Publishes template globally (visible to all users)

**Response:**

```json
{
  "success": true,
  "message": "Voucher template published successfully",
  "voucher": {
    "id": "63f8a1b2c3d4e5f6g7h8i9j0",
    "voucherNumber": "A1B2C3D4E5F6",
    "isPublished": true,
    "assignedToUsers": 0
  }
}
```

---

### 5. Delete Voucher

**DELETE** `/api/admin/vouchers/:id`

Deletes a voucher template permanently.

**Response:**

```json
{
  "success": true,
  "message": "Voucher deleted successfully"
}
```

---

## 📱 Mobile App Integration

### User Vouchers Endpoint

**GET** `/api/mlm/vouchers`

Users can fetch their vouchers (includes both assigned vouchers and published admin templates).

The mobile app automatically fetches:

1. User's personal vouchers: `GET /api/mlm/vouchers`
2. Published admin vouchers: `GET /api/mlm/vouchers?source=admin&isPublished=true`

**User-Side Response:**

```json
{
  "success": true,
  "vouchers": [
    {
      "_id": "63f8a1b2c3d4e5f6g7h8i9j0",
      "voucherNumber": "A1B2C3D4E5F6",
      "companyName": "Instantlly",
      "companyLogo": "https://example.com/logo.png",
      "phoneNumber": "+91 9820329571",
      "address": "Jogeshwari, Mumbai",
      "amount": 3600,
      "discountPercentage": 40,
      "validity": "Valid till August 30th, 2026",
      "voucherImage": "https://example.com/voucher-detail.jpg",
      "redeemedStatus": "unredeemed",
      "expiryDate": "2026-08-30T00:00:00.000Z"
    }
  ]
}
```

---

## 🎨 Voucher Card Design (Mobile App)

The mobile app displays voucher cards with this design:

```
┌─────────────────────────────────────┐
│  [-40%]                        ┐    │
│  🏢 Instantlly            ₹3600│    │
│  📞 +91 9820329571        Value│    │
│  📍 Jogeshwari, Mumbai          │   │
│                                  │   │
│  ┌──────────────────────────┐   │   │
│  │  Redeem Voucher      →  │   │   │
│  └──────────────────────────┘   │   │
│                                  │   │
│  ⭐ Valid till August 30th, 2026│   │
└─────────────────────────────────────┘
```

---

## 🔄 Workflow

### Admin Panel Workflow:

1. **Create Voucher**
   - Admin fills form with voucher details
   - Upload company logo
   - Upload voucher detail image
   - Set amount, discount, validity
   - Save as draft (isPublished: false)

2. **Review & Edit**
   - Admin can view all vouchers
   - Edit voucher details
   - Preview voucher card design

3. **Publish Voucher**
   - Admin clicks "Publish" button
   - Voucher becomes visible in mobile app
   - Optional: Assign to specific users

4. **Manage Vouchers**
   - View published/unpublished vouchers
   - Edit or delete vouchers
   - Track voucher usage

### Mobile App Workflow:

1. **Vouchers Tab (1st Screen)**
   - User opens Vouchers tab
   - Sees list of all available vouchers
   - Vouchers show company logo, name, phone, address, amount, discount badge

2. **Voucher Detail (2nd Screen)**
   - User taps on a voucher card
   - Sees full-size voucher image (from admin panel)
   - Has "Continue to Dashboard" button

3. **User Dashboard (3rd Screen)**
   - User taps "Continue"
   - Opens dashboard with all stats
   - "Transfer Voucher" button in header to share vouchers

---

## 📝 Form Fields for Admin Panel

Create a form with these fields:

```typescript
interface VoucherForm {
  companyLogo: File | string; // Image upload
  companyName: string; // Text input
  phoneNumber: string; // Phone input
  address: string; // Text input
  amount: number; // Number input
  discountPercentage: number; // Number input (0-100)
  validity: string; // Text input or date picker
  voucherImage: File | string; // Image upload (main detail image)
  description: string; // Textarea
  expiryDate: Date; // Date picker
  isPublished: boolean; // Checkbox or toggle
}
```

---

## 🚀 Example cURL Requests

### Create Voucher:

```bash
curl -X POST https://your-api.com/api/admin/vouchers \
  -H "Authorization: Bearer YOUR_ADMIN_JWT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "companyName": "Instantlly",
    "phoneNumber": "+91 9820329571",
    "address": "Jogeshwari, Mumbai",
    "amount": 3600,
    "discountPercentage": 40,
    "validity": "Valid till August 30th, 2026",
    "voucherImage": "https://example.com/voucher.jpg",
    "isPublished": false
  }'
```

### Publish Voucher:

```bash
curl -X POST https://your-api.com/api/admin/vouchers/VOUCHER_ID/publish \
  -H "Authorization: Bearer YOUR_ADMIN_JWT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "userIds": []
  }'
```

---

## 🎯 Database Schema

```typescript
{
  voucherNumber: String (unique, 12-char),
  companyLogo: String (URL),
  companyName: String,
  phoneNumber: String,
  address: String,
  amount: Number,
  MRP: Number,
  discountPercentage: Number,
  validity: String,
  voucherImage: String (URL),
  description: String,
  isPublished: Boolean,
  publishedAt: Date,
  createdByAdmin: ObjectId,
  expiryDate: Date,
  issueDate: Date,
  redeemedStatus: "unredeemed" | "redeemed" | "expired",
  source: "admin" | "purchase" | "transfer",
  userId: ObjectId (optional),
  originalOwner: ObjectId (optional)
}
```

---

## ✅ Complete Implementation Checklist

- ✅ Backend API endpoints created
- ✅ Voucher model extended with admin fields
- ✅ Mobile app displays rich voucher cards
- ✅ Voucher detail screen shows admin-uploaded image
- ✅ Published vouchers visible to all users
- ✅ Transfer voucher functionality maintained

---

## 📞 Support

For questions or issues, contact the development team.
