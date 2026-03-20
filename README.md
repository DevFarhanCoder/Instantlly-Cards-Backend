# Instantlly Cards Backend

Node.js / Express / TypeScript REST API powering the **Instantlly Cards** mobile app, admin panel, and channel partner portal.

---

## Tech Stack

| Layer | Technology |
|---|---|
| Runtime | Node.js 18+ |
| Language | TypeScript |
| Framework | Express.js |
| Database | MongoDB (Mongoose) |
| Real-time | Socket.IO |
| File Storage | AWS S3 |
| Process Manager | PM2 (production) |
| Auth | JWT + OTP |

---

## Prerequisites

- Node.js ≥ 18
- MongoDB connection string
- AWS S3 bucket credentials
- `.env` file at project root (see below)

---

## Setup

```bash
# Install dependencies
npm install

# Development (ts-node with hot-reload)
npm run dev

# Build TypeScript → dist/
npm run build

# Start compiled build
npm start
```

---

## Environment Variables

Create a `.env` file in the project root:

```env
PORT=8080
MONGODB_URI=mongodb+srv://<user>:<pass>@cluster.mongodb.net/dbname
JWT_SECRET=your_jwt_secret
ADMIN_SECRET_KEY=your_admin_key
AWS_ACCESS_KEY_ID=
AWS_SECRET_ACCESS_KEY=
AWS_REGION=ap-south-1
AWS_S3_BUCKET=
EXPO_ACCESS_TOKEN=
FCM_SERVER_KEY=
```

---

## Deployment (AWS EC2)

```bash
# Build locally
npm run build

# Upload compiled dist/ to server
scp -i production_instant_key.pem -r dist/ ubuntu@<EC2_IP>:~/Instantlly-Cards-Backend/

# Restart on server
ssh -i production_instant_key.pem ubuntu@<EC2_IP> "pm2 restart all"
```

> The `.ppk` key (`production_instant_key.ppk`) must be converted to `.pem` with `puttygen` before use on macOS/Linux.

---

## API Overview

All routes are prefixed with `/api`.  
Admin routes require the header: `x-admin-key: <ADMIN_SECRET_KEY>`  
User routes require the header: `Authorization: Bearer <JWT>`

### Auth
| Method | Endpoint | Description |
|---|---|---|
| POST | `/auth/register` | Register new user |
| POST | `/auth/login` | Login, returns JWT |
| POST | `/otp/send` | Send OTP |
| POST | `/otp/verify` | Verify OTP |

### Categories
| Method | Endpoint | Auth | Description |
|---|---|---|---|
| GET | `/categories/mobile` | — | List root categories (mobile) |
| GET | `/categories/mobile/:id/subcategories` | — | List subcategories |
| GET | `/categories/tree` | — | Full category tree |
| GET | `/categories/admin/all` | Admin | All categories flat list |
| POST | `/categories/admin/add-category` | Admin | Create root category |
| POST | `/categories/admin/node` | Admin | Create child node |
| PUT | `/categories/admin/node/:id` | Admin | Update node (name, icon, isActive, parent_id) |
| DELETE | `/categories/admin/node/:id` | Admin | Delete node + all descendants |
| POST | `/categories/admin/node/:id/upload-csv` | Admin | Bulk upload businesses to a node |
| POST | `/categories/admin/check-duplicate` | Admin | Check for duplicate before creating |

#### Duplicate Detection (`POST /categories/admin/check-duplicate`)
```json
// Request
{ "name": "Restaurants", "parent_id": "optional_parent_id" }

// Response
{
  "isDuplicate": true,
  "hasSimilar": false,
  "exactMatch": { "_id": "...", "name": "Food & Restaurants" },
  "similarMatches": [],
  "normalizedInput": "restaurant",
  "previewName": "Restaurant"
}
```

#### Node Duplicate Handling (`POST /categories/admin/node`)
Pass `"force": true` to bypass similar-match warnings:
```json
{ "name": "Fast Food", "parent_id": "abc123", "icon": "🍔", "force": false }
```
- Returns `409` with `wasExisting: true` if exact duplicate found
- Returns `409` with `similarMatches[]` if similar match found (override with `force: true`)

### Business Cards
| Method | Endpoint | Auth | Description |
|---|---|---|---|
| GET | `/cards` | User | List user's cards |
| POST | `/cards` | User | Create card |
| PUT | `/cards/:id` | User | Update card |
| DELETE | `/cards/:id` | User | Delete card |

### Business Listings
| Method | Endpoint | Description |
|---|---|---|
| GET | `/business-listing/search` | Search businesses by category |
| POST | `/business-listing` | Create listing (User) |

### Vouchers
| Method | Endpoint | Description |
|---|---|---|
| GET | `/credits/balance` | User credit balance |
| POST | `/credits/transfer` | Transfer credits |

### Ads
| Method | Endpoint | Description |
|---|---|---|
| GET | `/ads` | Active ads |
| POST | `/ads` | Create ad (Admin) |

### Real-time (Socket.IO)
Events: `message`, `group-message`, `notification`, `typing`

---

## Branch Strategy

| Branch | Purpose |
|---|---|
| `main` | Stable production |
| `dev` | Development integration |
| `feature/primiumCategories` | Category management enhancement (duplicate detection, normalization, smart mapping) |
| `Voucher` | Voucher system features |

---

## Category Normalization System

Located in `src/utils/categoryNormalization.ts`.

- **Stop-word stripping** — removes `best`, `top`, `near me`, `services`, `solutions`, etc.
- **Levenshtein distance** — catches typos (≥75% similarity for names ≤25 chars)
- **Jaccard word-set similarity** — catches reordered/partial names (≥55% similarity)
- **Substring containment** — catches one name containing another (≥50%)

This prevents duplicate categories like `Business` + `Business Services`, `Education` + `Education & Training` from being created again.

---

## Project Structure

```
src/
├── index.ts              # App entry point, Express + Socket.IO setup
├── db.ts                 # MongoDB connection
├── routes/
│   ├── auth.ts
│   ├── categories.ts     # Full category CRUD + duplicate detection
│   ├── cards.ts
│   ├── credits.ts
│   ├── ads.ts
│   ├── admin.ts
│   └── ...
├── models/
│   ├── Category.ts       # N-level hierarchy (parent_id, level)
│   ├── User.ts
│   ├── Card.ts
│   └── ...
├── middleware/
│   └── auth.ts           # JWT + admin key middleware
├── utils/
│   └── categoryNormalization.ts
└── services/
    └── ...
dist/                     # Compiled JS output (gitignored on main)
```

---

## Scripts

```bash
npm run dev                        # Dev server with hot reload
npm run build                      # Compile TypeScript
npm start                          # Run compiled build
npm run migrate:category-nodes     # Migrate category data
npm run upload:categories          # Upload categories from CSV
```
