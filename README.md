# CardSync Backend (Lite)

Minimal Node + Express + Mongo backend for your Instantlly Cards mobile app.

## Endpoints (all under `/api`)
- `POST /api/auth/signup` → `{ token, user }`
- `POST /api/auth/login` → `{ token, user }`
- **(Bearer required)** `GET /api/cards` list (own cards)
- **(Bearer required)** `POST /api/cards` create
- **(Bearer required)** `GET /api/cards/:id` read
- **(Bearer required)** `PUT /api/cards/:id` update
- **(Bearer required)** `DELETE /api/cards/:id` delete

## Quick start
1. `cp .env.example .env` and fill values
2. `npm i`
3. `npm run dev` (localhost:8080)

## Deploy on Render
- New **Web Service**
- Build command: `npm run build`
- Start command: `npm start`
- Add env vars: `MONGODB_URI`, `JWT_SECRET`, `PORT` (optional)

## Notes
- Accepts **Authorization: Bearer <token>** (mobile-friendly).
- Card schema matches your mobile form fields (personal/business/social flattened).
