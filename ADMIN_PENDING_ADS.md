# Admin Pending Ads Integration

This document describes how the admin web UI can fetch pending ads uploaded from mobile users.

## Endpoint

GET /api/ads/admin/pending

- Protected: requires admin authentication (use existing `requireAdminAuth` middleware).
- Query params:
  - `page` (number, default 1)
  - `limit` (number, default 50, max 200)
  - `uploadedBy` (optional, filter by uploader phone)
  - `search` (optional, searches title/description/uploadedBy)

Response:
```json
{
  "success": true,
  "data": [
    {
      "_id": "...",
      "title": "...",
      "description": "...",
      "uploadedBy": "+911234567890",
      "uploaderName": "User Name",
      "createdAt": "2025-11-24T...",
      "status": "pending",
      "priority": 1,
      "imageUrls": {
        "bottom": "https://api.example.com/api/ads/image/<id>/bottom",
        "fullscreen": "https://api.example.com/api/ads/image/<id>/fullscreen"
      }
    }
  ],
  "pagination": {
    "page": 1,
    "limit": 50,
    "total": 123,
    "totalPages": 3
  }
}
```

## CORS / Origin

- Set the `ADMIN_WEB_ORIGIN` environment variable to the admin UI origin (for example `https://instantllychannelpatneradmin.vercel.app`).
- The backend reads `ADMIN_WEB_ORIGIN` and allows requests from it.

## Authentication

- Prefer server-side calls from the admin UI (Vercel server functions) using a secret stored in Vercel (recommended).
- If you must call from the browser, do NOT hardcode an admin JWT in client code. Instead provide a secure sign-in flow for the admin UI and use `Authorization: Bearer <ADMIN_JWT>`.
- The backend protects the endpoint with `requireAdminAuth`; the admin JWT must include an admin role claim.

## Image URLs

- Each ad includes `imageUrls` with endpoints that stream images from GridFS:
  - `GET /api/ads/image/:id/bottom`
  - `GET /api/ads/image/:id/fullscreen`

- Images are streamed and cached; the admin UI should request them with the same Authorization header if your images are restricted. The current implementation serves images publicly, but you can add auth to the image route if desired.

## Example

PowerShell (server-side or testing):

```powershell
$headers = @{ Authorization = "Bearer $env:ADMIN_JWT" }
Invoke-RestMethod -Uri 'https://api.example.com/api/ads/admin/pending?page=1&limit=20' -Headers $headers -Method GET
```

cURL:

```bash
curl -H "Authorization: Bearer $ADMIN_JWT" "https://api.example.com/api/ads/admin/pending?page=1&limit=20"
```

## Next steps

- Decide whether images should require admin auth. If so, protect `/api/ads/image/:id/:type` with `requireAdminAuth`.
- Add server-side caching headers or a CDN in front of the image endpoint for performance.
- Add UI pagination and search in the admin dashboard.
