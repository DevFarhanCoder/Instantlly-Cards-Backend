# Category Admin API (Read + Management)

This is a short admin-only reference for category and nested-category APIs.

## Admin Auth
- All admin routes require header: `x-admin-key`
- Valid keys are configured in `src/routes/categories.ts` via:
  - `process.env.ADMIN_SECRET_KEY`
  - (plus any hardcoded fallback keys present in the route)

## Read APIs (Admin)
1. **Full category tree (includes inactive)**
   - `GET /api/categories/tree/admin`
   - Returns nested tree: `{ _id, name, icon, level, order, isActive, children: [...] }`
   - Optional cache bypass: `?fresh=true`

2. **Flat list of all categories (includes inactive)**
   - `GET /api/categories/admin/all`
   - Raw list of category documents (no nesting)

3. **Custom services (admin review queue)**
   - `GET /api/categories/admin/custom-services?status=pending|approved|rejected|all&page=1&limit=50`
   - Used to review user‑submitted custom services

## How Nested Categories Work
- Categories are stored in a single collection with:
  - `parent_id` for hierarchy
  - `level` for depth
  - `order` for sorting
  - `isActive` to hide/show
- The API builds a **tree** by linking children to parents.
- Public tree only includes active nodes; admin tree includes inactive.

## Cache Notes
- Tree endpoints are cached in memory.
- Use `?fresh=true` to bypass cache after admin edits.

## How to Manipulate Category Data (Admin)
### Create
- `POST /api/categories/admin/node`
  - Body: `{ name, icon?, parent_id? }`
  - `parent_id` omitted → root category
  - `parent_id` set → nested category

### Update
- `PUT /api/categories/admin/node/:id`
  - Body: `{ name?, icon?, isActive? }`

### Delete (node + all descendants)
- `DELETE /api/categories/admin/node/:id`
  - Deletes the node and all its nested children

### Remove one subcategory from a legacy flat category
- `DELETE /api/categories/admin/category/:id/subcategory/:subName`
  - Used only for legacy flat `subcategories[]`

### Approve/Reject custom services
- `PUT /api/categories/admin/approve-custom/:id`
  - Body: `{ approveAs: "category" | "subcategory", categoryId?, newCategoryName?, newCategoryIcon? }`
- `PUT /api/categories/admin/reject-custom/:id`
- `DELETE /api/categories/admin/custom-service/:id`

## Related Non‑Admin Read APIs (for reference)
- `GET /api/categories/tree` (active-only tree for app)
- `GET /api/categories/:id/children` (active direct children)
- `GET /api/categories` (flat active categories with legacy subcategories)
