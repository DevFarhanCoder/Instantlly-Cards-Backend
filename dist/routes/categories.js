"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// src/routes/categories.ts
const express_1 = __importDefault(require("express"));
const mongoose_1 = __importDefault(require("mongoose"));
const Category_1 = __importDefault(require("../models/Category"));
const CustomService_1 = __importDefault(require("../models/CustomService"));
const BusinessPromotion_1 = __importDefault(require("../models/BusinessPromotion"));
const router = express_1.default.Router();
const parsedMobileCategoryCacheTtl = Number(process.env.MOBILE_CATEGORY_CACHE_TTL_MS ?? "0");
const MOBILE_CATEGORY_CACHE_TTL_MS = Number.isFinite(parsedMobileCategoryCacheTtl) &&
    parsedMobileCategoryCacheTtl > 0
    ? Math.floor(parsedMobileCategoryCacheTtl)
    : 0;
let cachedMobileCategorySummary = null;
const invalidateMobileCategoryCache = () => {
    cachedMobileCategorySummary = null;
};
const normalizeSubcategories = (value) => {
    if (!Array.isArray(value))
        return [];
    return value
        .filter((item) => typeof item === "string")
        .map((item) => item.trim())
        .filter(Boolean);
};
const parsePositiveInt = (value, defaultValue) => {
    const parsed = Number(value);
    if (!Number.isFinite(parsed) || parsed <= 0)
        return defaultValue;
    return Math.floor(parsed);
};
const setNoStoreHeaders = (res) => {
    res.set({
        "Cache-Control": "no-store, no-cache, must-revalidate, proxy-revalidate",
        Pragma: "no-cache",
        Expires: "0",
        "Surrogate-Control": "no-store",
    });
};
const getMobileCategorySummary = async (options) => {
    const now = Date.now();
    const bypassCache = Boolean(options?.bypassCache) || MOBILE_CATEGORY_CACHE_TTL_MS <= 0;
    if (!bypassCache &&
        cachedMobileCategorySummary &&
        cachedMobileCategorySummary.expiresAt > now) {
        console.log("[CATEGORIES] Mobile summary cache hit");
        return cachedMobileCategorySummary.data;
    }
    const categories = (await Category_1.default.find({ isActive: true })
        .select("_id name icon order subcategories updatedAt")
        .sort({ order: 1, name: 1 })
        .lean());
    const summary = categories.map((category) => ({
        _id: String(category._id),
        name: category.name?.trim() || "",
        icon: category.icon || "📁",
        order: category.order ?? 0,
        subcategoryCount: normalizeSubcategories(category.subcategories).length,
        updatedAt: category.updatedAt,
    }));
    if (MOBILE_CATEGORY_CACHE_TTL_MS > 0 && !bypassCache) {
        cachedMobileCategorySummary = {
            data: summary,
            expiresAt: now + MOBILE_CATEGORY_CACHE_TTL_MS,
        };
        console.log("[CATEGORIES] Mobile summary cache refreshed", {
            count: summary.length,
            ttlMs: MOBILE_CATEGORY_CACHE_TTL_MS,
        });
    }
    else {
        cachedMobileCategorySummary = null;
        console.log("[CATEGORIES] Mobile summary fetched from DB", {
            count: summary.length,
            bypassCache,
            ttlMs: MOBILE_CATEGORY_CACHE_TTL_MS,
        });
    }
    return summary;
};
// Simple admin authentication middleware
const adminAuth = (req, res, next) => {
    const adminKey = req.headers["x-admin-key"];
    const validKeys = [
        process.env.ADMIN_SECRET_KEY,
        "your-secure-admin-key-here",
        "Farhan_90",
    ].filter(Boolean);
    if (adminKey && validKeys.includes(adminKey)) {
        next();
    }
    else {
        res.status(401).json({ error: "Unauthorized", message: "Invalid admin key" });
    }
};
// ── Fuzzy category-name matching helpers ─────────────────────────────────────
/** Lowercase, strip punctuation (& / - _ , . ; : etc.) → spaces, collapse whitespace */
const normalizeCatName = (name) => name
    .toLowerCase()
    .replace(/[&/\\|\-_.,;:!?()'"+@#%*[\]{}]+/g, " ")
    .replace(/\s+/g, " ")
    .trim();
/** Return significant words, removing stop-words and single-char tokens */
const catNameWords = (name) => {
    const stops = new Set(["and", "or", "the", "a", "an", "of", "in", "at", "for", "to", "with", "by", "de", "la"]);
    return new Set(normalizeCatName(name)
        .split(" ")
        .filter((w) => w.length > 1 && !stops.has(w)));
};
/**
 * Returns true if two category names have the same meaning:
 *  1. Identical after normalization
 *  2. One is a substring of the other  (e.g. "Hotels" ⊂ "Hotels & Restaurants")
 *  3. Jaccard word-set similarity ≥ 0.55  (e.g. "Auto Repair" vs "Auto Repairs")
 */
const fuzzyMatchCatNames = (a, b) => {
    const na = normalizeCatName(a);
    const nb = normalizeCatName(b);
    if (!na || !nb)
        return false;
    if (na === nb)
        return true;
    if (na.includes(nb) || nb.includes(na))
        return true;
    const wa = catNameWords(a);
    const wb = catNameWords(b);
    if (wa.size === 0 || wb.size === 0)
        return false;
    const intersection = [...wa].filter((w) => wb.has(w)).length;
    const union = new Set([...wa, ...wb]).size;
    return intersection / union >= 0.55;
};
// ============================================================
// PUBLIC ENDPOINTS (for mobile app)
// ============================================================
// GET /api/categories/mobile - Lightweight mobile list (optimized for home grid)
router.get("/mobile", async (req, res) => {
    try {
        const freshQuery = String(req.query.fresh ?? "").trim().toLowerCase();
        const bypassCache = freshQuery === "1" || freshQuery === "true" || freshQuery === "yes";
        console.log("[CATEGORIES] GET /api/categories/mobile", {
            fresh: bypassCache,
            ttlMs: MOBILE_CATEGORY_CACHE_TTL_MS,
        });
        const categories = await getMobileCategorySummary({ bypassCache });
        setNoStoreHeaders(res);
        res.json({
            success: true,
            data: categories,
        });
    }
    catch (error) {
        console.error("Error fetching mobile category summary:", error);
        res.status(500).json({
            success: false,
            error: "Failed to fetch category summary",
        });
    }
});
// GET /api/categories/mobile/:categoryId/subcategories - Lazy subcategory fetch for a single category
router.get("/mobile/:categoryId/subcategories", async (req, res) => {
    try {
        const { categoryId } = req.params;
        console.log("[CATEGORIES] GET /api/categories/mobile/:categoryId/subcategories", {
            categoryId,
            page: req.query.page,
            limit: req.query.limit,
            search: req.query.search,
        });
        if (!mongoose_1.default.Types.ObjectId.isValid(categoryId)) {
            return res.status(400).json({
                success: false,
                error: "Invalid categoryId",
            });
        }
        const page = parsePositiveInt(req.query.page, 1);
        const limit = Math.min(parsePositiveInt(req.query.limit, 50), 200);
        const search = typeof req.query.search === "string" ? req.query.search.trim() : "";
        const category = (await Category_1.default.findOne({
            _id: categoryId,
            isActive: true,
        })
            .select("_id name subcategories")
            .lean());
        if (!category) {
            return res.status(404).json({
                success: false,
                error: "Category not found",
            });
        }
        let subcategories = normalizeSubcategories(category.subcategories);
        if (search) {
            const normalizedSearch = search.toLowerCase();
            subcategories = subcategories.filter((subcategory) => subcategory.toLowerCase().includes(normalizedSearch));
        }
        const total = subcategories.length;
        const totalPages = total === 0 ? 0 : Math.ceil(total / limit);
        const start = (page - 1) * limit;
        const data = subcategories.slice(start, start + limit);
        console.log("[CATEGORIES] Subcategories fetched", {
            categoryId,
            returned: data.length,
            total,
            page,
            limit,
            hasMore: start + limit < total,
        });
        setNoStoreHeaders(res);
        res.json({
            success: true,
            data: {
                categoryId: String(category._id),
                categoryName: category.name?.trim() || "",
                subcategories: data,
            },
            meta: {
                page,
                limit,
                total,
                totalPages,
                hasMore: start + limit < total,
                search: search || null,
            },
        });
    }
    catch (error) {
        console.error("Error fetching category subcategories:", error);
        res.status(500).json({
            success: false,
            error: "Failed to fetch subcategories",
        });
    }
});
// GET /api/categories - Get all active categories with subcategories
router.get("/", async (req, res) => {
    try {
        const categories = (await Category_1.default.find({ isActive: true })
            .select("_id name icon subcategories isActive order")
            .sort({ order: 1, name: 1 })
            .lean());
        const normalized = categories.map((category) => ({
            ...category,
            name: category.name?.trim() || "",
            icon: category.icon || "📁",
            subcategories: normalizeSubcategories(category.subcategories),
            order: category.order ?? 0,
        }));
        setNoStoreHeaders(res);
        res.json({
            success: true,
            data: normalized,
        });
    }
    catch (error) {
        console.error("Error fetching categories:", error);
        res.status(500).json({ success: false, error: "Failed to fetch categories" });
    }
});
// POST /api/categories/custom-service - Mobile app reports a custom service
router.post("/custom-service", async (req, res) => {
    try {
        const { serviceName, userId, userName, cardId, parentCategory } = req.body;
        if (!serviceName || !userId) {
            return res.status(400).json({
                success: false,
                error: "serviceName and userId are required",
            });
        }
        // Check if this custom service was already reported (avoid duplicates)
        const existing = await CustomService_1.default.findOne({
            serviceName: { $regex: new RegExp(`^${serviceName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") },
            status: "pending",
        });
        if (existing) {
            return res.json({
                success: true,
                message: "Custom service already reported",
                data: existing,
            });
        }
        // Check if it already exists in categories
        const matchingCategory = await Category_1.default.findOne({
            $or: [
                { name: { $regex: new RegExp(`^${serviceName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") } },
                { subcategories: { $regex: new RegExp(`^${serviceName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") } },
            ],
        });
        if (matchingCategory) {
            // Determine if it matched as a category name or subcategory
            const isCategory = matchingCategory.name.toLowerCase() === serviceName.toLowerCase();
            const matchedSub = matchingCategory.subcategories.find((s) => s.toLowerCase() === serviceName.toLowerCase());
            return res.json({
                success: true,
                message: isCategory
                    ? `"${serviceName}" already exists as a category`
                    : `"${serviceName}" already exists as a subcategory under "${matchingCategory.name}"`,
                alreadyExists: true,
                matchedAs: isCategory ? "category" : "subcategory",
                categoryName: matchingCategory.name,
                subcategoryName: matchedSub || null,
            });
        }
        const customService = new CustomService_1.default({
            serviceName: serviceName.trim(),
            addedBy: userId,
            userName: userName || "",
            cardId: cardId || "",
            parentCategory: parentCategory || "",
        });
        await customService.save();
        res.status(201).json({
            success: true,
            message: "Custom service reported successfully",
            data: customService,
        });
    }
    catch (error) {
        console.error("Error saving custom service:", error);
        res.status(500).json({ success: false, error: "Failed to save custom service" });
    }
});
// ============================================================
// ADMIN ENDPOINTS
// ============================================================
// GET /api/categories/admin/custom-services - Get all pending custom services
router.get("/admin/custom-services", adminAuth, async (req, res) => {
    try {
        const { status = "pending", page = 1, limit = 50 } = req.query;
        const skip = (Number(page) - 1) * Number(limit);
        const filter = {};
        if (status && status !== "all") {
            filter.status = status;
        }
        const [customServices, total] = await Promise.all([
            CustomService_1.default.find(filter)
                .sort({ createdAt: -1 })
                .skip(skip)
                .limit(Number(limit))
                .lean(),
            CustomService_1.default.countDocuments(filter),
        ]);
        res.json({
            success: true,
            data: customServices,
            total,
            page: Number(page),
            totalPages: Math.ceil(total / Number(limit)),
        });
    }
    catch (error) {
        console.error("Error fetching custom services:", error);
        res.status(500).json({ success: false, error: "Failed to fetch custom services" });
    }
});
// GET /api/categories/admin/all - Get all categories (including inactive) for admin
router.get("/admin/all", adminAuth, async (req, res) => {
    try {
        const categories = await Category_1.default.find()
            .sort({ order: 1, name: 1 })
            .lean();
        res.json({
            success: true,
            data: categories,
        });
    }
    catch (error) {
        console.error("Error fetching categories:", error);
        res.status(500).json({ success: false, error: "Failed to fetch categories" });
    }
});
// POST /api/categories/admin/seed - Seed initial categories from the hardcoded list
router.post("/admin/seed", adminAuth, async (req, res) => {
    try {
        const SERVICE_CATEGORIES = {
            Travel: [
                "Hotels", "Resorts", "Hostels", "PG Accommodations", "Travel Agents",
                "Domestic Tours", "International Tours", "Visa Assistance",
                "International Air Ticketing", "Train Ticketing",
            ],
            Technology: [
                "CCTV Systems", "Security Systems", "Computer Repairs", "Laptop Repairs",
                "Mobile & Internet Services", "Refrigerator Repairs", "Appliance Repairs",
                "Computer Training Institutes", "Website & App Development",
            ],
            Shopping: [
                "Cake Shops & Bakeries", "Daily Needs Stores", "Groceries", "Florists",
                "Restaurants", "Food Delivery Services", "Online Food Ordering",
                "Foreign Exchange Services", "Furniture Stores", "Wallpapers & Home Decor",
                "Water Suppliers", "Medical Stores & Pharmacies", "Optical Stores",
                "Pet Shops", "Pet Care Services", "Online Shopping", "T-Shirt Printing",
            ],
            Rentals: [
                "Bus on Hire", "Car & Cab Rentals", "Generators on Hire",
                "Equipment Rentals", "Tempos on Hire",
            ],
            Lifestyle: [
                "Astrologers", "Beauty Salons", "Bridal Makeup Artists", "Makeup Artists",
                "Dance Classes", "Music Classes", "Fitness Centres", "Gyms",
                "Photographers & Videographers", "Tattoo Artists", "Weight Loss Centres",
                "Movies", "Online Movie Platforms", "Parties & Nightlife",
            ],
            Health: [
                "General Physicians", "General Surgeons", "Cardiologists",
                "Child Specialists", "Paediatricians", "Dentists", "Dermatologists",
                "Skin & Hair Specialists", "ENT Doctors", "Eye Specialists",
                "Ophthalmologists", "Gastroenterologists", "Gynaecologists & Obstetricians",
                "Neurologists", "Orthopaedic Doctors", "Ayurvedic Doctors",
                "Homeopathic Doctors", "Pathology Labs", "Physiotherapists",
                "Vaccination Centres", "Hearing Aids & Solutions",
            ],
            Education: [
                "Schools & Educational Institutions", "Playgroups", "Kindergartens",
                "Home Tutors", "Tutorials & Coaching Classes", "Training Institutes",
                "Language Classes", "Motor Training Schools",
                "Overseas Education Consultants", "Yoga & Wellness Classes",
            ],
            Construction: [
                "Borewell Contractors", "Builders & Contractors", "Carpentry Contractors",
                "Civil Contractors", "Electrical Contractors", "Electricians",
                "False Ceiling Contractors", "Home Services", "Housekeeping Services",
                "Modular Kitchen Designers", "Painting Contractors", "Plumbers",
                "Ready Mix Concrete Suppliers", "Waterproofing Contractors",
            ],
            Automotive: [
                "Automobile Dealers", "Car Insurance Agents", "Car Loans & Finance",
                "Car Repairs & Services", "Taxi & Cab Services", "Tempos on Hire",
                "Towing Services", "Transporters & Logistics",
            ],
            Services: [
                "Courier Services", "Pest Control", "Security Services",
                "Cleaning Services", "Laundry & Dry Cleaning", "Catering Services",
                "Photography Services", "Printing Services", "AC Repair & Services",
                "Appliance Installation",
            ],
            Business: [
                "Bulk SMS & Digital Marketing", "Chartered Accountants",
                "Business Consultants", "GST Registration Consultants",
                "Income Tax Consultants", "Registration Consultants",
                "Event Organizers", "Party Organisers",
                "Wedding Planners & Requisites", "Interior Designers",
                "Lawyers & Legal Services", "Logistics & Supply Chain",
                "Online Passport Agents", "Packers & Movers",
                "Repairs & Maintenance Services", "Website Designers & Developers",
            ],
        };
        const CATEGORY_ICONS = {
            Travel: "✈️",
            Technology: "💻",
            Shopping: "🛒",
            Rentals: "🔑",
            Lifestyle: "💄",
            Health: "⚕️",
            Education: "🎓",
            Construction: "🔨",
            Automotive: "🚗",
            Services: "🔧",
            Business: "💼",
        };
        let created = 0;
        let updated = 0;
        for (const [categoryName, subcategories] of Object.entries(SERVICE_CATEGORIES)) {
            const existing = await Category_1.default.findOne({ name: categoryName });
            if (existing) {
                // Merge subcategories (add new ones, keep existing)
                const existingSubs = new Set(existing.subcategories.map((s) => s.toLowerCase()));
                const newSubs = subcategories.filter((s) => !existingSubs.has(s.toLowerCase()));
                if (newSubs.length > 0) {
                    existing.subcategories.push(...newSubs);
                    existing.icon = CATEGORY_ICONS[categoryName] || existing.icon;
                    await existing.save();
                    updated++;
                }
            }
            else {
                await Category_1.default.create({
                    name: categoryName,
                    icon: CATEGORY_ICONS[categoryName] || "📁",
                    subcategories,
                    order: Object.keys(SERVICE_CATEGORIES).indexOf(categoryName),
                });
                created++;
            }
        }
        if (created > 0 || updated > 0) {
            invalidateMobileCategoryCache();
        }
        res.json({
            success: true,
            message: `Seeded categories: ${created} created, ${updated} updated`,
            created,
            updated,
        });
    }
    catch (error) {
        console.error("Error seeding categories:", error);
        res.status(500).json({ success: false, error: "Failed to seed categories" });
    }
});
// POST /api/categories/admin/add-category - Add a new category
router.post("/admin/add-category", adminAuth, async (req, res) => {
    try {
        const { name, icon, subcategories } = req.body;
        if (!name) {
            return res.status(400).json({ success: false, error: "Category name is required" });
        }
        const existing = await Category_1.default.findOne({
            name: { $regex: new RegExp(`^${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") },
        });
        if (existing) {
            return res.status(409).json({
                success: false,
                error: "Category already exists",
                data: existing,
            });
        }
        const maxOrder = await Category_1.default.findOne().sort({ order: -1 }).lean();
        const newCategory = await Category_1.default.create({
            name: name.trim(),
            icon: icon || "📁",
            subcategories: subcategories || [],
            order: (maxOrder?.order || 0) + 1,
        });
        invalidateMobileCategoryCache();
        res.status(201).json({
            success: true,
            message: `Category "${name}" created`,
            data: newCategory,
        });
    }
    catch (error) {
        console.error("Error adding category:", error);
        res.status(500).json({ success: false, error: "Failed to add category" });
    }
});
// POST /api/categories/admin/add-subcategory - Add a subcategory to an existing category
router.post("/admin/add-subcategory", adminAuth, async (req, res) => {
    try {
        const { categoryId, subcategoryName } = req.body;
        if (!categoryId || !subcategoryName) {
            return res.status(400).json({
                success: false,
                error: "categoryId and subcategoryName are required",
            });
        }
        const category = await Category_1.default.findById(categoryId);
        if (!category) {
            return res.status(404).json({ success: false, error: "Category not found" });
        }
        // Check if subcategory already exists
        const exists = category.subcategories.some((s) => s.toLowerCase() === subcategoryName.trim().toLowerCase());
        if (exists) {
            return res.status(409).json({
                success: false,
                error: `Subcategory "${subcategoryName}" already exists in "${category.name}"`,
            });
        }
        category.subcategories.push(subcategoryName.trim());
        await category.save();
        invalidateMobileCategoryCache();
        res.json({
            success: true,
            message: `Subcategory "${subcategoryName}" added to "${category.name}"`,
            data: category,
        });
    }
    catch (error) {
        console.error("Error adding subcategory:", error);
        res.status(500).json({ success: false, error: "Failed to add subcategory" });
    }
});
// PUT /api/categories/admin/approve-custom/:id - Approve a custom service as category or subcategory
router.put("/admin/approve-custom/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { approveAs, categoryId, newCategoryName, newCategoryIcon } = req.body;
        // approveAs: "category" | "subcategory"
        // categoryId: existing category ID (when adding as subcategory)
        // newCategoryName: if creating a brand new category (when approveAs = "category")
        if (!approveAs || !["category", "subcategory"].includes(approveAs)) {
            return res.status(400).json({
                success: false,
                error: "approveAs must be 'category' or 'subcategory'",
            });
        }
        const customService = await CustomService_1.default.findById(id);
        if (!customService) {
            return res.status(404).json({ success: false, error: "Custom service not found" });
        }
        if (customService.status !== "pending") {
            return res.status(400).json({
                success: false,
                error: `Custom service already ${customService.status}`,
            });
        }
        let resultCategory = null;
        if (approveAs === "category") {
            // Create as a new category (the custom service name becomes a new top-level category)
            const catName = newCategoryName || customService.serviceName;
            const existing = await Category_1.default.findOne({
                name: { $regex: new RegExp(`^${catName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") },
            });
            if (existing) {
                return res.status(409).json({
                    success: false,
                    error: `Category "${catName}" already exists`,
                });
            }
            const maxOrder = await Category_1.default.findOne().sort({ order: -1 }).lean();
            resultCategory = await Category_1.default.create({
                name: catName.trim(),
                icon: newCategoryIcon || "📁",
                subcategories: [],
                order: (maxOrder?.order || 0) + 1,
            });
            customService.approvedAs = { type: "category", categoryName: catName };
        }
        else {
            // Add as subcategory to existing category
            if (!categoryId) {
                return res.status(400).json({
                    success: false,
                    error: "categoryId is required when adding as subcategory",
                });
            }
            const category = await Category_1.default.findById(categoryId);
            if (!category) {
                return res.status(404).json({ success: false, error: "Category not found" });
            }
            const subExists = category.subcategories.some((s) => s.toLowerCase() === customService.serviceName.toLowerCase());
            if (subExists) {
                return res.status(409).json({
                    success: false,
                    error: `"${customService.serviceName}" already exists in "${category.name}"`,
                });
            }
            category.subcategories.push(customService.serviceName.trim());
            await category.save();
            resultCategory = category;
            customService.approvedAs = { type: "subcategory", categoryName: category.name };
        }
        customService.status = "approved";
        customService.approvedAt = new Date();
        await customService.save();
        invalidateMobileCategoryCache();
        res.json({
            success: true,
            message: `Custom service "${customService.serviceName}" approved as ${approveAs}`,
            data: {
                customService,
                category: resultCategory,
            },
        });
    }
    catch (error) {
        console.error("Error approving custom service:", error);
        res.status(500).json({ success: false, error: "Failed to approve custom service" });
    }
});
// PUT /api/categories/admin/reject-custom/:id - Reject a custom service
router.put("/admin/reject-custom/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const customService = await CustomService_1.default.findById(id);
        if (!customService) {
            return res.status(404).json({ success: false, error: "Custom service not found" });
        }
        customService.status = "rejected";
        await customService.save();
        res.json({
            success: true,
            message: `Custom service "${customService.serviceName}" rejected`,
            data: customService,
        });
    }
    catch (error) {
        console.error("Error rejecting custom service:", error);
        res.status(500).json({ success: false, error: "Failed to reject custom service" });
    }
});
// DELETE /api/categories/admin/custom-service/:id - Delete a custom service
router.delete("/admin/custom-service/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const result = await CustomService_1.default.findByIdAndDelete(id);
        if (!result) {
            return res.status(404).json({ success: false, error: "Custom service not found" });
        }
        res.json({ success: true, message: "Custom service deleted" });
    }
    catch (error) {
        console.error("Error deleting custom service:", error);
        res.status(500).json({ success: false, error: "Failed to delete custom service" });
    }
});
// POST /api/categories/admin/upload-companies - Bulk upload companies via CSV for a subcategory
// Body: { subcategory: string, category: string, rows: Array<{ businessName, ownerName, description, phone, whatsapp, email, website, area, city, state, pincode, listingType }> }
router.post("/admin/upload-companies", adminAuth, async (req, res) => {
    try {
        const { subcategory, category, rows } = req.body;
        if (!subcategory || !Array.isArray(rows) || rows.length === 0) {
            return res.status(400).json({
                success: false,
                error: "subcategory and rows[] are required",
            });
        }
        // Placeholder ObjectId for admin-imported records (no real user)
        const ADMIN_PLACEHOLDER_ID = new mongoose_1.default.Types.ObjectId("000000000000000000000001");
        const results = {
            created: 0,
            skipped: 0,
            errors: [],
        };
        for (const row of rows) {
            try {
                if (!row.businessName?.trim() || !row.phone?.trim()) {
                    results.skipped++;
                    results.errors.push(`Skipped: missing businessName or phone (${row.businessName || "—"})`);
                    continue;
                }
                // category array: [subcategory, parentCategory] so listing is found by subcategory filter
                const categoryArray = [subcategory.trim()];
                if (category && category.trim() !== subcategory.trim()) {
                    categoryArray.push(category.trim());
                }
                await BusinessPromotion_1.default.create({
                    userId: ADMIN_PLACEHOLDER_ID,
                    businessName: row.businessName.trim(),
                    ownerName: (row.ownerName || row.businessName).trim(),
                    description: row.description?.trim() || "",
                    category: categoryArray,
                    phone: row.phone.trim(),
                    whatsapp: row.whatsapp?.trim() || row.phone.trim(),
                    email: row.email?.trim() || "",
                    website: row.website?.trim() || "",
                    area: row.area?.trim() || "",
                    city: row.city?.trim() || "",
                    state: row.state?.trim() || "",
                    pincode: row.pincode?.trim() || "",
                    landmark: row.landmark?.trim() || "",
                    listingType: row.listingType === "promoted" ? "promoted" : "free",
                    listingIntent: row.listingType === "promoted" ? "promoted" : "free",
                    status: "active",
                    isActive: true,
                    currentStep: "location",
                    progress: 100,
                    stepIndex: 4,
                    paymentStatus: row.listingType === "promoted" ? "paid" : "not_required",
                });
                results.created++;
            }
            catch (rowErr) {
                results.skipped++;
                results.errors.push(`${row.businessName}: ${rowErr.message}`);
            }
        }
        res.json({
            success: true,
            message: `Upload complete: ${results.created} created, ${results.skipped} skipped`,
            created: results.created,
            skipped: results.skipped,
            errors: results.errors,
        });
    }
    catch (error) {
        console.error("Error uploading companies:", error);
        res.status(500).json({ success: false, error: "Failed to upload companies" });
    }
});
// PUT /api/categories/admin/category/:id - Update category name, icon, or isActive
router.put("/admin/category/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const { name, icon, isActive } = req.body;
        const category = await Category_1.default.findById(id);
        if (!category) {
            return res.status(404).json({ success: false, error: "Category not found" });
        }
        if (name !== undefined && name.trim())
            category.name = name.trim();
        if (icon !== undefined)
            category.icon = icon;
        if (isActive !== undefined)
            category.isActive = Boolean(isActive);
        await category.save();
        invalidateMobileCategoryCache();
        res.json({
            success: true,
            message: `Category "${category.name}" updated`,
            data: category,
        });
    }
    catch (error) {
        console.error("Error updating category:", error);
        res.status(500).json({ success: false, error: "Failed to update category" });
    }
});
// DELETE /api/categories/admin/category/:id - Delete a category
router.delete("/admin/category/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const category = await Category_1.default.findByIdAndDelete(id);
        if (!category) {
            return res.status(404).json({ success: false, error: "Category not found" });
        }
        invalidateMobileCategoryCache();
        res.json({ success: true, message: `Category "${category.name}" deleted` });
    }
    catch (error) {
        console.error("Error deleting category:", error);
        res.status(500).json({ success: false, error: "Failed to delete category" });
    }
});
// DELETE /api/categories/admin/category/:id/subcategory/:subName - Remove a subcategory from a category
router.delete("/admin/category/:id/subcategory/:subName", adminAuth, async (req, res) => {
    try {
        const { id, subName } = req.params;
        const subcategoryName = decodeURIComponent(subName);
        const category = await Category_1.default.findById(id);
        if (!category) {
            return res.status(404).json({ success: false, error: "Category not found" });
        }
        const idx = category.subcategories.findIndex((s) => s.toLowerCase() === subcategoryName.toLowerCase());
        if (idx === -1) {
            return res.status(404).json({ success: false, error: `Subcategory "${subcategoryName}" not found` });
        }
        category.subcategories.splice(idx, 1);
        await category.save();
        invalidateMobileCategoryCache();
        res.json({
            success: true,
            message: `Subcategory "${subcategoryName}" removed from "${category.name}"`,
            data: category,
        });
    }
    catch (error) {
        console.error("Error removing subcategory:", error);
        res.status(500).json({ success: false, error: "Failed to remove subcategory" });
    }
});
// ============================================================
// HIERARCHY ENDPOINTS  (N-level tree)
// ============================================================
// ── GET /api/categories/tree
// Returns the full nested tree:
//   [{ _id, name, icon, level, isActive, order, children: [...] }]
router.get("/tree", async (req, res) => {
    try {
        const all = await Category_1.default.find({ isActive: true })
            .select("_id name icon parent_id level order isActive subcategories")
            .sort({ order: 1, name: 1 })
            .lean();
        const map = new Map();
        for (const node of all) {
            map.set(String(node._id), {
                _id: String(node._id),
                name: node.name?.trim() || "",
                icon: node.icon || "📁",
                level: node.level ?? 0,
                order: node.order ?? 0,
                isActive: node.isActive ?? true,
                subcategories: Array.isArray(node.subcategories) ? node.subcategories : [],
                children: [],
            });
        }
        const roots = [];
        for (const node of all) {
            const treeNode = map.get(String(node._id));
            const parentId = node.parent_id ? String(node.parent_id) : null;
            if (parentId && map.has(parentId)) {
                map.get(parentId).children.push(treeNode);
            }
            else {
                roots.push(treeNode);
            }
        }
        res.json({ success: true, data: roots });
    }
    catch (err) {
        console.error("Error fetching category tree:", err);
        res.status(500).json({ success: false, error: "Failed to fetch category tree" });
    }
});
// ── GET /api/categories/tree/admin  (includes inactive)
router.get("/tree/admin", adminAuth, async (req, res) => {
    try {
        const all = await Category_1.default.find()
            .select("_id name icon parent_id level order isActive subcategories")
            .sort({ order: 1, name: 1 })
            .lean();
        const map = new Map();
        for (const node of all) {
            map.set(String(node._id), {
                _id: String(node._id),
                name: node.name?.trim() || "",
                icon: node.icon || "📁",
                level: node.level ?? 0,
                order: node.order ?? 0,
                isActive: node.isActive ?? true,
                subcategories: Array.isArray(node.subcategories) ? node.subcategories : [],
                children: [],
            });
        }
        const roots = [];
        for (const node of all) {
            const treeNode = map.get(String(node._id));
            const parentId = node.parent_id ? String(node.parent_id) : null;
            if (parentId && map.has(parentId)) {
                map.get(parentId).children.push(treeNode);
            }
            else {
                roots.push(treeNode);
            }
        }
        res.json({ success: true, data: roots });
    }
    catch (err) {
        console.error("Error fetching admin category tree:", err);
        res.status(500).json({ success: false, error: "Failed to fetch category tree" });
    }
});
// ── GET /api/categories/:id/children  — direct children of a node
router.get("/:id/children", async (req, res) => {
    try {
        const { id } = req.params;
        if (!mongoose_1.default.Types.ObjectId.isValid(id)) {
            return res.status(400).json({ success: false, error: "Invalid category id" });
        }
        const children = await Category_1.default.find({ parent_id: id, isActive: true })
            .sort({ order: 1, name: 1 })
            .lean();
        res.json({ success: true, data: children });
    }
    catch (err) {
        res.status(500).json({ success: false, error: "Failed to fetch children" });
    }
});
// ── POST /api/categories/admin/node — create a node at any level
// Body: { name, icon?, parent_id? }
//   parent_id absent or null  → root category
//   parent_id present         → child of that node
router.post("/admin/node", adminAuth, async (req, res) => {
    try {
        const { name, icon, parent_id } = req.body;
        if (!name?.trim()) {
            return res.status(400).json({ success: false, error: "name is required" });
        }
        let level = 0;
        let resolvedParentId = null;
        if (parent_id) {
            if (!mongoose_1.default.Types.ObjectId.isValid(parent_id)) {
                return res.status(400).json({ success: false, error: "Invalid parent_id" });
            }
            const parent = await Category_1.default.findById(parent_id).lean();
            if (!parent) {
                return res.status(404).json({ success: false, error: "Parent category not found" });
            }
            level = (parent.level ?? 0) + 1;
            resolvedParentId = new mongoose_1.default.Types.ObjectId(parent_id);
        }
        // ── 1. Exact name duplicate check ──────────────────────────────────────
        const exactDup = await Category_1.default.findOne({
            parent_id: resolvedParentId,
            name: { $regex: new RegExp(`^${name.trim().replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`, "i") },
        }).lean();
        if (exactDup) {
            // Return the existing node so callers can use its ID without a second round-trip
            return res.status(200).json({ success: true, wasExisting: true, data: exactDup });
        }
        // ── 2. Fuzzy sibling check (prefix / suffix / word-overlap) ────────────
        const siblings = await Category_1.default.find({ parent_id: resolvedParentId })
            .select("name _id icon level order isActive parent_id")
            .lean();
        const fuzzyDup = siblings.find((s) => fuzzyMatchCatNames(s.name ?? "", name.trim()));
        if (fuzzyDup) {
            console.log(`[CATEGORIES] Fuzzy-matched "${name.trim()}" → existing "${fuzzyDup.name}" (id: ${fuzzyDup._id})`);
            return res.status(200).json({ success: true, wasExisting: true, data: fuzzyDup });
        }
        const maxOrderDoc = await Category_1.default.findOne({ parent_id: resolvedParentId }).sort({ order: -1 }).lean();
        const newOrder = (maxOrderDoc?.order ?? 0) + 1;
        const node = await Category_1.default.create({
            name: name.trim(),
            icon: icon || "📁",
            parent_id: resolvedParentId,
            level,
            order: newOrder,
            isActive: true,
        });
        invalidateMobileCategoryCache();
        res.status(201).json({ success: true, message: `Created "${node.name}"`, data: node });
    }
    catch (err) {
        console.error("Error creating category node:", err);
        res.status(500).json({ success: false, error: "Failed to create category" });
    }
});
// ── PUT /api/categories/admin/node/:id — update any node
// Body: { name?, icon?, isActive? }
router.put("/admin/node/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        if (!mongoose_1.default.Types.ObjectId.isValid(id)) {
            return res.status(400).json({ success: false, error: "Invalid id" });
        }
        const { name, icon, isActive } = req.body;
        const node = await Category_1.default.findById(id);
        if (!node)
            return res.status(404).json({ success: false, error: "Category not found" });
        if (name !== undefined && name.trim())
            node.name = name.trim();
        if (icon !== undefined)
            node.icon = icon;
        if (isActive !== undefined)
            node.isActive = Boolean(isActive);
        await node.save();
        invalidateMobileCategoryCache();
        res.json({ success: true, message: `"${node.name}" updated`, data: node });
    }
    catch (err) {
        res.status(500).json({ success: false, error: "Failed to update category" });
    }
});
// ── DELETE /api/categories/admin/node/:id — delete node + all descendants recursively
router.delete("/admin/node/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        if (!mongoose_1.default.Types.ObjectId.isValid(id)) {
            return res.status(400).json({ success: false, error: "Invalid id" });
        }
        // Collect all descendant IDs using BFS
        const idsToDelete = [id];
        const queue = [id];
        while (queue.length > 0) {
            const parentId = queue.shift();
            const children = await Category_1.default.find({ parent_id: parentId }).select("_id").lean();
            for (const child of children) {
                const cid = String(child._id);
                idsToDelete.push(cid);
                queue.push(cid);
            }
        }
        await Category_1.default.deleteMany({ _id: { $in: idsToDelete } });
        invalidateMobileCategoryCache();
        res.json({ success: true, message: `Deleted ${idsToDelete.length} category node(s)`, deleted: idsToDelete.length });
    }
    catch (err) {
        console.error("Error deleting category node:", err);
        res.status(500).json({ success: false, error: "Failed to delete category" });
    }
});
// ── POST /api/categories/admin/node/:id/upload-csv
// Upload business listings CSV to a specific category node
router.post("/admin/node/:id/upload-csv", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        if (!mongoose_1.default.Types.ObjectId.isValid(id)) {
            return res.status(400).json({ success: false, error: "Invalid category id" });
        }
        const node = await Category_1.default.findById(id).lean();
        if (!node)
            return res.status(404).json({ success: false, error: "Category not found" });
        const { rows } = req.body;
        if (!Array.isArray(rows) || rows.length === 0) {
            return res.status(400).json({ success: false, error: "rows[] is required" });
        }
        // Build full category path for tagging
        const pathNames = [node.name?.trim() || ""];
        let cur = node;
        while (cur.parent_id) {
            const p = await Category_1.default.findById(cur.parent_id).lean();
            if (!p)
                break;
            pathNames.unshift(p.name?.trim() || "");
            cur = p;
        }
        const ADMIN_PLACEHOLDER_ID = new mongoose_1.default.Types.ObjectId("000000000000000000000001");
        const results = { created: 0, skipped: 0, errors: [] };
        for (const row of rows) {
            try {
                if (!row.businessName?.trim() || !row.phone?.trim()) {
                    results.skipped++;
                    results.errors.push(`Skipped: missing businessName or phone (${row.businessName || "—"})`);
                    continue;
                }
                // Deduplicate: skip if same businessName + phone already exists under this node
                const exists = await BusinessPromotion_1.default.exists({
                    categoryId: String(node._id),
                    businessName: { $regex: new RegExp(`^${row.businessName.trim().replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`, "i") },
                    phone: row.phone.trim(),
                });
                if (exists) {
                    results.skipped++;
                    continue;
                }
                await BusinessPromotion_1.default.create({
                    userId: ADMIN_PLACEHOLDER_ID,
                    businessName: row.businessName.trim(),
                    ownerName: (row.ownerName || row.businessName).trim(),
                    description: row.description?.trim() || "",
                    category: pathNames, // Full path array: [root, sub, sub-sub, ...]
                    categoryId: String(node._id), // Specific node id
                    phone: row.phone.trim(),
                    whatsapp: row.whatsapp?.trim() || row.phone.trim(),
                    email: row.email?.trim() || "",
                    website: row.website?.trim() || "",
                    area: row.area?.trim() || "",
                    city: row.city?.trim() || "",
                    state: row.state?.trim() || "",
                    pincode: row.pincode?.trim() || "",
                    landmark: row.landmark?.trim() || "",
                    listingType: row.listingType === "promoted" ? "promoted" : "free",
                    listingIntent: row.listingType === "promoted" ? "promoted" : "free",
                    status: "active",
                    isActive: true,
                    currentStep: "location",
                    progress: 100,
                    stepIndex: 4,
                    paymentStatus: row.listingType === "promoted" ? "paid" : "not_required",
                });
                results.created++;
            }
            catch (rowErr) {
                results.skipped++;
                results.errors.push(`${row.businessName}: ${rowErr.message}`);
            }
        }
        res.json({
            success: true,
            message: `Upload complete: ${results.created} created, ${results.skipped} skipped`,
            ...results,
        });
    }
    catch (err) {
        console.error("Error uploading CSV to category node:", err);
        res.status(500).json({ success: false, error: "Failed to upload CSV" });
    }
});
exports.default = router;
