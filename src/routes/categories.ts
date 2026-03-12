// src/routes/categories.ts
import express, { Request, Response, NextFunction } from "express";
import mongoose from "mongoose";
import Category from "../models/Category";
import CustomService from "../models/CustomService";
import BusinessPromotion from "../models/BusinessPromotion";
import Card from "../models/Card";
import User from "../models/User";

const router = express.Router();

const parsedMobileCategoryCacheTtl = Number(
  process.env.MOBILE_CATEGORY_CACHE_TTL_MS ?? "0",
);
const MOBILE_CATEGORY_CACHE_TTL_MS =
  Number.isFinite(parsedMobileCategoryCacheTtl) &&
  parsedMobileCategoryCacheTtl > 0
    ? Math.floor(parsedMobileCategoryCacheTtl)
    : 0;

type CategoryLeanDoc = {
  _id: mongoose.Types.ObjectId;
  name?: string;
  icon?: string;
  order?: number;
  subcategories?: unknown;
  updatedAt?: Date;
};

type CachedCategorySummary = {
  expiresAt: number;
  data: Array<{
    _id: string;
    name: string;
    icon: string;
    order: number;
    subcategoryCount: number;
    updatedAt?: Date;
  }>;
};

let cachedMobileCategorySummary: CachedCategorySummary | null = null;

const invalidateMobileCategoryCache = () => {
  cachedMobileCategorySummary = null;
};

const normalizeSubcategories = (value: unknown): string[] => {
  if (!Array.isArray(value)) return [];

  return value
    .filter((item): item is string => typeof item === "string")
    .map((item) => item.trim())
    .filter(Boolean);
};

const parsePositiveInt = (value: unknown, defaultValue: number): number => {
  const parsed = Number(value);
  if (!Number.isFinite(parsed) || parsed <= 0) return defaultValue;
  return Math.floor(parsed);
};

const setNoStoreHeaders = (res: Response) => {
  res.set({
    "Cache-Control": "no-store, no-cache, must-revalidate, proxy-revalidate",
    Pragma: "no-cache",
    Expires: "0",
    "Surrogate-Control": "no-store",
  });
};

const getMobileCategorySummary = async (options?: { bypassCache?: boolean }) => {
  const now = Date.now();
  const bypassCache = Boolean(options?.bypassCache) || MOBILE_CATEGORY_CACHE_TTL_MS <= 0;

  if (
    !bypassCache &&
    cachedMobileCategorySummary &&
    cachedMobileCategorySummary.expiresAt > now
  ) {
    console.log("[CATEGORIES] Mobile summary cache hit");
    return cachedMobileCategorySummary.data;
  }

  const categories = (await Category.find({ isActive: true })
    .select("_id name icon order subcategories updatedAt")
    .sort({ order: 1, name: 1 })
    .lean()) as CategoryLeanDoc[];

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
  } else {
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
const adminAuth = (req: Request, res: Response, next: NextFunction) => {
  const adminKey = req.headers["x-admin-key"];
  const validKeys = [
    process.env.ADMIN_SECRET_KEY,
    "your-secure-admin-key-here",
    "Farhan_90",
  ].filter(Boolean);

  if (adminKey && validKeys.includes(adminKey as string)) {
    next();
  } else {
    res.status(401).json({ error: "Unauthorized", message: "Invalid admin key" });
  }
};

// ============================================================
// PUBLIC ENDPOINTS (for mobile app)
// ============================================================

// GET /api/categories/mobile - Lightweight mobile list (optimized for home grid)
router.get("/mobile", async (req: Request, res: Response) => {
  try {
    const freshQuery = String(req.query.fresh ?? "").trim().toLowerCase();
    const bypassCache =
      freshQuery === "1" || freshQuery === "true" || freshQuery === "yes";

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
  } catch (error: any) {
    console.error("Error fetching mobile category summary:", error);
    res.status(500).json({
      success: false,
      error: "Failed to fetch category summary",
    });
  }
});

// GET /api/categories/mobile/:categoryId/subcategories - Lazy subcategory fetch for a single category
router.get("/mobile/:categoryId/subcategories", async (req: Request, res: Response) => {
  try {
    const { categoryId } = req.params;
    console.log("[CATEGORIES] GET /api/categories/mobile/:categoryId/subcategories", {
      categoryId,
      page: req.query.page,
      limit: req.query.limit,
      search: req.query.search,
    });
    if (!mongoose.Types.ObjectId.isValid(categoryId)) {
      return res.status(400).json({
        success: false,
        error: "Invalid categoryId",
      });
    }

    const page = parsePositiveInt(req.query.page, 1);
    const limit = Math.min(parsePositiveInt(req.query.limit, 50), 200);
    const search =
      typeof req.query.search === "string" ? req.query.search.trim() : "";

    const category = (await Category.findOne({
      _id: categoryId,
      isActive: true,
    })
      .select("_id name subcategories")
      .lean()) as CategoryLeanDoc | null;

    if (!category) {
      return res.status(404).json({
        success: false,
        error: "Category not found",
      });
    }

    let subcategories = normalizeSubcategories(category.subcategories);
    if (search) {
      const normalizedSearch = search.toLowerCase();
      subcategories = subcategories.filter((subcategory) =>
        subcategory.toLowerCase().includes(normalizedSearch),
      );
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
  } catch (error: any) {
    console.error("Error fetching category subcategories:", error);
    res.status(500).json({
      success: false,
      error: "Failed to fetch subcategories",
    });
  }
});

// GET /api/categories - Get all active categories with subcategories
router.get("/", async (req: Request, res: Response) => {
  try {
    const categories = (await Category.find({ isActive: true })
      .select("_id name icon subcategories isActive order")
      .sort({ order: 1, name: 1 })
      .lean()) as CategoryLeanDoc[];

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
  } catch (error: any) {
    console.error("Error fetching categories:", error);
    res.status(500).json({ success: false, error: "Failed to fetch categories" });
  }
});

// POST /api/categories/custom-service - Mobile app reports a custom service
router.post("/custom-service", async (req: Request, res: Response) => {
  try {
    const { serviceName, userId, userName, cardId, parentCategory } = req.body;

    if (!serviceName || !userId) {
      return res.status(400).json({
        success: false,
        error: "serviceName and userId are required",
      });
    }

    // Check if this custom service was already reported (avoid duplicates)
    const existing = await CustomService.findOne({
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
    const matchingCategory = await Category.findOne({
      $or: [
        { name: { $regex: new RegExp(`^${serviceName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") } },
        { subcategories: { $regex: new RegExp(`^${serviceName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") } },
      ],
    });

    if (matchingCategory) {
      // Determine if it matched as a category name or subcategory
      const isCategory = matchingCategory.name.toLowerCase() === serviceName.toLowerCase();
      const matchedSub = matchingCategory.subcategories.find(
        (s: string) => s.toLowerCase() === serviceName.toLowerCase()
      );
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

    const customService = new CustomService({
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
  } catch (error: any) {
    console.error("Error saving custom service:", error);
    res.status(500).json({ success: false, error: "Failed to save custom service" });
  }
});

// ============================================================
// ADMIN ENDPOINTS
// ============================================================

// GET /api/categories/admin/custom-services - Get all pending custom services
router.get("/admin/custom-services", adminAuth, async (req: Request, res: Response) => {
  try {
    const { status = "pending", page = 1, limit = 50 } = req.query;
    const skip = (Number(page) - 1) * Number(limit);

    const filter: any = {};
    if (status && status !== "all") {
      filter.status = status;
    }

    const [customServices, total] = await Promise.all([
      CustomService.find(filter)
        .sort({ createdAt: -1 })
        .skip(skip)
        .limit(Number(limit))
        .lean(),
      CustomService.countDocuments(filter),
    ]);

    res.json({
      success: true,
      data: customServices,
      total,
      page: Number(page),
      totalPages: Math.ceil(total / Number(limit)),
    });
  } catch (error: any) {
    console.error("Error fetching custom services:", error);
    res.status(500).json({ success: false, error: "Failed to fetch custom services" });
  }
});

// GET /api/categories/admin/all - Get all categories (including inactive) for admin
router.get("/admin/all", adminAuth, async (req: Request, res: Response) => {
  try {
    const categories = await Category.find()
      .sort({ order: 1, name: 1 })
      .lean();

    res.json({
      success: true,
      data: categories,
    });
  } catch (error: any) {
    console.error("Error fetching categories:", error);
    res.status(500).json({ success: false, error: "Failed to fetch categories" });
  }
});

// POST /api/categories/admin/seed - Seed initial categories from the hardcoded list
router.post("/admin/seed", adminAuth, async (req: Request, res: Response) => {
  try {
    const SERVICE_CATEGORIES: Record<string, string[]> = {
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

    const CATEGORY_ICONS: Record<string, string> = {
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
      const existing = await Category.findOne({ name: categoryName });
      if (existing) {
        // Merge subcategories (add new ones, keep existing)
        const existingSubs = new Set(existing.subcategories.map((s: string) => s.toLowerCase()));
        const newSubs = subcategories.filter((s) => !existingSubs.has(s.toLowerCase()));
        if (newSubs.length > 0) {
          existing.subcategories.push(...newSubs);
          existing.icon = CATEGORY_ICONS[categoryName] || existing.icon;
          await existing.save();
          updated++;
        }
      } else {
        await Category.create({
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
  } catch (error: any) {
    console.error("Error seeding categories:", error);
    res.status(500).json({ success: false, error: "Failed to seed categories" });
  }
});

// POST /api/categories/admin/add-category - Add a new category
router.post("/admin/add-category", adminAuth, async (req: Request, res: Response) => {
  try {
    const { name, icon, subcategories } = req.body;

    if (!name) {
      return res.status(400).json({ success: false, error: "Category name is required" });
    }

    const existing = await Category.findOne({
      name: { $regex: new RegExp(`^${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") },
    });

    if (existing) {
      return res.status(409).json({
        success: false,
        error: "Category already exists",
        data: existing,
      });
    }

    const maxOrder = await Category.findOne().sort({ order: -1 }).lean();
    const newCategory = await Category.create({
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
  } catch (error: any) {
    console.error("Error adding category:", error);
    res.status(500).json({ success: false, error: "Failed to add category" });
  }
});

// POST /api/categories/admin/add-subcategory - Add a subcategory to an existing category
router.post("/admin/add-subcategory", adminAuth, async (req: Request, res: Response) => {
  try {
    const { categoryId, subcategoryName } = req.body;

    if (!categoryId || !subcategoryName) {
      return res.status(400).json({
        success: false,
        error: "categoryId and subcategoryName are required",
      });
    }

    const category = await Category.findById(categoryId);
    if (!category) {
      return res.status(404).json({ success: false, error: "Category not found" });
    }

    // Check if subcategory already exists
    const exists = category.subcategories.some(
      (s: string) => s.toLowerCase() === subcategoryName.trim().toLowerCase()
    );

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
  } catch (error: any) {
    console.error("Error adding subcategory:", error);
    res.status(500).json({ success: false, error: "Failed to add subcategory" });
  }
});

// PUT /api/categories/admin/approve-custom/:id - Approve a custom service as category or subcategory
router.put("/admin/approve-custom/:id", adminAuth, async (req: Request, res: Response) => {
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

    const customService = await CustomService.findById(id);
    if (!customService) {
      return res.status(404).json({ success: false, error: "Custom service not found" });
    }

    if (customService.status !== "pending") {
      return res.status(400).json({
        success: false,
        error: `Custom service already ${customService.status}`,
      });
    }

    let resultCategory: any = null;

    if (approveAs === "category") {
      // Create as a new category (the custom service name becomes a new top-level category)
      const catName = newCategoryName || customService.serviceName;
      const existing = await Category.findOne({
        name: { $regex: new RegExp(`^${catName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`, "i") },
      });

      if (existing) {
        return res.status(409).json({
          success: false,
          error: `Category "${catName}" already exists`,
        });
      }

      const maxOrder = await Category.findOne().sort({ order: -1 }).lean();
      resultCategory = await Category.create({
        name: catName.trim(),
        icon: newCategoryIcon || "📁",
        subcategories: [],
        order: (maxOrder?.order || 0) + 1,
      });

      customService.approvedAs = { type: "category", categoryName: catName };
    } else {
      // Add as subcategory to existing category
      if (!categoryId) {
        return res.status(400).json({
          success: false,
          error: "categoryId is required when adding as subcategory",
        });
      }

      const category = await Category.findById(categoryId);
      if (!category) {
        return res.status(404).json({ success: false, error: "Category not found" });
      }

      const subExists = category.subcategories.some(
        (s: string) => s.toLowerCase() === customService.serviceName.toLowerCase()
      );

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
  } catch (error: any) {
    console.error("Error approving custom service:", error);
    res.status(500).json({ success: false, error: "Failed to approve custom service" });
  }
});

// PUT /api/categories/admin/reject-custom/:id - Reject a custom service
router.put("/admin/reject-custom/:id", adminAuth, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;

    const customService = await CustomService.findById(id);
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
  } catch (error: any) {
    console.error("Error rejecting custom service:", error);
    res.status(500).json({ success: false, error: "Failed to reject custom service" });
  }
});

// DELETE /api/categories/admin/custom-service/:id - Delete a custom service
router.delete("/admin/custom-service/:id", adminAuth, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const result = await CustomService.findByIdAndDelete(id);
    if (!result) {
      return res.status(404).json({ success: false, error: "Custom service not found" });
    }
    res.json({ success: true, message: "Custom service deleted" });
  } catch (error: any) {
    console.error("Error deleting custom service:", error);
    res.status(500).json({ success: false, error: "Failed to delete custom service" });
  }
});

// POST /api/categories/admin/upload-companies - Bulk upload companies via CSV for a subcategory
// Body: { subcategory: string, category: string, rows: Array<{ businessName, ownerName, description, phone, whatsapp, email, website, area, city, state, pincode, listingType }> }
router.post("/admin/upload-companies", adminAuth, async (req: Request, res: Response) => {
  try {
    const { subcategory, category, rows } = req.body;

    if (!subcategory || !Array.isArray(rows) || rows.length === 0) {
      return res.status(400).json({
        success: false,
        error: "subcategory and rows[] are required",
      });
    }

    // Placeholder ObjectId for admin-imported records (no real user)
    const ADMIN_PLACEHOLDER_ID = new mongoose.Types.ObjectId("000000000000000000000001");
    const results: { created: number; skipped: number; errors: string[] } = {
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
        const categoryArray: string[] = [subcategory.trim()];
        if (category && category.trim() !== subcategory.trim()) {
          categoryArray.push(category.trim());
        }

        await BusinessPromotion.create({
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
      } catch (rowErr: any) {
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
  } catch (error: any) {
    console.error("Error uploading companies:", error);
    res.status(500).json({ success: false, error: "Failed to upload companies" });
  }
});

// PUT /api/categories/admin/category/:id - Update category name, icon, or isActive
router.put("/admin/category/:id", adminAuth, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const { name, icon, isActive } = req.body;

    const category = await Category.findById(id);
    if (!category) {
      return res.status(404).json({ success: false, error: "Category not found" });
    }

    if (name !== undefined && name.trim()) category.name = name.trim();
    if (icon !== undefined) category.icon = icon;
    if (isActive !== undefined) category.isActive = Boolean(isActive);

    await category.save();
    invalidateMobileCategoryCache();

    res.json({
      success: true,
      message: `Category "${category.name}" updated`,
      data: category,
    });
  } catch (error: any) {
    console.error("Error updating category:", error);
    res.status(500).json({ success: false, error: "Failed to update category" });
  }
});

// DELETE /api/categories/admin/category/:id - Delete a category
router.delete("/admin/category/:id", adminAuth, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const category = await Category.findByIdAndDelete(id);
    if (!category) {
      return res.status(404).json({ success: false, error: "Category not found" });
    }
    invalidateMobileCategoryCache();
    res.json({ success: true, message: `Category "${category.name}" deleted` });
  } catch (error: any) {
    console.error("Error deleting category:", error);
    res.status(500).json({ success: false, error: "Failed to delete category" });
  }
});

// DELETE /api/categories/admin/category/:id/subcategory/:subName - Remove a subcategory from a category
router.delete("/admin/category/:id/subcategory/:subName", adminAuth, async (req: Request, res: Response) => {
  try {
    const { id, subName } = req.params;
    const subcategoryName = decodeURIComponent(subName);

    const category = await Category.findById(id);
    if (!category) {
      return res.status(404).json({ success: false, error: "Category not found" });
    }

    const idx = category.subcategories.findIndex(
      (s: string) => s.toLowerCase() === subcategoryName.toLowerCase()
    );
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
  } catch (error: any) {
    console.error("Error removing subcategory:", error);
    res.status(500).json({ success: false, error: "Failed to remove subcategory" });
  }
});

export default router;
