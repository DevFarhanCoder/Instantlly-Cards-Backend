"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// src/routes/categories.ts
const express_1 = __importDefault(require("express"));
const Category_1 = __importDefault(require("../models/Category"));
const CustomService_1 = __importDefault(require("../models/CustomService"));
const router = express_1.default.Router();
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
// ============================================================
// PUBLIC ENDPOINTS (for mobile app)
// ============================================================
// GET /api/categories - Get all active categories with subcategories
router.get("/", async (req, res) => {
    try {
        const categories = await Category_1.default.find({ isActive: true })
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
            return res.json({
                success: true,
                message: "Service already exists in categories",
                alreadyExists: true,
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
                "Car Repairs & Services", "Taxi & Cab Services", "Towing Services",
                "Transporters & Logistics",
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
// DELETE /api/categories/admin/category/:id - Delete a category
router.delete("/admin/category/:id", adminAuth, async (req, res) => {
    try {
        const { id } = req.params;
        const category = await Category_1.default.findByIdAndDelete(id);
        if (!category) {
            return res.status(404).json({ success: false, error: "Category not found" });
        }
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
exports.default = router;
