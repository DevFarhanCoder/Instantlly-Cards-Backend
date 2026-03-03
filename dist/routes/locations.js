"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = require("express");
const auth_1 = require("../middleware/auth");
const UserLocation_1 = __importDefault(require("../models/UserLocation"));
const BusinessPromotion_1 = __importDefault(require("../models/BusinessPromotion"));
const Review_1 = __importDefault(require("../models/Review"));
const router = (0, express_1.Router)();
// ============================================================================
// USER LOCATION ENDPOINTS
// ============================================================================
/**
 * POST /api/locations/user/location
 * Save or update user's current location
 */
router.post("/user/location", auth_1.requireAuth, async (req, res) => {
    try {
        const { latitude, longitude, accuracy, address, radius } = req.body;
        const userId = req.userId;
        // Validations
        if (typeof latitude !== 'number' || latitude < -90 || latitude > 90) {
            return res.status(400).json({
                success: false,
                error: "INVALID_LATITUDE",
                message: "Latitude must be between -90 and 90"
            });
        }
        if (typeof longitude !== 'number' || longitude < -180 || longitude > 180) {
            return res.status(400).json({
                success: false,
                error: "INVALID_LONGITUDE",
                message: "Longitude must be between -180 and 180"
            });
        }
        if (!address || !address.city) {
            return res.status(400).json({
                success: false,
                error: "CITY_REQUIRED",
                message: "City is required in address"
            });
        }
        if (radius && (radius < 1000 || radius > 50000)) {
            return res.status(400).json({
                success: false,
                error: "INVALID_RADIUS",
                message: "Radius must be between 1000 and 50000 meters"
            });
        }
        // Store location in GeoJSON format [longitude, latitude]
        let userLocation = await UserLocation_1.default.findOne({ userId });
        if (userLocation) {
            // Update existing location
            userLocation.currentLocation = {
                type: 'Point',
                coordinates: [longitude, latitude]
            };
            userLocation.address = {
                plotNo: address.plotNo || userLocation.address?.plotNo,
                streetName: address.streetName || userLocation.address?.streetName,
                area: address.area || userLocation.address?.area,
                landmark: address.landmark || userLocation.address?.landmark,
                city: address.city,
                state: address.state || userLocation.address?.state,
                pincode: address.pincode || userLocation.address?.pincode,
                formattedAddress: address.formattedAddress || userLocation.address?.formattedAddress
            };
            userLocation.accuracy = accuracy || userLocation.accuracy;
            userLocation.radius = radius || userLocation.radius || 5000;
            userLocation.lastUpdated = new Date();
        }
        else {
            // Create new location
            userLocation = new UserLocation_1.default({
                userId,
                currentLocation: {
                    type: 'Point',
                    coordinates: [longitude, latitude]
                },
                address: {
                    plotNo: address.plotNo,
                    streetName: address.streetName,
                    area: address.area,
                    landmark: address.landmark,
                    city: address.city,
                    state: address.state,
                    pincode: address.pincode,
                    formattedAddress: address.formattedAddress
                },
                accuracy,
                radius: radius || 5000,
                isLocationEnabled: true
            });
        }
        await userLocation.save();
        const statusCode = await UserLocation_1.default.countDocuments({ userId }) > 1 ? 200 : 201;
        res.status(statusCode).json({
            success: true,
            location: {
                _id: userLocation._id,
                userId: userLocation.userId,
                currentLocation: userLocation.currentLocation,
                address: userLocation.address,
                radius: userLocation.radius,
                lastUpdated: userLocation.lastUpdated,
                isLocationEnabled: userLocation.isLocationEnabled
            }
        });
    }
    catch (error) {
        console.error("Error saving location:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to save location"
        });
    }
});
/**
 * GET /api/locations/user/location
 * Get user's current location
 */
router.get("/user/location", auth_1.requireAuth, async (req, res) => {
    try {
        const userId = req.userId;
        const userLocation = await UserLocation_1.default.findOne({ userId });
        if (!userLocation) {
            return res.status(404).json({
                success: false,
                error: "LOCATION_NOT_FOUND",
                message: "User location not saved yet"
            });
        }
        res.status(200).json({
            success: true,
            location: {
                _id: userLocation._id,
                userId: userLocation.userId,
                currentLocation: userLocation.currentLocation,
                address: userLocation.address,
                radius: userLocation.radius,
                isLocationEnabled: userLocation.isLocationEnabled,
                lastUpdated: userLocation.lastUpdated
            }
        });
    }
    catch (error) {
        console.error("Error fetching location:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to fetch location"
        });
    }
});
/**
 * PATCH /api/locations/user/location/preferences
 * Update location preferences
 */
router.patch("/user/location/preferences", auth_1.requireAuth, async (req, res) => {
    try {
        const { radius, isLocationEnabled, shareLocationWith } = req.body;
        const userId = req.userId;
        const userLocation = await UserLocation_1.default.findOne({ userId });
        if (!userLocation) {
            return res.status(404).json({
                success: false,
                error: "LOCATION_NOT_FOUND",
                message: "User location not saved yet"
            });
        }
        if (typeof radius === 'number') {
            if (radius < 1000 || radius > 50000) {
                return res.status(400).json({
                    success: false,
                    error: "INVALID_RADIUS",
                    message: "Radius must be between 1000 and 50000 meters"
                });
            }
            userLocation.radius = radius;
        }
        if (typeof isLocationEnabled === 'boolean') {
            userLocation.isLocationEnabled = isLocationEnabled;
        }
        if (shareLocationWith && ['public', 'private'].includes(shareLocationWith)) {
            userLocation.shareLocationWith = shareLocationWith;
        }
        await userLocation.save();
        res.status(200).json({
            success: true,
            preferences: {
                radius: userLocation.radius,
                isLocationEnabled: userLocation.isLocationEnabled,
                shareLocationWith: userLocation.shareLocationWith
            }
        });
    }
    catch (error) {
        console.error("Error updating location preferences:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to update preferences"
        });
    }
});
// ============================================================================
// LOCATION-BASED BUSINESS LISTINGS
// ============================================================================
/**
 * GET /api/locations/business-listings/nearby
 * Get businesses near user using geospatial queries
 */
router.get("/business-listings/nearby", auth_1.requireAuth, async (req, res) => {
    try {
        const { category, sort = 'distance', limit = 20, includeExpired = false } = req.query;
        // Get user's location
        const userLocation = await UserLocation_1.default.findOne({ userId: req.userId });
        if (!userLocation) {
            return res.status(400).json({
                success: false,
                error: "LOCATION_NOT_SET",
                message: "Please set your location first"
            });
        }
        const [userLon, userLat] = userLocation.currentLocation.coordinates;
        const searchRadius = userLocation.radius || 5000;
        // Build category filter
        const categoryFilter = {};
        if (category) {
            categoryFilter.category = category;
        }
        // Build status filter
        const statusFilter = {
            status: 'active',
            isActive: true
        };
        if (includeExpired !== 'true') {
            statusFilter.$or = [
                { paymentStatus: { $in: ['not_required', 'paid'] } },
                { plan: { $ne: null } }
            ];
        }
        // Geospatial query - find businesses near user
        // Note: BusinessPromotion needs a 2dsphere index on location coordinates
        // For now, we'll do client-side distance calculation or assume location is stored
        const allBusinesses = await BusinessPromotion_1.default.find({
            ...statusFilter,
            ...categoryFilter,
            // Basic status check - in production, add geospatial index
        })
            .populate('userId', 'name phone')
            .limit(parseInt(limit) || 20);
        // Calculate distance for each business and filter
        const businessesWithDistance = allBusinesses
            .map(business => {
            // Try to get business location from address fields
            // In production, businesses should have proper GeoJSON location field
            const distance = calculateDistance(userLat, userLon, userLat, userLon); // Placeholder
            return {
                ...business.toObject(),
                distance: distance / 1000 // Convert to km
            };
        })
            .filter(b => (b.distance) <= (searchRadius / 1000))
            .sort((a, b) => {
            if (sort === 'rating') {
                return (b.rating || 0) - (a.rating || 0);
            }
            return a.distance - b.distance;
        });
        // Get review stats for each business
        const businessesWithStats = await Promise.all(businessesWithDistance.map(async (business) => {
            const reviews = await Review_1.default.find({
                businessId: business._id,
                isApproved: true,
                isSpam: false
            });
            const avgRating = reviews.length > 0
                ? (reviews.reduce((sum, r) => sum + r.rating, 0) / reviews.length).toFixed(1)
                : 0;
            return {
                _id: business._id,
                businessName: business.businessName,
                category: business.category,
                email: business.email,
                phone: business.phone,
                whatsapp: business.whatsapp,
                address: {
                    city: business.city,
                    area: business.area,
                    formattedAddress: `${business.plotNo} ${business.streetName}, ${business.area}, ${business.city}`
                },
                media: business.media || [],
                distance: business.distance,
                rating: avgRating,
                totalReviews: reviews.length,
                listingType: business.listingType,
                status: business.status,
                businessHours: business.businessHours,
                visibility: business.visibility
            };
        }));
        const pageNum = 1;
        const limitNum = parseInt(limit) || 20;
        res.status(200).json({
            success: true,
            userLocation: {
                latitude: userLat,
                longitude: userLon,
                city: userLocation.address.city
            },
            listings: businessesWithStats.slice(0, limitNum),
            pagination: {
                total: businessesWithStats.length,
                limit: limitNum,
                page: pageNum,
                pages: Math.ceil(businessesWithStats.length / limitNum)
            },
            filters: {
                availableCategories: await getAvailableCategories(),
                distanceRange: { min: 0.5, max: searchRadius / 1000 }
            }
        });
    }
    catch (error) {
        console.error("Error fetching nearby businesses:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to fetch nearby businesses"
        });
    }
});
/**
 * GET /api/locations/business-listings/nearby?anonymous=true
 * Get nearby businesses without authentication (guest mode)
 */
router.get("/business-listings/nearby-guest", async (req, res) => {
    try {
        const { latitude, longitude, radius = 5000, category, sort = 'distance', limit = 20 } = req.query;
        if (!latitude || !longitude) {
            return res.status(400).json({
                success: false,
                error: "LOCATION_REQUIRED",
                message: "latitude and longitude are required"
            });
        }
        const userLat = parseFloat(latitude);
        const userLon = parseFloat(longitude);
        const searchRadius = parseInt(radius) || 5000;
        // Build filters
        const categoryFilter = {};
        if (category) {
            categoryFilter.category = category;
        }
        const statusFilter = {
            status: 'active',
            isActive: true
        };
        const businesses = await BusinessPromotion_1.default.find({
            ...statusFilter,
            ...categoryFilter
        })
            .populate('userId', 'name phone')
            .limit(parseInt(limit) || 20);
        // Get review stats
        const businessesWithData = await Promise.all(businesses.map(async (business) => {
            const reviews = await Review_1.default.find({
                businessId: business._id,
                isApproved: true,
                isSpam: false
            });
            const avgRating = reviews.length > 0
                ? (reviews.reduce((sum, r) => sum + r.rating, 0) / reviews.length).toFixed(1)
                : 0;
            return {
                _id: business._id,
                businessName: business.businessName,
                category: business.category,
                email: business.email,
                phone: business.phone,
                whatsapp: business.whatsapp,
                address: {
                    city: business.city,
                    area: business.area
                },
                media: business.media || [],
                rating: avgRating,
                totalReviews: reviews.length,
                listingType: business.listingType,
                status: business.status
            };
        }));
        res.status(200).json({
            success: true,
            userLocation: {
                latitude: userLat,
                longitude: userLon
            },
            listings: businessesWithData,
            pagination: {
                total: businessesWithData.length,
                limit: parseInt(limit) || 20,
                page: 1
            }
        });
    }
    catch (error) {
        console.error("Error fetching nearby businesses (guest):", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to fetch nearby businesses"
        });
    }
});
/**
 * GET /api/locations/business-listings/category/:category/nearby
 * Get businesses in specific category near user
 */
router.get("/business-listings/category/:category/nearby", auth_1.requireAuth, async (req, res) => {
    try {
        const { category } = req.params;
        const { sort = 'rating', limit = 20 } = req.query;
        // Get user's location
        const userLocation = await UserLocation_1.default.findOne({ userId: req.userId });
        if (!userLocation) {
            return res.status(400).json({
                success: false,
                error: "LOCATION_NOT_SET",
                message: "Please set your location first"
            });
        }
        const [userLon, userLat] = userLocation.currentLocation.coordinates;
        // Find businesses in category
        const businesses = await BusinessPromotion_1.default.find({
            category: category,
            status: 'active',
            isActive: true
        })
            .limit(parseInt(limit) || 20);
        // Add review stats
        const businessesWithStats = await Promise.all(businesses.map(async (business) => {
            const reviews = await Review_1.default.find({
                businessId: business._id,
                isApproved: true,
                isSpam: false
            });
            const avgRating = reviews.length > 0
                ? (reviews.reduce((sum, r) => sum + r.rating, 0) / reviews.length).toFixed(1)
                : 0;
            return {
                _id: business._id,
                businessName: business.businessName,
                category: business.category,
                email: business.email,
                phone: business.phone,
                address: {
                    city: business.city,
                    area: business.area
                },
                rating: avgRating,
                totalReviews: reviews.length,
                listingType: business.listingType,
                status: business.status
            };
        }));
        // Sort
        let sorted = businessesWithStats;
        if (sort === 'rating') {
            sorted = businessesWithStats.sort((a, b) => parseFloat(b.rating) - parseFloat(a.rating));
        }
        res.status(200).json({
            success: true,
            listings: sorted,
            pagination: {
                total: sorted.length,
                limit: parseInt(limit) || 20
            }
        });
    }
    catch (error) {
        console.error("Error fetching category businesses:", error);
        res.status(500).json({
            success: false,
            error: "INTERNAL_ERROR",
            message: "Failed to fetch businesses"
        });
    }
});
// ============================================================================
// HELPER FUNCTIONS
// ============================================================================
/**
 * Calculate distance between two coordinates using Haversine formula
 */
function calculateDistance(lat1, lon1, lat2, lon2) {
    const R = 6371000; // Earth's radius in meters
    const dLat = (lat2 - lat1) * Math.PI / 180;
    const dLon = (lon2 - lon1) * Math.PI / 180;
    const a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
        Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
            Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    return R * c;
}
/**
 * Get available categories for filter
 */
async function getAvailableCategories() {
    const businesses = await BusinessPromotion_1.default.find({ status: 'active' });
    const categories = new Set();
    businesses.forEach(b => {
        b.category?.forEach((c) => categories.add(c));
    });
    return Array.from(categories);
}
exports.default = router;
