"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = require("mongoose");
const UserLocationSchema = new mongoose_1.Schema({
    userId: {
        type: mongoose_1.Schema.Types.ObjectId,
        ref: 'User',
        required: true,
        unique: true,
        index: true
    },
    // Current Location (GeoJSON format for geospatial queries)
    currentLocation: {
        type: {
            type: String,
            enum: ['Point'],
            default: 'Point'
        },
        coordinates: {
            type: [Number], // [longitude, latitude]
            required: true
        }
    },
    // Address Components
    address: {
        plotNo: String,
        streetName: String,
        area: String,
        landmark: String,
        city: {
            type: String,
            required: true,
            index: true
        },
        state: String,
        pincode: String,
        formattedAddress: String
    },
    // Location Metadata
    accuracy: Number, // GPS accuracy in meters
    lastUpdated: {
        type: Date,
        default: Date.now,
        index: true
    },
    // Preferences
    radius: {
        type: Number,
        default: 5000 // meters, default 5km
    }, // Used for searching nearby businesses
    // Location History (optional - for analytics)
    previousLocations: [
        {
            coordinates: [Number],
            city: String,
            updatedAt: Date
        }
    ],
    // Privacy
    isLocationEnabled: {
        type: Boolean,
        default: true
    },
    shareLocationWith: {
        type: String,
        enum: ['public', 'private'],
        default: 'private'
    },
    createdAt: { type: Date, default: Date.now },
    updatedAt: { type: Date, default: Date.now }
}, { timestamps: true });
// Geospatial index for efficient location queries
UserLocationSchema.index({ 'currentLocation': '2dsphere' });
UserLocationSchema.index({ city: 1 });
exports.default = mongoose_1.models.UserLocation || (0, mongoose_1.model)("UserLocation", UserLocationSchema);
