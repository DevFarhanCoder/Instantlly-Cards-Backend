"use strict";
/**
 * In-Memory Image Cache Service
 *
 * Provides LRU (Least Recently Used) caching for GridFS images
 * to dramatically reduce MongoDB Atlas free tier timeout issues.
 *
 * Features:
 * - 100MB memory limit (configurable)
 * - 24 hour TTL per image (configurable)
 * - Automatic LRU eviction when full
 * - Periodic cleanup of expired entries
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.imageCache = void 0;
class ImageCache {
    constructor(maxSizeMB = 100, maxAgeHours = 24) {
        this.cache = new Map();
        this.maxSizeBytes = maxSizeMB * 1024 * 1024; // Convert MB to bytes
        this.maxAgeMs = maxAgeHours * 60 * 60 * 1000; // Convert hours to ms
        this.currentSizeBytes = 0;
        this.cleanupInterval = null;
        // Start periodic cleanup (every 30 minutes)
        this.startCleanup();
    }
    /**
     * Get image from cache
     * Returns null if not found or expired
     */
    get(key) {
        const cached = this.cache.get(key);
        if (!cached) {
            return null;
        }
        // Check if expired
        const age = Date.now() - cached.timestamp;
        if (age > this.maxAgeMs) {
            this.delete(key);
            return null;
        }
        // Update access count
        cached.accessCount++;
        return cached.buffer;
    }
    /**
     * Store image in cache
     * Evicts oldest items if necessary
     */
    set(key, buffer) {
        const size = buffer.length;
        // If image is larger than max cache size, don't cache it
        if (size > this.maxSizeBytes) {
            return;
        }
        // If already cached, remove old entry first
        if (this.cache.has(key)) {
            this.delete(key);
        }
        // Evict oldest entries until we have space
        while (this.currentSizeBytes + size > this.maxSizeBytes && this.cache.size > 0) {
            this.evictOldest();
        }
        // Add to cache
        this.cache.set(key, {
            buffer,
            timestamp: Date.now(),
            size,
            accessCount: 0
        });
        this.currentSizeBytes += size;
    }
    /**
     * Delete specific image from cache
     */
    delete(key) {
        const cached = this.cache.get(key);
        if (cached) {
            this.currentSizeBytes -= cached.size;
            this.cache.delete(key);
        }
    }
    /**
     * Evict the least recently used (oldest) image
     */
    evictOldest() {
        let oldestKey = null;
        let oldestTime = Date.now();
        for (const [key, value] of this.cache.entries()) {
            if (value.timestamp < oldestTime) {
                oldestTime = value.timestamp;
                oldestKey = key;
            }
        }
        if (oldestKey) {
            this.delete(oldestKey);
        }
    }
    /**
     * Remove expired entries
     */
    cleanup() {
        const now = Date.now();
        let removedCount = 0;
        let freedBytes = 0;
        for (const [key, value] of this.cache.entries()) {
            const age = now - value.timestamp;
            if (age > this.maxAgeMs) {
                freedBytes += value.size;
                this.delete(key);
                removedCount++;
            }
        }
        void removedCount;
        void freedBytes;
    }
    /**
     * Clear oldest N entries (for memory pressure relief)
     */
    clearOldest(count) {
        // Sort by timestamp (oldest first)
        const sorted = Array.from(this.cache.entries()).sort((a, b) => a[1].timestamp - b[1].timestamp);
        let removedCount = 0;
        let freedBytes = 0;
        for (let i = 0; i < Math.min(count, sorted.length); i++) {
            const [key, value] = sorted[i];
            freedBytes += value.size;
            this.delete(key);
            removedCount++;
        }
        void removedCount;
        void freedBytes;
    }
    /**
     * Start periodic cleanup
     */
    startCleanup() {
        // Run cleanup every 30 minutes
        this.cleanupInterval = setInterval(() => {
            this.cleanup();
            // Check memory usage and clear cache if approaching limit
            const memUsage = process.memoryUsage();
            const heapUsedMB = memUsage.heapUsed / 1024 / 1024;
            // If using >400MB (80% of 512MB limit), aggressively clear cache
            if (heapUsedMB > 400) {
                this.clearOldest(Math.floor(this.cache.size / 2));
            }
        }, 30 * 60 * 1000);
    }
    /**
     * Stop periodic cleanup (for graceful shutdown)
     */
    stopCleanup() {
        if (this.cleanupInterval) {
            clearInterval(this.cleanupInterval);
            this.cleanupInterval = null;
        }
    }
    /**
     * Get cache statistics
     */
    getStats() {
        return {
            size: this.cache.size,
            sizeBytes: this.currentSizeBytes,
            sizeMB: (this.currentSizeBytes / 1024 / 1024).toFixed(2),
            maxSizeMB: (this.maxSizeBytes / 1024 / 1024).toFixed(2),
            utilizationPercent: ((this.currentSizeBytes / this.maxSizeBytes) * 100).toFixed(1)
        };
    }
    /**
     * Clear entire cache
     */
    clear() {
        this.cache.clear();
        this.currentSizeBytes = 0;
    }
}
// Export singleton instance with 50MB limit and 24 hour TTL (reduced for Render 512MB memory limit)
exports.imageCache = new ImageCache(50, 24);
// Graceful shutdown handler
process.on('SIGTERM', () => {
    exports.imageCache.stopCleanup();
});
