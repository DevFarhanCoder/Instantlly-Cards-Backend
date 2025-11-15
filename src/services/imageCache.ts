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
 * - Cache hit/miss logging for monitoring
 */

interface CachedImage {
  buffer: Buffer;
  timestamp: number;
  size: number;
  accessCount: number;
}

class ImageCache {
  private cache: Map<string, CachedImage>;
  private maxSizeBytes: number;
  private maxAgeMs: number;
  private currentSizeBytes: number;
  private cleanupInterval: NodeJS.Timeout | null;

  constructor(maxSizeMB: number = 100, maxAgeHours: number = 24) {
    this.cache = new Map();
    this.maxSizeBytes = maxSizeMB * 1024 * 1024; // Convert MB to bytes
    this.maxAgeMs = maxAgeHours * 60 * 60 * 1000; // Convert hours to ms
    this.currentSizeBytes = 0;
    this.cleanupInterval = null;

    // Start periodic cleanup (every 30 minutes)
    this.startCleanup();

    console.log(`🗄️  Image Cache initialized:`);
    console.log(`   Max size: ${maxSizeMB} MB`);
    console.log(`   Max age: ${maxAgeHours} hours`);
    console.log(`   Cleanup interval: 30 minutes`);
  }

  /**
   * Get image from cache
   * Returns null if not found or expired
   */
  get(key: string): Buffer | null {
    const cached = this.cache.get(key);
    
    if (!cached) {
      console.log(`💨 [CACHE MISS] Image not in cache: ${key}`);
      return null;
    }

    // Check if expired
    const age = Date.now() - cached.timestamp;
    if (age > this.maxAgeMs) {
      console.log(`⏰ [CACHE EXPIRED] Image too old (${Math.round(age / 1000 / 60)} min): ${key}`);
      this.delete(key);
      return null;
    }

    // Update access count
    cached.accessCount++;
    
    const ageSeconds = Math.round(age / 1000);
    const sizeKB = (cached.size / 1024).toFixed(2);
    console.log(`✅ [CACHE HIT] Serving image from cache (${sizeKB} KB, age: ${ageSeconds}s): ${key}`);
    
    return cached.buffer;
  }

  /**
   * Store image in cache
   * Evicts oldest items if necessary
   */
  set(key: string, buffer: Buffer): void {
    const size = buffer.length;

    // If image is larger than max cache size, don't cache it
    if (size > this.maxSizeBytes) {
      console.log(`⚠️  [CACHE SKIP] Image too large (${(size / 1024 / 1024).toFixed(2)} MB): ${key}`);
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

    const sizeKB = (size / 1024).toFixed(2);
    const cacheSizeMB = (this.currentSizeBytes / 1024 / 1024).toFixed(2);
    console.log(`💾 [CACHE SET] Stored image ${key} (${sizeKB} KB) - Cache: ${this.cache.size} images, ${cacheSizeMB} MB`);
  }

  /**
   * Delete specific image from cache
   */
  delete(key: string): void {
    const cached = this.cache.get(key);
    if (cached) {
      this.currentSizeBytes -= cached.size;
      this.cache.delete(key);
    }
  }

  /**
   * Evict the least recently used (oldest) image
   */
  private evictOldest(): void {
    let oldestKey: string | null = null;
    let oldestTime = Date.now();

    for (const [key, value] of this.cache.entries()) {
      if (value.timestamp < oldestTime) {
        oldestTime = value.timestamp;
        oldestKey = key;
      }
    }

    if (oldestKey) {
      const cached = this.cache.get(oldestKey)!;
      const sizeKB = (cached.size / 1024).toFixed(2);
      console.log(`🗑️  [CACHE EVICT] Removing oldest image (${sizeKB} KB): ${oldestKey}`);
      this.delete(oldestKey);
    }
  }

  /**
   * Remove expired entries
   */
  cleanup(): void {
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

    if (removedCount > 0) {
      const freedMB = (freedBytes / 1024 / 1024).toFixed(2);
      console.log(`🧹 [CACHE CLEANUP] Removed ${removedCount} expired images (freed ${freedMB} MB)`);
    }
  }

  /**
   * Start periodic cleanup
   */
  private startCleanup(): void {
    // Run cleanup every 30 minutes
    this.cleanupInterval = setInterval(() => {
      this.cleanup();
    }, 30 * 60 * 1000);
  }

  /**
   * Stop periodic cleanup (for graceful shutdown)
   */
  stopCleanup(): void {
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
  clear(): void {
    this.cache.clear();
    this.currentSizeBytes = 0;
    console.log('🗑️  [CACHE CLEAR] All cached images removed');
  }
}

// Export singleton instance with 100MB limit and 24 hour TTL
export const imageCache = new ImageCache(100, 24);

// Graceful shutdown handler
process.on('SIGTERM', () => {
  console.log('📊 [CACHE SHUTDOWN] Final stats:', imageCache.getStats());
  imageCache.stopCleanup();
});
