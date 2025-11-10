/**
 * Simple in-memory cache for ad image metadata
 * Prevents repeated database queries for the same ad images
 */

interface CachedImage {
  gridfsId: string | null;
  base64Data: string | null;
  timestamp: number;
}

class ImageCache {
  private cache: Map<string, CachedImage> = new Map();
  private readonly TTL = 5 * 60 * 1000; // 5 minutes cache TTL
  private readonly MAX_SIZE = 1000; // Maximum number of cached items

  /**
   * Get cached image metadata
   */
  get(adId: string, type: 'bottom' | 'fullscreen'): CachedImage | null {
    const key = `${adId}:${type}`;
    const cached = this.cache.get(key);

    if (!cached) {
      return null;
    }

    // Check if cache entry is still valid
    const now = Date.now();
    if (now - cached.timestamp > this.TTL) {
      this.cache.delete(key);
      return null;
    }

    return cached;
  }

  /**
   * Set cached image metadata
   */
  set(adId: string, type: 'bottom' | 'fullscreen', gridfsId: string | null, base64Data: string | null): void {
    // Enforce max cache size (LRU-like behavior)
    if (this.cache.size >= this.MAX_SIZE) {
      // Delete oldest entry (first key in map)
      const firstKey = this.cache.keys().next().value;
      if (firstKey) {
        this.cache.delete(firstKey);
      }
    }

    const key = `${adId}:${type}`;
    this.cache.set(key, {
      gridfsId,
      base64Data,
      timestamp: Date.now()
    });
  }

  /**
   * Clear a specific cache entry
   */
  clear(adId: string, type?: 'bottom' | 'fullscreen'): void {
    if (type) {
      const key = `${adId}:${type}`;
      this.cache.delete(key);
    } else {
      // Clear both types for this ad
      this.cache.delete(`${adId}:bottom`);
      this.cache.delete(`${adId}:fullscreen`);
    }
  }

  /**
   * Clear all cache entries
   */
  clearAll(): void {
    this.cache.clear();
  }

  /**
   * Get cache statistics
   */
  getStats(): { size: number; maxSize: number; ttl: number } {
    return {
      size: this.cache.size,
      maxSize: this.MAX_SIZE,
      ttl: this.TTL
    };
  }
}

// Export singleton instance
export const imageCache = new ImageCache();
