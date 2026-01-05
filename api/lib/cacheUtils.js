/**
 * Shared Cache Utilities
 * Provides consistent caching functionality across all route modules
 */

const logger = require("../config/winston");

/**
 * Cache configuration constants
 */
const CACHE_CONFIG = Object.freeze({
  DEFAULT_TTL: 300, // 5 minutes for standard operations
  COMPLEX_TTL: 600, // 10 minutes for complex queries (guidelines)
  PARSING_TTL: 900, // 15 minutes for parsing operations
  MAX_CACHE_SIZE: 150, // Default max cache entries
});

/**
 * Creates a cache instance with utilities
 * @param {Object} options - Cache configuration options
 * @param {string} options.prefix - Cache key prefix
 * @param {number} options.ttl - Default TTL in seconds
 * @param {number} options.maxSize - Maximum cache size
 * @returns {Object} Cache utilities object
 */
function createCache({
  prefix = "cache:",
  ttl = CACHE_CONFIG.DEFAULT_TTL,
  maxSize = CACHE_CONFIG.MAX_CACHE_SIZE,
} = {}) {
  const cache = new Map();

  return {
    /**
     * Generate a cache key from route and parameters
     * @param {string} route - Route identifier
     * @param {Object} params - Parameters to include in key
     * @returns {string} Generated cache key
     */
    generateKey(route, params = {}) {
      const sortedParams = Object.keys(params)
        .sort()
        .map((key) => `${key}:${params[key]}`)
        .join(",");
      return `${prefix}${route}${sortedParams ? `:${sortedParams}` : ""}`;
    },

    /**
     * Get value from cache
     * @param {string} key - Cache key
     * @returns {*} Cached value or null
     */
    get(key) {
      const cached = cache.get(key);
      if (!cached) return null;

      if (Date.now() > cached.expiry) {
        cache.delete(key);
        return null;
      }

      logger.debug("Cache hit", { key });
      return cached.data;
    },

    /**
     * Set value in cache
     * @param {string} key - Cache key
     * @param {*} data - Data to cache
     * @param {number} ttlSeconds - TTL in seconds (optional)
     */
    set(key, data, ttlSeconds = ttl) {
      const expiry = Date.now() + ttlSeconds * 1000;
      cache.set(key, { data, expiry });

      // Cache size management
      if (cache.size > maxSize) {
        const firstKey = cache.keys().next().value;
        cache.delete(firstKey);
      }

      logger.debug("Cache set", { key, ttlSeconds });
    },

    /**
     * Clear cache entries matching pattern
     * @param {string} pattern - Pattern to match (empty string clears all)
     */
    clear(pattern = "") {
      const keysToDelete = [];
      for (const key of cache.keys()) {
        if (pattern === "" || key.includes(pattern)) {
          keysToDelete.push(key);
        }
      }
      keysToDelete.forEach((key) => cache.delete(key));
      logger.info("Cache cleared", {
        pattern: pattern || "all",
        deletedCount: keysToDelete.length,
      });
    },

    /**
     * Invalidate cache entries matching pattern (alias for clear)
     * @param {string} pattern - Pattern to match
     */
    invalidatePattern(pattern) {
      this.clear(pattern);
    },

    /**
     * Get cache statistics
     * @returns {Object} Cache statistics
     */
    getStats() {
      let totalSize = 0;
      let expiredCount = 0;
      const now = Date.now();

      for (const [key, value] of cache.entries()) {
        totalSize++;
        if (now > value.expiry) {
          expiredCount++;
        }
      }

      return {
        totalEntries: totalSize,
        expiredEntries: expiredCount,
        activeEntries: totalSize - expiredCount,
        maxSize,
        prefix,
        defaultTtl: ttl,
      };
    },

    /**
     * Get cache size
     * @returns {number} Current cache size
     */
    size() {
      return cache.size;
    },

    /**
     * Check if key exists in cache
     * @param {string} key - Cache key
     * @returns {boolean} True if key exists and not expired
     */
    has(key) {
      return this.get(key) !== null;
    },
  };
}

module.exports = {
  createCache,
  CACHE_CONFIG,
};
