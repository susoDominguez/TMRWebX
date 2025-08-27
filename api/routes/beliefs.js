/**
 * Beliefs Routes
 * Handles querying of TMR-based causation beliefs
 * Enhanced with caching, validation, and monitoring
 */

const express = require("express");
const { query, validationResult } = require("express-validator");
const { StatusCodes } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

// Core dependencies
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

// Rate limiting for query operations
const queryLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 query requests per windowMs
  message: {
    status: "error",
    message: "Too many belief query requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Simple in-memory cache
const cache = new Map();
const CACHE_TTL = 300; // 5 minutes
const CACHE_PREFIX = "beliefs:";

/**
 * Cache utilities
 */
const cacheUtils = {
  generateKey(route, params = {}) {
    const sortedParams = Object.keys(params)
      .sort()
      .map((key) => `${key}:${params[key]}`)
      .join(",");
    return `${CACHE_PREFIX}${route}${sortedParams ? `:${sortedParams}` : ""}`;
  },

  get(key) {
    const cached = cache.get(key);
    if (!cached) return null;

    if (Date.now() > cached.expiry) {
      cache.delete(key);
      return null;
    }

    logger.debug("Cache hit for beliefs", { key });
    return cached.data;
  },

  set(key, data, ttlSeconds = CACHE_TTL) {
    const expiry = Date.now() + ttlSeconds * 1000;
    cache.set(key, { data, expiry });

    // Simple cache size management
    if (cache.size > 100) {
      const firstKey = cache.keys().next().value;
      cache.delete(firstKey);
    }

    logger.debug("Cache set for beliefs", { key, ttlSeconds });
  },

  clear(pattern = "") {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Beliefs cache cleared", {
      pattern,
      deletedCount: keysToDelete.length,
    });
  },

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
    };
  },
};

/**
 * Validation rules for belief queries
 */
const queryValidationRules = [
  query("limit")
    .optional()
    .isInt({ min: 1, max: 1000 })
    .withMessage("Limit must be an integer between 1 and 1000"),

  query("offset")
    .optional()
    .isInt({ min: 0 })
    .withMessage("Offset must be a non-negative integer"),

  query("format")
    .optional()
    .isIn(["json", "rdf", "turtle"])
    .withMessage("Format must be one of: json, rdf, turtle"),

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),

  query("filter")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 100 })
    .matches(/^[a-zA-Z0-9_\s\-\.]*$/)
    .withMessage(
      "Filter must be alphanumeric with spaces, underscores, hyphens, and periods only"
    ),
];

/**
 * Enhanced endpoint to retrieve causation beliefs from named graphs
 */
router.post("/get", [queryLimiter, queryValidationRules], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Beliefs query validation failed", {
        requestId,
        errors: errors.array(),
        query: req.query,
        ip: req.ip,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
        requestId,
      });
    }

    // Extract query parameters
    const {
      limit = 100,
      offset = 0,
      format = "json",
      include_metadata = false,
      filter = "",
    } = req.query;

    const queryParams = { limit, offset, format, include_metadata, filter };

    logger.info("Retrieving causation beliefs", {
      requestId,
      queryParams,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey("/get", queryParams);
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      logger.info("Returning cached beliefs result", {
        requestId,
        responseTime,
        cached: true,
      });

      return res.status(StatusCodes.OK).json({
        ...cachedResult,
        cached: true,
        requestId,
        responseTime,
      });
    }

    // Fetch causation beliefs from the data store
    const {
      status = 500,
      bindings,
      head_vars,
    } = await utils.get_named_subject_in_named_graphs_from_object(
      "beliefs",
      "vocab:CausationBelief"
    );

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve beliefs from triple store"
      );
    }

    let result = [];
    if (bindings && bindings.length > 0) {
      // Process the bindings into a readable format
      result = await auxFuncts.get_rdf_atom_as_array(bindings);

      // Apply filtering if requested
      if (filter && filter.trim() !== "") {
        const filterLower = filter.toLowerCase().trim();
        result = result.filter((item) => {
          const searchableText = [
            item.label || "",
            item.id || "",
            item.uri || "",
          ]
            .join(" ")
            .toLowerCase();

          return searchableText.includes(filterLower);
        });
      }

      // Apply pagination
      const totalCount = result.length;
      const paginatedResult = result.slice(
        parseInt(offset),
        parseInt(offset) + parseInt(limit)
      );

      // Build response
      const responseData = {
        status: "success",
        data: paginatedResult,
        metadata: {
          total_count: totalCount,
          returned_count: paginatedResult.length,
          limit: parseInt(limit),
          offset: parseInt(offset),
          has_more: parseInt(offset) + parseInt(limit) < totalCount,
        },
      };

      // Include additional metadata if requested
      if (include_metadata === "true" || include_metadata === true) {
        responseData.metadata.query_params = queryParams;
        responseData.metadata.sparql_status = status;
        responseData.metadata.data_source = "beliefs";
        responseData.metadata.object_type = "vocab:CausationBelief";
      }

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_TTL);

      const responseTime = Date.now() - startTime;

      logger.info("Causation beliefs retrieved successfully", {
        requestId,
        totalCount,
        returnedCount: paginatedResult.length,
        responseTime,
        cached: false,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        ...responseData,
        cached: false,
        requestId,
        responseTime,
      });
    } else {
      // No beliefs found
      const responseData = {
        status: "success",
        data: [],
        metadata: {
          total_count: 0,
          returned_count: 0,
          limit: parseInt(limit),
          offset: parseInt(offset),
          has_more: false,
        },
      };

      // Cache empty result for shorter time
      cacheUtils.set(cacheKey, responseData, 60); // 1 minute for empty results

      const responseTime = Date.now() - startTime;

      logger.info("No causation beliefs found", {
        requestId,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        ...responseData,
        cached: false,
        requestId,
        responseTime,
      });
    }
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Failed to retrieve causation beliefs", {
      requestId,
      error: error.message,
      stack: error.stack,
      responseTime,
      ip: req.ip,
    });

    if (error instanceof ErrorHandler) {
      return res.status(error.statusCode).json({
        status: "error",
        message: error.message,
        requestId,
        responseTime,
      });
    }

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "An unexpected error occurred while retrieving beliefs",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Health check endpoint
 */
router.get("/health", (req, res) => {
  const cacheStats = cacheUtils.getStats();

  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "causation-beliefs-query",
    timestamp: new Date().toISOString(),
    version: "2.0.0",
    cache: cacheStats,
  });
});

/**
 * Service information endpoint
 */
router.get("/info", (req, res) => {
  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      service: "causation-beliefs-query",
      description: "Query service for TMR-based causation beliefs",
      endpoints: [
        {
          path: "/get",
          method: "POST",
          description:
            "Retrieve all causation beliefs with pagination and filtering",
          query_parameters: [
            "limit (1-1000)",
            "offset (>=0)",
            "format (json, rdf, turtle)",
            "include_metadata (boolean)",
            "filter (string)",
          ],
        },
        {
          path: "/health",
          method: "GET",
          description: "Health check endpoint",
        },
        {
          path: "/info",
          method: "GET",
          description: "Service information and API documentation",
        },
        {
          path: "/cache/clear",
          method: "POST",
          description: "Clear query cache",
        },
        {
          path: "/cache/stats",
          method: "GET",
          description: "Get cache statistics",
        },
      ],
      data_source: "beliefs triple store",
      object_type: "vocab:CausationBelief",
      caching: {
        enabled: true,
        ttl_seconds: CACHE_TTL,
        max_entries: 100,
      },
    },
  });
});

/**
 * Cache management endpoints
 */
router.post("/cache/clear", (req, res) => {
  try {
    const { pattern = "" } = req.body;
    const statsBefore = cacheUtils.getStats();

    cacheUtils.clear(pattern);

    const statsAfter = cacheUtils.getStats();
    const clearedCount = statsBefore.totalEntries - statsAfter.totalEntries;

    logger.info("Beliefs cache cleared via API", {
      pattern,
      clearedCount,
      ip: req.ip,
    });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: "Cache cleared successfully",
      data: {
        pattern: pattern || "all",
        entries_cleared: clearedCount,
        entries_remaining: statsAfter.totalEntries,
      },
    });
  } catch (error) {
    logger.error("Failed to clear beliefs cache", {
      error: error.message,
      ip: req.ip,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to clear cache",
    });
  }
});

/**
 * Get cache statistics
 */
router.get("/cache/stats", (req, res) => {
  try {
    const stats = cacheUtils.getStats();

    res.status(StatusCodes.OK).json({
      status: "success",
      data: {
        cache_statistics: stats,
        cache_efficiency:
          stats.totalEntries > 0
            ? Math.round((stats.activeEntries / stats.totalEntries) * 100)
            : 0,
      },
    });
  } catch (error) {
    logger.error("Failed to get beliefs cache stats", {
      error: error.message,
      ip: req.ip,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to retrieve cache statistics",
    });
  }
});

module.exports = router;
module.exports.cacheUtils = cacheUtils;
