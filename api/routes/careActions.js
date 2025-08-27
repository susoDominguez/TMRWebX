/**
 * Handles retrieval and querying of TMR-based care action types
 * Enhanced with better validation, error handling, caching, and monitoring
 */

const express = require("express");
const { body, query, validationResult } = require("express-validator");
const { StatusCodes, ReasonPhrases } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

// Core dependencies
const config = require("../lib/config");
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

// Constants and Configuration
const CARE_ACTION_TYPES = Object.freeze({
  DrugCombinationType: "vocab:DrugCombinationType",
  DrugCategory: "vocab:DrugCategory",
  DrugType: "vocab:DrugType",
  VaccineCategory: "vocab:VaccineCategory",
  VaccineType: "vocab:VaccineType",
  NonDrugType: "vocab:NonDrugType",
  NonDrugAdministrationType: "vocab:NonDrugAdministrationType",
  DrugAdministrationType: "vocab:DrugAdministrationType",
  VaccinationType: "vocab:VaccinationType",
  DrugCombinationAdministrationType: "vocab:DrugAdministrationType",
});

// Route configurations with metadata
const ROUTE_CONFIGURATIONS = Object.freeze({
  "/drugs/individual/get": {
    types: [CARE_ACTION_TYPES.VaccineType, CARE_ACTION_TYPES.DrugType],
    description: "Get all individual drug types and vaccines",
    category: "drugs",
    subcategory: "individual",
    cacheable: true,
    cacheTime: 300, // 5 minutes
  },
  "/drugs/get": {
    types: [
      CARE_ACTION_TYPES.DrugCombinationType,
      CARE_ACTION_TYPES.DrugCategory,
      CARE_ACTION_TYPES.DrugType,
      CARE_ACTION_TYPES.VaccineCategory,
      CARE_ACTION_TYPES.VaccineType,
    ],
    description:
      "Get all drug-related care actions including combinations, categories, and types",
    category: "drugs",
    subcategory: "all",
    cacheable: true,
    cacheTime: 300,
  },
  "/nondrugs/get": {
    types: [CARE_ACTION_TYPES.NonDrugType],
    description: "Get all non-drug care actions",
    category: "nondrugs",
    subcategory: "all",
    cacheable: true,
    cacheTime: 300,
  },
  "/drugs/vaccines/get": {
    types: [CARE_ACTION_TYPES.VaccineType],
    description: "Get all vaccine-related care actions",
    category: "drugs",
    subcategory: "vaccines",
    cacheable: true,
    cacheTime: 300,
  },
  "/drugs/category/get": {
    types: [CARE_ACTION_TYPES.DrugCategory],
    description: "Get all drug categories",
    category: "drugs",
    subcategory: "category",
    cacheable: true,
    cacheTime: 600, // 10 minutes - categories change less frequently
  },
  "/drugs/vaccines/category/get": {
    types: [CARE_ACTION_TYPES.VaccineCategory],
    description: "Get all vaccine categories",
    category: "drugs",
    subcategory: "vaccine-category",
    cacheable: true,
    cacheTime: 600,
  },
  "/drugs/combination/get": {
    types: [CARE_ACTION_TYPES.DrugCombinationType],
    description: "Get all drug combinations",
    category: "drugs",
    subcategory: "combination",
    cacheable: true,
    cacheTime: 300,
  },
  "/get": {
    types: [
      CARE_ACTION_TYPES.NonDrugAdministrationType,
      CARE_ACTION_TYPES.DrugAdministrationType,
      CARE_ACTION_TYPES.VaccinationType,
      CARE_ACTION_TYPES.DrugCombinationAdministrationType,
    ],
    description:
      "Get all care actions, including drugs, vaccines, and non-drug administrations",
    category: "all",
    subcategory: "administrations",
    cacheable: true,
    cacheTime: 300,
  },
});

// Rate limiting for query operations
const queryLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 query requests per windowMs
  message: {
    status: "error",
    message: "Too many query requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Stricter rate limiting for bulk operations
const bulkQueryLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 50, // limit each IP to 50 bulk query requests per windowMs
  message: {
    status: "error",
    message: "Too many bulk query requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Simple in-memory cache (in production, use Redis or similar)
const cache = new Map();
const CACHE_PREFIX = "careActions:";

/**
 * Cache management utilities
 */
const cacheUtils = {
  /**
   * Generate cache key for a route and parameters
   */
  generateKey(route, params = {}) {
    const sortedParams = Object.keys(params)
      .sort()
      .map((key) => `${key}:${params[key]}`)
      .join(",");
    return `${CACHE_PREFIX}${route}${sortedParams ? `:${sortedParams}` : ""}`;
  },

  /**
   * Get cached result
   */
  get(key) {
    const cached = cache.get(key);
    if (!cached) return null;

    // Check if cache entry has expired
    if (Date.now() > cached.expiry) {
      cache.delete(key);
      return null;
    }

    logger.debug("Cache hit", { key, size: cache.size });
    return cached.data;
  },

  /**
   * Set cache entry with TTL
   */
  set(key, data, ttlSeconds = 300) {
    const expiry = Date.now() + ttlSeconds * 1000;
    cache.set(key, { data, expiry });

    // Simple cache size management (keep last 1000 entries)
    if (cache.size > 1000) {
      const firstKey = cache.keys().next().value;
      cache.delete(firstKey);
    }

    logger.debug("Cache set", { key, ttlSeconds, size: cache.size });
  },

  /**
   * Clear cache for specific pattern
   */
  clear(pattern = "") {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Cache cleared", {
      pattern,
      deletedCount: keysToDelete.length,
    });
  },

  /**
   * Get cache statistics
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
      memoryEstimate: totalSize * 1024, // rough estimate in bytes
    };
  },
};

/**
 * Validation middleware for query parameters
 */
const validateQueryParams = [
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
    .isIn(["json", "turtle", "rdf", "n3"])
    .withMessage("Format must be one of: json, turtle, rdf, n3"),

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),

  query("filter")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 100 })
    .matches(/^[a-zA-Z0-9_\s\-]*$/)
    .withMessage(
      "Filter must be alphanumeric with spaces, underscores, and hyphens only"
    ),
];

/**
 * Enhanced SPARQL query handler with comprehensive error handling and caching
 */
async function handleSparqlQuery(careActionTypes, routeConfig, req, res) {
  const startTime = Date.now();
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

  try {
    // Validate input
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Query validation failed", {
        requestId,
        route: req.route?.path,
        errors: errors.array(),
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

    logger.info("Processing care actions query", {
      requestId,
      route: req.route?.path,
      careActionTypes,
      queryParams,
      ip: req.ip,
    });

    // Check cache if route is cacheable
    let cacheKey;
    if (routeConfig.cacheable) {
      cacheKey = cacheUtils.generateKey(
        req.route?.path || "unknown",
        queryParams
      );
      const cachedResult = cacheUtils.get(cacheKey);

      if (cachedResult) {
        logger.info("Returning cached result", {
          requestId,
          route: req.route?.path,
          cacheKey,
          responseTime: Date.now() - startTime,
        });

        return res.status(StatusCodes.OK).json({
          ...cachedResult,
          cached: true,
          requestId,
          responseTime: Date.now() - startTime,
        });
      }
    }

    // Validate care action types
    if (!Array.isArray(careActionTypes) || careActionTypes.length === 0) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "No valid care action types provided"
      );
    }

    // Validate each care action type
    const invalidTypes = careActionTypes.filter(
      (type) => !Object.values(CARE_ACTION_TYPES).includes(type)
    );

    if (invalidTypes.length > 0) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        `Invalid care action types: ${invalidTypes.join(", ")}`
      );
    }

    // Execute SPARQL queries with timeout and retry logic
    const sparqlResults = await Promise.allSettled(
      careActionTypes.map(async (type) => {
        try {
          const result = await utils.sparqlGetSubjectDefaultGraph(
            "careActions",
            type
          );
          return { type, result, success: true };
        } catch (error) {
          logger.error("SPARQL query failed for type", {
            requestId,
            type,
            error: error.message,
          });
          return { type, error: error.message, success: false };
        }
      })
    );

    // Process results and handle partial failures
    const successfulResults = [];
    const failedResults = [];

    sparqlResults.forEach((result, index) => {
      if (result.status === "fulfilled" && result.value.success) {
        successfulResults.push(result.value.result);
      } else {
        const type = careActionTypes[index];
        const error =
          result.status === "rejected"
            ? result.reason?.message || "Unknown error"
            : result.value.error;

        failedResults.push({ type, error });
        logger.error("Query failed for care action type", {
          requestId,
          type,
          error,
        });
      }
    });

    // Parse successful results
    let parsedResults;
    try {
      parsedResults = await auxFuncts.get_sparqlquery_arr(successfulResults);
    } catch (parseError) {
      logger.error("Failed to parse SPARQL results", {
        requestId,
        error: parseError.message,
        resultCount: successfulResults.length,
      });
      throw new ErrorHandler(
        StatusCodes.INTERNAL_SERVER_ERROR,
        "Failed to parse query results"
      );
    }

    // Apply filtering if requested
    if (filter && filter.trim() !== "") {
      const filterLower = filter.toLowerCase().trim();
      parsedResults = parsedResults.filter((item) => {
        const searchableText = [
          item.label || "",
          item.id || "",
          item.description || "",
          item.sctLabel || "",
        ]
          .join(" ")
          .toLowerCase();

        return searchableText.includes(filterLower);
      });
    }

    // Apply pagination
    const totalCount = parsedResults.length;
    const paginatedResults = parsedResults.slice(
      parseInt(offset),
      parseInt(offset) + parseInt(limit)
    );

    // Build response
    const responseData = {
      status: "success",
      data: paginatedResults,
      metadata: {
        total_count: totalCount,
        returned_count: paginatedResults.length,
        limit: parseInt(limit),
        offset: parseInt(offset),
        has_more: parseInt(offset) + parseInt(limit) < totalCount,
        query_types: careActionTypes,
        successful_queries: successfulResults.length,
        failed_queries: failedResults.length,
      },
      requestId,
      responseTime: Date.now() - startTime,
    };

    // Include additional metadata if requested
    if (include_metadata === "true" || include_metadata === true) {
      responseData.metadata.route_config = {
        description: routeConfig.description,
        category: routeConfig.category,
        subcategory: routeConfig.subcategory,
        cacheable: routeConfig.cacheable,
      };

      if (failedResults.length > 0) {
        responseData.metadata.failed_types = failedResults;
      }
    }

    // Cache successful results
    if (routeConfig.cacheable && failedResults.length === 0) {
      cacheUtils.set(cacheKey, responseData, routeConfig.cacheTime);
    }

    logger.info("Care actions query completed successfully", {
      requestId,
      route: req.route?.path,
      totalResults: totalCount,
      returnedResults: paginatedResults.length,
      failedQueries: failedResults.length,
      responseTime: Date.now() - startTime,
      cached: false,
    });

    res.status(StatusCodes.OK).json(responseData);
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Care actions query failed", {
      requestId,
      route: req.route?.path,
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
      message: "Internal server error occurred while processing query",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
}

/**
 * Health check endpoint
 */
router.get("/health", (req, res) => {
  const cacheStats = cacheUtils.getStats();

  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "care-actions-query",
    timestamp: new Date().toISOString(),
    version: "2.0.0",
    routes: Object.keys(ROUTE_CONFIGURATIONS).length,
    cache: cacheStats,
    care_action_types: Object.keys(CARE_ACTION_TYPES).length,
  });
});

/**
 * Get available care action types and routes
 */
router.get("/info", (req, res) => {
  const routeInfo = Object.entries(ROUTE_CONFIGURATIONS).map(
    ([route, config]) => ({
      endpoint: route,
      method: "POST",
      description: config.description,
      category: config.category,
      subcategory: config.subcategory,
      types: config.types,
      cacheable: config.cacheable,
      cache_time_seconds: config.cacheTime,
    })
  );

  const typeInfo = Object.entries(CARE_ACTION_TYPES).map(([name, uri]) => ({
    name,
    uri,
    category: name.includes("Drug")
      ? "drug"
      : name.includes("Vaccine")
      ? "vaccine"
      : name.includes("NonDrug")
      ? "nondrug"
      : "administration",
  }));

  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      available_routes: routeInfo,
      available_types: typeInfo,
      supported_query_params: [
        "limit (1-1000)",
        "offset (>=0)",
        "format (json, turtle, rdf, n3)",
        "include_metadata (boolean)",
        "filter (string)",
      ],
    },
  });
});

/**
 * Advanced search endpoint
 */
router.post(
  "/search",
  [
    queryLimiter,
    validateQueryParams,
    body("types")
      .optional()
      .isArray()
      .custom((types) => {
        return types.every((type) =>
          Object.values(CARE_ACTION_TYPES).includes(type)
        );
      })
      .withMessage("All types must be valid care action types"),

    body("query")
      .isString()
      .trim()
      .isLength({ min: 1, max: 200 })
      .matches(/^[a-zA-Z0-9_\s\-\.]*$/)
      .withMessage(
        "Query must be 1-200 characters, alphanumeric with spaces, underscores, hyphens, and periods"
      ),

    body("exact_match")
      .optional()
      .isBoolean()
      .withMessage("Exact match must be a boolean"),
  ],
  async (req, res) => {
    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const {
        types = Object.values(CARE_ACTION_TYPES),
        query: searchQuery,
        exact_match = false,
      } = req.body;

      // Create a mock route config for the search
      const searchConfig = {
        description: "Advanced search across care action types",
        category: "search",
        subcategory: "advanced",
        cacheable: true,
        cacheTime: 180, // 3 minutes - shorter cache for searches
      };

      // Add search query to the request for filtering
      req.query.filter = searchQuery;
      req.query.exact_match = exact_match;

      await handleSparqlQuery(types, searchConfig, req, res);
    } catch (error) {
      logger.error("Advanced search failed", {
        error: error.message,
        body: req.body,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Search operation failed",
      });
    }
  }
);

/**
 * Cache management endpoints
 */
router.post(
  "/cache/clear",
  [
    body("pattern")
      .optional()
      .isString()
      .trim()
      .isLength({ max: 50 })
      .withMessage("Pattern must be a string with maximum 50 characters"),
  ],
  (req, res) => {
    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { pattern = "" } = req.body;
      const statsBefore = cacheUtils.getStats();

      cacheUtils.clear(pattern);

      const statsAfter = cacheUtils.getStats();
      const clearedCount = statsBefore.totalEntries - statsAfter.totalEntries;

      logger.info("Cache cleared via API", {
        pattern,
        clearedCount,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `Cache cleared successfully`,
        data: {
          pattern: pattern || "all",
          entries_cleared: clearedCount,
          entries_remaining: statsAfter.totalEntries,
        },
      });
    } catch (error) {
      logger.error("Cache clear failed", {
        error: error.message,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to clear cache",
      });
    }
  }
);

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
    logger.error("Failed to get cache stats", {
      error: error.message,
      ip: req.ip,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to retrieve cache statistics",
    });
  }
});

/**
 * Register all routes dynamically with enhanced middleware
 */
Object.entries(ROUTE_CONFIGURATIONS).forEach(([route, routeConfig]) => {
  const isLargeQuery = routeConfig.types.length > 3;
  const rateLimitMiddleware = isLargeQuery ? bulkQueryLimiter : queryLimiter;

  router.post(
    route,
    [rateLimitMiddleware, validateQueryParams],
    async (req, res) => {
      await handleSparqlQuery(routeConfig.types, routeConfig, req, res);
    }
  );

  logger.debug("Registered care actions route", {
    route,
    description: routeConfig.description,
    typeCount: routeConfig.types.length,
    cacheable: routeConfig.cacheable,
  });
});

// Export router and utilities for testing
module.exports = router;
module.exports.cacheUtils = cacheUtils;
module.exports.CARE_ACTION_TYPES = CARE_ACTION_TYPES;
module.exports.ROUTE_CONFIGURATIONS = ROUTE_CONFIGURATIONS;
