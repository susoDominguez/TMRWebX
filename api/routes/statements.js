/**
 * Statements Routes
 * Handles querying of TMR-based clinical statements
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
    message: "Too many statement query requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const searchLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 50, // limit each IP to 50 search requests per windowMs
  message: {
    status: "error",
    message: "Too many search requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Enhanced caching system
const cache = new Map();
const CACHE_TTL = 300; // 5 minutes for statements query
const CACHE_PREFIX = "statements:";

/**
 * Cache utilities for statements queries
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

    logger.debug("Cache hit for statements", { key });
    return cached.data;
  },

  set(key, data, ttlSeconds = CACHE_TTL) {
    const expiry = Date.now() + ttlSeconds * 1000;
    cache.set(key, { data, expiry });

    // Cache size management
    if (cache.size > 200) {
      const firstKey = cache.keys().next().value;
      cache.delete(firstKey);
    }

    logger.debug("Cache set for statements", { key, ttlSeconds });
  },

  clear(pattern = "") {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Statements cache cleared", {
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
 * Validation rules for statement queries
 */
const queryValidationRules = [
  query("limit")
    .optional()
    .isInt({ min: 1, max: 500 })
    .withMessage("Limit must be an integer between 1 and 500"),

  query("offset")
    .optional()
    .isInt({ min: 0 })
    .withMessage("Offset must be a non-negative integer"),

  query("type")
    .optional()
    .isIn([
      "clinical_observation",
      "diagnosis",
      "treatment_plan",
      "outcome_measure",
      "risk_factor",
    ])
    .withMessage(
      "Type must be one of: clinical_observation, diagnosis, treatment_plan, outcome_measure, risk_factor"
    ),

  query("confidence_min")
    .optional()
    .isFloat({ min: 0, max: 1 })
    .withMessage("Confidence minimum must be a number between 0 and 1"),

  query("confidence_max")
    .optional()
    .isFloat({ min: 0, max: 1 })
    .withMessage("Confidence maximum must be a number between 0 and 1"),

  query("format")
    .optional()
    .isIn(["json", "rdf", "turtle"])
    .withMessage("Format must be one of: json, rdf, turtle"),

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),

  query("patient_id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_\-]+$/)
    .withMessage(
      "Patient ID must be alphanumeric with underscores and hyphens only"
    ),

  query("clinician_id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_\-]+$/)
    .withMessage(
      "Clinician ID must be alphanumeric with underscores and hyphens only"
    ),

  query("date_from")
    .optional()
    .isISO8601()
    .withMessage("Date from must be a valid ISO 8601 date"),

  query("date_to")
    .optional()
    .isISO8601()
    .withMessage("Date to must be a valid ISO 8601 date"),

  query("tags")
    .optional()
    .isString()
    .withMessage("Tags must be a comma-separated string"),

  query("search")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 200 })
    .matches(/^[a-zA-Z0-9_\s\-\.]*$/)
    .withMessage(
      "Search must be alphanumeric with spaces, underscores, hyphens, and periods only"
    ),
];

/**
 * Enhanced endpoint to retrieve all clinical statements with advanced filtering
 */
router.get("/", [queryLimiter, queryValidationRules], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Statements query validation failed", {
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

    // Extract and validate query parameters
    const {
      limit = 50,
      offset = 0,
      type,
      confidence_min,
      confidence_max,
      format = "json",
      include_metadata = false,
      patient_id,
      clinician_id,
      date_from,
      date_to,
      tags,
      search,
    } = req.query;

    // Validate confidence range
    if (
      confidence_min &&
      confidence_max &&
      parseFloat(confidence_min) > parseFloat(confidence_max)
    ) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Confidence minimum cannot be greater than confidence maximum",
        requestId,
      });
    }

    // Validate date range
    if (date_from && date_to && new Date(date_from) > new Date(date_to)) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Date from cannot be after date to",
        requestId,
      });
    }

    const queryParams = {
      limit,
      offset,
      type,
      confidence_min,
      confidence_max,
      format,
      include_metadata,
      patient_id,
      clinician_id,
      date_from,
      date_to,
      tags,
      search,
    };

    logger.info("Retrieving clinical statements", {
      requestId,
      queryParams,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey("/get", queryParams);
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      logger.info("Returning cached statements result", {
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

    // Build SPARQL query filters
    const filters = [];

    if (type) {
      filters.push(`?statement vocab:statementType "${type}"`);
    }

    if (confidence_min) {
      filters.push(
        `?statement vocab:confidence ?conf . FILTER(?conf >= ${confidence_min})`
      );
    }

    if (confidence_max) {
      filters.push(
        `?statement vocab:confidence ?conf . FILTER(?conf <= ${confidence_max})`
      );
    }

    if (patient_id) {
      filters.push(`?statement vocab:patientId "${patient_id}"`);
    }

    if (clinician_id) {
      filters.push(`?statement vocab:clinicianId "${clinician_id}"`);
    }

    if (date_from) {
      filters.push(
        `?statement vocab:createdAt ?date . FILTER(?date >= "${date_from}"^^xsd:dateTime)`
      );
    }

    if (date_to) {
      filters.push(
        `?statement vocab:createdAt ?date . FILTER(?date <= "${date_to}"^^xsd:dateTime)`
      );
    }

    if (tags) {
      const tagList = tags
        .split(",")
        .map((tag) => tag.trim())
        .filter((tag) => tag.length > 0);
      if (tagList.length > 0) {
        const tagFilters = tagList.map(
          (tag) => `?statement vocab:hasTag "${tag}"`
        );
        filters.push(`(${tagFilters.join(" || ")})`);
      }
    }

    if (search) {
      filters.push(
        `?statement vocab:text ?text . FILTER(CONTAINS(LCASE(?text), "${search.toLowerCase()}"))`
      );
    }

    // Fetch statements from the data store
    const {
      status = 500,
      bindings,
      head_vars,
    } = await utils.get_named_subject_in_named_graphs_from_object_with_filters(
      "statements",
      "vocab:ClinicalStatement",
      filters
    );

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve statements from triple store"
      );
    }

    let result = [];
    if (bindings && bindings.length > 0) {
      // Process the bindings into a readable format
      result = await auxFuncts.get_rdf_atom_as_array(bindings);

      // Apply text search filtering if needed (fallback for complex searches)
      if (search && search.trim() !== "") {
        const searchLower = search.toLowerCase().trim();
        result = result.filter((item) => {
          const searchableText = [
            item.text || "",
            item.source || "",
            item.type || "",
            ...(item.tags || []),
          ]
            .join(" ")
            .toLowerCase();

          return searchableText.includes(searchLower);
        });
      }

      // Sort by confidence and creation date
      result.sort((a, b) => {
        const confDiff = (b.confidence || 0) - (a.confidence || 0);
        if (confDiff !== 0) return confDiff;

        const dateA = new Date(a.created_at || 0);
        const dateB = new Date(b.created_at || 0);
        return dateB - dateA;
      });

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
          filters_applied: Object.keys(queryParams).filter(
            (key) =>
              queryParams[key] !== undefined &&
              queryParams[key] !== null &&
              queryParams[key] !== ""
          ).length,
        },
      };

      // Include additional metadata if requested
      if (include_metadata === "true" || include_metadata === true) {
        responseData.metadata.query_params = queryParams;
        responseData.metadata.sparql_status = status;
        responseData.metadata.data_source = "statements";
        responseData.metadata.object_type = "vocab:ClinicalStatement";

        // Add statistics
        if (totalCount > 0) {
          const typeStats = {};
          const confidenceStats = { min: 1, max: 0, avg: 0 };
          let confidenceSum = 0;
          let confidenceCount = 0;

          result.forEach((statement) => {
            // Type statistics
            const type = statement.type || "unknown";
            typeStats[type] = (typeStats[type] || 0) + 1;

            // Confidence statistics
            if (
              statement.confidence !== undefined &&
              statement.confidence !== null
            ) {
              const conf = parseFloat(statement.confidence);
              confidenceStats.min = Math.min(confidenceStats.min, conf);
              confidenceStats.max = Math.max(confidenceStats.max, conf);
              confidenceSum += conf;
              confidenceCount++;
            }
          });

          if (confidenceCount > 0) {
            confidenceStats.avg =
              Math.round((confidenceSum / confidenceCount) * 100) / 100;
          }

          responseData.metadata.statistics = {
            type_distribution: typeStats,
            confidence_stats: confidenceStats,
          };
        }
      }

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_TTL);

      const responseTime = Date.now() - startTime;

      logger.info("Clinical statements retrieved successfully", {
        requestId,
        totalCount,
        returnedCount: paginatedResult.length,
        filtersApplied: responseData.metadata.filters_applied,
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
      // No statements found
      const responseData = {
        status: "success",
        data: [],
        metadata: {
          total_count: 0,
          returned_count: 0,
          limit: parseInt(limit),
          offset: parseInt(offset),
          has_more: false,
          filters_applied: Object.keys(queryParams).filter(
            (key) =>
              queryParams[key] !== undefined &&
              queryParams[key] !== null &&
              queryParams[key] !== ""
          ).length,
        },
      };

      // Cache empty result for shorter time
      cacheUtils.set(cacheKey, responseData, 60); // 1 minute for empty results

      const responseTime = Date.now() - startTime;

      logger.info("No clinical statements found", {
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

    logger.error("Failed to retrieve clinical statements", {
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
      message: "An unexpected error occurred while retrieving statements",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced endpoint to search statements by text content
 */
router.get(
  "/search",
  [searchLimiter, queryValidationRules],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
          requestId,
        });
      }

      const {
        search: searchTerm,
        limit = 20,
        offset = 0,
        type,
        confidence_min = 0,
        include_metadata = false,
      } = req.query;

      if (!searchTerm || searchTerm.trim() === "") {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Search term is required",
          requestId,
        });
      }

      const queryParams = {
        search: searchTerm,
        limit,
        offset,
        type,
        confidence_min,
        include_metadata,
      };

      logger.info("Searching clinical statements", {
        requestId,
        searchTerm,
        queryParams,
        ip: req.ip,
      });

      // Check cache
      const cacheKey = cacheUtils.generateKey("/search", queryParams);
      const cachedResult = cacheUtils.get(cacheKey);

      if (cachedResult) {
        const responseTime = Date.now() - startTime;
        return res.status(StatusCodes.OK).json({
          ...cachedResult,
          cached: true,
          requestId,
          responseTime,
        });
      }

      // Perform advanced search
      const searchResults = await auxFuncts.searchClinicalStatements({
        searchTerm: searchTerm.trim(),
        type,
        confidenceMin: parseFloat(confidence_min),
        limit: parseInt(limit),
        offset: parseInt(offset),
      });

      const responseData = {
        status: "success",
        data: searchResults.statements || [],
        metadata: {
          search_term: searchTerm,
          total_count: searchResults.totalCount || 0,
          returned_count: searchResults.statements?.length || 0,
          limit: parseInt(limit),
          offset: parseInt(offset),
          has_more:
            parseInt(offset) + parseInt(limit) <
            (searchResults.totalCount || 0),
          search_time_ms: searchResults.searchTime || 0,
        },
      };

      if (include_metadata === "true" || include_metadata === true) {
        responseData.metadata.search_details = {
          query_expansion: searchResults.queryExpansion || false,
          relevance_scoring: searchResults.relevanceScoring || false,
          search_method: searchResults.searchMethod || "text_match",
        };
      }

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_TTL);

      const responseTime = Date.now() - startTime;

      logger.info("Clinical statements search completed", {
        requestId,
        searchTerm,
        totalCount: searchResults.totalCount || 0,
        returnedCount: searchResults.statements?.length || 0,
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
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to search clinical statements", {
        requestId,
        searchTerm: req.query.search,
        error: error.message,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "An unexpected error occurred during search",
        requestId,
        responseTime,
      });
    }
  }
);

/**
 * Enhanced endpoint to get statements statistics
 */
router.get("/stats", [queryLimiter], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    logger.info("Retrieving statements statistics", {
      requestId,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey("/stats");
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      return res.status(StatusCodes.OK).json({
        ...cachedResult,
        cached: true,
        requestId,
        responseTime,
      });
    }

    // Get statistics from triple store
    const stats = await auxFuncts.getClinicalStatementsStatistics();

    const responseData = {
      status: "success",
      data: {
        overview: {
          total_statements: stats.totalStatements || 0,
          last_updated: stats.lastUpdated || new Date().toISOString(),
          data_source: "statements triple store",
        },
        by_type: stats.byType || {},
        by_confidence: stats.byConfidence || {},
        temporal_distribution: stats.temporalDistribution || {},
        top_tags: stats.topTags || [],
        quality_metrics: {
          average_confidence: stats.averageConfidence || 0,
          statements_with_metadata: stats.statementsWithMetadata || 0,
          statements_with_tags: stats.statementsWithTags || 0,
        },
      },
    };

    // Cache the result for longer (stats don't change frequently)
    cacheUtils.set(cacheKey, responseData, CACHE_TTL * 4); // 20 minutes

    const responseTime = Date.now() - startTime;

    logger.info("Statements statistics retrieved successfully", {
      requestId,
      totalStatements: stats.totalStatements || 0,
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
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Failed to retrieve statements statistics", {
      requestId,
      error: error.message,
      responseTime,
      ip: req.ip,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "An unexpected error occurred while retrieving statistics",
      requestId,
      responseTime,
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
    service: "clinical-statements-query",
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
      service: "clinical-statements-query",
      description: "Query and search service for TMR-based clinical statements",
      endpoints: [
        {
          path: "/",
          method: "GET",
          description:
            "Retrieve all clinical statements with advanced filtering",
          query_parameters: [
            "limit (1-500)",
            "offset (>=0)",
            "type (statement type)",
            "confidence_min (0-1)",
            "confidence_max (0-1)",
            "format (json, rdf, turtle)",
            "include_metadata (boolean)",
            "patient_id (string)",
            "clinician_id (string)",
            "date_from (ISO 8601 date)",
            "date_to (ISO 8601 date)",
            "tags (comma-separated)",
            "search (text search)",
          ],
        },
        {
          path: "/search",
          method: "GET",
          description: "Search statements by text content",
          query_parameters: [
            "search (required)",
            "limit (1-500)",
            "offset (>=0)",
            "type (statement type)",
            "confidence_min (0-1)",
            "include_metadata (boolean)",
          ],
        },
        {
          path: "/stats",
          method: "GET",
          description: "Get comprehensive statistics about clinical statements",
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
      ],
      supported_statement_types: [
        "clinical_observation",
        "diagnosis",
        "treatment_plan",
        "outcome_measure",
        "risk_factor",
      ],
      data_source: "statements triple store",
      object_type: "vocab:ClinicalStatement",
      features: [
        "Advanced filtering",
        "Text search",
        "Pagination",
        "Statistics",
        "Result caching",
        "Confidence-based sorting",
      ],
      caching: {
        enabled: true,
        ttl_seconds: CACHE_TTL,
        max_entries: 200,
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

    logger.info("Statements cache cleared via API", {
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
    logger.error("Failed to clear statements cache", {
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
    logger.error("Failed to get statements cache stats", {
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
