/**
 * Transitions Routes
 * Handles querying of TMR-based transitions, situations, and properties
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
    message: "Too many transition query requests, please try again later.",
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

const complexQueryLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 30, // limit each IP to 30 complex queries per windowMs
  message: {
    status: "error",
    message: "Too many complex query requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Enhanced caching system
const cache = new Map();
const CACHE_TTL = 300; // 5 minutes for transitions query
const CACHE_PREFIX = "transitions:";

/**
 * Cache utilities for transitions queries
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

    logger.debug("Cache hit for transitions", { key });
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

    logger.debug("Cache set for transitions", { key, ttlSeconds });
  },

  clear(pattern = "") {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Transitions cache cleared", {
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
 * Remove duplicate items by a chosen key and report duplicates
 * Returns { items: dedupedArray, duplicates: [{ key, count }], removedCount }
 */
function dedupeAndReport(arr, keyFn = (it) => it.id || it.uri || JSON.stringify(it)) {
  const seen = new Map();
  for (const it of arr) {
    const key = keyFn(it) || "__undefined__";
    seen.set(key, (seen.get(key) || 0) + 1);
  }

  const duplicates = [];
  for (const [key, count] of seen.entries()) {
    if (count > 1) duplicates.push({ key, count });
  }

  const items = [];
  const keep = new Set();
  for (const it of arr) {
    const key = keyFn(it) || "__undefined__";
    if (!keep.has(key)) {
      keep.add(key);
      items.push(it);
    }
  }

  return { items, duplicates, removedCount: arr.length - items.length };
}

/**
 * Validation rules for transition queries
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
    .isIn(["transition", "situation", "property"])
    .withMessage("Type must be one of: transition, situation, property"),

  query("priority_min")
    .optional()
    .isInt({ min: 1, max: 10 })
    .withMessage("Priority minimum must be an integer between 1 and 10"),

  query("priority_max")
    .optional()
    .isInt({ min: 1, max: 10 })
    .withMessage("Priority maximum must be an integer between 1 and 10"),

  query("format")
    .optional()
    .isIn(["json", "rdf", "turtle"])
    .withMessage("Format must be one of: json, rdf, turtle"),

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),

  query("include_relationships")
    .optional()
    .isBoolean()
    .withMessage("Include relationships must be a boolean"),

  query("evidence_level")
    .optional()
    .isIn(["high", "moderate", "low", "very_low"])
    .withMessage(
      "Evidence level must be one of: high, moderate, low, very_low"
    ),

  query("date_from")
    .optional()
    .isISO8601()
    .withMessage("Date from must be a valid ISO 8601 date"),

  query("date_to")
    .optional()
    .isISO8601()
    .withMessage("Date to must be a valid ISO 8601 date"),

  query("has_conditions")
    .optional()
    .isBoolean()
    .withMessage("Has conditions must be a boolean"),

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

const relationshipValidationRules = [
  query("relationship_type")
    .optional()
    .isIn(["precedes", "follows", "enables", "conflicts", "requires"])
    .withMessage(
      "Relationship type must be one of: precedes, follows, enables, conflicts, requires"
    ),

  query("target_id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_\-\.]+$/)
    .withMessage(
      "Target ID must be alphanumeric with underscores, hyphens, and periods only"
    ),
];

/**
 * Enhanced endpoint to retrieve all transitions/situations/properties with advanced filtering
 */
router.get("/", [queryLimiter, queryValidationRules], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Transitions query validation failed", {
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
      priority_min,
      priority_max,
      format = "json",
      include_metadata = false,
      include_relationships = false,
      evidence_level,
      date_from,
      date_to,
      has_conditions,
      search,
    } = req.query;

    // Validate priority range
    if (
      priority_min &&
      priority_max &&
      parseInt(priority_min) > parseInt(priority_max)
    ) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Priority minimum cannot be greater than priority maximum",
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
      priority_min,
      priority_max,
      format,
      include_metadata,
      include_relationships,
      evidence_level,
      date_from,
      date_to,
      has_conditions,
      search,
    };

    logger.info("Retrieving transitions/situations/properties", {
      requestId,
      queryParams,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey("/get", queryParams);
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      logger.info("Returning cached transitions result", {
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
      filters.push(`?transition vocab:transitionType "${type}"`);
    }

    if (priority_min) {
      filters.push(
        `?transition vocab:priority ?priority . FILTER(?priority >= ${priority_min})`
      );
    }

    if (priority_max) {
      filters.push(
        `?transition vocab:priority ?priority . FILTER(?priority <= ${priority_max})`
      );
    }

    if (evidence_level) {
      filters.push(`?transition vocab:evidenceLevel "${evidence_level}"`);
    }

    if (date_from) {
      filters.push(
        `?transition vocab:createdAt ?date . FILTER(?date >= "${date_from}"^^xsd:dateTime)`
      );
    }

    if (date_to) {
      filters.push(
        `?transition vocab:createdAt ?date . FILTER(?date <= "${date_to}"^^xsd:dateTime)`
      );
    }

    if (has_conditions === "true") {
      filters.push(`?transition vocab:hasCondition ?condition`);
    } else if (has_conditions === "false") {
      filters.push(`NOT EXISTS { ?transition vocab:hasCondition ?condition }`);
    }

    if (search) {
      filters.push(
        `(?transition vocab:label ?label . FILTER(CONTAINS(LCASE(?label), "${search.toLowerCase()}")))`
      );
    }

    // Fetch transitions from the data store
    const objectTypes = type
      ? [`vocab:${type.charAt(0).toUpperCase() + type.slice(1)}`]
      : ["vocab:Transition", "vocab:Situation", "vocab:Property"];

    const {
      status = 500,
      bindings,
      head_vars,
    } = await utils.get_named_subject_in_named_graphs_from_multiple_objects_with_filters(
      "transitions",
      objectTypes,
      filters
    );

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve transitions from triple store"
      );
    }

    let result = [];
    let duplicateInfo = { duplicates: [], removedCount: 0 };
    if (bindings && bindings.length > 0) {
      // Process the bindings into a readable format with error handling
      try {
        const parsed = await auxFuncts.get_rdf_atom_as_array(bindings);
        // Deduplicate parsed results and record duplicates
        const deduped = dedupeAndReport(parsed, (it) => it.id || it.uri);
        result = deduped.items;
        duplicateInfo = { duplicates: deduped.duplicates, removedCount: deduped.removedCount };
        if (duplicateInfo.removedCount > 0) {
          logger.warn("Duplicates found in transitions result", {
            requestId,
            removed: duplicateInfo.removedCount,
            duplicates: duplicateInfo.duplicates,
          });
        }
      } catch (err) {
        logger.error("Failed to parse SPARQL bindings for transitions", {
          requestId,
          error: err.message,
          stack: err.stack,
        });
        throw new ErrorHandler(
          StatusCodes.INTERNAL_SERVER_ERROR,
          "Failed to process transition results"
        );
      }

      // Apply text search filtering if needed (fallback for complex searches)
      if (search && search.trim() !== "") {
        const searchLower = search.toLowerCase().trim();
        result = result.filter((item) => {
          const searchableText = [
            item.label || "",
            item.description || "",
            item.type || "",
          ]
            .join(" ")
            .toLowerCase();

          return searchableText.includes(searchLower);
        });
      }

      // Sort by priority and creation date
      result.sort((a, b) => {
        const priorityDiff = (b.priority || 5) - (a.priority || 5);
        if (priorityDiff !== 0) return priorityDiff;

        const dateA = new Date(a.created_at || 0);
        const dateB = new Date(b.created_at || 0);
        return dateB - dateA;
      });

      // Include relationships if requested
      if (include_relationships === "true" || include_relationships === true) {
        for (let item of result) {
          try {
            const relationships = await auxFuncts.getTransitionRelationships(
              item.id
            );
            item.relationships = relationships;
          } catch (error) {
            logger.warn("Failed to load relationships", {
              requestId,
              transitionId: item.id,
              error: error.message,
            });
            item.relationships = [];
          }
        }
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
          filters_applied: Object.keys(queryParams).filter(
            (key) =>
              queryParams[key] !== undefined &&
              queryParams[key] !== null &&
              queryParams[key] !== ""
          ).length,
          duplicates: duplicateInfo.duplicates || [],
          duplicates_removed: duplicateInfo.removedCount || 0,
        },
      };

      // Include additional metadata if requested
      if (include_metadata === "true" || include_metadata === true) {
        responseData.metadata.query_params = queryParams;
        responseData.metadata.sparql_status = status;
        responseData.metadata.data_source = "transitions";
        responseData.metadata.object_types = objectTypes.map((type) =>
          type.replace("vocab:", "http://anonymous.org/vocab/")
        );

        // Add statistics
        if (totalCount > 0) {
          const typeStats = {};
          const priorityStats = { min: 10, max: 1, avg: 0 };
          const evidenceStats = {};
          let prioritySum = 0;
          let priorityCount = 0;

          result.forEach((item) => {
            // Type statistics
            const itemType = item.type || "unknown";
            typeStats[itemType] = (typeStats[itemType] || 0) + 1;

            // Priority statistics
            if (item.priority !== undefined && item.priority !== null) {
              const priority = parseInt(item.priority);
              priorityStats.min = Math.min(priorityStats.min, priority);
              priorityStats.max = Math.max(priorityStats.max, priority);
              prioritySum += priority;
              priorityCount++;
            }

            // Evidence level statistics
            const evidenceLevel = item.evidence_level || "unknown";
            evidenceStats[evidenceLevel] =
              (evidenceStats[evidenceLevel] || 0) + 1;
          });

          if (priorityCount > 0) {
            priorityStats.avg =
              Math.round((prioritySum / priorityCount) * 100) / 100;
          }

          responseData.metadata.statistics = {
            type_distribution: typeStats,
            priority_stats: priorityStats,
            evidence_distribution: evidenceStats,
          };
        }
      }

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_TTL);

      const responseTime = Date.now() - startTime;

      logger.info("Transitions retrieved successfully", {
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
      // No transitions found
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
          duplicates: duplicateInfo.duplicates || [],
          duplicates_removed: duplicateInfo.removedCount || 0,
        },
      };

      // Cache empty result for shorter time
      cacheUtils.set(cacheKey, responseData, 60); // 1 minute for empty results

      const responseTime = Date.now() - startTime;

      logger.info("No transitions found", {
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

    logger.error("Failed to retrieve transitions", {
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
      message: "An unexpected error occurred while retrieving transitions",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced endpoint to search transitions by relationships
 */
router.get(
  "/relationships",
  [complexQueryLimiter, relationshipValidationRules],
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
        relationship_type,
        target_id,
        limit = 50,
        offset = 0,
        include_metadata = false,
      } = req.query;

      if (!relationship_type && !target_id) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Either relationship_type or target_id must be provided",
          requestId,
        });
      }

      const queryParams = {
        relationship_type,
        target_id,
        limit,
        offset,
        include_metadata,
      };

      logger.info("Searching transitions by relationships", {
        requestId,
        queryParams,
        ip: req.ip,
      });

      // Check cache
      const cacheKey = cacheUtils.generateKey("/relationships", queryParams);
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

      // Perform relationship search
      const searchResults = await auxFuncts.searchTransitionsByRelationships({
        relationshipType: relationship_type,
        targetId: target_id,
        limit: parseInt(limit),
        offset: parseInt(offset),
      });

      const responseData = {
        status: "success",
        data: searchResults.transitions || [],
        metadata: {
          relationship_type,
          target_id,
          total_count: searchResults.totalCount || 0,
          returned_count: searchResults.transitions?.length || 0,
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
          relationship_search: true,
          search_method: searchResults.searchMethod || "relationship_graph",
        };
      }

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_TTL);

      const responseTime = Date.now() - startTime;

      logger.info("Relationship search completed", {
        requestId,
        relationship_type,
        target_id,
        totalCount: searchResults.totalCount || 0,
        returnedCount: searchResults.transitions?.length || 0,
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

      logger.error("Failed to search transitions by relationships", {
        requestId,
        error: error.message,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "An unexpected error occurred during relationship search",
        requestId,
        responseTime,
      });
    }
  }
);

/**
 * Enhanced endpoint to get transitions statistics
 */
router.get("/stats", [queryLimiter], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    logger.info("Retrieving transitions statistics", {
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
    const stats = await auxFuncts.getTransitionsStatistics();

    const responseData = {
      status: "success",
      data: {
        overview: {
          total_items: stats.totalItems || 0,
          transitions: stats.transitions || 0,
          situations: stats.situations || 0,
          properties: stats.properties || 0,
          last_updated: stats.lastUpdated || new Date().toISOString(),
          data_source: "transitions triple store",
        },
        by_type: stats.byType || {},
        by_priority: stats.byPriority || {},
        by_evidence_level: stats.byEvidenceLevel || {},
        relationships: {
          total_relationships: stats.totalRelationships || 0,
          by_type: stats.relationshipsByType || {},
          most_connected: stats.mostConnected || [],
        },
        temporal_distribution: stats.temporalDistribution || {},
        quality_metrics: {
          average_priority: stats.averagePriority || 0,
          items_with_conditions: stats.itemsWithConditions || 0,
          items_with_relationships: stats.itemsWithRelationships || 0,
          coverage_by_evidence: stats.coverageByEvidence || {},
        },
      },
    };

    // Cache the result for longer (stats don't change frequently)
    cacheUtils.set(cacheKey, responseData, CACHE_TTL * 4); // 20 minutes

    const responseTime = Date.now() - startTime;

    logger.info("Transitions statistics retrieved successfully", {
      requestId,
      totalItems: stats.totalItems || 0,
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

    logger.error("Failed to retrieve transitions statistics", {
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
    service: "transitions-situations-properties-query",
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
      service: "transitions-situations-properties-query",
      description:
        "Query and search service for TMR-based transitions, situations, and properties",
      endpoints: [
        {
          path: "/",
          method: "GET",
          description:
            "Retrieve all transitions/situations/properties with advanced filtering",
          query_parameters: [
            "limit (1-500)",
            "offset (>=0)",
            "type (transition, situation, property)",
            "priority_min (1-10)",
            "priority_max (1-10)",
            "format (json, rdf, turtle)",
            "include_metadata (boolean)",
            "include_relationships (boolean)",
            "evidence_level (high, moderate, low, very_low)",
            "date_from (ISO 8601 date)",
            "date_to (ISO 8601 date)",
            "has_conditions (boolean)",
            "search (text search)",
          ],
        },
        {
          path: "/relationships",
          method: "GET",
          description: "Search transitions by relationships",
          query_parameters: [
            "relationship_type (precedes, follows, enables, conflicts, requires)",
            "target_id (string)",
            "limit (1-500)",
            "offset (>=0)",
            "include_metadata (boolean)",
          ],
        },
        {
          path: "/stats",
          method: "GET",
          description:
            "Get comprehensive statistics about transitions/situations/properties",
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
      supported_types: ["transition", "situation", "property"],
      supported_relationships: [
        "precedes",
        "follows",
        "enables",
        "conflicts",
        "requires",
      ],
      data_source: "transitions triple store",
      object_types: [
        "http://anonymous.org/vocab/Transition",
        "http://anonymous.org/vocab/Situation",
        "http://anonymous.org/vocab/Property",
      ],
      features: [
        "Advanced filtering",
        "Relationship search",
        "Pagination",
        "Statistics",
        "Result caching",
        "Priority-based sorting",
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

    logger.info("Transitions cache cleared via API", {
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
    logger.error("Failed to clear transitions cache", {
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
    logger.error("Failed to get transitions cache stats", {
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
