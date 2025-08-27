/**
 * Transition Routes
 * Handles CRUD operations for TMR-based transitions, situations, and properties
 * Enhanced with validation, caching, and monitoring
 */

const express = require("express");
const { body, param, validationResult } = require("express-validator");
const { StatusCodes } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

// Core dependencies
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

// Rate limiting for different operations
const readLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 150, // limit each IP to 150 read requests per windowMs
  message: {
    status: "error",
    message: "Too many read requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const writeLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 50, // limit each IP to 50 write requests per windowMs
  message: {
    status: "error",
    message: "Too many write requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Enhanced caching system
const cache = new Map();
const CACHE_TTL = 300; // 5 minutes for transitions
const CACHE_PREFIX = "transition:";

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

    logger.debug("Cache hit for transition", { key });
    return cached.data;
  },

  set(key, data, ttlSeconds = CACHE_TTL) {
    const expiry = Date.now() + ttlSeconds * 1000;
    cache.set(key, { data, expiry });

    // Cache size management
    if (cache.size > 150) {
      const firstKey = cache.keys().next().value;
      cache.delete(firstKey);
    }

    logger.debug("Cache set for transition", { key, ttlSeconds });
  },

  invalidatePattern(pattern) {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Transition cache invalidated", {
      pattern,
      deletedCount: keysToDelete.length,
    });
  },

  clear() {
    cache.clear();
    logger.info("Transition cache cleared completely");
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
 * Validation rules for transitions
 */
const transitionValidationRules = [
  body("label")
    .notEmpty()
    .withMessage("Transition label is required")
    .isString()
    .withMessage("Label must be a string")
    .isLength({ min: 2, max: 200 })
    .withMessage("Label must be between 2 and 200 characters"),

  body("type")
    .notEmpty()
    .withMessage("Transition type is required")
    .isIn(["transition", "situation", "property"])
    .withMessage("Type must be one of: transition, situation, property"),

  body("description")
    .optional()
    .isString()
    .withMessage("Description must be a string")
    .isLength({ max: 1000 })
    .withMessage("Description must be max 1000 characters"),

  body("properties")
    .optional()
    .isObject()
    .withMessage("Properties must be an object"),

  body("properties.priority")
    .optional()
    .isInt({ min: 1, max: 10 })
    .withMessage("Priority must be an integer between 1 and 10"),

  body("properties.temporal")
    .optional()
    .isObject()
    .withMessage("Temporal properties must be an object"),

  body("properties.temporal.duration")
    .optional()
    .isString()
    .matches(/^P(\d+Y)?(\d+M)?(\d+D)?(T(\d+H)?(\d+M)?(\d+S)?)?$/)
    .withMessage("Duration must be in ISO 8601 duration format"),

  body("properties.conditions")
    .optional()
    .isArray()
    .withMessage("Conditions must be an array"),

  body("properties.conditions.*")
    .isObject()
    .withMessage("Each condition must be an object"),

  body("metadata")
    .optional()
    .isObject()
    .withMessage("Metadata must be an object"),

  body("metadata.source")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 200 })
    .withMessage("Source must be a string with max 200 characters"),

  body("metadata.evidence_level")
    .optional()
    .isIn(["high", "moderate", "low", "very_low"])
    .withMessage(
      "Evidence level must be one of: high, moderate, low, very_low"
    ),

  body("relationships")
    .optional()
    .isArray()
    .withMessage("Relationships must be an array"),

  body("relationships.*")
    .isObject()
    .withMessage("Each relationship must be an object"),

  body("relationships.*.target")
    .isString()
    .trim()
    .isLength({ min: 1 })
    .withMessage("Relationship target is required"),

  body("relationships.*.type")
    .isIn(["precedes", "follows", "enables", "conflicts", "requires"])
    .withMessage(
      "Relationship type must be one of: precedes, follows, enables, conflicts, requires"
    ),
];

const transitionIdValidation = [
  param("id")
    .notEmpty()
    .withMessage("Transition ID is required")
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .matches(/^[a-zA-Z0-9_\-\.]+$/)
    .withMessage(
      "ID must be alphanumeric with underscores, hyphens, and periods only"
    ),
];

/**
 * Enhanced endpoint to create a new transition/situation/property
 */
router.post(
  "/",
  [writeLimiter, transitionValidationRules],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Transition creation validation failed", {
          requestId,
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

      const {
        label,
        type,
        description,
        properties = {},
        metadata = {},
        relationships = [],
      } = req.body;

      // Generate unique transition ID
      const transitionId = `${type}_${Date.now()}_${Math.random()
        .toString(36)
        .substr(2, 5)}`;

      logger.info("Creating transition/situation/property", {
        requestId,
        transitionId,
        type,
        label,
        relationshipsCount: relationships.length,
        ip: req.ip,
      });

      // Build transition data structure
      const transitionData = {
        id: transitionId,
        label,
        type,
        description,
        properties: {
          ...properties,
          priority: properties.priority || 5,
        },
        relationships,
        metadata: {
          ...metadata,
          created_at: new Date().toISOString(),
          created_by: req.ip,
          version: "1.0.0",
        },
      };

      // Validate relationships if provided
      if (relationships.length > 0) {
        for (const relationship of relationships) {
          if (!relationship.target || !relationship.type) {
            return res.status(StatusCodes.BAD_REQUEST).json({
              status: "error",
              message: "Each relationship must have a target and type",
              requestId,
            });
          }
        }
      }

      // Build RDF nanopublication for the transition
      const nanopubData = await auxFuncts.buildTransitionNanopub(
        transitionData
      );

      // Store transition in triple store
      const { status, result } = await utils.store_transition(nanopubData);

      if (status >= 400) {
        throw new ErrorHandler(
          status,
          "Failed to store transition in triple store"
        );
      }

      // Invalidate related cache entries
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");
      cacheUtils.invalidatePattern(type);

      const responseTime = Date.now() - startTime;

      logger.info("Transition created successfully", {
        requestId,
        transitionId,
        type,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.CREATED).json({
        status: "success",
        message: `${type} created successfully`,
        data: {
          id: transitionId,
          type,
          label,
          metadata: transitionData.metadata,
          nanopub_uri: result.nanopub_uri,
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to create transition", {
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
        message: "An unexpected error occurred while creating transition",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to get a specific transition/situation/property
 */
router.get("/:id", [readLimiter, transitionIdValidation], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Transition get validation failed", {
        requestId,
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

    const { id } = req.params;
    const { include_relationships = true } = req.query;

    logger.info("Retrieving transition", {
      requestId,
      transitionId: id,
      include_relationships,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey(`/get/${id}`, {
      include_relationships,
    });
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      logger.info("Returning cached transition", {
        requestId,
        transitionId: id,
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

    // Retrieve transition from triple store
    const { status, transition } = await utils.get_transition_by_id(id);

    if (status === 404) {
      logger.warn("Transition not found", {
        requestId,
        transitionId: id,
        ip: req.ip,
      });

      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Transition not found",
        requestId,
      });
    }

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve transition from triple store"
      );
    }

    // Process transition data
    let processedTransition = transition;

    if (include_relationships === "true" || include_relationships === true) {
      // Get related transitions/situations/properties
      const relatedItems = await auxFuncts.getRelatedTransitions(id);
      processedTransition = {
        ...transition,
        related_items: relatedItems,
      };
    }

    const responseData = {
      status: "success",
      data: processedTransition,
    };

    // Cache the result
    cacheUtils.set(cacheKey, responseData, CACHE_TTL);

    const responseTime = Date.now() - startTime;

    logger.info("Transition retrieved successfully", {
      requestId,
      transitionId: id,
      type: transition.type,
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

    logger.error("Failed to retrieve transition", {
      requestId,
      transitionId: req.params.id,
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
      message: "An unexpected error occurred while retrieving transition",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced endpoint to update a transition/situation/property
 */
router.put(
  "/:id",
  [writeLimiter, transitionIdValidation, transitionValidationRules],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Transition update validation failed", {
          requestId,
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

      const { id } = req.params;
      const {
        label,
        type,
        description,
        properties = {},
        metadata = {},
        relationships = [],
      } = req.body;

      logger.info("Updating transition", {
        requestId,
        transitionId: id,
        type,
        label,
        ip: req.ip,
      });

      // Check if transition exists
      const { status: existsStatus } = await utils.get_transition_by_id(id);

      if (existsStatus === 404) {
        logger.warn("Cannot update non-existent transition", {
          requestId,
          transitionId: id,
          ip: req.ip,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Transition not found",
          requestId,
        });
      }

      // Build updated transition data
      const updatedTransitionData = {
        id,
        label,
        type,
        description,
        properties,
        relationships,
        metadata: {
          ...metadata,
          updated_at: new Date().toISOString(),
          updated_by: req.ip,
        },
      };

      // Build updated RDF nanopublication
      const nanopubData = await auxFuncts.buildTransitionNanopub(
        updatedTransitionData
      );

      // Update transition in triple store
      const { status, result } = await utils.update_transition(id, nanopubData);

      if (status >= 400) {
        throw new ErrorHandler(
          status,
          "Failed to update transition in triple store"
        );
      }

      // Invalidate cache entries
      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");
      cacheUtils.invalidatePattern(type);

      const responseTime = Date.now() - startTime;

      logger.info("Transition updated successfully", {
        requestId,
        transitionId: id,
        type,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `${type} updated successfully`,
        data: {
          id,
          type,
          label,
          metadata: updatedTransitionData.metadata,
          nanopub_uri: result.nanopub_uri,
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to update transition", {
        requestId,
        transitionId: req.params.id,
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
        message: "An unexpected error occurred while updating transition",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to delete a transition/situation/property
 */
router.delete(
  "/:id",
  [writeLimiter, transitionIdValidation],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Transition deletion validation failed", {
          requestId,
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

      const { id } = req.params;

      logger.info("Deleting transition", {
        requestId,
        transitionId: id,
        ip: req.ip,
      });

      // Check if transition exists and get its type
      const { status: existsStatus, transition } =
        await utils.get_transition_by_id(id);

      if (existsStatus === 404) {
        logger.warn("Cannot delete non-existent transition", {
          requestId,
          transitionId: id,
          ip: req.ip,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Transition not found",
          requestId,
        });
      }

      const transitionType = transition.type;

      // Delete transition from triple store
      const { status } = await utils.delete_transition(id);

      if (status >= 400) {
        throw new ErrorHandler(
          status,
          "Failed to delete transition from triple store"
        );
      }

      // Invalidate cache entries
      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");
      cacheUtils.invalidatePattern(transitionType);

      const responseTime = Date.now() - startTime;

      logger.info("Transition deleted successfully", {
        requestId,
        transitionId: id,
        type: transitionType,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `${transitionType} deleted successfully`,
        data: {
          id,
          type: transitionType,
          deleted_at: new Date().toISOString(),
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to delete transition", {
        requestId,
        transitionId: req.params.id,
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
        message: "An unexpected error occurred while deleting transition",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Health check endpoint
 */
router.get("/health", (req, res) => {
  const cacheStats = cacheUtils.getStats();

  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "transitions-situations-properties",
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
      service: "transitions-situations-properties",
      description:
        "CRUD service for TMR-based transitions, situations, and properties",
      endpoints: [
        {
          path: "/",
          method: "POST",
          description: "Create a new transition, situation, or property",
        },
        {
          path: "/:id",
          method: "GET",
          description: "Retrieve a specific transition/situation/property",
        },
        {
          path: "/:id",
          method: "PUT",
          description: "Update a transition/situation/property",
        },
        {
          path: "/:id",
          method: "DELETE",
          description: "Delete a transition/situation/property",
        },
        {
          path: "/health",
          method: "GET",
          description: "Health check endpoint",
        },
        {
          path: "/info",
          method: "GET",
          description: "Service information",
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
      data_storage: "RDF nanopublications in triple store",
      caching: {
        enabled: true,
        ttl_seconds: CACHE_TTL,
      },
    },
  });
});

/**
 * Cache management endpoints
 */
router.post("/cache/clear", (req, res) => {
  try {
    cacheUtils.clear();

    logger.info("Transition cache cleared via API", { ip: req.ip });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: "Cache cleared successfully",
    });
  } catch (error) {
    logger.error("Failed to clear transition cache", {
      error: error.message,
      ip: req.ip,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to clear cache",
    });
  }
});

module.exports = router;
module.exports.cacheUtils = cacheUtils;
