/**
 * Statement Routes
 * Handles CRUD operations for TMR-based clinical statements
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
const CACHE_TTL = 300; // 5 minutes for statements
const CACHE_PREFIX = "statement:";

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

    logger.debug("Cache hit for statement", { key });
    return cached.data;
  },

  set(key, data, ttlSeconds = CACHE_TTL) {
    const expiry = Date.now() + ttlSeconds * 1000;
    cache.set(key, { data, expiry });

    // Cache size management
    if (cache.size > 100) {
      const firstKey = cache.keys().next().value;
      cache.delete(firstKey);
    }

    logger.debug("Cache set for statement", { key, ttlSeconds });
  },

  invalidatePattern(pattern) {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Statement cache invalidated", {
      pattern,
      deletedCount: keysToDelete.length,
    });
  },

  clear() {
    cache.clear();
    logger.info("Statement cache cleared completely");
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
 * Validation rules for clinical statements
 */
const statementValidationRules = [
  body("text")
    .notEmpty()
    .withMessage("Statement text is required")
    .isString()
    .withMessage("Statement text must be a string")
    .isLength({ min: 5, max: 2000 })
    .withMessage("Statement text must be between 5 and 2000 characters"),

  body("type")
    .notEmpty()
    .withMessage("Statement type is required")
    .isIn([
      "clinical_observation",
      "diagnosis",
      "treatment_plan",
      "outcome_measure",
      "risk_factor",
    ])
    .withMessage(
      "Statement type must be one of: clinical_observation, diagnosis, treatment_plan, outcome_measure, risk_factor"
    ),

  body("confidence")
    .optional()
    .isFloat({ min: 0, max: 1 })
    .withMessage("Confidence must be a number between 0 and 1"),

  body("source")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 200 })
    .withMessage("Source must be a string with max 200 characters"),

  body("metadata")
    .optional()
    .isObject()
    .withMessage("Metadata must be an object"),

  body("metadata.patient_id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_\-]+$/)
    .withMessage(
      "Patient ID must be alphanumeric with underscores and hyphens only"
    ),

  body("metadata.clinician_id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_\-]+$/)
    .withMessage(
      "Clinician ID must be alphanumeric with underscores and hyphens only"
    ),

  body("metadata.context")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 500 })
    .withMessage("Context must be a string with max 500 characters"),

  body("tags").optional().isArray().withMessage("Tags must be an array"),

  body("tags.*")
    .isString()
    .trim()
    .isLength({ min: 1, max: 50 })
    .matches(/^[a-zA-Z0-9_\-\s]+$/)
    .withMessage(
      "Each tag must be alphanumeric with underscores, hyphens, and spaces only"
    ),
];

const statementIdValidation = [
  param("id")
    .notEmpty()
    .withMessage("Statement ID is required")
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .matches(/^[a-zA-Z0-9_\-\.]+$/)
    .withMessage(
      "ID must be alphanumeric with underscores, hyphens, and periods only"
    ),
];

/**
 * Enhanced endpoint to create a new clinical statement
 */
router.post("/", [writeLimiter, statementValidationRules], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Statement creation validation failed", {
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
      text,
      type,
      confidence = 1.0,
      source,
      metadata = {},
      tags = [],
    } = req.body;

    // Generate unique statement ID
    const statementId = `statement_${Date.now()}_${Math.random()
      .toString(36)
      .substr(2, 5)}`;

    logger.info("Creating clinical statement", {
      requestId,
      statementId,
      type,
      textLength: text.length,
      confidence,
      tagsCount: tags.length,
      ip: req.ip,
    });

    // Build statement data structure
    const statementData = {
      id: statementId,
      text,
      type,
      confidence,
      source,
      tags,
      metadata: {
        ...metadata,
        created_at: new Date().toISOString(),
        created_by: req.ip,
        version: "1.0.0",
      },
    };

    // Build RDF nanopublication for the statement
    const nanopubData = await auxFuncts.buildStatementNanopub(statementData);

    // Store statement in triple store
    const { status, result } = await utils.store_clinical_statement(
      nanopubData
    );

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to store clinical statement in triple store"
      );
    }

    // Invalidate related cache entries
    cacheUtils.invalidatePattern("list");
    cacheUtils.invalidatePattern("search");

    const responseTime = Date.now() - startTime;

    logger.info("Clinical statement created successfully", {
      requestId,
      statementId,
      responseTime,
      ip: req.ip,
    });

    res.status(StatusCodes.CREATED).json({
      status: "success",
      message: "Clinical statement created successfully",
      data: {
        id: statementId,
        type,
        confidence,
        metadata: statementData.metadata,
        nanopub_uri: result.nanopub_uri,
      },
      requestId,
      responseTime,
    });
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Failed to create clinical statement", {
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
      message: "An unexpected error occurred while creating statement",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced endpoint to get a specific clinical statement
 */
router.get("/:id", [readLimiter, statementIdValidation], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Statement get validation failed", {
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

    logger.info("Retrieving clinical statement", {
      requestId,
      statementId: id,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey(`/get/${id}`);
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      logger.info("Returning cached statement", {
        requestId,
        statementId: id,
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

    // Retrieve statement from triple store
    const { status, statement } = await utils.get_clinical_statement_by_id(id);

    if (status === 404) {
      logger.warn("Clinical statement not found", {
        requestId,
        statementId: id,
        ip: req.ip,
      });

      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Clinical statement not found",
        requestId,
      });
    }

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve clinical statement from triple store"
      );
    }

    const responseData = {
      status: "success",
      data: statement,
    };

    // Cache the result
    cacheUtils.set(cacheKey, responseData, CACHE_TTL);

    const responseTime = Date.now() - startTime;

    logger.info("Clinical statement retrieved successfully", {
      requestId,
      statementId: id,
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

    logger.error("Failed to retrieve clinical statement", {
      requestId,
      statementId: req.params.id,
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
      message: "An unexpected error occurred while retrieving statement",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced endpoint to update a clinical statement
 */
router.put(
  "/:id",
  [writeLimiter, statementIdValidation, statementValidationRules],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Statement update validation failed", {
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
        text,
        type,
        confidence,
        source,
        metadata = {},
        tags = [],
      } = req.body;

      logger.info("Updating clinical statement", {
        requestId,
        statementId: id,
        type,
        textLength: text.length,
        confidence,
        ip: req.ip,
      });

      // Check if statement exists
      const { status: existsStatus } = await utils.get_clinical_statement_by_id(
        id
      );

      if (existsStatus === 404) {
        logger.warn("Cannot update non-existent statement", {
          requestId,
          statementId: id,
          ip: req.ip,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Clinical statement not found",
          requestId,
        });
      }

      // Build updated statement data
      const updatedStatementData = {
        id,
        text,
        type,
        confidence,
        source,
        tags,
        metadata: {
          ...metadata,
          updated_at: new Date().toISOString(),
          updated_by: req.ip,
        },
      };

      // Build updated RDF nanopublication
      const nanopubData = await auxFuncts.buildStatementNanopub(
        updatedStatementData
      );

      // Update statement in triple store
      const { status, result } = await utils.update_clinical_statement(
        id,
        nanopubData
      );

      if (status >= 400) {
        throw new ErrorHandler(
          status,
          "Failed to update clinical statement in triple store"
        );
      }

      // Invalidate cache entries
      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");

      const responseTime = Date.now() - startTime;

      logger.info("Clinical statement updated successfully", {
        requestId,
        statementId: id,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: "Clinical statement updated successfully",
        data: {
          id,
          type,
          confidence,
          metadata: updatedStatementData.metadata,
          nanopub_uri: result.nanopub_uri,
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to update clinical statement", {
        requestId,
        statementId: req.params.id,
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
        message: "An unexpected error occurred while updating statement",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to delete a clinical statement
 */
router.delete(
  "/:id",
  [writeLimiter, statementIdValidation],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Statement deletion validation failed", {
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

      logger.info("Deleting clinical statement", {
        requestId,
        statementId: id,
        ip: req.ip,
      });

      // Check if statement exists
      const { status: existsStatus } = await utils.get_clinical_statement_by_id(
        id
      );

      if (existsStatus === 404) {
        logger.warn("Cannot delete non-existent statement", {
          requestId,
          statementId: id,
          ip: req.ip,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Clinical statement not found",
          requestId,
        });
      }

      // Delete statement from triple store
      const { status } = await utils.delete_clinical_statement(id);

      if (status >= 400) {
        throw new ErrorHandler(
          status,
          "Failed to delete clinical statement from triple store"
        );
      }

      // Invalidate cache entries
      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");

      const responseTime = Date.now() - startTime;

      logger.info("Clinical statement deleted successfully", {
        requestId,
        statementId: id,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: "Clinical statement deleted successfully",
        data: {
          id,
          deleted_at: new Date().toISOString(),
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to delete clinical statement", {
        requestId,
        statementId: req.params.id,
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
        message: "An unexpected error occurred while deleting statement",
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
    service: "clinical-statements",
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
      service: "clinical-statements",
      description: "CRUD service for TMR-based clinical statements",
      endpoints: [
        {
          path: "/",
          method: "POST",
          description: "Create a new clinical statement",
        },
        {
          path: "/:id",
          method: "GET",
          description: "Retrieve a specific clinical statement",
        },
        {
          path: "/:id",
          method: "PUT",
          description: "Update a clinical statement",
        },
        {
          path: "/:id",
          method: "DELETE",
          description: "Delete a clinical statement",
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
      supported_statement_types: [
        "clinical_observation",
        "diagnosis",
        "treatment_plan",
        "outcome_measure",
        "risk_factor",
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

    logger.info("Statement cache cleared via API", { ip: req.ip });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: "Cache cleared successfully",
    });
  } catch (error) {
    logger.error("Failed to clear statement cache", {
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
