/**
 * Guideline Routes
 * Handles TMR-based Clinical Implementation Guidelines (CIGs)
 * Enhanced with comprehensive validation, caching, and monitoring
 */

const express = require("express");
const { body, param, query, validationResult } = require("express-validator");
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
  max: 200, // limit each IP to 200 read requests per windowMs
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

const complexOperationLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 20, // limit each IP to 20 complex operations per windowMs
  message: {
    status: "error",
    message: "Too many complex operations, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Enhanced caching system
const cache = new Map();
const CACHE_TTL = 600; // 10 minutes for guidelines (longer due to complexity)
const CACHE_PREFIX = "guideline:";

/**
 * Advanced cache utilities
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

    logger.debug("Cache hit for guideline", { key });
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

    logger.debug("Cache set for guideline", { key, ttlSeconds });
  },

  invalidatePattern(pattern) {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Guideline cache invalidated", {
      pattern,
      deletedCount: keysToDelete.length,
    });
  },

  clear() {
    cache.clear();
    logger.info("Guideline cache cleared completely");
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
      memoryUsage: process.memoryUsage().heapUsed,
    };
  },
};

/**
 * Comprehensive validation rules
 */
const guidelineValidationRules = [
  body("guideline")
    .notEmpty()
    .withMessage("Guideline content is required")
    .isString()
    .withMessage("Guideline must be a string")
    .isLength({ min: 10, max: 50000 })
    .withMessage("Guideline must be between 10 and 50000 characters"),

  body("filename")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 1, max: 255 })
    .matches(/^[a-zA-Z0-9_\-\.]+$/)
    .withMessage(
      "Filename must be alphanumeric with underscores, hyphens, and periods only"
    ),

  body("metadata")
    .optional()
    .isObject()
    .withMessage("Metadata must be an object"),

  body("metadata.title")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 200 })
    .withMessage("Title must be a string with max 200 characters"),

  body("metadata.version")
    .optional()
    .isString()
    .trim()
    .matches(/^\d+\.\d+\.\d+$/)
    .withMessage("Version must follow semantic versioning (e.g., 1.0.0)"),

  body("metadata.author")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 100 })
    .withMessage("Author must be a string with max 100 characters"),

  body("metadata.organization")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 200 })
    .withMessage("Organization must be a string with max 200 characters"),
];

const guidelineIdValidation = [
  param("id")
    .notEmpty()
    .withMessage("Guideline ID is required")
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .matches(/^[a-zA-Z0-9_\-\.]+$/)
    .withMessage(
      "ID must be alphanumeric with underscores, hyphens, and periods only"
    ),
];

const queryValidationRules = [
  query("limit")
    .optional()
    .isInt({ min: 1, max: 100 })
    .withMessage("Limit must be an integer between 1 and 100"),

  query("offset")
    .optional()
    .isInt({ min: 0 })
    .withMessage("Offset must be a non-negative integer"),

  query("include_recommendations")
    .optional()
    .isBoolean()
    .withMessage("Include recommendations must be a boolean"),

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),
];

/**
 * Enhanced endpoint to create a new guideline
 */
router.post("/", [writeLimiter, guidelineValidationRules], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Guideline creation validation failed", {
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

    const { guideline, filename, metadata = {} } = req.body;

    logger.info("Creating new guideline", {
      requestId,
      filename,
      guidelineLength: guideline.length,
      metadata,
      ip: req.ip,
    });

    // Generate unique ID if filename not provided
    const guidelineId =
      filename ||
      `guideline_${Date.now()}_${Math.random().toString(36).substr(2, 5)}`;

    // Parse and validate guideline structure
    let parsedGuideline;
    try {
      parsedGuideline = await auxFuncts.parseGuidelineContent(guideline);
    } catch (parseError) {
      logger.error("Guideline parsing failed", {
        requestId,
        error: parseError.message,
        ip: req.ip,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Invalid guideline format",
        details: parseError.message,
        requestId,
      });
    }

    // Create guideline with metadata
    const guidelineData = {
      id: guidelineId,
      content: guideline,
      parsed: parsedGuideline,
      metadata: {
        ...metadata,
        created_at: new Date().toISOString(),
        created_by: req.ip,
        version: metadata.version || "1.0.0",
      },
    };

    // Create RDF for guideline storage
    const guidelineUri = `http://anonymous.org/data/CIG${guidelineId}`;
    const sparqlQuery = `
      PREFIX vocab: <http://anonymous.org/vocab/>
      PREFIX data: <http://anonymous.org/data/>
      
      INSERT DATA {
        <${guidelineUri}> a vocab:ClinicalGuideline ;
          vocab:hasLabel "${guidelineId}" ;
          vocab:hasContent "${guideline.replace(/"/g, '\\"')}" ;
          vocab:hasVersion "${guidelineData.metadata.version}" ;
          vocab:createdAt "${guidelineData.metadata.created_at}" ;
          vocab:createdBy "${guidelineData.metadata.created_by}" .
      }
    `;

    // Store guideline in triple store
    const { status, result } = await utils.sparqlUpdate(
      "guidelines",
      sparqlQuery
    );

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to store guideline in triple store"
      );
    }

    // Invalidate related cache entries
    cacheUtils.invalidatePattern("list");
    cacheUtils.invalidatePattern("search");

    const responseTime = Date.now() - startTime;

    logger.info("Guideline created successfully", {
      requestId,
      guidelineId,
      responseTime,
      ip: req.ip,
    });

    res.status(StatusCodes.CREATED).json({
      status: "success",
      message: "Guideline created successfully",
      data: {
        id: guidelineId,
        guideline_uri: guidelineUri,
        metadata: guidelineData.metadata,
        recommendations_count: parsedGuideline.recommendations?.length || 0,
      },
      requestId,
      responseTime,
    });
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Failed to create guideline", {
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
      message: "An unexpected error occurred while creating guideline",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced endpoint to get a specific guideline
 */
router.get("/:id", [readLimiter, guidelineIdValidation], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Guideline get validation failed", {
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
    const { include_recommendations = true } = req.query;

    logger.info("Retrieving guideline", {
      requestId,
      guidelineId: id,
      include_recommendations,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey(`/get/${id}`, {
      include_recommendations,
    });
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      logger.info("Returning cached guideline", {
        requestId,
        guidelineId: id,
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

    // Retrieve guideline from triple store
    const { status, guideline } = await utils.get_guideline_by_id(id);

    if (status === 404) {
      logger.warn("Guideline not found", {
        requestId,
        guidelineId: id,
        ip: req.ip,
      });

      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Guideline not found",
        requestId,
      });
    }

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve guideline from triple store"
      );
    }

    // Process guideline data
    let processedGuideline = guideline;

    if (
      include_recommendations === "true" ||
      include_recommendations === true
    ) {
      // Extract and enhance recommendations
      const recommendations = await auxFuncts.extractRecommendations(guideline);
      processedGuideline = {
        ...guideline,
        recommendations,
      };
    }

    const responseData = {
      status: "success",
      data: processedGuideline,
    };

    // Cache the result
    cacheUtils.set(cacheKey, responseData, CACHE_TTL);

    const responseTime = Date.now() - startTime;

    logger.info("Guideline retrieved successfully", {
      requestId,
      guidelineId: id,
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

    logger.error("Failed to retrieve guideline", {
      requestId,
      guidelineId: req.params.id,
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
      message: "An unexpected error occurred while retrieving guideline",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced endpoint to update a guideline
 */
router.put(
  "/:id",
  [writeLimiter, guidelineIdValidation, guidelineValidationRules],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Guideline update validation failed", {
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
      const { guideline, metadata = {} } = req.body;

      logger.info("Updating guideline", {
        requestId,
        guidelineId: id,
        guidelineLength: guideline.length,
        metadata,
        ip: req.ip,
      });

      // Check if guideline exists
      const { status: existsStatus } = await utils.get_guideline_by_id(id);

      if (existsStatus === 404) {
        logger.warn("Cannot update non-existent guideline", {
          requestId,
          guidelineId: id,
          ip: req.ip,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Guideline not found",
          requestId,
        });
      }

      // Parse and validate updated guideline
      let parsedGuideline;
      try {
        parsedGuideline = await auxFuncts.parseGuidelineContent(guideline);
      } catch (parseError) {
        logger.error("Updated guideline parsing failed", {
          requestId,
          guidelineId: id,
          error: parseError.message,
          ip: req.ip,
        });

        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Invalid guideline format",
          details: parseError.message,
          requestId,
        });
      }

      // Update guideline with metadata
      const updatedGuidelineData = {
        id,
        content: guideline,
        parsed: parsedGuideline,
        metadata: {
          ...metadata,
          updated_at: new Date().toISOString(),
          updated_by: req.ip,
        },
      };

      // Update guideline in triple store
      const { status, result } = await utils.update_guideline(
        id,
        updatedGuidelineData
      );

      if (status >= 400) {
        throw new ErrorHandler(
          status,
          "Failed to update guideline in triple store"
        );
      }

      // Invalidate cache entries
      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");

      const responseTime = Date.now() - startTime;

      logger.info("Guideline updated successfully", {
        requestId,
        guidelineId: id,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: "Guideline updated successfully",
        data: {
          id,
          metadata: updatedGuidelineData.metadata,
          recommendations_count: parsedGuideline.recommendations?.length || 0,
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to update guideline", {
        requestId,
        guidelineId: req.params.id,
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
        message: "An unexpected error occurred while updating guideline",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to delete a guideline
 */
router.delete(
  "/:id",
  [writeLimiter, guidelineIdValidation],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Guideline deletion validation failed", {
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

      logger.info("Deleting guideline", {
        requestId,
        guidelineId: id,
        ip: req.ip,
      });

      // Check if guideline exists
      const { status: existsStatus } = await utils.get_guideline_by_id(id);

      if (existsStatus === 404) {
        logger.warn("Cannot delete non-existent guideline", {
          requestId,
          guidelineId: id,
          ip: req.ip,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Guideline not found",
          requestId,
        });
      }

      // Delete guideline from triple store
      const { status } = await utils.delete_guideline(id);

      if (status >= 400) {
        throw new ErrorHandler(
          status,
          "Failed to delete guideline from triple store"
        );
      }

      // Invalidate cache entries
      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");

      const responseTime = Date.now() - startTime;

      logger.info("Guideline deleted successfully", {
        requestId,
        guidelineId: id,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: "Guideline deleted successfully",
        data: {
          id,
          deleted_at: new Date().toISOString(),
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to delete guideline", {
        requestId,
        guidelineId: req.params.id,
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
        message: "An unexpected error occurred while deleting guideline",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to list all guidelines
 */
router.get("/", [readLimiter, queryValidationRules], async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Guidelines list validation failed", {
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
      limit = 20,
      offset = 0,
      include_recommendations = false,
      include_metadata = true,
    } = req.query;

    const queryParams = {
      limit,
      offset,
      include_recommendations,
      include_metadata,
    };

    logger.info("Listing guidelines", {
      requestId,
      queryParams,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey("/list", queryParams);
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      const responseTime = Date.now() - startTime;
      logger.info("Returning cached guidelines list", {
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

    // Retrieve guidelines from triple store
    const { status, guidelines, totalCount } = await utils.list_guidelines({
      limit: parseInt(limit),
      offset: parseInt(offset),
      include_recommendations:
        include_recommendations === "true" || include_recommendations === true,
      include_metadata:
        include_metadata === "true" || include_metadata === true,
    });

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve guidelines from triple store"
      );
    }

    const responseData = {
      status: "success",
      data: guidelines || [],
      metadata: {
        total_count: totalCount || 0,
        returned_count: guidelines?.length || 0,
        limit: parseInt(limit),
        offset: parseInt(offset),
        has_more: parseInt(offset) + parseInt(limit) < (totalCount || 0),
      },
    };

    // Cache the result
    cacheUtils.set(cacheKey, responseData, CACHE_TTL);

    const responseTime = Date.now() - startTime;

    logger.info("Guidelines listed successfully", {
      requestId,
      totalCount: totalCount || 0,
      returnedCount: guidelines?.length || 0,
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

    logger.error("Failed to list guidelines", {
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
      message: "An unexpected error occurred while listing guidelines",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Complex operation: Get guideline recommendations
 */
router.get(
  "/:id/recommendations",
  [complexOperationLimiter, guidelineIdValidation],
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

      const { id } = req.params;

      logger.info("Retrieving guideline recommendations", {
        requestId,
        guidelineId: id,
        ip: req.ip,
      });

      // Check cache
      const cacheKey = cacheUtils.generateKey(`/recommendations/${id}`);
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

      // Get guideline and extract recommendations
      const { status, guideline } = await utils.get_guideline_by_id(id);

      if (status === 404) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Guideline not found",
          requestId,
        });
      }

      if (status >= 400) {
        throw new ErrorHandler(status, "Failed to retrieve guideline");
      }

      const recommendations = await auxFuncts.extractRecommendations(guideline);

      const responseData = {
        status: "success",
        data: {
          guideline_id: id,
          recommendations,
          count: recommendations.length,
        },
      };

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_TTL);

      const responseTime = Date.now() - startTime;

      logger.info("Guideline recommendations retrieved successfully", {
        requestId,
        guidelineId: id,
        recommendationsCount: recommendations.length,
        responseTime,
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

      logger.error("Failed to retrieve guideline recommendations", {
        requestId,
        guidelineId: req.params.id,
        error: error.message,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message:
          "An unexpected error occurred while retrieving recommendations",
        requestId,
        responseTime,
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
    service: "clinical-implementation-guidelines",
    timestamp: new Date().toISOString(),
    version: "2.0.0",
    cache: cacheStats,
  });
});

/**
 * Cache management endpoints
 */
router.post("/cache/clear", (req, res) => {
  try {
    cacheUtils.clear();

    logger.info("Guideline cache cleared via API", { ip: req.ip });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: "Cache cleared successfully",
    });
  } catch (error) {
    logger.error("Failed to clear guideline cache", {
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
