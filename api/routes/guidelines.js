/**
 * Guidelines Routes
 * Handles TMR-based guideline interactions parsing with grammar
 * Enhanced with validation, caching, and monitoring
 */

const express = require("express");
const { body, query, validationResult } = require("express-validator");
const { StatusCodes } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

// Core dependencies
const { ErrorHandler } = require("../lib/errorHandler");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

// Parser for TMR guideline interactions
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar.js");

// Rate limiting for parser operations
const parsingLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 50, // limit each IP to 50 parsing requests per windowMs
  message: {
    status: "error",
    message: "Too many parsing requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

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

// Enhanced caching system for parsed interactions
const cache = new Map();
const CACHE_TTL = 900; // 15 minutes for parsing results
const CACHE_PREFIX = "guidelines:";

/**
 * Cache utilities for guideline interactions
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

    logger.debug("Cache hit for guidelines", { key });
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

    logger.debug("Cache set for guidelines", { key, ttlSeconds });
  },

  clear(pattern = "") {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }
    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Guidelines cache cleared", {
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
      memoryUsage: process.memoryUsage().heapUsed,
    };
  },
};

/**
 * Validation rules for guideline interactions
 */
const parseValidationRules = [
  body("interaction")
    .notEmpty()
    .withMessage("Interaction content is required")
    .isString()
    .withMessage("Interaction must be a string")
    .isLength({ min: 5, max: 10000 })
    .withMessage("Interaction must be between 5 and 10000 characters"),

  body("options")
    .optional()
    .isObject()
    .withMessage("Options must be an object"),

  body("options.strict_mode")
    .optional()
    .isBoolean()
    .withMessage("Strict mode must be a boolean"),

  body("options.include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),

  body("options.validate_semantics")
    .optional()
    .isBoolean()
    .withMessage("Validate semantics must be a boolean"),

  body("context")
    .optional()
    .isObject()
    .withMessage("Context must be an object"),

  body("context.guideline_id")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Guideline ID must be a string between 1 and 100 characters"),

  body("context.patient_data")
    .optional()
    .isObject()
    .withMessage("Patient data must be an object"),
];

const queryValidationRules = [
  query("format")
    .optional()
    .isIn(["json", "rdf", "turtle", "n3"])
    .withMessage("Format must be one of: json, rdf, turtle, n3"),

  query("include_grammar_info")
    .optional()
    .isBoolean()
    .withMessage("Include grammar info must be a boolean"),

  query("limit")
    .optional()
    .isInt({ min: 1, max: 100 })
    .withMessage("Limit must be an integer between 1 and 100"),
];

/**
 * Parser initialization and utilities
 */
function initializeParser() {
  try {
    const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
    return parser;
  } catch (error) {
    logger.error("Failed to initialize grammar parser", {
      error: error.message,
    });
    throw new ErrorHandler(500, "Parser initialization failed");
  }
}

/**
 * Enhanced parsing function with error handling
 */
async function parseInteraction(interactionText, options = {}) {
  const {
    strict_mode = false,
    include_metadata = true,
    validate_semantics = true,
  } = options;

  const parser = initializeParser();

  try {
    // Feed the interaction text to the parser
    parser.feed(interactionText);

    if (parser.results.length === 0) {
      throw new Error("No valid parse results found");
    }

    if (parser.results.length > 1 && strict_mode) {
      throw new Error("Ambiguous grammar - multiple parse results found");
    }

    // Get the best parse result
    const parseResult = parser.results[0];

    // Process and enhance the parse result
    let processedResult = {
      parsed: parseResult,
      input: interactionText,
      success: true,
    };

    if (include_metadata) {
      processedResult.metadata = {
        parse_count: parser.results.length,
        parser_version: "1.0.0",
        parsed_at: new Date().toISOString(),
        strict_mode,
        input_length: interactionText.length,
      };
    }

    if (validate_semantics) {
      // Perform semantic validation
      const semanticValidation = await auxFuncts.validateSemantics(parseResult);
      processedResult.semantic_validation = semanticValidation;

      if (!semanticValidation.valid && strict_mode) {
        throw new Error(
          `Semantic validation failed: ${semanticValidation.errors.join(", ")}`
        );
      }
    }

    // Extract structured information
    const structuredInfo = await auxFuncts.extractStructuredInfo(parseResult);
    processedResult.structured = structuredInfo;

    return processedResult;
  } catch (error) {
    logger.error("Parsing failed", {
      error: error.message,
      inputLength: interactionText.length,
      options,
    });

    return {
      parsed: null,
      input: interactionText,
      success: false,
      error: {
        message: error.message,
        type: "parse_error",
        position: error.location || null,
      },
      metadata: include_metadata
        ? {
            parsed_at: new Date().toISOString(),
            strict_mode,
            input_length: interactionText.length,
          }
        : undefined,
    };
  }
}

/**
 * Enhanced endpoint to parse guideline interactions
 */
router.post(
  "/parse",
  [parsingLimiter, parseValidationRules],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Guidelines parsing validation failed", {
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

      const { interaction, options = {}, context = {} } = req.body;

      logger.info("Parsing guideline interaction", {
        requestId,
        inputLength: interaction.length,
        options,
        context: Object.keys(context),
        ip: req.ip,
      });

      // Generate cache key including options and context
      const cacheKey = cacheUtils.generateKey("/parse", {
        interaction: interaction.substring(0, 100), // Use first 100 chars for cache key
        options: JSON.stringify(options),
        context: JSON.stringify(context),
      });

      // Check cache
      const cachedResult = cacheUtils.get(cacheKey);
      if (cachedResult) {
        const responseTime = Date.now() - startTime;
        logger.info("Returning cached parsing result", {
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

      // Parse the interaction
      const parseResult = await parseInteraction(interaction, options);

      // Add context information if provided
      if (Object.keys(context).length > 0) {
        parseResult.context = context;

        // If guideline_id is provided, try to enhance with guideline info
        if (context.guideline_id) {
          try {
            const guidelineInfo = await auxFuncts.getGuidelineInfo(
              context.guideline_id
            );
            parseResult.guideline_context = guidelineInfo;
          } catch (error) {
            logger.warn("Failed to load guideline context", {
              requestId,
              guidelineId: context.guideline_id,
              error: error.message,
            });
          }
        }
      }

      const responseData = {
        status: parseResult.success ? "success" : "error",
        message: parseResult.success
          ? "Interaction parsed successfully"
          : "Parsing failed",
        data: parseResult,
      };

      // Cache successful results
      if (parseResult.success) {
        cacheUtils.set(cacheKey, responseData, CACHE_TTL);
      }

      const responseTime = Date.now() - startTime;

      logger.info("Guideline interaction parsing completed", {
        requestId,
        success: parseResult.success,
        responseTime,
        cached: false,
        ip: req.ip,
      });

      const statusCode = parseResult.success
        ? StatusCodes.OK
        : StatusCodes.BAD_REQUEST;

      res.status(statusCode).json({
        ...responseData,
        cached: false,
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to parse guideline interaction", {
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
        message: "An unexpected error occurred while parsing interaction",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to get grammar information
 */
router.get(
  "/grammar",
  [queryLimiter, queryValidationRules],
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
        format = "json",
        include_grammar_info = true,
        limit = 50,
      } = req.query;

      logger.info("Retrieving grammar information", {
        requestId,
        format,
        include_grammar_info,
        ip: req.ip,
      });

      // Check cache
      const cacheKey = cacheUtils.generateKey("/grammar", {
        format,
        include_grammar_info,
        limit,
      });
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

      // Build grammar information
      const grammarInfo = {
        name: "TMR Guidelines Grammar",
        version: "1.0.0",
        description:
          "Grammar for parsing TMR-based clinical guideline interactions",
        last_updated: new Date().toISOString(),
      };

      if (include_grammar_info === "true" || include_grammar_info === true) {
        try {
          // Get grammar rules and structure
          const grammarRules = await auxFuncts.getGrammarRules(parseInt(limit));
          grammarInfo.rules = grammarRules;
          grammarInfo.rule_count = grammarRules.length;

          // Get example interactions
          const examples = await auxFuncts.getGrammarExamples();
          grammarInfo.examples = examples;

          // Get supported constructs
          const constructs = await auxFuncts.getSupportedConstructs();
          grammarInfo.supported_constructs = constructs;
        } catch (error) {
          logger.warn("Failed to load complete grammar info", {
            requestId,
            error: error.message,
          });

          grammarInfo.warning = "Some grammar information could not be loaded";
        }
      }

      const responseData = {
        status: "success",
        data: grammarInfo,
      };

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_TTL);

      const responseTime = Date.now() - startTime;

      logger.info("Grammar information retrieved successfully", {
        requestId,
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

      logger.error("Failed to retrieve grammar information", {
        requestId,
        error: error.message,
        stack: error.stack,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message:
          "An unexpected error occurred while retrieving grammar information",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to validate interaction syntax
 */
router.post(
  "/validate",
  [parsingLimiter, parseValidationRules],
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

      const { interaction, options = {} } = req.body;

      logger.info("Validating interaction syntax", {
        requestId,
        inputLength: interaction.length,
        ip: req.ip,
      });

      // Parse with strict validation options
      const validationOptions = {
        ...options,
        strict_mode: true,
        validate_semantics: true,
        include_metadata: true,
      };

      const parseResult = await parseInteraction(
        interaction,
        validationOptions
      );

      const validationResult = {
        valid: parseResult.success,
        input: interaction,
        issues: [],
      };

      if (!parseResult.success) {
        validationResult.issues.push({
          type: "syntax_error",
          message: parseResult.error.message,
          position: parseResult.error.position,
        });
      }

      if (
        parseResult.semantic_validation &&
        !parseResult.semantic_validation.valid
      ) {
        validationResult.issues.push(
          ...parseResult.semantic_validation.errors.map((error) => ({
            type: "semantic_error",
            message: error,
          }))
        );
      }

      const responseTime = Date.now() - startTime;

      logger.info("Interaction validation completed", {
        requestId,
        valid: validationResult.valid,
        issueCount: validationResult.issues.length,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        data: validationResult,
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to validate interaction", {
        requestId,
        error: error.message,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "An unexpected error occurred during validation",
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

  // Test parser initialization
  let parserStatus = "healthy";
  try {
    initializeParser();
  } catch (error) {
    parserStatus = "error";
    logger.error("Parser health check failed", { error: error.message });
  }

  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "guideline-interactions-parser",
    timestamp: new Date().toISOString(),
    version: "2.0.0",
    parser_status: parserStatus,
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
      service: "guideline-interactions-parser",
      description:
        "Parser service for TMR-based guideline interactions using nearley grammar",
      grammar: {
        name: "TMR Guidelines Grammar",
        version: "1.0.0",
        parser: "nearley",
      },
      endpoints: [
        {
          path: "/parse",
          method: "POST",
          description: "Parse guideline interaction text using TMR grammar",
        },
        {
          path: "/validate",
          method: "POST",
          description: "Validate interaction syntax and semantics",
        },
        {
          path: "/grammar",
          method: "GET",
          description: "Get grammar information and rules",
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
        {
          path: "/cache/clear",
          method: "POST",
          description: "Clear parsing cache",
        },
        {
          path: "/cache/stats",
          method: "GET",
          description: "Get cache statistics",
        },
      ],
      features: [
        "TMR guideline interaction parsing",
        "Syntax validation",
        "Semantic validation",
        "Grammar introspection",
        "Result caching",
        "Parse ambiguity detection",
      ],
    },
  });
});

/**
 * Cache management endpoints
 */
router.post("/cache/clear", (req, res) => {
  try {
    const { pattern = "" } = req.body;
    cacheUtils.clear(pattern);

    logger.info("Guidelines cache cleared via API", {
      pattern,
      ip: req.ip,
    });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: "Cache cleared successfully",
    });
  } catch (error) {
    logger.error("Failed to clear guidelines cache", {
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
    logger.error("Failed to get guidelines cache stats", {
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
module.exports.parseInteraction = parseInteraction;
