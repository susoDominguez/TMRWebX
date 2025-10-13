/**
 * Belief Routes
 * Handles CRUD operations for TMR-based causation beliefs
 * Enhanced with better validation, error handling, and monitoring
 * Refactored to follow careAction and transition patterns
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
const { isValidId, escapeQuotes } = require("../lib/router_functs/route_helpers");

// Constants and Configuration
const DATA_PREFIX = "http://anonymous.org/data/";
const VOCAB_PREFIX = "http://anonymous.org/vocab/";
const NANOPUB_PREFIX = "http://www.nanopub.org/nschema#";

// Rate limiting for create/delete operations
const createLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 30, // limit each IP to 30 create requests per windowMs
  message: {
    status: "error",
    message: "Too many belief creation requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const deleteLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 20, // limit each IP to 20 delete requests per windowMs
  message: {
    status: "error",
    message: "Too many belief deletion requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

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

// Route mappings with metadata
const ROUTE_MAPPINGS = Object.freeze({
  "": {
    description: "Causation belief between care action and transition",
    requiresCareAction: true,
    requiresTransition: true,
  },
});

/**
 * Enhanced validation rules for belief creation
 */
const getValidationRules = () => [
  body("id")
    .isString()
    .trim()
    .isLength({ min: 1, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "ID must be 1-50 alphanumeric characters, underscores, or hyphens"
    ),

  body("care_action_id")
    .isString()
    .trim()
    .isLength({ min: 1, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "Care action ID must be 1-50 alphanumeric characters, underscores, or hyphens"
    ),

  body("transition_id")
    .isString()
    .trim()
    .isLength({ min: 1, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "Transition ID must be 1-50 alphanumeric characters, underscores, or hyphens"
    ),

  body("strength")
    .isString()
    .trim()
    .customSanitizer((v) => (typeof v === "string" ? v.toLowerCase() : v))
    .isIn(["high", "medium", "low"])
    .withMessage("Strength must be one of: 'high', 'medium', 'low'"),

  body("frequency")
    .isString()
    .trim()
    .customSanitizer((v) => (typeof v === "string" ? v.toLowerCase() : v))
    .isIn(["always", "never"])
    .withMessage("Frequency must be either 'always' or 'never'"),

  body("author")
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .matches(/^[a-zA-Z0-9_\s-]+$/)
    .withMessage(
      "Author must be 1-100 characters, alphanumeric with spaces, underscores, and hyphens"
    ),

  body("derivedFrom")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 1000 })
    .withMessage("DerivedFrom must be maximum 1000 characters"),
];

/**
 * Validation rules for belief retrieval
 */
const beliefRetrievalRules = [
  body("id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "ID must contain only alphanumeric characters, underscores, or hyphens"
    ),

  body("uri")
    .optional()
    .isString()
    .trim()
    .isURL({ protocols: ["http", "https"] })
    .withMessage("URI must be a valid URL"),

  // At least one of id or uri must be provided (custom validation)
  body().custom((value, { req }) => {
    if (!req.body.id && !req.body.uri) {
      throw new Error("Either ID or URI must be provided");
    }
    return true;
  }),
];

/**
 * Validation rules for querying all beliefs
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

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),
];

// Simple in-memory cache
const cache = new Map();

/**
 * Cache utilities
 */
const cacheUtils = {
  generateKey(prefix, params = {}) {
    const sortedParams = Object.keys(params)
      .sort()
      .map((key) => `${key}:${params[key]}`)
      .join(":");
    return sortedParams ? `${prefix}:${sortedParams}` : prefix;
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

  set(key, data, ttlSeconds = 300) {
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
};

/**
 * Process derivedFrom sources into SPARQL format
 */
function processDerivedFromSources(derivedFrom) {
  if (!derivedFrom || derivedFrom.trim() === "") {
    return `<${DATA_PREFIX}Not_given>`;
  }

  try {
    return derivedFrom
      .split(",")
      .map((source) => {
        const trimmedSource = source.trim();
        // Validate URL format
        if (
          !trimmedSource.startsWith("http://") &&
          !trimmedSource.startsWith("https://")
        ) {
          return `<${DATA_PREFIX}${trimmedSource}>`;
        }
        return `<${trimmedSource}>`;
      })
      .join(", ");
  } catch (error) {
    logger.error("Error processing derivedFrom sources", {
      derivedFrom,
      error: error.message,
    });
    return `<${DATA_PREFIX}Not_given>`;
  }
}

/**
 * Create nanopublication structure for causation belief
 */
function createNanopublicationDefinition(requestBody) {
  const {
    id,
    care_action_id,
    transition_id,
    strength,
    frequency,
    author,
    derivedFrom,
  } = requestBody;

  // Validate required parameters
  if (!id || !care_action_id || !transition_id || !author) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      "Missing required parameters for belief creation"
    );
  }

  // Ensure proper prefixes for care action and transition IDs
  // Care actions should have ActAdminister prefix
  const careActionUri = care_action_id.startsWith('ActAdminister')
    ? `${DATA_PREFIX}${care_action_id}`
    : `${DATA_PREFIX}ActAdminister${care_action_id}`;
  
  // Transitions should have Tr prefix
  const transitionUri = transition_id.startsWith('Tr')
    ? `${DATA_PREFIX}${transition_id}`
    : `${DATA_PREFIX}Tr${transition_id}`;

  const beliefId = `${DATA_PREFIX}CB${id}`;
  const date = new Date().toISOString();
  const derivedFromSources = processDerivedFromSources(derivedFrom);
  const escapedAuthor = escapeQuotes(author);

  // Build nanopublication structure
  const head = `GRAPH <${beliefId}_head> {
    <${beliefId}> a nanopub:Nanopublication ;
              nanopub:hasAssertion <${beliefId}> ;
              nanopub:hasProvenance <${beliefId}_provenance> ;
              nanopub:hasPublicationInfo <${beliefId}_publicationinfo> .
  }`;

  const assertion = `GRAPH <${beliefId}> {
    <${careActionUri}> vocab:causes <${transitionUri}> .
    <${beliefId}> a vocab:CausationBelief ;
            vocab:strength "${escapeQuotes(strength)}"^^xsd:string ;
            vocab:frequency "${escapeQuotes(frequency)}"^^xsd:string .
  }`;

  const provenance = `GRAPH <${beliefId}_provenance> {
    <${beliefId}_provenance> a oa:Annotation ;
                      oa:hasBody <${beliefId}> ;
                      prov:wasDerivedFrom ${derivedFromSources} .
  }`;

  const publication = `GRAPH <${beliefId}_publicationinfo> {
    <${beliefId}_head> prov:generatedAtTime "${date}"^^xsd:dateTime ;
                  prov:wasAttributedTo <${DATA_PREFIX}${escapedAuthor}> .
  }`;

  const completeDefinition = `${head} ${assertion} ${provenance} ${publication}`;

  logger.debug("Generated nanopublication definition", {
    id,
    beliefId,
    careActionUri,
    transitionUri,
    definitionLength: completeDefinition.length,
  });

  return {
    definition: completeDefinition,
    createdIds: {
      beliefId: id,
      beliefUri: beliefId,
      careActionUri,
      transitionUri,
    },
  };
}

/**
 * Execute belief operation (INSERT or DELETE)
 */
async function executeBeliefOperation(sparqlQuery, operationType, id) {
  try {
    let sparqlStatement;

    if (operationType === config.INSERT) {
      sparqlStatement = `INSERT DATA { ${sparqlQuery} }`;
    } else if (operationType === config.DELETE) {
      // Use helper function for dropping named graphs
      sparqlStatement = auxFuncts.sparql_drop_named_graphs("beliefs", `CB${id}`);
    } else {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        `Invalid operation type: ${operationType}`
      );
    }

    logger.debug("Executing SPARQL operation", {
      operation: operationType,
      id,
      queryLength: sparqlStatement.length,
    });

    const result = await utils.sparqlUpdate("beliefs", sparqlStatement);

    if (result.status >= 400) {
      throw new ErrorHandler(
        result.status,
        `SPARQL operation failed: ${result.data}`
      );
    }

    return result;
  } catch (error) {
    logger.error("SPARQL operation failed", {
      operation: operationType,
      id,
      error: error.message,
    });
    throw error;
  }
}

/**
 * Enhanced add handler with comprehensive validation and error handling
 */
const createAddHandler = () => async (req, res) => {
  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Belief creation validation failed", {
        errors: errors.array(),
        body: req.body,
        ip: req.ip,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    logger.info("Creating causation belief", {
      id: req.body.id,
      care_action_id: req.body.care_action_id,
      transition_id: req.body.transition_id,
      ip: req.ip,
    });

    // Generate nanopublication definition
    const { definition: sparqlQuery, createdIds } =
      createNanopublicationDefinition(req.body);

    // Execute the operation
    const { status, data } = await executeBeliefOperation(
      sparqlQuery,
      config.INSERT,
      req.body.id
    );

    logger.info("Causation belief created successfully", {
      id: req.body.id,
      beliefUri: createdIds.beliefUri,
      status,
    });

    res.status(StatusCodes.CREATED).json({
      status: "success",
      message: `Causation belief ${req.body.id} created successfully`,
      data: {
        operation: data || "Operation completed",
        createdResources: createdIds,
      },
    });
  } catch (error) {
    logger.error("Failed to create causation belief", {
      id: req.body?.id,
      error: error.message,
      stack: error.stack,
    });

    if (error instanceof ErrorHandler) {
      return res.status(error.statusCode).json({
        status: "error",
        message: error.message,
      });
    }

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to create causation belief",
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
};

/**
 * Enhanced delete handler with validation
 */
const createDeleteHandler = () => async (req, res) => {
  try {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const { id } = req.body;

    logger.info("Deleting causation belief", {
      id,
      ip: req.ip,
    });

    const { status, data } = await executeBeliefOperation(
      null,
      config.DELETE,
      id
    );

    logger.info("Causation belief deleted successfully", {
      id,
      status,
    });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: `Causation belief ${id} deleted successfully`,
      data: data || "Operation completed",
    });
  } catch (error) {
    logger.error("Failed to delete causation belief", {
      id: req.body?.id,
      error: error.message,
    });

    if (error instanceof ErrorHandler) {
      return res.status(error.statusCode).json({
        status: "error",
        message: error.message,
      });
    }

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to delete causation belief",
    });
  }
};

/**
 * Enhanced endpoint to retrieve a specific causation belief by ID or URI
 */
router.post("/get", beliefRetrievalRules, async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Belief retrieval validation failed", {
        requestId,
        errors: errors.array(),
        body: req.body,
        ip: req.ip,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
        requestId,
      });
    }

    const { id, uri } = req.body;

    // Determine belief URI
    let beliefUri;
    if (uri) {
      beliefUri = uri;
    } else {
      // Handle both CB-prefixed and non-prefixed IDs
      beliefUri = id.includes("CB") ? `${DATA_PREFIX}${id}` : `${DATA_PREFIX}CB${id}`;
    }

    logger.info("Retrieving causation belief", {
      requestId,
      id,
      uri,
      beliefUri,
      ip: req.ip,
    });

    // Retrieve belief data
    const { status, head_vars, bindings } = await utils.getBeliefData(
      "beliefs",
      beliefUri,
      "transitions",
      "careActions"
    );

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        "Failed to retrieve belief data from triple store"
      );
    }

    const responseTime = Date.now() - startTime;

    if (bindings && bindings.length > 0) {
      // Process the belief data
      const data = auxFuncts.get_CB_object(head_vars, bindings[0]);

      logger.info("Causation belief retrieved successfully", {
        requestId,
        id,
        beliefUri,
        status,
        responseTime,
        ip: req.ip,
      });

      logger.debug("Retrieved belief data", {
        requestId,
        data,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        data,
        requestId,
        responseTime,
      });
    } else {
      logger.info("Causation belief not found", {
        requestId,
        id,
        beliefUri,
        status,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Causation belief not found",
        requestId,
        responseTime,
      });
    }
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Failed to retrieve causation belief", {
      requestId,
      id: req.body?.id,
      uri: req.body?.uri,
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
      message: "Failed to retrieve causation belief",
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
  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "causation-beliefs",
    timestamp: new Date().toISOString(),
    routes: Object.keys(ROUTE_MAPPINGS).length,
  });
});

/**
 * Get belief types/info endpoint
 */
router.get("/types", (req, res) => {
  const types = Object.entries(ROUTE_MAPPINGS).map(([route, config]) => ({
    route: route || "/",
    description: config.description,
    requiresCareAction: config.requiresCareAction,
    requiresTransition: config.requiresTransition,
  }));

  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      available_types: types,
      total_count: types.length,
      service: "causation-beliefs",
      description:
        "Manages TMR-based causation beliefs between care actions and transitions",
      required_fields: {
        add: [
          "id",
          "care_action_id",
          "transition_id",
          "strength",
          "frequency",
          "author",
        ],
        delete: ["id"],
        get: ["id OR uri"],
      },
      data_types: {
        strength: "string (high|medium|low)",
        frequency: "string (always|never)",
        derivedFrom: "string (comma-separated URLs)",
      },
    },
  });
});

/**
 * Register all routes dynamically with enhanced validation
 */
Object.entries(ROUTE_MAPPINGS).forEach(([route, routeConfig]) => {
  const validationRules = getValidationRules();

  // Add endpoints
  router.post(
    `${route}/add`,
    createLimiter,
    validationRules,
    createAddHandler()
  );

  // Delete endpoints
  router.post(
    `${route}/delete`,
    deleteLimiter,
    [
      body("id")
        .isString()
        .trim()
        .notEmpty()
        .matches(/^[a-zA-Z0-9_-]+$/)
        .withMessage(
          "ID is required and must contain only alphanumeric characters, underscores, or hyphens"
        ),
    ],
    createDeleteHandler()
  );
});

module.exports = router;
