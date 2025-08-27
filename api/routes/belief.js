/**
 * Belief Routes
 * Handles CRUD operations for TMR-based causation beliefs
 * Enhanced with better validation, error handling, and monitoring
 */

const express = require("express");
const { body, validationResult } = require("express-validator");
const { StatusCodes, ReasonPhrases } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

// Core dependencies
const config = require("../lib/config");
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

// Constants
const PREFIX = "http://anonymous.org/data/";
const NANOPUB_PREFIX = "http://www.nanopub.org/nschema#";

// Rate limiting
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

/**
 * Validation rules for belief creation
 */
const beliefValidationRules = [
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
    .isFloat({ min: 0, max: 1 })
    .withMessage("Strength must be a float between 0 and 1"),

  body("frequency")
    .isFloat({ min: 0, max: 1 })
    .withMessage("Frequency must be a float between 0 and 1"),

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
 * Enhanced function to construct SPARQL insert query for causation belief
 */
function constructInsertQuery(req) {
  const {
    id,
    care_action_id,
    transition_id,
    strength,
    frequency,
    author,
    derivedFrom,
  } = req.body;

  // Validate required parameters
  if (!id || !care_action_id || !transition_id || !author) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      "Missing required parameters for belief creation"
    );
  }

  const beliefId = `${PREFIX}CB${id}`;
  const date = new Date().toISOString();

  // Handle derivedFrom sources
  let derivedFromSources;
  if (derivedFrom && derivedFrom.trim() !== "") {
    try {
      derivedFromSources = derivedFrom
        .split(",")
        .map((source) => {
          const trimmedSource = source.trim();
          // Validate URL format
          if (
            !trimmedSource.startsWith("http://") &&
            !trimmedSource.startsWith("https://")
          ) {
            return `<${PREFIX}${trimmedSource}>`;
          }
          return `<${trimmedSource}>`;
        })
        .join(", ");
    } catch (error) {
      logger.error("Error processing derivedFrom sources", {
        derivedFrom,
        error: error.message,
      });
      derivedFromSources = `<${PREFIX}Not_given>`;
    }
  } else {
    derivedFromSources = `<${PREFIX}Not_given>`;
  }

  // Escape special characters in string literals
  const escapedAuthor = author.replace(/['"\\]/g, "\\$&");

  // Build nanopublication structure
  const head = `GRAPH <${beliefId}_head> {
    <${beliefId}> a nanopub:Nanopublication ;
              nanopub:hasAssertion <${beliefId}> ;
              nanopub:hasProvenance <${beliefId}_provenance> ;
              nanopub:hasPublicationInfo <${beliefId}_publicationinfo> .
  }`;

  const assertion = `GRAPH <${beliefId}> {
    <${PREFIX}ActAdminister${care_action_id}> vocab:causes <${PREFIX}Tr${transition_id}> .
    <${beliefId}> a vocab:CausationBelief ;
            vocab:strength "${strength}"^^xsd:float ;
            vocab:frequency "${frequency}"^^xsd:float .
  }`;

  const provenance = `GRAPH <${beliefId}_provenance> {
    <${beliefId}_provenance> a oa:Annotation ;
                      oa:hasBody <${beliefId}> ;
                      prov:wasDerivedFrom ${derivedFromSources} .
  }`;

  const publication = `GRAPH <${beliefId}_publicationinfo> {
    <${beliefId}_head> prov:generatedAtTime "${date}"^^xsd:dateTime ;
                  prov:wasAttributedTo <${PREFIX}${escapedAuthor}> .
  }`;

  return `INSERT DATA { ${head} ${assertion} ${provenance} ${publication} }`;
}

/**
 * Enhanced endpoint to add a causation belief
 */
router.post(
  "/add",
  [createLimiter, ...beliefValidationRules],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Belief creation validation failed", {
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

      logger.info("Creating causation belief", {
        requestId,
        id: req.body.id,
        care_action_id: req.body.care_action_id,
        transition_id: req.body.transition_id,
        ip: req.ip,
      });

      // Construct SPARQL query
      const sparqlQuery = constructInsertQuery(req);

      logger.debug("Generated SPARQL query for belief creation", {
        requestId,
        query: sparqlQuery,
        queryLength: sparqlQuery.length,
      });

      // Execute SPARQL update
      const { status, data } = await utils.sparqlUpdate("beliefs", sparqlQuery);

      if (status >= 400) {
        throw new ErrorHandler(status, `SPARQL update failed: ${data}`);
      }

      const responseTime = Date.now() - startTime;

      logger.info("Causation belief created successfully", {
        requestId,
        id: req.body.id,
        status,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.CREATED).json({
        status: "success",
        message: `Causation belief ${req.body.id} created successfully`,
        data: {
          id: req.body.id,
          belief_uri: `${PREFIX}CB${req.body.id}`,
          operation: "create",
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to create causation belief", {
        requestId,
        id: req.body?.id,
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
        message: "Failed to create causation belief",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to delete a causation belief
 */
router.post(
  "/delete",
  [
    deleteLimiter,
    body("id")
      .isString()
      .trim()
      .notEmpty()
      .matches(/^[a-zA-Z0-9_-]+$/)
      .withMessage(
        "ID is required and must contain only alphanumeric characters, underscores, or hyphens"
      ),
  ],
  async (req, res) => {
    const requestId = `${Date.now()}-${Math.random()
      .toString(36)
      .substr(2, 9)}`;
    const startTime = Date.now();

    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logger.warn("Belief deletion validation failed", {
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

      const { id } = req.body;

      logger.info("Deleting causation belief", {
        requestId,
        id,
        ip: req.ip,
      });

      // Construct delete query using helper function
      const query = auxFuncts.sparql_drop_named_graphs("beliefs", `CB${id}`);

      logger.debug("Generated SPARQL delete query", {
        requestId,
        id,
        query,
      });

      // Execute delete operation
      const { status, data } = await utils.sparqlUpdate("beliefs", query);

      if (status >= 400) {
        throw new ErrorHandler(status, `SPARQL delete failed: ${data}`);
      }

      const responseTime = Date.now() - startTime;

      logger.info("Causation belief deleted successfully", {
        requestId,
        id,
        status,
        responseTime,
        ip: req.ip,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `Causation belief ${id} deleted successfully`,
        data: {
          id,
          operation: "delete",
        },
        requestId,
        responseTime,
      });
    } catch (error) {
      const responseTime = Date.now() - startTime;

      logger.error("Failed to delete causation belief", {
        requestId,
        id: req.body?.id,
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
        message: "Failed to delete causation belief",
        requestId,
        responseTime,
        error:
          process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * Enhanced endpoint to retrieve a specific causation belief
 */
router.post("/all/get", beliefRetrievalRules, async (req, res) => {
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
      beliefUri = id.includes("CB") ? `${PREFIX}${id}` : `${PREFIX}CB${id}`;
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
    version: "2.0.0",
  });
});

/**
 * Get belief schema/info endpoint
 */
router.get("/info", (req, res) => {
  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      service: "causation-beliefs",
      description:
        "Manages TMR-based causation beliefs between care actions and transitions",
      endpoints: [
        {
          path: "/add",
          method: "POST",
          description: "Create a new causation belief",
          rate_limit: "30 requests per 15 minutes",
        },
        {
          path: "/delete",
          method: "POST",
          description: "Delete an existing causation belief",
          rate_limit: "20 requests per 15 minutes",
        },
        {
          path: "/all/get",
          method: "POST",
          description: "Retrieve a specific causation belief by ID or URI",
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
        strength: "float (0-1)",
        frequency: "float (0-1)",
        derivedFrom: "string (comma-separated URLs)",
      },
    },
  });
});

module.exports = router;
