/**
 * Belief Routes
 * Handles CRUD operations for TMR-based causation beliefs
 * Enhanced with better validation, error handling, and monitoring
 * Refactored to follow careAction and transition patterns
 */

const express = require("express");
const { body, query, validationResult } = require("express-validator");
const { StatusCodes } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

// Core dependencies
const config = require("../lib/config");
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");
const { isValidId, escapeQuotes } = require("../lib/router_functs/route_helpers");
const { logStart, logSuccess, logWarn, logError } = require("../lib/requestLogger");

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

const ALLOWED_FREQUENCIES = Object.freeze([
  "always",
  "never",
  "sometimes",
  "frequently",
  "occasionally",
  "rarely",
  "often",
]);

const STRENGTH_LEVEL_PATTERN = /^L\d+$/i;

function getBindingValue(binding, key) {
  return binding && binding[key] && binding[key].value
    ? binding[key].value
    : null;
}

function addToSet(targetSet, value) {
  if (!targetSet || !(targetSet instanceof Set)) {
    return;
  }
  if (value && typeof value === "string" && value.trim().length > 0) {
    targetSet.add(value.trim());
  }
}

function buildBeliefResponse(bindings = [], { graph, graphsConsidered } = {}) {
  if (!Array.isArray(bindings) || bindings.length === 0) {
    return null;
  }

  const aggregated = {
    uri: null,
    id: null,
    frequency: null,
    strength: null,
    derivedFrom: new Set(),
    sources: new Set(),
    careAction: {
      administration: null,
      label: null,
      type: null,
      resource: null,
      resourceLabel: null,
    },
    transition: {
      uri: null,
      derivative: null,
      property: {
        uri: null,
        label: null,
        sctId: null,
        sctLabel: null,
      },
      from: {
        uri: null,
        label: null,
        sctId: null,
        sctLabel: null,
      },
      to: {
        uri: null,
        label: null,
        sctId: null,
        sctLabel: null,
      },
    },
    generatedAtTime: null,
    attributedTo: null,
    assertion_graph: graph || null,
    graph_candidates: Array.isArray(graphsConsidered)
      ? graphsConsidered
      : [],
  };

  bindings.forEach((binding) => {
    const cbUri = getBindingValue(binding, "cbUri") || getBindingValue(binding, "belief");
    if (cbUri) {
      aggregated.uri ??= cbUri;
    }

    const cbId = getBindingValue(binding, "cbId");
    if (cbId) {
      aggregated.id ??= cbId;
    }

    const freq = getBindingValue(binding, "freq");
    if (freq) {
      aggregated.frequency ??= freq;
    }

    const strength = getBindingValue(binding, "strength");
    if (strength) {
      aggregated.strength ??= strength;
    }

    addToSet(aggregated.derivedFrom, getBindingValue(binding, "derivedFromCB"));
    addToSet(aggregated.sources, getBindingValue(binding, "hasSourcesCB"));

    const actAdmin = getBindingValue(binding, "actAdmin");
    if (actAdmin) {
      aggregated.careAction.administration ??= actAdmin;
    }

    const actId = getBindingValue(binding, "actId");
    if (actId) {
      aggregated.careAction.resource ??= actId;
    }

    const adminLabel = getBindingValue(binding, "act_label");
    if (adminLabel) {
      aggregated.careAction.label ??= adminLabel;
    }

    const actLabel = getBindingValue(binding, "actLabel");
    if (actLabel) {
      aggregated.careAction.resourceLabel ??= actLabel;
    }

    const actType = getBindingValue(binding, "actType");
    if (actType) {
      aggregated.careAction.type ??= actType;
    }

    const trUri = getBindingValue(binding, "TrUri") ||
      getBindingValue(binding, "trId") ||
      getBindingValue(binding, "transition");
    if (trUri) {
      aggregated.transition.uri ??= trUri;
    }

    const derivative = getBindingValue(binding, "deriv");
    if (derivative) {
      aggregated.transition.derivative ??= derivative;
    }

    const propUri = getBindingValue(binding, "propUri");
    if (propUri) {
      aggregated.transition.property.uri ??= propUri;
    }

    const propLabel = getBindingValue(binding, "propLabel");
    if (propLabel) {
      aggregated.transition.property.label ??= propLabel;
    }

    const propUriSct = getBindingValue(binding, "propUriSCT");
    if (propUriSct) {
      aggregated.transition.property.sctId ??= propUriSct;
    }

    const propLabelSct = getBindingValue(binding, "propLabelSCT");
    if (propLabelSct) {
      aggregated.transition.property.sctLabel ??= propLabelSct;
    }

    const sitFromId = getBindingValue(binding, "sitFromId");
    if (sitFromId) {
      aggregated.transition.from.uri ??= sitFromId;
    }

    const sitFromLabel = getBindingValue(binding, "sitFromLabel");
    if (sitFromLabel) {
      aggregated.transition.from.label ??= sitFromLabel;
    }

    const sitFromIdSct = getBindingValue(binding, "sitFromIdSCT");
    if (sitFromIdSct) {
      aggregated.transition.from.sctId ??= sitFromIdSct;
    }

    const sitFromLabelSct = getBindingValue(binding, "sitFromLabelSCT");
    if (sitFromLabelSct) {
      aggregated.transition.from.sctLabel ??= sitFromLabelSct;
    }

    const sitToId = getBindingValue(binding, "sitToId");
    if (sitToId) {
      aggregated.transition.to.uri ??= sitToId;
    }

    const sitToLabel = getBindingValue(binding, "sitToLabel");
    if (sitToLabel) {
      aggregated.transition.to.label ??= sitToLabel;
    }

    const sitToIdSct = getBindingValue(binding, "sitToIdSCT");
    if (sitToIdSct) {
      aggregated.transition.to.sctId ??= sitToIdSct;
    }

    const sitToLabelSct = getBindingValue(binding, "sitToLabelSCT");
    if (sitToLabelSct) {
      aggregated.transition.to.sctLabel ??= sitToLabelSct;
    }

    const generated = getBindingValue(binding, "generatedAtTime");
    if (generated) {
      aggregated.generatedAtTime ??= generated;
    }

    const attributed = getBindingValue(binding, "attributedTo");
    if (attributed) {
      aggregated.attributedTo ??= attributed;
    }
  });

  if (!aggregated.id && aggregated.uri) {
    aggregated.id = aggregated.uri.startsWith(DATA_PREFIX)
      ? aggregated.uri.slice(DATA_PREFIX.length)
      : aggregated.uri;
  }

  aggregated.derivedFrom = Array.from(aggregated.derivedFrom);
  aggregated.sources = Array.from(aggregated.sources);

  return aggregated;
}

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
    .custom((value) => {
      if (typeof value !== "string") {
        return false;
      }
      const trimmed = value.trim();
      if (trimmed.length === 0) {
        return false;
      }
      if (STRENGTH_LEVEL_PATTERN.test(trimmed)) {
        return true;
      }
      const normalized = trimmed.toLowerCase();
      return ["high", "medium", "low"].includes(normalized);
    })
    .withMessage(
      "Strength must be 'high', 'medium', 'low', or match the pattern 'L<number>'"
    ),

  body("frequency")
    .isString()
    .trim()
    .custom((value) => {
      if (typeof value !== "string") {
        return false;
      }
      const normalized = value.trim().toLowerCase();
      return ALLOWED_FREQUENCIES.includes(normalized);
    })
    .withMessage(
      `Frequency must be one of: ${ALLOWED_FREQUENCIES.join(", ")}`
    ),

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

function sanitizeLocalIdentifier(value, fallback = "Not_given") {
  if (!value || typeof value !== "string") {
    return fallback;
  }
  const sanitized = value
    .trim()
    .replace(/\s+/g, "_")
    .replace(/[^A-Za-z0-9:_.\/\-]/g, "");
  return sanitized.length > 0 ? sanitized : fallback;
}

function normalizeStrengthValue(value) {
  if (typeof value !== "string") {
    return value;
  }
  const trimmed = value.trim();
  if (STRENGTH_LEVEL_PATTERN.test(trimmed)) {
    return trimmed.toUpperCase();
  }
  const normalized = trimmed.toLowerCase();
  if (["high", "medium", "low"].includes(normalized)) {
    return normalized;
  }
  return trimmed;
}

function normalizeFrequencyValue(value) {
  if (typeof value !== "string") {
    return value;
  }
  const normalized = value.trim().toLowerCase();
  return ALLOWED_FREQUENCIES.includes(normalized)
    ? normalized
    : value.trim();
}

function normalizeAuthorId(author) {
  return sanitizeLocalIdentifier(author, "UnknownAuthor");
}

/**
 * Process derivedFrom sources into SPARQL format
 */
function processDerivedFromSources(derivedFrom) {
  const fallback = [`<${DATA_PREFIX}Not_given>`];

  if (!derivedFrom || typeof derivedFrom !== "string") {
    return fallback;
  }

  try {
    const sources = derivedFrom
      .split(",")
      .map((source) => source.trim())
      .filter((source) => source.length > 0)
      .map((source) => {
        if (
          source.startsWith("http://") ||
          source.startsWith("https://")
        ) {
          return `<${source}>`;
        }
        return `<${DATA_PREFIX}${sanitizeLocalIdentifier(source)}>`;
      });

    const uniqueSources = [...new Set(sources)];
    return uniqueSources.length > 0 ? uniqueSources : fallback;
  } catch (error) {
    logger.error("Error processing derivedFrom sources", {
      derivedFrom,
      error: error.message,
    });
    return fallback;
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
  const careActionLocalId = care_action_id.startsWith("ActAdminister")
    ? care_action_id
    : `ActAdminister${care_action_id}`;
  const transitionLocalId = transition_id.startsWith("Tr")
    ? transition_id
    : `Tr${transition_id}`;

  const careActionUri = `${DATA_PREFIX}${careActionLocalId}`;
  const transitionUri = `${DATA_PREFIX}${transitionLocalId}`;
  const careActionRef = `<${careActionUri}>`;
  const transitionRef = `<${transitionUri}>`;

  const beliefLocalId = `CB${id}`;
  const beliefUri = `${DATA_PREFIX}${beliefLocalId}`;
  const beliefRef = `<${beliefUri}>`;
  const headGraph = `<${beliefUri}_head>`;
  const provenanceGraph = `<${beliefUri}_provenance>`;
  const publicationGraph = `<${beliefUri}_publicationinfo>`;
  const nanopublicationUri = `<${beliefUri}_nanopub>`;

  const normalizedStrength = normalizeStrengthValue(strength);
  const normalizedFrequency = normalizeFrequencyValue(frequency);
  const derivedFromSources = processDerivedFromSources(derivedFrom);
  const derivedFromValues = derivedFromSources.map((source) =>
    source.startsWith("<") && source.endsWith(">")
      ? source.slice(1, -1)
      : source
  );
  const authorId = normalizeAuthorId(author);
  const authorUri = `<${DATA_PREFIX}${authorId}>`;
  const issuedAt = new Date().toISOString();

  const derivedStatements = derivedFromSources
    .map((source) => `    ${beliefRef} prov:wasDerivedFrom ${source} .`)
    .join("\n");

  const annotationTargetLines = derivedFromSources
    .map((source, index) => {
      const terminator =
        index === derivedFromSources.length - 1 ? " ." : " ;";
      return `            oa:hasTarget [ oa:hasSource ${source} ]${terminator}`;
    })
    .join("\n");

  // Build nanopublication structure mirroring existing RDF
  const head = `GRAPH ${headGraph} {
    ${nanopublicationUri} a nanopub:Nanopublication ;
            nanopub:hasAssertion ${beliefRef} ;
            nanopub:hasProvenance ${provenanceGraph} ;
            nanopub:hasPublicationInfo ${publicationGraph} .
  }`;

  const assertion = `GRAPH ${beliefRef} {
    ${careActionRef} vocab:causes ${transitionRef} .
    ${beliefRef} a vocab:CausationBelief ;
            vocab:frequency "${escapeQuotes(normalizedFrequency)}" ;
            vocab:strength "${escapeQuotes(normalizedStrength)}" .
  }`;

  const provenance = `GRAPH ${provenanceGraph} {
${derivedStatements}
    ${provenanceGraph} a oa:Annotation ;
            oa:hasBody ${beliefRef} ;
${annotationTargetLines}
  }`;

  const publication = `GRAPH ${publicationGraph} {
    ${nanopublicationUri} prov:generatedAtTime "${issuedAt}"^^xsd:dateTime ;
            prov:wasAttributedTo ${authorUri} .
  }`;

  const completeDefinition = [head, assertion, provenance, publication].join(
    "\n\n"
  );

  logger.debug("Generated nanopublication definition", {
    id,
    beliefUri,
    nanopublicationUri,
    careActionUri,
    transitionUri,
    definitionLength: completeDefinition.length,
  });

  return {
    definition: completeDefinition,
    createdIds: {
      beliefId: beliefLocalId,
      beliefUri,
      nanopublicationUri: `${DATA_PREFIX}${beliefLocalId}_nanopub`,
      headGraph: `${DATA_PREFIX}${beliefLocalId}_head`,
      provenanceGraph: `${DATA_PREFIX}${beliefLocalId}_provenance`,
      publicationGraph: `${DATA_PREFIX}${beliefLocalId}_publicationinfo`,
      careActionUri,
      transitionUri,
      authorUri: `${DATA_PREFIX}${authorId}`,
      derivedFrom: derivedFromValues,
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
  const startTime = logStart(req, "Belief creation requested", {
    id: req.body?.id,
    careActionId: req.body?.care_action_id,
    transitionId: req.body?.transition_id,
  });
  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logWarn(req, "Belief creation validation failed", startTime, {
        errors: errors.array(),
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    // Generate nanopublication definition
    const { definition: sparqlQuery, createdIds } =
      createNanopublicationDefinition(req.body);

    // Execute the operation
    const { status, data } = await executeBeliefOperation(
      sparqlQuery,
      config.INSERT,
      req.body.id
    );

    cacheUtils.clear();

    logSuccess(req, "Belief creation completed", startTime, {
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
    logError(req, "Belief creation failed", startTime, error, {
      id: req.body?.id,
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
  const startTime = logStart(req, "Belief deletion requested", {
    id: req.body?.id,
  });
  try {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logWarn(req, "Belief deletion validation failed", startTime, {
        errors: errors.array(),
      });
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const { id } = req.body;

    const { status, data } = await executeBeliefOperation(
      null,
      config.DELETE,
      id
    );

    cacheUtils.clear();

    logSuccess(req, "Belief deletion completed", startTime, {
      id,
      status,
    });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: `Causation belief ${id} deleted successfully`,
      data: data || "Operation completed",
    });
  } catch (error) {
    logError(req, "Belief deletion failed", startTime, error, {
      id: req.body?.id,
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
  const requestId =
    req.requestId || `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  req.requestId = requestId;
  const startTime = logStart(req, "Belief retrieval requested", {
    id: req.body?.id,
    uri: req.body?.uri,
  });

  try {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logWarn(req, "Belief retrieval validation failed", startTime, {
        errors: errors.array(),
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
        requestId,
      });
    }

    const { id, uri } = req.body;

    const beliefUri = uri
      ? uri
      : id.includes("CB")
      ? `${DATA_PREFIX}${id}`
      : `${DATA_PREFIX}CB${id}`;

    const { status, head_vars, bindings, raw, graph, graphsConsidered } =
      await utils.getBeliefData(
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
      const data =
        buildBeliefResponse(bindings, {
          graph,
          graphsConsidered,
        }) || {};

      logSuccess(req, "Belief retrieval completed", startTime, {
        id,
        beliefUri,
        status,
        responseTime,
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
      logWarn(req, "Belief retrieval yielded no results", startTime, {
        id,
        beliefUri,
        status,
        responseTime,
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

    logError(req, "Belief retrieval failed", startTime, error, {
      id: req.body?.id,
      uri: req.body?.uri,
      responseTime,
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
  const startTime = logStart(req, "Belief service health check");

  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "causation-beliefs",
    timestamp: new Date().toISOString(),
    routes: Object.keys(ROUTE_MAPPINGS).length,
  });

  logSuccess(req, "Belief service health reported", startTime, {
    routes: Object.keys(ROUTE_MAPPINGS).length,
  });
});

/**
 * Get belief types/info endpoint
 */
router.get("/types", (req, res) => {
  const startTime = logStart(req, "Belief types requested");

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

  logSuccess(req, "Belief types delivered", startTime, {
    totalTypes: types.length,
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
