/**
 * Guideline Routes
 * Handles TMR-based Clinical Implementation Guidelines (CIGs)
 * Enhanced with comprehensive validation, caching, and monitoring
 * Refactored to follow careAction, transition, and belief patterns
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
const { isValidId, escapeQuotes } = require("../lib/router_functs/route_helpers");

const parseGuidelineContent =
  typeof auxFuncts.parseGuidelineContent === "function"
    ? auxFuncts.parseGuidelineContent
    : async (guidelineText) => ({ raw: guidelineText });

const extractRecommendations =
  typeof auxFuncts.extractRecommendations === "function"
    ? auxFuncts.extractRecommendations
    : async () => [];

// Constants and Configuration
const DATA_PREFIX = "http://anonymous.org/data/";
const VOCAB_PREFIX = "http://anonymous.org/vocab/";
const RDF_TYPE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

const GUIDELINE_PREDICATES = Object.freeze({
  CONTENT: `${VOCAB_PREFIX}hasContent`,
  LABEL: `${VOCAB_PREFIX}hasLabel`,
  VERSION: `${VOCAB_PREFIX}hasVersion`,
  CREATED_AT: `${VOCAB_PREFIX}createdAt`,
  UPDATED_AT: `${VOCAB_PREFIX}updatedAt`,
  TITLE: `${VOCAB_PREFIX}hasTitle`,
  AUTHOR: `${VOCAB_PREFIX}hasAuthor`,
  ORGANIZATION: `${VOCAB_PREFIX}hasOrganization`,
});

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

// Route mappings with metadata
const ROUTE_MAPPINGS = Object.freeze({
  "": {
    description: "Clinical Implementation Guideline (CIG)",
    maxContentLength: 50000,
    minContentLength: 10,
    supportsVersioning: true,
  },
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

function buildGuidelineSubject(guidelineId) {
  return `data:CIG${guidelineId}`;
}

function serializePredicates(subject, predicateObjects) {
  if (!predicateObjects || predicateObjects.length === 0) {
    return "";
  }

  const lines = predicateObjects.map(([predicate, object], index) => {
    const suffix = index === predicateObjects.length - 1 ? " ." : " ;";
    const indent = index === 0 ? "" : "  ";
    return `${indent}${predicate} ${object}${suffix}`;
  });

  lines[0] = `${subject} ${lines[0]}`;
  return lines.join("\n");
}

function normalizeGuidelineMetadata(metadata = {}, { isUpdate = false } = {}) {
  const safeMeta =
    metadata && typeof metadata === "object" ? metadata : {};
  const hasOwn = (prop) => Object.prototype.hasOwnProperty.call(safeMeta, prop);
  const trim = (value) => (typeof value === "string" ? value.trim() : undefined);

  const provided = {
    version: hasOwn("version"),
    title: hasOwn("title"),
    author: hasOwn("author"),
    organization: hasOwn("organization"),
    createdAt: hasOwn("created_at") || hasOwn("createdAt"),
  };

  const values = {
    version: provided.version
      ? trim(safeMeta.version)
      : isUpdate
      ? undefined
      : "1.0.0",
    title: provided.title ? trim(safeMeta.title) : undefined,
    author: provided.author ? trim(safeMeta.author) : undefined,
    organization: provided.organization ? trim(safeMeta.organization) : undefined,
    createdAt: provided.createdAt
      ? trim(safeMeta.created_at || safeMeta.createdAt)
      : undefined,
  };

  if (!isUpdate) {
    if (!values.version) {
      values.version = "1.0.0";
    }
    if (!values.createdAt) {
      values.createdAt = new Date().toISOString();
      provided.createdAt = true;
    }
  }

  if (isUpdate) {
    values.updatedAt = new Date().toISOString();
    provided.updatedAt = true;
  }

  return { values, provided };
}

function buildGuidelineInsertTriples({
  guidelineId,
  content,
  metadataValues,
  includeCreatedAt = false,
  includeUpdatedAt = false,
}) {
  const subject = buildGuidelineSubject(guidelineId);
  const predicateObjects = [
    ["a", "vocab:ClinicalGuideline , owl:NamedIndividual"],
    ["vocab:hasLabel", `"${escapeQuotes(guidelineId)}"`],
    ["vocab:hasContent", `"${escapeQuotes(content)}"`],
  ];

  if (metadataValues.version) {
    predicateObjects.push([
      "vocab:hasVersion",
      `"${escapeQuotes(metadataValues.version)}"`,
    ]);
  }

  if (metadataValues.title) {
    predicateObjects.push([
      "vocab:hasTitle",
      `"${escapeQuotes(metadataValues.title)}"`,
    ]);
  }

  if (metadataValues.author) {
    predicateObjects.push([
      "vocab:hasAuthor",
      `"${escapeQuotes(metadataValues.author)}"`,
    ]);
  }

  if (metadataValues.organization) {
    predicateObjects.push([
      "vocab:hasOrganization",
      `"${escapeQuotes(metadataValues.organization)}"`,
    ]);
  }

  if (includeCreatedAt && metadataValues.createdAt) {
    predicateObjects.push([
      "vocab:createdAt",
      `"${escapeQuotes(metadataValues.createdAt)}"`,
    ]);
  }

  if (includeUpdatedAt && metadataValues.updatedAt) {
    predicateObjects.push([
      "vocab:updatedAt",
      `"${escapeQuotes(metadataValues.updatedAt)}"`,
    ]);
  }

  return serializePredicates(subject, predicateObjects);
}

function buildGuidelineUpdateStatement({
  guidelineId,
  content,
  metadataValues = {},
  metadataProvided = {},
}) {
  const subject = buildGuidelineSubject(guidelineId);
  const deleteClauses = [
    `${subject} vocab:hasContent ?oldContent .`,
  ];
  const whereClauses = [
    `OPTIONAL { ${subject} vocab:hasContent ?oldContent . }`,
  ];
  const predicateObjects = [
    ["vocab:hasContent", `"${escapeQuotes(content)}"`],
  ];

  if (metadataValues.version !== undefined) {
    deleteClauses.push(`${subject} vocab:hasVersion ?oldVersion .`);
    whereClauses.push(
      `OPTIONAL { ${subject} vocab:hasVersion ?oldVersion . }`
    );
    if (metadataValues.version) {
      predicateObjects.push([
        "vocab:hasVersion",
        `"${escapeQuotes(metadataValues.version)}"`,
      ]);
    }
  }

  if (metadataProvided.title) {
    deleteClauses.push(`${subject} vocab:hasTitle ?oldTitle .`);
    whereClauses.push(
      `OPTIONAL { ${subject} vocab:hasTitle ?oldTitle . }`
    );
    if (metadataValues.title) {
      predicateObjects.push([
        "vocab:hasTitle",
        `"${escapeQuotes(metadataValues.title)}"`,
      ]);
    }
  }

  if (metadataProvided.author) {
    deleteClauses.push(`${subject} vocab:hasAuthor ?oldAuthor .`);
    whereClauses.push(
      `OPTIONAL { ${subject} vocab:hasAuthor ?oldAuthor . }`
    );
    if (metadataValues.author) {
      predicateObjects.push([
        "vocab:hasAuthor",
        `"${escapeQuotes(metadataValues.author)}"`,
      ]);
    }
  }

  if (metadataProvided.organization) {
    deleteClauses.push(`${subject} vocab:hasOrganization ?oldOrganization .`);
    whereClauses.push(
      `OPTIONAL { ${subject} vocab:hasOrganization ?oldOrganization . }`
    );
    if (metadataValues.organization) {
      predicateObjects.push([
        "vocab:hasOrganization",
        `"${escapeQuotes(metadataValues.organization)}"`,
      ]);
    }
  }

  if (metadataProvided.createdAt) {
    deleteClauses.push(`${subject} vocab:createdAt ?oldCreatedAt .`);
    whereClauses.push(
      `OPTIONAL { ${subject} vocab:createdAt ?oldCreatedAt . }`
    );
    if (metadataValues.createdAt) {
      predicateObjects.push([
        "vocab:createdAt",
        `"${escapeQuotes(metadataValues.createdAt)}"`,
      ]);
    }
  }

  if (metadataValues.updatedAt) {
    deleteClauses.push(`${subject} vocab:updatedAt ?oldUpdatedAt .`);
    whereClauses.push(
      `OPTIONAL { ${subject} vocab:updatedAt ?oldUpdatedAt . }`
    );
    predicateObjects.push([
      "vocab:updatedAt",
      `"${escapeQuotes(metadataValues.updatedAt)}"`,
    ]);
  }

  const insertBlock = serializePredicates(subject, predicateObjects);

  return `
    DELETE {
      ${deleteClauses.join("\n      ")}
    }
    INSERT {
      ${insertBlock}
    }
    WHERE {
      ${whereClauses.join("\n      ")}
    }
  `;
}

function buildGuidelineDeleteStatement(guidelineId) {
  const subject = buildGuidelineSubject(guidelineId);
  return `
    DELETE {
      ${subject} ?p ?o .
    }
    WHERE {
      ${subject} ?p ?o .
    }
  `;
}

function extractGuidelineIdFromUri(uri) {
  if (!uri) return null;
  if (uri.startsWith(`${DATA_PREFIX}CIG`)) {
    return uri.substring(`${DATA_PREFIX}CIG`.length);
  }
  if (uri.startsWith(DATA_PREFIX)) {
    return uri.substring(DATA_PREFIX.length);
  }
  if (uri.startsWith("data:CIG")) {
    return uri.substring("data:CIG".length);
  }
  return uri;
}

function parseGuidelineBindings(guidelineId, bindings = []) {
  const guideline = {
    id: guidelineId,
    uri: `${DATA_PREFIX}CIG${guidelineId}`,
    label: guidelineId,
    content: "",
    version: undefined,
    metadata: {},
    types: [],
    raw: [],
  };

  bindings.forEach((binding) => {
    const predicate = binding.p?.value || binding.predicate?.value;
    const object = binding.o || binding.object;

    if (!predicate || !object) {
      return;
    }

    const value = object.value;

    switch (predicate) {
      case GUIDELINE_PREDICATES.CONTENT:
        guideline.content = value;
        break;
      case GUIDELINE_PREDICATES.LABEL:
        guideline.label = value;
        break;
      case GUIDELINE_PREDICATES.VERSION:
        guideline.version = value;
        guideline.metadata.version = value;
        break;
      case GUIDELINE_PREDICATES.CREATED_AT:
        guideline.metadata.created_at = value;
        guideline.metadata.createdAt = value;
        break;
      case GUIDELINE_PREDICATES.UPDATED_AT:
        guideline.metadata.updated_at = value;
        guideline.metadata.updatedAt = value;
        break;
      case GUIDELINE_PREDICATES.TITLE:
        guideline.metadata.title = value;
        break;
      case GUIDELINE_PREDICATES.AUTHOR:
        guideline.metadata.author = value;
        break;
      case GUIDELINE_PREDICATES.ORGANIZATION:
        guideline.metadata.organization = value;
        break;
      case RDF_TYPE:
        guideline.types.push(value);
        break;
      default:
        guideline.metadata.extra ??= [];
        guideline.metadata.extra.push({ predicate, value });
    }

    guideline.raw.push({ predicate, object });
  });

  return guideline;
}

async function fetchGuidelineById(guidelineId) {
  try {
    const subject = buildGuidelineSubject(guidelineId);
    const query = `
      SELECT ?p ?o WHERE {
        ${subject} ?p ?o .
      }
    `;

    const results = await utils.sparqlJSONQuery("guidelines", query);
    const bindings = results?.results?.bindings ?? [];

    if (bindings.length === 0) {
      return { status: StatusCodes.NOT_FOUND };
    }

    const guideline = parseGuidelineBindings(guidelineId, bindings);
    return { status: StatusCodes.OK, guideline };
  } catch (error) {
    logger.error("Failed to fetch guideline by ID", {
      guidelineId,
      error: error.message,
    });

    return {
      status: StatusCodes.INTERNAL_SERVER_ERROR,
      error: error.message,
    };
  }
}

async function listGuidelines({ limit = 20, offset = 0 } = {}) {
  try {
    const query = `
      SELECT ?guideline ?label
             (SAMPLE(?content) AS ?contentValue)
             (SAMPLE(?version) AS ?versionValue)
             (SAMPLE(?createdAt) AS ?createdAtValue)
             (SAMPLE(?updatedAt) AS ?updatedAtValue)
             (SAMPLE(?title) AS ?titleValue)
             (SAMPLE(?author) AS ?authorValue)
             (SAMPLE(?organization) AS ?organizationValue)
      WHERE {
        ?guideline a vocab:ClinicalGuideline ;
                   vocab:hasLabel ?label ;
                   vocab:hasContent ?content .
        OPTIONAL { ?guideline vocab:hasVersion ?version . }
        OPTIONAL { ?guideline vocab:createdAt ?createdAt . }
        OPTIONAL { ?guideline vocab:updatedAt ?updatedAt . }
        OPTIONAL { ?guideline vocab:hasTitle ?title . }
        OPTIONAL { ?guideline vocab:hasAuthor ?author . }
        OPTIONAL { ?guideline vocab:hasOrganization ?organization . }
      }
      GROUP BY ?guideline ?label
      ORDER BY LCASE(?label)
      LIMIT ${limit}
      OFFSET ${offset}
    `;

    const countQuery = `
      SELECT (COUNT(DISTINCT ?guideline) AS ?total)
      WHERE {
        ?guideline a vocab:ClinicalGuideline .
      }
    `;

    const [dataResult, countResult] = await Promise.all([
      utils.sparqlJSONQuery("guidelines", query),
      utils.sparqlJSONQuery("guidelines", countQuery),
    ]);

    const bindings = dataResult?.results?.bindings ?? [];

    const guidelines = bindings.map((binding) => {
      const uri = binding.guideline?.value;
      const id = extractGuidelineIdFromUri(uri);
      const metadata = {};

      const version = binding.versionValue?.value;
      if (version) {
        metadata.version = version;
      }

      const createdAt = binding.createdAtValue?.value;
      if (createdAt) {
        metadata.created_at = createdAt;
        metadata.createdAt = createdAt;
      }

      const updatedAt = binding.updatedAtValue?.value;
      if (updatedAt) {
        metadata.updated_at = updatedAt;
        metadata.updatedAt = updatedAt;
      }

      const title = binding.titleValue?.value;
      if (title) {
        metadata.title = title;
      }

      const author = binding.authorValue?.value;
      if (author) {
        metadata.author = author;
      }

      const organization = binding.organizationValue?.value;
      if (organization) {
        metadata.organization = organization;
      }

      return {
        id,
        uri,
        label: binding.label?.value || id,
        content: binding.contentValue?.value || "",
        version: version || undefined,
        metadata,
      };
    });

    const totalCountBinding = countResult?.results?.bindings?.[0];
    const totalCount = totalCountBinding
      ? parseInt(totalCountBinding.total.value, 10)
      : guidelines.length;

    return {
      status: StatusCodes.OK,
      guidelines,
      totalCount,
    };
  } catch (error) {
    logger.error("Failed to list guidelines", { error: error.message });
    return {
      status: StatusCodes.INTERNAL_SERVER_ERROR,
      guidelines: [],
      totalCount: 0,
      error: error.message,
    };
  }
}

async function deleteGuideline(guidelineId) {
  const deleteQuery = buildGuidelineDeleteStatement(guidelineId);
  return utils.sparqlUpdate("guidelines", deleteQuery);
}

/**
 * Enhanced validation rules for guideline operations
 */
const getGuidelineValidationRules = (routeConfig) => [
  body("guideline")
    .notEmpty()
    .withMessage("Guideline content is required")
    .isString()
    .withMessage("Guideline must be a string")
    .isLength({ 
      min: routeConfig.minContentLength, 
      max: routeConfig.maxContentLength 
    })
    .withMessage(`Guideline must be between ${routeConfig.minContentLength} and ${routeConfig.maxContentLength} characters`),

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

function createGuidelineDefinition(requestBody) {
      const { guideline, filename, metadata = {} } = requestBody;

      const guidelineId =
        filename ||
        `guideline_${Date.now()}_${Math.random().toString(36).substr(2, 5)}`;

      const { values: metadataValues } = normalizeGuidelineMetadata(metadata);

      const sparqlTriples = buildGuidelineInsertTriples({
        guidelineId,
        content: guideline,
        metadataValues,
        includeCreatedAt: true,
      });

      const guidelineUri = `${DATA_PREFIX}CIG${guidelineId}`;

      logger.debug("Generated guideline definition", {
        guidelineId,
        guidelineUri,
        contentLength: guideline.length,
        definitionLength: sparqlTriples.length,
      });

      const createdMetadata = {
        version: metadataValues.version,
        created_at: metadataValues.createdAt,
        title: metadataValues.title,
        author: metadataValues.author,
        organization: metadataValues.organization,
      };

      return {
        definition: sparqlTriples,
        createdIds: {
          guidelineId,
          guidelineUri,
          metadata: createdMetadata,
        },
        metadataValues,
      };
    }

/**
 * Execute guideline operation (INSERT, UPDATE, or DELETE)
 */
async function executeGuidelineOperation(
  sparqlPayload,
  operationType,
  id
) {
  try {
    let sparqlStatement;

    if (operationType === "INSERT") {
      if (typeof sparqlPayload !== "string" || sparqlPayload.trim() === "") {
        throw new ErrorHandler(
          StatusCodes.BAD_REQUEST,
          "Invalid guideline insert payload"
        );
      }
      sparqlStatement = `INSERT DATA { ${sparqlPayload} }`;
    } else if (operationType === "UPDATE") {
      if (typeof sparqlPayload !== "string" || sparqlPayload.trim() === "") {
        throw new ErrorHandler(
          StatusCodes.BAD_REQUEST,
          "Invalid guideline update payload"
        );
      }
      sparqlStatement = sparqlPayload;
    } else if (operationType === "DELETE") {
      sparqlStatement = buildGuidelineDeleteStatement(id);
    } else {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        `Invalid operation type: ${operationType}`
      );
    }

    logger.debug("Executing guideline operation", {
      operation: operationType,
      id,
    });

    const result = await utils.sparqlUpdate("guidelines", sparqlStatement);

    if (result.status >= 400) {
      throw new ErrorHandler(
        result.status,
        `Guideline operation failed: ${result.data || result.result}`
      );
    }

    return result;
  } catch (error) {
    logger.error("Guideline operation failed", {
      operation: operationType,
      id,
      error: error.message,
    });
    throw error;
  }
}

/**
 * Enhanced create handler with comprehensive validation and error handling
 */
const createAddHandler = (routeConfig) => async (req, res) => {
  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Guideline creation validation failed", {
        errors: errors.array(),
        ip: req.ip,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const { guideline, filename, metadata = {} } = req.body;

    logger.info("Creating new guideline", {
      filename,
      guidelineLength: guideline.length,
      metadata,
      ip: req.ip,
    });

    // Parse and validate guideline structure
    let parsedGuideline;
    try {
      parsedGuideline = await parseGuidelineContent(guideline);
    } catch (parseError) {
      logger.error("Guideline parsing failed", {
        error: parseError.message,
        ip: req.ip,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Invalid guideline format",
        details: parseError.message,
      });
    }

    // Generate guideline definition
    const {
      definition: sparqlQuery,
      createdIds,
      metadataValues,
    } = createGuidelineDefinition(req.body);

    // Execute the operation
    const { status, data: result } = await executeGuidelineOperation(
      sparqlQuery,
      "INSERT",
      createdIds.guidelineId
    );

    // Invalidate related cache entries
    cacheUtils.invalidatePattern("list");
    cacheUtils.invalidatePattern("search");

    logger.info("Guideline created successfully", {
      guidelineId: createdIds.guidelineId,
      guidelineUri: createdIds.guidelineUri,
      status,
    });

    res.status(StatusCodes.CREATED).json({
      status: "success",
      message: `Guideline ${createdIds.guidelineId} created successfully`,
      data: {
        operation: result || "Operation completed",
        createdResources: createdIds,
        metadata: metadataValues,
        recommendations_count: parsedGuideline.recommendations?.length || 0,
      },
    });
  } catch (error) {
    logger.error("Failed to create guideline", {
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
      message: "Failed to create guideline",
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
};

/**
 * Enhanced endpoint to get a specific guideline
 */
router.get("/:id", [readLimiter, guidelineIdValidation], async (req, res) => {
  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const { id } = req.params;
    const includeRecommendationsParam = req.query.include_recommendations;
    const includeRecommendations =
      includeRecommendationsParam === undefined
        ? true
        : includeRecommendationsParam === "true" ||
          includeRecommendationsParam === true;

    logger.info("Retrieving guideline", {
      guidelineId: id,
      include_recommendations: includeRecommendations,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey(`/get/${id}`, {
      include_recommendations: includeRecommendations,
    });
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      logger.info("Returning cached guideline", {
        guidelineId: id,
        cached: true,
      });

      return res.status(StatusCodes.OK).json({
        ...cachedResult,
        cached: true,
      });
    }

    // Retrieve guideline from triple store
    const guidelineResult = await fetchGuidelineById(id);
    const { status, guideline, error: retrievalError } = guidelineResult;

    if (status === StatusCodes.NOT_FOUND) {
      logger.warn("Guideline not found", {
        guidelineId: id,
      });

      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Guideline not found",
      });
    }

    if (status !== StatusCodes.OK) {
      throw new ErrorHandler(
        status || StatusCodes.INTERNAL_SERVER_ERROR,
        retrievalError || "Failed to retrieve guideline from triple store"
      );
    }

    // Process guideline data
    let processedGuideline = guideline;

    if (includeRecommendations) {
      const recommendations = await extractRecommendations(guideline);
      processedGuideline = {
        ...guideline,
        recommendations: recommendations || [],
      };
    }

    const responseData = {
      status: "success",
      data: processedGuideline,
    };

    // Cache the result
    cacheUtils.set(cacheKey, responseData, CACHE_TTL);

    logger.info("Guideline retrieved successfully", {
      guidelineId: id,
      cached: false,
    });

    res.status(StatusCodes.OK).json({
      ...responseData,
      cached: false,
    });
  } catch (error) {
    logger.error("Failed to retrieve guideline", {
      guidelineId: req.params.id,
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
      message: "Failed to retrieve guideline",
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced update handler with validation
 */
const createUpdateHandler = (routeConfig) => async (req, res) => {
  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const { id } = req.params;
    const { guideline, metadata = {} } = req.body;

    logger.info("Updating guideline", {
      guidelineId: id,
      guidelineLength: guideline.length,
      metadata,
      ip: req.ip,
    });

    // Check if guideline exists
    const { status: existsStatus } = await fetchGuidelineById(id);

    if (existsStatus === StatusCodes.NOT_FOUND) {
      logger.warn("Cannot update non-existent guideline", {
        guidelineId: id,
      });

      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Guideline not found",
      });
    }

    if (existsStatus !== StatusCodes.OK) {
      throw new ErrorHandler(
        existsStatus || StatusCodes.INTERNAL_SERVER_ERROR,
        "Failed to verify guideline existence"
      );
    }

    // Parse and validate updated guideline
    let parsedGuideline;
    try {
      parsedGuideline = await parseGuidelineContent(guideline);
    } catch (parseError) {
      logger.error("Updated guideline parsing failed", {
        guidelineId: id,
        error: parseError.message,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Invalid guideline format",
        details: parseError.message,
      });
    }

    const { values: metadataValues, provided: metadataProvided } =
      normalizeGuidelineMetadata(metadata, { isUpdate: true });

    const updateStatement = buildGuidelineUpdateStatement({
      guidelineId: id,
      content: guideline,
      metadataValues,
      metadataProvided,
    });

    const { status, data: result } = await executeGuidelineOperation(
      updateStatement,
      "UPDATE",
      id
    );

    // Invalidate cache entries
    cacheUtils.invalidatePattern(id);
    cacheUtils.invalidatePattern("list");
    cacheUtils.invalidatePattern("search");

    logger.info("Guideline updated successfully", {
      guidelineId: id,
      status,
    });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: `Guideline ${id} updated successfully`,
      data: {
        operation: result || "Operation completed",
        id,
        metadata: Object.fromEntries(
          Object.entries({
            version: metadataValues.version,
            title: metadataValues.title,
            author: metadataValues.author,
            organization: metadataValues.organization,
            created_at: metadataValues.createdAt,
            updated_at: metadataValues.updatedAt,
          }).filter(([, value]) => value !== undefined)
        ),
        recommendations_count: parsedGuideline.recommendations?.length || 0,
      },
    });
  } catch (error) {
    logger.error("Failed to update guideline", {
      guidelineId: req.params.id,
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
      message: "Failed to update guideline",
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

    const { id } = req.params;

    logger.info("Deleting guideline", {
      guidelineId: id,
      ip: req.ip,
    });

    // Check if guideline exists
    const { status: existsStatus } = await fetchGuidelineById(id);

    if (existsStatus === StatusCodes.NOT_FOUND) {
      logger.warn("Cannot delete non-existent guideline", {
        guidelineId: id,
      });

      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Guideline not found",
      });
    }

    if (existsStatus !== StatusCodes.OK) {
      throw new ErrorHandler(
        existsStatus || StatusCodes.INTERNAL_SERVER_ERROR,
        "Failed to verify guideline existence"
      );
    }

    // Delete guideline from triple store
    const { status } = await executeGuidelineOperation(null, "DELETE", id);

    // Invalidate cache entries
    cacheUtils.invalidatePattern(id);
    cacheUtils.invalidatePattern("list");
    cacheUtils.invalidatePattern("search");

    logger.info("Guideline deleted successfully", {
      guidelineId: id,
      status,
    });

    res.status(StatusCodes.OK).json({
      status: "success",
      message: `Guideline ${id} deleted successfully`,
      data: {
        id,
        deleted_at: new Date().toISOString(),
      },
    });
  } catch (error) {
    logger.error("Failed to delete guideline", {
      guidelineId: req.params.id,
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
      message: "Failed to delete guideline",
    });
  }
};

/**
 * Enhanced endpoint to list all guidelines
 */
router.get("/", [readLimiter, queryValidationRules], async (req, res) => {
  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const {
      limit = 20,
      offset = 0,
      include_recommendations = false,
      include_metadata = true,
    } = req.query;

    const includeRecommendations =
      include_recommendations === "true" || include_recommendations === true;
    const includeMetadata =
      include_metadata === "true" || include_metadata === true;

    const queryParams = {
      limit,
      offset,
      include_recommendations: includeRecommendations,
      include_metadata: includeMetadata,
    };

    logger.info("Listing guidelines", {
      queryParams,
      ip: req.ip,
    });

    // Check cache
    const cacheKey = cacheUtils.generateKey("/list", queryParams);
    const cachedResult = cacheUtils.get(cacheKey);

    if (cachedResult) {
      logger.info("Returning cached guidelines list", {
        cached: true,
      });

      return res.status(StatusCodes.OK).json({
        ...cachedResult,
        cached: true,
      });
    }

    // Retrieve guidelines from triple store
    const listResult = await listGuidelines({
      limit: parseInt(limit, 10),
      offset: parseInt(offset, 10),
    });

    if (listResult.status !== StatusCodes.OK) {
      throw new ErrorHandler(
        listResult.status || StatusCodes.INTERNAL_SERVER_ERROR,
        "Failed to retrieve guidelines from triple store"
      );
    }

    let guidelines = listResult.guidelines || [];

    if (!includeMetadata) {
      guidelines = guidelines.map(({ metadata, ...rest }) => rest);
    }

    if (includeRecommendations) {
      guidelines = guidelines.map((item) => ({
        ...item,
        recommendations: item.recommendations || [],
      }));
    }

    const totalCount = listResult.totalCount || guidelines.length;

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

    logger.info("Guidelines listed successfully", {
      totalCount: totalCount || 0,
      returnedCount: guidelines?.length || 0,
      cached: false,
    });

    res.status(StatusCodes.OK).json({
      ...responseData,
      cached: false,
    });
  } catch (error) {
    logger.error("Failed to list guidelines", {
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
      message: "Failed to list guidelines",
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Alias endpoint for /all - retrieves all guidelines (same as GET /)
 */
router.get("/all", [readLimiter, queryValidationRules], async (req, res) => {
  // Forward to the main list endpoint
  req.url = "/";
  router.handle(req, res);
});

/**
 * Complex operation: Get guideline recommendations
 */
router.get(
  "/:id/recommendations",
  [complexOperationLimiter, guidelineIdValidation],
  async (req, res) => {
    try {
      // Check validation results
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { id } = req.params;

      logger.info("Retrieving guideline recommendations", {
        guidelineId: id,
        ip: req.ip,
      });

      // Check cache
      const cacheKey = cacheUtils.generateKey(`/recommendations/${id}`);
      const cachedResult = cacheUtils.get(cacheKey);

      if (cachedResult) {
        return res.status(StatusCodes.OK).json({
          ...cachedResult,
          cached: true,
        });
      }

      // Get guideline and extract recommendations
      const { status, guideline, error: retrievalError } =
        await fetchGuidelineById(id);

      if (status === StatusCodes.NOT_FOUND) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Guideline not found",
        });
      }

      if (status !== StatusCodes.OK) {
        throw new ErrorHandler(
          status || StatusCodes.INTERNAL_SERVER_ERROR,
          retrievalError || "Failed to retrieve guideline"
        );
      }

      const recommendations = await extractRecommendations(guideline);

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

      logger.info("Guideline recommendations retrieved successfully", {
        guidelineId: id,
        recommendationsCount: recommendations.length,
      });

      res.status(StatusCodes.OK).json({
        ...responseData,
        cached: false,
      });
    } catch (error) {
      logger.error("Failed to retrieve guideline recommendations", {
        guidelineId: req.params.id,
        error: error.message,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to retrieve guideline recommendations",
      });
    }
  }
);

/**
 * Add a clinical recommendation to a guideline
 * Creates a recommendation nanopublication with belief, care action, and metadata
 */
router.post("/rec/add", [writeLimiter], async (req, res) => {
  try {
    const {
      cig_id,
      id,
      belief_id,
      careAction_id,
      label,
      contribution,
      isRecommended,
      derivedFrom,
      author,
      strength,
    } = req.body;

    // Validate required fields
    if (!cig_id || !id || !belief_id || !careAction_id || !label || !author || !strength) {
      logger.warn("Recommendation creation validation failed - missing required fields", {
        provided: Object.keys(req.body),
        ip: req.ip,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Missing required fields",
        required: ["cig_id", "id", "belief_id", "careAction_id", "label", "author", "strength"],
      });
    }

    logger.info("Creating clinical recommendation", {
      cig_id,
      id,
      belief_id,
      ip: req.ip,
    });

    // Build recommendation nanopublication
    const recId = `data:Rec${cig_id}-${id}`;
    const date = new Date().toISOString();
    
    // Handle derivedFrom sources
    let sources = "";
    if (derivedFrom) {
      sources = `  prov:wasDerivedFrom  `;
      derivedFrom.split(",").forEach((code) => {
        sources += ` <${code.trim()}> ,`;
      });
      // Remove last comma
      sources = sources.substring(0, sources.length - 1);
      sources += " .";
    }

    // Build nanopublication structure
    const head = `${recId}_head { 
      ${recId}_head
        a nanopub:Nanopublication ;
        nanopub:hasAssertion ${recId} ;
        nanopub:hasProvenance ${recId}_provenance ;
        nanopub:hasPublicationInfo ${recId}_publicationinfo .
    }`;

    const assertion = `${recId} {
      ${recId} a vocab:ClinicalRecommendation ;
        rdfs:label '''${escapeQuotes(label)}'''@en ;
        vocab:aboutExecutionOf data:ActAdminister${careAction_id} ;
        vocab:partOf data:CIG-${cig_id} ;
        vocab:basedOn data:CB${belief_id} ;
        vocab:strength '''${escapeQuotes(strength)}''' .
      data:CB${belief_id} vocab:contribution '''${escapeQuotes(contribution || 'positive')}''' .
    }`;

    const provenance = `${recId}_provenance {
      ${recId}_provenance
        a oa:Annotation ;
        oa:hasBody ${recId} ;
        oa:hasTarget [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
      ${recId}
        ${sources}
    }`;

    const publication = `${recId}_publicationinfo {
      ${recId}_head
        prov:generatedAtTime "${date}"^^xsd:dateTime ;
        prov:wasAttributedTo data:${escapeQuotes(author)} .
    }`;

    const completeDefinition = `GRAPH ${head} GRAPH ${assertion} GRAPH ${provenance} GRAPH ${publication}`;
    const sparqlQuery = `INSERT DATA { ${completeDefinition} }`;

    logger.debug("Generated recommendation SPARQL", {
      recId,
      queryLength: sparqlQuery.length,
    });

    // Execute the SPARQL update
    const { status, data } = await utils.sparqlUpdate(`CIG-${cig_id}`, sparqlQuery);

    if (status >= 400) {
      throw new ErrorHandler(
        status,
        `Failed to create recommendation: ${data || 'Unknown error'}`
      );
    }

    // Invalidate related cache entries
    cacheUtils.invalidatePattern(cig_id);
    cacheUtils.invalidatePattern("list");

    logger.info("Clinical recommendation created successfully", {
      cig_id,
      recId,
      status,
    });

    res.status(StatusCodes.CREATED).json({
      status: "success",
      message: `Recommendation ${id} created successfully in guideline ${cig_id}`,
      data: {
        operation: data || "Operation completed",
        recId: `Rec${cig_id}-${id}`,
        recUri: recId,
        cig_id: `CIG-${cig_id}`,
        belief_id: `CB${belief_id}`,
        careAction_id: `ActAdminister${careAction_id}`,
      },
    });
  } catch (error) {
    logger.error("Failed to create clinical recommendation", {
      error: error.message,
      stack: error.stack,
      body: req.body,
    });

    if (error instanceof ErrorHandler) {
      return res.status(error.statusCode).json({
        status: "error",
        message: error.message,
      });
    }

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to create clinical recommendation",
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
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
    service: "clinical-implementation-guidelines",
    timestamp: new Date().toISOString(),
    routes: Object.keys(ROUTE_MAPPINGS).length,
    cache: cacheStats,
  });
});

/**
 * Get guideline types/info endpoint
 */
router.get("/types", (req, res) => {
  const types = Object.entries(ROUTE_MAPPINGS).map(([route, config]) => ({
    route: route || "/",
    description: config.description,
    maxContentLength: config.maxContentLength,
    minContentLength: config.minContentLength,
    supportsVersioning: config.supportsVersioning,
  }));

  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      available_types: types,
      total_count: types.length,
      service: "clinical-implementation-guidelines",
      description: "Manages TMR-based Clinical Implementation Guidelines (CIGs)",
    },
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

/**
 * Register all routes dynamically with enhanced validation
 */
Object.entries(ROUTE_MAPPINGS).forEach(([route, routeConfig]) => {
  const validationRules = getGuidelineValidationRules(routeConfig);

  // POST endpoint (create guideline)
  router.post(
    `${route}/`,
    writeLimiter,
    validationRules,
    createAddHandler(routeConfig)
  );

  // PUT endpoint (update guideline)
  router.put(
    `${route}/:id`,
    writeLimiter,
    guidelineIdValidation,
    validationRules,
    createUpdateHandler(routeConfig)
  );

  // DELETE endpoint (delete guideline)
  router.delete(
    `${route}/:id`,
    writeLimiter,
    guidelineIdValidation,
    createDeleteHandler()
  );
});

module.exports = router;
module.exports.cacheUtils = cacheUtils;
