/**
 * Statement Routes
 * Handles CRUD operations for TMR-based clinical statements
 * Refactored to align with other route patterns
 */

const express = require("express");
const { body, param, validationResult } = require("express-validator");
const { StatusCodes } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const logger = require("../config/winston");
const { escapeQuotes } = require("../lib/router_functs/route_helpers");

const DATA_PREFIX = "http://anonymous.org/data/";
const STATEMENTS_DATASET = "statements";
const DEFAULT_VERSION = "1.0.0";
const DEFAULT_AGENT_URI = "data:SystemAgent";

const STATEMENT_PREDICATES = Object.freeze({
  TEXT: "vocab:hasStatementText",
  TYPE: "vocab:statementType",
  CONFIDENCE: "vocab:confidence",
  SOURCE: "vocab:source",
  PATIENT: "vocab:patientId",
  CLINICIAN: "vocab:clinicianId",
  CONTEXT: "vocab:context",
  TAG: "vocab:hasTag",
  CREATED_AT: "vocab:createdAt",
  UPDATED_AT: "vocab:updatedAt",
  VERSION: "vocab:version",
  CREATED_BY: "vocab:createdBy",
  UPDATED_BY: "vocab:updatedBy",
});

const readLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 150,
  message: {
    status: "error",
    message: "Too many read requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const writeLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 50,
  message: {
    status: "error",
    message: "Too many write requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const cache = new Map();
const CACHE_TTL = 300;
const CACHE_PREFIX = "statement:";

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

    if (cached.expiry < Date.now()) {
      cache.delete(key);
      return null;
    }

    logger.debug("Cache hit for statement", { key });
    return cached.data;
  },

  set(key, data, ttlSeconds = CACHE_TTL) {
    const expiry = Date.now() + ttlSeconds * 1000;
    cache.set(key, { data, expiry });

    if (cache.size > 100) {
      const oldestKey = cache.keys().next().value;
      cache.delete(oldestKey);
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

  clear(pattern = "") {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (!pattern || key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }

    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Statement cache cleared", {
      pattern: pattern || "*",
      deletedCount: keysToDelete.length,
    });
  },

  getStats() {
    let totalEntries = 0;
    let expiredEntries = 0;
    const now = Date.now();

    for (const [, value] of cache.entries()) {
      totalEntries += 1;
      if (value.expiry < now) {
        expiredEntries += 1;
      }
    }

    return {
      totalEntries,
      expiredEntries,
      activeEntries: totalEntries - expiredEntries,
      memoryUsage: process.memoryUsage().heapUsed,
    };
  },
};

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

  body("metadata.version")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Version must be a non-empty string up to 100 characters"),

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

function generateRequestId() {
  return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

function sanitizeIdentifier(value, fallbackPrefix = "ST") {
  if (!value) {
    return `${fallbackPrefix}${Date.now()}`;
  }
  return String(value)
    .trim()
    .replace(/[^a-zA-Z0-9_-]/g, "_")
    .replace(/^$/, `${fallbackPrefix}${Date.now()}`);
}

function generateStatementId() {
  return `ST${Date.now()}${Math.random().toString(36).substr(2, 5)}`;
}

function normalizeTags(tagsInput) {
  if (!Array.isArray(tagsInput)) return [];
  const unique = new Set();
  tagsInput.forEach((tag) => {
    if (typeof tag !== "string") return;
    const trimmed = tag.trim();
    if (trimmed) unique.add(trimmed);
  });
  return Array.from(unique);
}

function makeAgentUri(value, fallback = DEFAULT_AGENT_URI) {
  if (!value) return fallback;
  return `data:${sanitizeIdentifier(value, "Agent")}`;
}

function normalizeStatementMetadata(metadata = {}, { isUpdate = false, ip }) {
  const safeMeta = metadata && typeof metadata === "object" ? { ...metadata } : {};
  const nowIso = new Date().toISOString();

  const createdAt = safeMeta.created_at || safeMeta.createdAt || (!isUpdate ? nowIso : undefined);
  const updatedAt = isUpdate ? nowIso : safeMeta.updated_at || safeMeta.updatedAt;
  const version = safeMeta.version || DEFAULT_VERSION;
  const patientId = safeMeta.patient_id || safeMeta.patientId || undefined;
  const clinicianId = safeMeta.clinician_id || safeMeta.clinicianId || undefined;
  const context = safeMeta.context || undefined;

  const createdByInput = safeMeta.created_by || safeMeta.createdBy || ip;
  const updatedByInput = safeMeta.updated_by || safeMeta.updatedBy || (isUpdate ? ip : undefined);

  const createdByUri = makeAgentUri(createdByInput, DEFAULT_AGENT_URI);
  const updatedByUri = updatedByInput ? makeAgentUri(updatedByInput, createdByUri) : null;

  return {
    triples: {
      createdAt,
      updatedAt,
      version,
      patientId,
      clinicianId,
      context,
      createdByUri,
      updatedByUri,
    },
    response: {
      created_at: createdAt,
      updated_at: updatedAt,
      version,
      patient_id: patientId,
      clinician_id: clinicianId,
      context,
      created_by: createdByInput || undefined,
      updated_by: updatedByInput || undefined,
    },
  };
}

function buildGraphUris(statementId) {
  const base = `data:${statementId}`;
  return {
    assertion: base,
    head: `${base}_head`,
    provenance: `${base}_provenance`,
    publication: `${base}_publicationinfo`,
  };
}

function serializePredicates(subject, predicateObjects = []) {
  if (!predicateObjects.length) return "";
  const lines = predicateObjects.map(([predicate, object], index) => {
    const suffix = index === predicateObjects.length - 1 ? " ." : " ;";
    const indent = index === 0 ? "" : "  ";
    return `${indent}${predicate} ${object}${suffix}`;
  });
  lines[0] = `${subject} ${lines[0]}`;
  return lines.join("\n");
}

function buildStatementDefinition(payload, { isUpdate = false, ip }) {
  const {
    id,
    text,
    type,
    confidence,
    source,
    metadata = {},
    tags = [],
  } = payload;

  if (!text || !type) {
    throw new ErrorHandler(StatusCodes.BAD_REQUEST, "Statement text and type are required");
  }

  const statementId = id ? sanitizeIdentifier(id) : generateStatementId();
  const statementUri = `${DATA_PREFIX}${statementId}`;
  const graphUris = buildGraphUris(statementId);

  const normalizedTags = normalizeTags(tags);
  const confidenceValue = confidence === undefined ? null : Math.min(Math.max(Number(confidence), 0), 1);
  const confidenceLiteral =
    confidenceValue === null || Number.isNaN(confidenceValue)
      ? null
      : `"${confidenceValue.toFixed(3)}"^^xsd:decimal`;
  const normalizedSource =
    typeof source === "string" && source.trim().length > 0 ? source.trim() : undefined;

  const { triples, response } = normalizeStatementMetadata(metadata, { isUpdate, ip });

  const predicateObjects = [
    ["a", "vocab:ClinicalStatement , owl:NamedIndividual"],
    [STATEMENT_PREDICATES.TEXT, `"${escapeQuotes(text)}"`],
    [STATEMENT_PREDICATES.TYPE, `"${escapeQuotes(type)}"`],
  ];

  if (confidenceLiteral) {
    predicateObjects.push([STATEMENT_PREDICATES.CONFIDENCE, confidenceLiteral]);
  }

  if (normalizedSource) {
    predicateObjects.push([STATEMENT_PREDICATES.SOURCE, `"${escapeQuotes(normalizedSource)}"`]);
  }

  if (triples.patientId) {
    predicateObjects.push([STATEMENT_PREDICATES.PATIENT, `"${escapeQuotes(triples.patientId)}"`]);
  }

  if (triples.clinicianId) {
    predicateObjects.push([STATEMENT_PREDICATES.CLINICIAN, `"${escapeQuotes(triples.clinicianId)}"`]);
  }

  if (triples.context) {
    predicateObjects.push([STATEMENT_PREDICATES.CONTEXT, `"${escapeQuotes(triples.context)}"`]);
  }

  if (triples.createdAt) {
    predicateObjects.push([STATEMENT_PREDICATES.CREATED_AT, `"${escapeQuotes(triples.createdAt)}"`]);
  }

  if (triples.updatedAt) {
    predicateObjects.push([STATEMENT_PREDICATES.UPDATED_AT, `"${escapeQuotes(triples.updatedAt)}"`]);
  }

  if (triples.version) {
    predicateObjects.push([STATEMENT_PREDICATES.VERSION, `"${escapeQuotes(triples.version)}"`]);
  }

  if (triples.createdByUri) {
    predicateObjects.push([STATEMENT_PREDICATES.CREATED_BY, triples.createdByUri]);
  }

  if (triples.updatedByUri) {
    predicateObjects.push([STATEMENT_PREDICATES.UPDATED_BY, triples.updatedByUri]);
  }

  normalizedTags.forEach((tag) => {
    predicateObjects.push([STATEMENT_PREDICATES.TAG, `"${escapeQuotes(tag)}"`]);
  });

  const headSegment = serializePredicates(graphUris.head, [
    ["a", "nanopub:Nanopublication"],
    ["nanopub:hasAssertion", graphUris.assertion],
    ["nanopub:hasProvenance", graphUris.provenance],
    ["nanopub:hasPublicationInfo", graphUris.publication],
  ]);

  const assertionSegment = serializePredicates(`data:${statementId}`, predicateObjects);

  const provenanceSegment = serializePredicates(graphUris.provenance, [
    ["a", "oa:Annotation"],
    ["oa:hasBody", graphUris.assertion],
  ]);

  const publicationSegment = serializePredicates(graphUris.publication, [
    [
      "prov:generatedAtTime",
      `"${escapeQuotes(triples.updatedAt || triples.createdAt)}"^^xsd:dateTime`,
    ],
    ["prov:wasAttributedTo", triples.updatedByUri || triples.createdByUri || DEFAULT_AGENT_URI],
  ]);

  const graphSegments = [
    headSegment && `GRAPH ${graphUris.head} {\n${headSegment}\n}`,
    assertionSegment && `GRAPH ${graphUris.assertion} {\n${assertionSegment}\n}`,
    provenanceSegment && `GRAPH ${graphUris.provenance} {\n${provenanceSegment}\n}`,
    publicationSegment && `GRAPH ${graphUris.publication} {\n${publicationSegment}\n}`,
  ].filter(Boolean);

  return {
    statementId,
    statementUri,
    graphUris,
    payload: graphSegments.join("\n"),
    confidence: confidenceValue === null || Number.isNaN(confidenceValue) ? undefined : confidenceValue,
    tags: normalizedTags,
    metadata: {
      ...response,
      source: normalizedSource,
    },
  };
}

function buildDeleteClauses(graphUris) {
  const templates = [
    { graph: graphUris.assertion, vars: ["stmtS", "stmtP", "stmtO"] },
    { graph: graphUris.provenance, vars: ["provS", "provP", "provO"] },
    { graph: graphUris.publication, vars: ["pubS", "pubP", "pubO"] },
    { graph: graphUris.head, vars: ["headS", "headP", "headO"] },
  ];

  const deleteClauses = [];
  const whereClauses = [];

  templates.forEach(({ graph, vars: [s, p, o] }) => {
    deleteClauses.push(`GRAPH ${graph} { ?${s} ?${p} ?${o} . }`);
    whereClauses.push(`OPTIONAL { GRAPH ${graph} { ?${s} ?${p} ?${o} . } }`);
  });

  return { deleteClauses, whereClauses };
}

async function executeStatementOperation(definition, operationType) {
  let sparqlPayload;

  if (operationType === "INSERT") {
    sparqlPayload = `INSERT DATA {\n${definition.payload}\n}`;
  } else if (operationType === "UPDATE") {
    const { deleteClauses, whereClauses } = buildDeleteClauses(definition.graphUris);
    sparqlPayload = `
      DELETE {
        ${deleteClauses.join("\n        ")}
      }
      INSERT {
        ${definition.payload}
      }
      WHERE {
        ${whereClauses.join("\n        ")}
      }
    `;
  } else if (operationType === "DELETE") {
    const { deleteClauses, whereClauses } = buildDeleteClauses(definition.graphUris);
    sparqlPayload = `
      DELETE {
        ${deleteClauses.join("\n        ")}
      }
      WHERE {
        ${whereClauses.join("\n        ")}
      }
    `;
  } else {
    throw new ErrorHandler(StatusCodes.BAD_REQUEST, `Unsupported operation type: ${operationType}`);
  }

  try {
    const result = await utils.sparqlUpdate(STATEMENTS_DATASET, sparqlPayload);

    if (result.status >= 400) {
      throw new ErrorHandler(result.status, result.data || "Statement operation failed");
    }

    return result;
  } catch (error) {
    logger.error("Statement SPARQL operation failed", {
      operation: operationType,
      statementId: definition.statementId,
      error: error.message,
    });
    throw error;
  }
}

function extractStatementIdFromUri(uri) {
  if (!uri) return null;
  if (uri.startsWith(DATA_PREFIX)) return uri.slice(DATA_PREFIX.length);
  if (uri.startsWith("data:")) return uri.slice("data:".length);
  return uri;
}

function parseStatementBindings(bindings = []) {
  const grouped = new Map();

  bindings.forEach((binding) => {
    const statementUri = binding.statement?.value;
    if (!statementUri) return;

    const id = extractStatementIdFromUri(statementUri);
    const entry =
      grouped.get(statementUri) || {
        id,
        uri: statementUri,
        text: "",
        type: "",
        confidence: undefined,
        source: undefined,
        tags: new Set(),
        metadata: {},
      };

    if (binding.text?.value) entry.text = binding.text.value;
    if (binding.type?.value) entry.type = binding.type.value;
    if (binding.confidence?.value) {
      const numeric = Number.parseFloat(binding.confidence.value);
      entry.confidence = Number.isFinite(numeric) ? numeric : entry.confidence;
    }
    if (binding.source?.value) entry.source = binding.source.value;
    if (binding.tag?.value) entry.tags.add(binding.tag.value);
    if (binding.patient?.value) entry.metadata.patient_id = binding.patient.value;
    if (binding.clinician?.value) entry.metadata.clinician_id = binding.clinician.value;
    if (binding.context?.value) entry.metadata.context = binding.context.value;
    if (binding.createdAt?.value) entry.metadata.created_at = binding.createdAt.value;
    if (binding.updatedAt?.value) entry.metadata.updated_at = binding.updatedAt.value;
    if (binding.version?.value) entry.metadata.version = binding.version.value;
    if (binding.createdBy?.value) entry.metadata.created_by = extractStatementIdFromUri(binding.createdBy.value);
    if (binding.updatedBy?.value) entry.metadata.updated_by = extractStatementIdFromUri(binding.updatedBy.value);

    grouped.set(statementUri, entry);
  });

  return Array.from(grouped.values()).map((entry) => ({
    ...entry,
    tags: Array.from(entry.tags),
  }));
}

async function fetchStatementById(statementId) {
  const sanitized = sanitizeIdentifier(statementId);
  const statementUri = `${DATA_PREFIX}${sanitized}`;

  const query = `
    SELECT ?statement ?text ?type ?confidence ?source ?tag
           ?patient ?clinician ?context ?createdAt ?updatedAt ?version
           ?createdBy ?updatedBy
    WHERE {
      GRAPH ?graph {
        ?statement a vocab:ClinicalStatement ;
                   vocab:hasStatementText ?text ;
                   vocab:statementType ?type .
        OPTIONAL { ?statement vocab:confidence ?confidence . }
        OPTIONAL { ?statement vocab:source ?source . }
        OPTIONAL { ?statement vocab:patientId ?patient . }
        OPTIONAL { ?statement vocab:clinicianId ?clinician . }
        OPTIONAL { ?statement vocab:context ?context . }
        OPTIONAL { ?statement vocab:createdAt ?createdAt . }
        OPTIONAL { ?statement vocab:updatedAt ?updatedAt . }
        OPTIONAL { ?statement vocab:version ?version . }
        OPTIONAL { ?statement vocab:hasTag ?tag . }
        OPTIONAL { ?statement vocab:createdBy ?createdBy . }
        OPTIONAL { ?statement vocab:updatedBy ?updatedBy . }
      }
      FILTER(?statement = <${statementUri}>)
    }
  `;

  try {
    const result = await utils.sparqlJSONQuery(STATEMENTS_DATASET, query);
    const bindings = result?.results?.bindings ?? [];

    if (!bindings.length) {
      return { status: StatusCodes.NOT_FOUND };
    }

    const [statement] = parseStatementBindings(bindings);
    statement.graphUris = buildGraphUris(statement.id);

    return { status: StatusCodes.OK, statement };
  } catch (error) {
    logger.error("Failed to fetch statement", {
      statementId,
      error: error.message,
    });
    return {
      status: StatusCodes.INTERNAL_SERVER_ERROR,
      error: error.message,
    };
  }
}

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

router.get("/info", (req, res) => {
  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      service: "clinical-statements",
      description: "CRUD service for TMR-based clinical statements",
      endpoints: [
        { path: "/", method: "POST", description: "Create a clinical statement" },
        { path: "/:id", method: "GET", description: "Retrieve a clinical statement" },
        { path: "/:id", method: "PUT", description: "Update a clinical statement" },
        { path: "/:id", method: "DELETE", description: "Delete a clinical statement" },
        { path: "/health", method: "GET", description: "Health check endpoint" },
        { path: "/info", method: "GET", description: "Service information" },
      ],
      data_storage: "RDF nanopublications in triple store",
      caching: {
        enabled: true,
        ttl_seconds: CACHE_TTL,
      },
    },
  });
});

router.post("/cache/clear", (req, res) => {
  const requestId = generateRequestId();
  const startTime = Date.now();
  try {
    const { pattern = "" } = req.body || {};
    cacheUtils.clear(pattern);
    res.status(StatusCodes.OK).json({
      status: "success",
      message: "Cache cleared successfully",
      requestId,
      responseTime: Date.now() - startTime,
    });
  } catch (error) {
    logger.error("Failed to clear statement cache", {
      requestId,
      error: error.message,
      ip: req.ip,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to clear cache",
      requestId,
      responseTime: Date.now() - startTime,
    });
  }
});

router.get("/cache/stats", (req, res) => {
  const requestId = generateRequestId();
  const startTime = Date.now();
  try {
    const stats = cacheUtils.getStats();
    res.status(StatusCodes.OK).json({
      status: "success",
      data: {
        cache_statistics: stats,
        cache_efficiency: stats.totalEntries
          ? Math.round((stats.activeEntries / stats.totalEntries) * 100)
          : 0,
      },
      requestId,
      responseTime: Date.now() - startTime,
    });
  } catch (error) {
    logger.error("Failed to retrieve statement cache stats", {
      requestId,
      error: error.message,
      ip: req.ip,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to retrieve cache statistics",
      requestId,
      responseTime: Date.now() - startTime,
    });
  }
});

router.post("/", [writeLimiter, statementValidationRules], async (req, res) => {
  const requestId = generateRequestId();
  const startTime = Date.now();

  try {
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

    const { text, type, confidence = 1, source, metadata = {}, tags = [] } = req.body;

    logger.info("Creating clinical statement", {
      requestId,
      type,
      textLength: text.length,
      ip: req.ip,
    });

    const definition = buildStatementDefinition(
      { text, type, confidence, source, metadata, tags },
      { ip: req.ip }
    );

    const result = await executeStatementOperation(definition, "INSERT");

    cacheUtils.invalidatePattern("list");
    cacheUtils.invalidatePattern("search");

    const responseTime = Date.now() - startTime;

    res.status(StatusCodes.CREATED).json({
      status: "success",
      message: `Clinical statement ${definition.statementId} created successfully`,
      data: {
        id: definition.statementId,
        uri: definition.statementUri,
        text,
        type,
        confidence: definition.confidence,
        source: definition.metadata.source,
        tags: definition.tags,
        metadata: definition.metadata,
        graphs: definition.graphUris,
        sparql_status: result.status,
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
      message: "Failed to create clinical statement",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

router.get("/:id", [readLimiter, statementIdValidation], async (req, res) => {
  const requestId = generateRequestId();
  const startTime = Date.now();

  try {
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

    const cacheKey = cacheUtils.generateKey(`/get/${id}`);
    const cached = cacheUtils.get(cacheKey);
    if (cached) {
      const responseTime = Date.now() - startTime;
      return res.status(StatusCodes.OK).json({
        ...cached,
        cached: true,
        requestId,
        responseTime,
      });
    }

    const result = await fetchStatementById(id);

    if (result.status === StatusCodes.NOT_FOUND) {
      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Clinical statement not found",
        requestId,
      });
    }

    if (result.status !== StatusCodes.OK) {
      throw new ErrorHandler(
        result.status || StatusCodes.INTERNAL_SERVER_ERROR,
        result.error || "Failed to retrieve clinical statement"
      );
    }

    const responseData = {
      status: "success",
      data: result.statement,
    };

    cacheUtils.set(cacheKey, responseData, CACHE_TTL);

    const responseTime = Date.now() - startTime;

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
      message: "Failed to retrieve clinical statement",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

router.put(
  "/:id",
  [writeLimiter, statementIdValidation, statementValidationRules],
  async (req, res) => {
    const requestId = generateRequestId();
    const startTime = Date.now();

    try {
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
      const { text, type, confidence, source, metadata = {}, tags = [] } = req.body;

      const existing = await fetchStatementById(id);
      if (existing.status === StatusCodes.NOT_FOUND) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Clinical statement not found",
          requestId,
        });
      }

      if (existing.status !== StatusCodes.OK) {
        throw new ErrorHandler(
          existing.status || StatusCodes.INTERNAL_SERVER_ERROR,
          existing.error || "Failed to verify clinical statement"
        );
      }

      const mergedMetadata = {
        ...existing.statement.metadata,
        ...metadata,
        created_at: existing.statement.metadata?.created_at,
      };

      const definition = buildStatementDefinition(
        {
          id,
          text,
          type,
          confidence,
          source,
          metadata: mergedMetadata,
          tags,
        },
        { isUpdate: true, ip: req.ip }
      );

      const result = await executeStatementOperation(definition, "UPDATE");

      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");

      const responseTime = Date.now() - startTime;

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `Clinical statement ${definition.statementId} updated successfully`,
        data: {
          id: definition.statementId,
          uri: definition.statementUri,
          text,
          type,
          confidence: definition.confidence,
          source: definition.metadata.source,
          tags: definition.tags,
          metadata: definition.metadata,
          graphs: definition.graphUris,
          sparql_status: result.status,
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
        message: "Failed to update clinical statement",
        requestId,
        responseTime,
        error: process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

router.delete(
  "/:id",
  [writeLimiter, statementIdValidation],
  async (req, res) => {
    const requestId = generateRequestId();
    const startTime = Date.now();

    try {
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

      const existing = await fetchStatementById(id);
      if (existing.status === StatusCodes.NOT_FOUND) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Clinical statement not found",
          requestId,
        });
      }

      if (existing.status !== StatusCodes.OK) {
        throw new ErrorHandler(
          existing.status || StatusCodes.INTERNAL_SERVER_ERROR,
          existing.error || "Failed to verify clinical statement"
        );
      }

      await executeStatementOperation(
        {
          statementId: existing.statement.id,
          graphUris: existing.statement.graphUris,
          payload: "",
        },
        "DELETE"
      );

      cacheUtils.invalidatePattern(id);
      cacheUtils.invalidatePattern("list");
      cacheUtils.invalidatePattern("search");

      const responseTime = Date.now() - startTime;

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `Clinical statement ${existing.statement.id} deleted successfully`,
        data: {
          id: existing.statement.id,
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
        message: "Failed to delete clinical statement",
        requestId,
        responseTime,
        error: process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

module.exports = router;
module.exports.cacheUtils = cacheUtils;
module.exports.fetchStatementById = fetchStatementById;
