/**
 * Statements Routes
 * Handles querying of TMR-based clinical statements
 * Refactored to align with shared route patterns
 */

const express = require("express");
const { query, validationResult } = require("express-validator");
const { StatusCodes } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const logger = require("../config/winston");
const { escapeQuotes, parseIdsInput } = require("../lib/router_functs/route_helpers");

const STATEMENTS_DATASET = "statements";
const DATA_PREFIX = "http://anonymous.org/data/";
const DEFAULT_LIMIT = 50;
const MAX_LIMIT = 500;
const DEFAULT_OFFSET = 0;
const TAG_SEPARATOR = "||";

const queryLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 100,
  message: {
    status: "error",
    message: "Too many statement query requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const searchLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 50,
  message: {
    status: "error",
    message: "Too many search requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const cache = new Map();
const CACHE_TTL = 300;
const CACHE_PREFIX = "statements:";

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

    logger.debug("Cache hit for statements", { key });
    return cached.data;
  },

  set(key, data, ttlSeconds = CACHE_TTL) {
    const expiry = Date.now() + ttlSeconds * 1000;
    cache.set(key, { data, expiry });

    if (cache.size > 200) {
      const oldestKey = cache.keys().next().value;
      cache.delete(oldestKey);
    }

    logger.debug("Cache set for statements", { key, ttlSeconds });
  },

  invalidatePattern(pattern) {
    if (!pattern) return;
    const toDelete = [];
    for (const key of cache.keys()) {
      if (key.includes(pattern)) {
        toDelete.push(key);
      }
    }

    toDelete.forEach((key) => cache.delete(key));
    if (toDelete.length) {
      logger.info("Statements cache invalidated", {
        pattern,
        deletedCount: toDelete.length,
      });
    }
  },

  clear(pattern = "") {
    const keysToDelete = [];
    for (const key of cache.keys()) {
      if (!pattern || key.includes(pattern)) {
        keysToDelete.push(key);
      }
    }

    keysToDelete.forEach((key) => cache.delete(key));
    logger.info("Statements cache cleared", {
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

const queryValidationRules = [
  query("limit")
    .optional()
    .isInt({ min: 1, max: MAX_LIMIT })
    .withMessage(`Limit must be an integer between 1 and ${MAX_LIMIT}`),

  query("offset")
    .optional()
    .isInt({ min: 0 })
    .withMessage("Offset must be a non-negative integer"),

  query("type")
    .optional()
    .isIn([
      "clinical_observation",
      "diagnosis",
      "treatment_plan",
      "outcome_measure",
      "risk_factor",
    ])
    .withMessage(
      "Type must be one of: clinical_observation, diagnosis, treatment_plan, outcome_measure, risk_factor"
    ),

  query("confidence_min")
    .optional()
    .isFloat({ min: 0, max: 1 })
    .withMessage("Confidence minimum must be a number between 0 and 1"),

  query("confidence_max")
    .optional()
    .isFloat({ min: 0, max: 1 })
    .withMessage("Confidence maximum must be a number between 0 and 1"),

  query("format")
    .optional()
    .isIn(["json", "rdf", "turtle"])
    .withMessage("Format must be one of: json, rdf, turtle"),

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),

  query("patient_id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_\-]+$/)
    .withMessage(
      "Patient ID must be alphanumeric with underscores and hyphens only"
    ),

  query("clinician_id")
    .optional()
    .isString()
    .trim()
    .matches(/^[a-zA-Z0-9_\-]+$/)
    .withMessage(
      "Clinician ID must be alphanumeric with underscores and hyphens only"
    ),

  query("date_from")
    .optional()
    .isISO8601()
    .withMessage("Date from must be a valid ISO 8601 date"),

  query("date_to")
    .optional()
    .isISO8601()
    .withMessage("Date to must be a valid ISO 8601 date"),

  query("tags")
    .optional()
    .isString()
    .withMessage("Tags must be a comma-separated string"),

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

function generateRequestId() {
  return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

function coerceBoolean(value) {
  if (typeof value === "boolean") return value;
  if (typeof value === "string") {
    const normalized = value.trim().toLowerCase();
    return ["true", "1", "yes"].includes(normalized);
  }
  return false;
}

function normalizeLimit(value, fallback = DEFAULT_LIMIT) {
  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed) || parsed <= 0) return fallback;
  return Math.min(parsed, MAX_LIMIT);
}

function normalizeOffset(value, fallback = DEFAULT_OFFSET) {
  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed) || parsed < 0) return fallback;
  return parsed;
}

function normalizeTagsParam(tagsInput) {
  const parsed = parseIdsInput(tagsInput);
  const unique = Array.from(new Set(parsed.map((tag) => tag.trim())));
  return unique.filter((tag) => tag.length > 0);
}

function normalizeQueryParams(raw = {}) {
  const limit = normalizeLimit(raw.limit, DEFAULT_LIMIT);
  const offset = normalizeOffset(raw.offset, DEFAULT_OFFSET);
  const type = raw.type ? String(raw.type).trim() : undefined;
  const confidenceMinRaw = raw.confidence_min;
  const confidenceMaxRaw = raw.confidence_max;
  const confidenceMin =
    confidenceMinRaw !== undefined && confidenceMinRaw !== ""
      ? Number.parseFloat(confidenceMinRaw)
      : undefined;
  const confidenceMax =
    confidenceMaxRaw !== undefined && confidenceMaxRaw !== ""
      ? Number.parseFloat(confidenceMaxRaw)
      : undefined;
  const patientId = raw.patient_id ? String(raw.patient_id).trim() : undefined;
  const clinicianId = raw.clinician_id
    ? String(raw.clinician_id).trim()
    : undefined;
  const dateFrom = raw.date_from ? String(raw.date_from).trim() : undefined;
  const dateTo = raw.date_to ? String(raw.date_to).trim() : undefined;
  const tags = normalizeTagsParam(raw.tags);
  const searchTermRaw = raw.search ? String(raw.search).trim() : undefined;
  const searchTerm = searchTermRaw && searchTermRaw.length > 0 ? searchTermRaw : undefined;
  const includeMetadata = coerceBoolean(raw.include_metadata);
  const format = raw.format ? String(raw.format).toLowerCase() : "json";

  return {
    limit,
    offset,
    type,
    confidenceMin: Number.isFinite(confidenceMin) ? confidenceMin : undefined,
    confidenceMax: Number.isFinite(confidenceMax) ? confidenceMax : undefined,
    patientId,
    clinicianId,
    dateFrom,
    dateTo,
    tags,
    searchTerm,
    includeMetadata,
    format,
  };
}

function buildCacheKeyParams(params) {
  return {
    limit: params.limit,
    offset: params.offset,
    type: params.type || "",
    confidenceMin: params.confidenceMin ?? "",
    confidenceMax: params.confidenceMax ?? "",
    patientId: params.patientId || "",
    clinicianId: params.clinicianId || "",
    dateFrom: params.dateFrom || "",
    dateTo: params.dateTo || "",
    tags: params.tags.length ? params.tags.join("|") : "",
    search: params.searchTerm || "",
    includeMetadata: params.includeMetadata,
    format: params.format || "json",
  };
}

function countActiveFilters(params) {
  let count = 0;
  if (params.type) count += 1;
  if (params.confidenceMin !== undefined) count += 1;
  if (params.confidenceMax !== undefined) count += 1;
  if (params.patientId) count += 1;
  if (params.clinicianId) count += 1;
  if (params.dateFrom) count += 1;
  if (params.dateTo) count += 1;
  if (params.tags.length) count += 1;
  if (params.searchTerm) count += 1;
  return count;
}

function extractStatementIdFromUri(uri) {
  if (!uri) return null;
  if (uri.startsWith(DATA_PREFIX)) return uri.slice(DATA_PREFIX.length);
  if (uri.startsWith("data:")) return uri.slice("data:".length);
  return uri;
}

function parseConfidenceValue(value) {
  if (value === undefined || value === null) return undefined;
  const numeric = Number.parseFloat(value);
  return Number.isFinite(numeric) ? numeric : undefined;
}

function normalizeMetadata(binding) {
  const metadata = {};
  if (binding.patient?.value) metadata.patient_id = binding.patient.value;
  if (binding.clinician?.value) metadata.clinician_id = binding.clinician.value;
  if (binding.context?.value) metadata.context = binding.context.value;
  if (binding.createdAt?.value) metadata.created_at = binding.createdAt.value;
  if (binding.updatedAt?.value) metadata.updated_at = binding.updatedAt.value;
  if (binding.version?.value) metadata.version = binding.version.value;
  if (binding.createdBy?.value) {
    metadata.created_by = extractStatementIdFromUri(binding.createdBy.value);
  }
  if (binding.updatedBy?.value) {
    metadata.updated_by = extractStatementIdFromUri(binding.updatedBy.value);
  }
  return metadata;
}

function parseStatementBindings(bindings = []) {
  return bindings.map((binding) => {
    const statementUri = binding.statement?.value;
    const id = extractStatementIdFromUri(statementUri);
    const tags = binding.tagList?.value
      ? binding.tagList.value.split(TAG_SEPARATOR).filter((tag) => tag && tag.length > 0)
      : [];

    return {
      id,
      uri: statementUri,
      text: binding.text?.value || "",
      type: binding.type?.value || "",
      confidence: parseConfidenceValue(binding.confidence?.value),
      source: binding.source?.value || undefined,
      tags,
      metadata: normalizeMetadata(binding),
    };
  });
}

function buildFilterSegments(params) {
  const filters = [];

  if (params.type) {
    filters.push(`FILTER(LCASE(?type) = "${escapeQuotes(params.type.toLowerCase())}")`);
  }

  if (params.confidenceMin !== undefined) {
    filters.push(`FILTER(BOUND(?confidenceRaw) && ?confidenceRaw >= ${params.confidenceMin})`);
  }

  if (params.confidenceMax !== undefined) {
    filters.push(`FILTER(BOUND(?confidenceRaw) && ?confidenceRaw <= ${params.confidenceMax})`);
  }

  if (params.patientId) {
    filters.push(
      `FILTER(BOUND(?patientRaw) && LCASE(?patientRaw) = "${escapeQuotes(
        params.patientId.toLowerCase()
      )}")`
    );
  }

  if (params.clinicianId) {
    filters.push(
      `FILTER(BOUND(?clinicianRaw) && LCASE(?clinicianRaw) = "${escapeQuotes(
        params.clinicianId.toLowerCase()
      )}")`
    );
  }

  if (params.dateFrom) {
    filters.push(
      `FILTER(BOUND(?createdAtRaw) && ?createdAtRaw >= "${escapeQuotes(
        params.dateFrom
      )}"^^xsd:dateTime)`
    );
  }

  if (params.dateTo) {
    filters.push(
      `FILTER(BOUND(?createdAtRaw) && ?createdAtRaw <= "${escapeQuotes(
        params.dateTo
      )}"^^xsd:dateTime)`
    );
  }

  if (params.tags.length) {
    const tagClauses = params.tags
      .map((tag) => `LCASE(?tagRaw) = "${escapeQuotes(tag.toLowerCase())}"`)
      .join(" || ");
    filters.push(`FILTER(${tagClauses})`);
  }

  if (params.searchTerm) {
    filters.push(
      `FILTER(CONTAINS(LCASE(?text), "${escapeQuotes(params.searchTerm.toLowerCase())}"))`
    );
  }

  return filters;
}

function buildGraphBlock({ requireTag = false } = {}) {
  const lines = [
    "GRAPH ?graph {",
    "  ?statement a vocab:ClinicalStatement ;",
    "             vocab:hasStatementText ?text ;",
    "             vocab:statementType ?type .",
    "  OPTIONAL { ?statement vocab:confidence ?confidenceRaw . }",
    "  OPTIONAL { ?statement vocab:source ?sourceRaw . }",
    "  OPTIONAL { ?statement vocab:patientId ?patientRaw . }",
    "  OPTIONAL { ?statement vocab:clinicianId ?clinicianRaw . }",
    "  OPTIONAL { ?statement vocab:context ?contextRaw . }",
    "  OPTIONAL { ?statement vocab:createdAt ?createdAtRaw . }",
    "  OPTIONAL { ?statement vocab:updatedAt ?updatedAtRaw . }",
    "  OPTIONAL { ?statement vocab:version ?versionRaw . }",
  ];

  if (requireTag) {
    lines.push("  ?statement vocab:hasTag ?tagRaw .");
  } else {
    lines.push("  OPTIONAL { ?statement vocab:hasTag ?tagRaw . }");
  }

  lines.push("  OPTIONAL { ?statement vocab:createdBy ?createdByRaw . }");
  lines.push("  OPTIONAL { ?statement vocab:updatedBy ?updatedByRaw . }");
  lines.push("}");

  return lines.join("\n");
}

function indentBlock(block, spaces = 2) {
  const indent = " ".repeat(spaces);
  return block
    .split("\n")
    .map((line) => (line.trim().length ? `${indent}${line}` : line))
    .join("\n");
}

function buildStatementsQueries(params) {
  const requireTag = params.tags.length > 0;
  const graphBlock = buildGraphBlock({ requireTag });
  const filterSegments = buildFilterSegments(params);
  const whereBlock = [graphBlock, ...filterSegments].join("\n");

  const selectQuery = `
    SELECT ?statement ?text ?type
           (SAMPLE(?confidenceRaw) AS ?confidence)
           (SAMPLE(?sourceRaw) AS ?source)
           (SAMPLE(?patientRaw) AS ?patient)
           (SAMPLE(?clinicianRaw) AS ?clinician)
           (SAMPLE(?contextRaw) AS ?context)
           (SAMPLE(?createdAtRaw) AS ?createdAt)
           (SAMPLE(?updatedAtRaw) AS ?updatedAt)
           (SAMPLE(?versionRaw) AS ?version)
           (SAMPLE(?createdByRaw) AS ?createdBy)
           (SAMPLE(?updatedByRaw) AS ?updatedBy)
           (GROUP_CONCAT(DISTINCT ?tagRaw; separator="${TAG_SEPARATOR}") AS ?tagList)
    WHERE {
${indentBlock(whereBlock, 6)}
    }
    GROUP BY ?statement ?text ?type
    ORDER BY DESC(?confidence) DESC(?createdAt)
    LIMIT ${params.limit}
    OFFSET ${params.offset}
  `;

  const countQuery = `
    SELECT (COUNT(DISTINCT ?statement) AS ?totalCount)
    WHERE {
${indentBlock(whereBlock, 4)}
    }
  `;

  return { selectQuery, countQuery };
}

async function runStatementsQuery(params, { includeCount = true } = {}) {
  const { selectQuery, countQuery } = buildStatementsQueries(params);
  const start = Date.now();

  try {
    const selectResult = await utils.sparqlJSONQuery(STATEMENTS_DATASET, selectQuery);
    const firstDuration = Date.now() - start;
    const selectBindings = selectResult?.results?.bindings ?? [];
    const statements = parseStatementBindings(selectBindings);

    let totalCount = statements.length;
    let countDuration = 0;

    if (includeCount) {
      const countStart = Date.now();
      const countResult = await utils.sparqlJSONQuery(STATEMENTS_DATASET, countQuery);
      countDuration = Date.now() - countStart;
      const countBinding = countResult?.results?.bindings?.[0];
      if (countBinding?.totalCount?.value !== undefined) {
        totalCount = Number.parseInt(countBinding.totalCount.value, 10);
      }
    }

    return {
      statements,
      totalCount,
      queryTimeMs: firstDuration + countDuration,
    };
  } catch (error) {
    logger.error("Clinical statements SPARQL query failed", {
      error: error.message,
      filters: params,
    });
    throw new ErrorHandler(
      StatusCodes.INTERNAL_SERVER_ERROR,
      "Failed to retrieve clinical statements from triple store"
    );
  }
}

function mapBindingsToCounts(bindings = [], keyField, valueField = "count") {
  const result = {};
  bindings.forEach((binding) => {
    const key = binding[keyField]?.value;
    const value = binding[valueField]?.value;
    if (key !== undefined && value !== undefined) {
      result[key] = Number.parseInt(value, 10);
    }
  });
  return result;
}

function parseTemporalDistribution(bindings = []) {
  return bindings
    .map((binding) => ({
      period: binding.period?.value,
      count: binding.count?.value ? Number.parseInt(binding.count.value, 10) : 0,
    }))
    .filter((entry) => entry.period)
    .sort((a, b) => (a.period > b.period ? 1 : -1));
}

async function fetchStatementsStatistics() {
  const overviewQuery = `
    SELECT (COUNT(DISTINCT ?statement) AS ?totalStatements)
           (MAX(?updatedAtRaw) AS ?lastUpdated)
           (MAX(?createdAtRaw) AS ?latestCreated)
    WHERE {
      GRAPH ?graph {
        ?statement a vocab:ClinicalStatement .
        OPTIONAL { ?statement vocab:updatedAt ?updatedAtRaw . }
        OPTIONAL { ?statement vocab:createdAt ?createdAtRaw . }
      }
    }
  `;

  const typeDistributionQuery = `
    SELECT ?type (COUNT(DISTINCT ?statement) AS ?count)
    WHERE {
      GRAPH ?graph {
        ?statement a vocab:ClinicalStatement ;
                   vocab:statementType ?type .
      }
    }
    GROUP BY ?type
    ORDER BY DESC(?count)
  `;

  const confidenceStatsQuery = `
    SELECT (MIN(?confidenceRaw) AS ?minConfidence)
           (MAX(?confidenceRaw) AS ?maxConfidence)
           (AVG(?confidenceRaw) AS ?avgConfidence)
    WHERE {
      GRAPH ?graph {
        ?statement a vocab:ClinicalStatement ;
                   vocab:confidence ?confidenceRaw .
      }
    }
  `;

  const temporalDistributionQuery = `
    SELECT (SUBSTR(STR(?createdAtRaw), 1, 7) AS ?period)
           (COUNT(DISTINCT ?statement) AS ?count)
    WHERE {
      GRAPH ?graph {
        ?statement a vocab:ClinicalStatement ;
                   vocab:createdAt ?createdAtRaw .
      }
    }
    GROUP BY (SUBSTR(STR(?createdAtRaw), 1, 7))
    ORDER BY ?period
  `;

  const topTagsQuery = `
    SELECT ?tag (COUNT(*) AS ?count)
    WHERE {
      GRAPH ?graph {
        ?statement a vocab:ClinicalStatement ;
                   vocab:hasTag ?tag .
      }
    }
    GROUP BY ?tag
    ORDER BY DESC(?count)
    LIMIT 20
  `;

  const metadataCountsQuery = `
    SELECT (COUNT(DISTINCT ?statement) AS ?totalStatements)
           (COUNT(DISTINCT ?metadataStatement) AS ?withMetadata)
           (COUNT(DISTINCT ?tagStatement) AS ?withTags)
    WHERE {
      GRAPH ?graph {
        ?statement a vocab:ClinicalStatement .
        OPTIONAL {
          { ?statement vocab:patientId ?metadataPatient . BIND(?statement AS ?metadataStatement) }
          UNION { ?statement vocab:clinicianId ?metadataClinician . BIND(?statement AS ?metadataStatement) }
          UNION { ?statement vocab:context ?metadataContext . BIND(?statement AS ?metadataStatement) }
          UNION { ?statement vocab:createdAt ?metadataCreated . BIND(?statement AS ?metadataStatement) }
          UNION { ?statement vocab:updatedAt ?metadataUpdated . BIND(?statement AS ?metadataStatement) }
        }
        OPTIONAL { ?statement vocab:hasTag ?metadataTag . BIND(?statement AS ?tagStatement) }
      }
    }
  `;

  try {
    const [overviewResult, typeResult, confidenceResult, temporalResult, tagsResult, metadataResult] =
      await Promise.all([
        utils.sparqlJSONQuery(STATEMENTS_DATASET, overviewQuery),
        utils.sparqlJSONQuery(STATEMENTS_DATASET, typeDistributionQuery),
        utils.sparqlJSONQuery(STATEMENTS_DATASET, confidenceStatsQuery),
        utils.sparqlJSONQuery(STATEMENTS_DATASET, temporalDistributionQuery),
        utils.sparqlJSONQuery(STATEMENTS_DATASET, topTagsQuery),
        utils.sparqlJSONQuery(STATEMENTS_DATASET, metadataCountsQuery),
      ]);

    const overviewBinding = overviewResult?.results?.bindings?.[0] || {};
    const overview = {
      total_statements: overviewBinding.totalStatements?.value
        ? Number.parseInt(overviewBinding.totalStatements.value, 10)
        : 0,
      last_updated: overviewBinding.lastUpdated?.value || null,
      last_created: overviewBinding.latestCreated?.value || null,
      data_source: STATEMENTS_DATASET,
    };

    const confidenceBinding = confidenceResult?.results?.bindings?.[0] || {};
    const confidenceStats = {
      min: confidenceBinding.minConfidence?.value
        ? Number.parseFloat(confidenceBinding.minConfidence.value)
        : null,
      max: confidenceBinding.maxConfidence?.value
        ? Number.parseFloat(confidenceBinding.maxConfidence.value)
        : null,
      avg: confidenceBinding.avgConfidence?.value
        ? Math.round(Number.parseFloat(confidenceBinding.avgConfidence.value) * 100) / 100
        : null,
    };

    const metadataBinding = metadataResult?.results?.bindings?.[0] || {};
    const qualityMetrics = {
      statements_with_metadata: metadataBinding.withMetadata?.value
        ? Number.parseInt(metadataBinding.withMetadata.value, 10)
        : 0,
      statements_with_tags: metadataBinding.withTags?.value
        ? Number.parseInt(metadataBinding.withTags.value, 10)
        : 0,
      average_confidence: confidenceStats.avg || 0,
    };

    return {
      overview,
      by_type: mapBindingsToCounts(typeResult?.results?.bindings || [], "type"),
      by_confidence: confidenceStats,
      temporal_distribution: parseTemporalDistribution(
        temporalResult?.results?.bindings || []
      ),
      top_tags: (tagsResult?.results?.bindings || []).map((binding) => ({
        tag: binding.tag?.value,
        count: binding.count?.value ? Number.parseInt(binding.count.value, 10) : 0,
      })),
      quality_metrics: {
        ...qualityMetrics,
        total_statements: overview.total_statements,
      },
    };
  } catch (error) {
    logger.error("Failed to build statements statistics", {
      error: error.message,
    });
    throw new ErrorHandler(
      StatusCodes.INTERNAL_SERVER_ERROR,
      "Failed to retrieve statements statistics"
    );
  }
}

router.get("/health", (req, res) => {
  const cacheStats = cacheUtils.getStats();
  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "clinical-statements-query",
    timestamp: new Date().toISOString(),
    version: "2.0.0",
    cache: cacheStats,
  });
});

router.get("/info", (req, res) => {
  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      service: "clinical-statements-query",
      description: "Query and search service for TMR-based clinical statements",
      endpoints: [
        {
          path: "/",
          method: "GET",
          description: "Retrieve all clinical statements with advanced filtering",
          query_parameters: [
            "limit (1-500)",
            "offset (>=0)",
            "type",
            "confidence_min (0-1)",
            "confidence_max (0-1)",
            "format (json, rdf, turtle)",
            "include_metadata (boolean)",
            "patient_id",
            "clinician_id",
            "date_from",
            "date_to",
            "tags",
            "search",
          ],
        },
        {
          path: "/search",
          method: "GET",
          description: "Search statements by text content",
          query_parameters: [
            "search (required)",
            "limit (1-500)",
            "offset (>=0)",
            "type",
            "confidence_min",
            "include_metadata",
          ],
        },
        {
          path: "/stats",
          method: "GET",
          description: "Get comprehensive statistics about clinical statements",
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
      supported_statement_types: [
        "clinical_observation",
        "diagnosis",
        "treatment_plan",
        "outcome_measure",
        "risk_factor",
      ],
      data_source: STATEMENTS_DATASET,
      object_type: "vocab:ClinicalStatement",
      features: [
        "Advanced filtering",
        "Text search",
        "Pagination",
        "Statistics",
        "Result caching",
        "Confidence-based sorting",
      ],
      caching: {
        enabled: true,
        ttl_seconds: CACHE_TTL,
        max_entries: 200,
      },
    },
  });
});

router.post("/cache/clear", (req, res) => {
  const requestId = generateRequestId();
  const startTime = Date.now();

  try {
    const { pattern = "" } = req.body || {};
    const before = cacheUtils.getStats();
    cacheUtils.clear(pattern);
    const after = cacheUtils.getStats();

    res.status(StatusCodes.OK).json({
      status: "success",
      message: "Cache cleared successfully",
      data: {
        pattern: pattern || "*",
        entries_cleared: before.totalEntries - after.totalEntries,
        entries_remaining: after.totalEntries,
      },
      requestId,
      responseTime: Date.now() - startTime,
    });
  } catch (error) {
    logger.error("Failed to clear statements cache", {
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
    logger.error("Failed to retrieve statements cache stats", {
      requestId,
      error: error.message,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to retrieve cache statistics",
      requestId,
      responseTime: Date.now() - startTime,
    });
  }
});

router.get("/stats", [queryLimiter], async (req, res) => {
  const requestId = generateRequestId();
  const startTime = Date.now();

  try {
    const cacheKey = cacheUtils.generateKey("stats");
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

    const stats = await fetchStatementsStatistics();
    const responseData = {
      status: "success",
      data: stats,
    };

    cacheUtils.set(cacheKey, responseData, CACHE_TTL * 4);

    const responseTime = Date.now() - startTime;
    res.status(StatusCodes.OK).json({
      ...responseData,
      cached: false,
      requestId,
      responseTime,
    });
  } catch (error) {
    const responseTime = Date.now() - startTime;
    if (error instanceof ErrorHandler) {
      return res.status(error.statusCode).json({
        status: "error",
        message: error.message,
        requestId,
        responseTime,
      });
    }

    logger.error("Failed to retrieve statements statistics", {
      requestId,
      error: error.message,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to retrieve statements statistics",
      requestId,
      responseTime,
    });
  }
});

router.get(
  "/search",
  [searchLimiter, queryValidationRules],
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

      const normalized = normalizeQueryParams(req.query);
      if (!normalized.searchTerm) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Search term is required",
          requestId,
        });
      }

      const cacheParams = buildCacheKeyParams(normalized);
      const cacheKey = cacheUtils.generateKey("search", cacheParams);
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

      const queryResult = await runStatementsQuery(normalized, { includeCount: true });

      const metadata = {
        search_term: normalized.searchTerm,
        total_count: queryResult.totalCount,
        returned_count: queryResult.statements.length,
        limit: normalized.limit,
        offset: normalized.offset,
        has_more: normalized.offset + normalized.limit < queryResult.totalCount,
        search_time_ms: queryResult.queryTimeMs,
      };

      if (normalized.includeMetadata) {
        metadata.query_params = {
          ...cacheParams,
          tags: normalized.tags,
        };
        metadata.data_source = STATEMENTS_DATASET;
        metadata.filters_applied = countActiveFilters(normalized);
      }

      const responseData = {
        status: "success",
        data: queryResult.statements,
        metadata,
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
      logger.error("Failed to search clinical statements", {
        requestId,
        error: error.message,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to search clinical statements",
        requestId,
        responseTime,
      });
    }
  }
);

router.get("/", [queryLimiter, queryValidationRules], async (req, res) => {
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

    const normalized = normalizeQueryParams(req.query);

    if (
      normalized.confidenceMin !== undefined &&
      normalized.confidenceMax !== undefined &&
      normalized.confidenceMin > normalized.confidenceMax
    ) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Confidence minimum cannot be greater than confidence maximum",
        requestId,
      });
    }

    if (normalized.dateFrom && normalized.dateTo) {
      const fromDate = new Date(normalized.dateFrom);
      const toDate = new Date(normalized.dateTo);
      if (!Number.isNaN(fromDate.valueOf()) && !Number.isNaN(toDate.valueOf()) && fromDate > toDate) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Date from cannot be after date to",
          requestId,
        });
      }
    }

    const cacheParams = buildCacheKeyParams(normalized);
    const cacheKey = cacheUtils.generateKey("list", cacheParams);
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

    const queryResult = await runStatementsQuery(normalized, { includeCount: true });

    const metadata = {
      total_count: queryResult.totalCount,
      returned_count: queryResult.statements.length,
      limit: normalized.limit,
      offset: normalized.offset,
      has_more: normalized.offset + normalized.limit < queryResult.totalCount,
      filters_applied: countActiveFilters(normalized),
    };

    if (normalized.includeMetadata) {
      metadata.query_params = {
        ...cacheParams,
        tags: normalized.tags,
      };
      metadata.data_source = STATEMENTS_DATASET;
      metadata.execution_time_ms = queryResult.queryTimeMs;
    }

    const responseData = {
      status: "success",
      data: queryResult.statements,
      metadata,
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

    if (error instanceof ErrorHandler) {
      return res.status(error.statusCode).json({
        status: "error",
        message: error.message,
        requestId,
        responseTime,
      });
    }

    logger.error("Failed to retrieve clinical statements", {
      requestId,
      error: error.message,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to retrieve clinical statements",
      requestId,
      responseTime,
    });
  }
});

module.exports = router;
module.exports.cacheUtils = cacheUtils;
