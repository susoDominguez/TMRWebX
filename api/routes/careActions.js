/**
 * Handles retrieval and querying of TMR-based care action types
 * Enhanced with better validation, error handling, and monitoring
 */

const express = require("express");
const { body, query, validationResult } = require("express-validator");
const { StatusCodes } = require("http-status-codes");
const rateLimit = require("express-rate-limit");
const router = express.Router();

// Core dependencies
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const logger = require("../config/winston");

// Constants and Configuration
const CARE_ACTION_TYPES = Object.freeze({
  DrugCombinationType: "vocab:DrugCombinationType",
  DrugCategory: "vocab:DrugCategory",
  DrugType: "vocab:DrugType",
  VaccineCategory: "vocab:vaccineCategory",
  VaccineType: "vocab:VaccineType",
  NonDrugType: "vocab:NonDrugType",
  NonDrugAdministrationType: "vocab:NonDrugAdministrationType",
  DrugAdministrationType: "vocab:DrugAdministrationType",
  VaccinationType: "vocab:VaccinationType",
  DrugCombinationAdministrationType: "vocab:DrugAdministrationType",
});

const DATA_URI_PREFIX = "http://anonymous.org/data/";

function buildAllowFilter(prefixes = []) {
  const list = Array.isArray(prefixes)
    ? prefixes.filter((prefix) => typeof prefix === "string" && prefix.length > 0)
    : [];
  if (list.length === 0) {
    return "";
  }
  const clauses = list.map((prefix) => `STRSTARTS(STR(?resource), "${prefix}")`);
  return `FILTER(${clauses.join(" || ")})`;
}

function buildExcludeFilter(prefixes = []) {
  const list = Array.isArray(prefixes)
    ? prefixes.filter((prefix) => typeof prefix === "string" && prefix.length > 0)
    : [];
  if (list.length === 0) {
    return "";
  }
  const clauses = list.map((prefix) => `!STRSTARTS(STR(?resource), "${prefix}")`);
  return `FILTER(${clauses.join(" && ")})`;
}

// Route configurations with metadata
const ROUTE_CONFIGURATIONS = Object.freeze({
  "/drugs/individual/get": {
    types: [CARE_ACTION_TYPES.DrugType],
    description: "Get all individual drug types",
    category: "drugs",
    subcategory: "individual",
    uriPrefixes: [`${DATA_URI_PREFIX}DrugT`],
    linkPredicates: ["vocab:administrationOf"],
  },
  "/drugs/get": {
    types: [
      CARE_ACTION_TYPES.DrugCombinationType,
      CARE_ACTION_TYPES.DrugCategory,
      CARE_ACTION_TYPES.DrugType,
    ],
    description:
      "Get all drug-related care actions including combinations, categories, and types",
    category: "drugs",
    subcategory: "all",
    linkPredicates: [
      "vocab:administrationOf",
      "vocab:combinedAdministrationOf",
    ],
  },
  "/nondrugs/get": {
    types: [CARE_ACTION_TYPES.NonDrugType],
    description: "Get all non-drug care actions",
    category: "nondrugs",
    subcategory: "all",
    uriPrefixes: [`${DATA_URI_PREFIX}NonDrugT`],
    linkPredicates: ["vocab:applicationOf"],
  },
  "/drugs/vaccines/get": {
    types: [CARE_ACTION_TYPES.VaccineType],
    description: "Get all vaccine-related care actions",
    category: "drugs",
    subcategory: "vaccines",
    uriPrefixes: [`${DATA_URI_PREFIX}VacT`],
    linkPredicates: ["vocab:vaccinationWith"],
  },
  "/drugs/category/get": {
    types: [CARE_ACTION_TYPES.DrugCategory, CARE_ACTION_TYPES.DrugType],
    description: "Get all drug categories",
    category: "drugs",
    subcategory: "category",
    uriPrefixes: [`${DATA_URI_PREFIX}DrugCat`],
    linkPredicates: ["vocab:administrationOf"],
  },
  "/drugs/vaccines/category/get": {
    types: [CARE_ACTION_TYPES.VaccineCategory],
    description: "Get all vaccine categories",
    category: "drugs",
    subcategory: "vaccine-category",
    uriPrefixes: [`${DATA_URI_PREFIX}VacCat`],
    linkPredicates: ["vocab:vaccinationWith"],
  },
  "/drugs/combination/get": {
    types: [CARE_ACTION_TYPES.DrugCombinationType],
    description: "Get all drug combinations",
    category: "drugs",
    subcategory: "combination",
    uriPrefixes: [`${DATA_URI_PREFIX}CombDrugT`],
    linkPredicates: ["vocab:combinedAdministrationOf"],
  },
  "/get": {
    types: [
      CARE_ACTION_TYPES.DrugCombinationType,
      CARE_ACTION_TYPES.DrugCategory,
      CARE_ACTION_TYPES.DrugType,
      CARE_ACTION_TYPES.VaccineCategory,
      CARE_ACTION_TYPES.VaccineType,
      CARE_ACTION_TYPES.NonDrugType,
      CARE_ACTION_TYPES.NonDrugAdministrationType,
      CARE_ACTION_TYPES.DrugAdministrationType,
      CARE_ACTION_TYPES.VaccinationType,
      CARE_ACTION_TYPES.DrugCombinationAdministrationType,
    ],
    description:
      "Get all care action types including drug, vaccine, and non-drug therapies",
    category: "all",
    subcategory: "administrations",
  },
  // Additional endpoints requested by user
  "/vaccines/category/get": {
    types: [CARE_ACTION_TYPES.VaccineCategory],
    description: "Get vaccine category types",
    category: "vaccines",
    subcategory: "category",
    uriPrefixes: [`${DATA_URI_PREFIX}VacCat`],
    linkPredicates: ["vocab:vaccinationWith"],
  },
  "/vaccines/get": {
    types: [CARE_ACTION_TYPES.VaccineType],
    description: "Get vaccine types",
    category: "vaccines",
    subcategory: "all",
    uriPrefixes: [`${DATA_URI_PREFIX}VacT`],
    linkPredicates: ["vocab:vaccinationWith"],
  },
  "/medications/get": {
    types: [
      CARE_ACTION_TYPES.DrugCategory,
      CARE_ACTION_TYPES.DrugCombinationType,
      CARE_ACTION_TYPES.DrugType,
    ],
    description:
      "Get medication types including categories, combination therapies, and individual drugs",
    category: "medications",
    subcategory: "all",
    uriPrefixes: [
      `${DATA_URI_PREFIX}DrugCat`,
      `${DATA_URI_PREFIX}CombDrugT`,
      `${DATA_URI_PREFIX}DrugT`,
    ],
    linkPredicates: [
      "vocab:administrationOf",
      "vocab:combinedAdministrationOf",
    ],
  },
  "/medications/individual/get": {
    types: [CARE_ACTION_TYPES.DrugType],
    description: "Get individual drug types excluding categories",
    category: "medications",
    subcategory: "individual",
    uriPrefixes: [`${DATA_URI_PREFIX}DrugT`],
    excludePrefixes: [`${DATA_URI_PREFIX}DrugCat`, `${DATA_URI_PREFIX}CombDrugT`],
    linkPredicates: ["vocab:administrationOf"],
  },
});


// Rate limiting for query operations
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

// Stricter rate limiting for bulk operations
const bulkQueryLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 50, // limit each IP to 50 bulk query requests per windowMs
  message: {
    status: "error",
    message: "Too many bulk query requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

/**
 * Validation middleware for query parameters
 */
const validateQueryParams = [
  query("limit")
    .optional()
    .isInt({ min: 1, max: 1000 })
    .withMessage("Limit must be an integer between 1 and 1000"),

  query("offset")
    .optional()
    .isInt({ min: 0 })
    .withMessage("Offset must be a non-negative integer"),

  query("format")
    .optional()
    .isIn(["json", "turtle", "rdf", "n3"])
    .withMessage("Format must be one of: json, turtle, rdf, n3"),

  query("include_metadata")
    .optional()
    .isBoolean()
    .withMessage("Include metadata must be a boolean"),

  query("filter")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 100 })
    .matches(/^[a-zA-Z0-9_\s\-]*$/)
    .withMessage(
      "Filter must be alphanumeric with spaces, underscores, and hyphens only"
    ),
];

/**
 * Enhanced SPARQL query handler with comprehensive error handling and caching
 */
async function handleSparqlQuery(careActionTypes, routeConfig, req, res) {
  const startTime = Date.now();
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

  try {
    // Validate input
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logger.warn("Query validation failed", {
        requestId,
        route: req.route?.path,
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

    // Extract query parameters
    const {
      limit = 100,
      offset = 0,
      format = "json",
      include_metadata = false,
      filter = "",
    } = req.query;
    const exactMatch =
      req.query.exact_match === "true" || req.query.exact_match === true;

    const queryParams = {
      limit,
      offset,
      format,
      include_metadata,
      filter,
      exact_match: exactMatch,
    };

    // Deduplicate care action types before logging (some aliases map to same vocab URI)
    const uniqueCareActionTypes = [...new Set(careActionTypes)];

    logger.info("Processing care actions query", {
      requestId,
      route: req.route?.path,
      careActionTypes: uniqueCareActionTypes,
      originalTypesCount: careActionTypes.length,
      deduplicatedTypesCount: uniqueCareActionTypes.length,
      queryParams,
      ip: req.ip,
    });

    // Validate care action types
    if (!Array.isArray(uniqueCareActionTypes) || uniqueCareActionTypes.length === 0) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "No valid care action types provided"
      );
    }

    // Validate each care action type
    const invalidTypes = uniqueCareActionTypes.filter(
      (type) => !Object.values(CARE_ACTION_TYPES).includes(type)
    );

    if (invalidTypes.length > 0) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        `Invalid care action types: ${invalidTypes.join(", ")}`
      );
    }

    const allowedTypeValues = uniqueCareActionTypes.length
      ? `VALUES ?allowedType { ${uniqueCareActionTypes.join(" ")} }`
      : "";

    const linkPredicates = Array.isArray(routeConfig.linkPredicates)
      ? routeConfig.linkPredicates.filter(
          (predicate) => typeof predicate === "string" && predicate.length > 0
        )
      : [];
    const linkPredicateValues = linkPredicates.length
      ? `VALUES ?linkPredicate { ${linkPredicates.join(" ")} }`
      : "";

    const queryBlocks = [];

    if (allowedTypeValues) {
      queryBlocks.push(`
        {
          ${allowedTypeValues}
          ?resource a ?allowedType ;
                    rdfs:label ?label .
          OPTIONAL { ?resource vocab:hasSctId ?sctid . }
          BIND(?allowedType AS ?type)
        }
      `);
    }

    if (linkPredicateValues) {
      queryBlocks.push(`
        {
          ${linkPredicateValues}
          ?admin ?linkPredicate ?resource .
          ?resource rdfs:label ?label .
          OPTIONAL { ?resource a ?type . }
          OPTIONAL { ?resource vocab:hasSctId ?sctid . }
        }
      `);
    }

    if (queryBlocks.length === 0) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "No valid care action selectors provided"
      );
    }

    const allowFilterClause = buildAllowFilter(routeConfig.uriPrefixes);
    const excludeFilterClause = buildExcludeFilter(routeConfig.excludePrefixes);

    const careActionQuery = `
      SELECT DISTINCT ?resource ?type ?label ?sctid
      WHERE {
        ${queryBlocks.join("\n        UNION\n")}
        ${allowFilterClause}
        ${excludeFilterClause}
      }
    `;

    const sparqlResponse = await utils.sparqlJSONQuery(
      "careActions",
      careActionQuery
    );

    const bindings = sparqlResponse?.results?.bindings ?? [];
    const resources = new Map();

    bindings.forEach((binding) => {
      const uri = binding.resource?.value;
      if (!uri) {
        return;
      }

      const info = resources.get(uri) || {};
      info.type = info.type || binding.type?.value || null;
      info.label = info.label || binding.label?.value || null;
      info.sctid = info.sctid || binding.sctid?.value || null;
      resources.set(uri, info);
    });

    const allowPrefixes = Array.isArray(routeConfig.uriPrefixes)
      ? routeConfig.uriPrefixes.filter((prefix) => typeof prefix === "string" && prefix.length > 0)
      : [];
    const blockPrefixes = Array.isArray(routeConfig.excludePrefixes)
      ? routeConfig.excludePrefixes.filter((prefix) => typeof prefix === "string" && prefix.length > 0)
      : [];

    let entries = Array.from(resources.entries());

    if (allowPrefixes.length > 0) {
      entries = entries.filter(([uri]) =>
        allowPrefixes.some((prefix) => uri.startsWith(prefix))
      );
    }

    if (blockPrefixes.length > 0) {
      entries = entries.filter(([uri]) =>
        !blockPrefixes.some((prefix) => uri.startsWith(prefix))
      );
    }

    const filterLower = filter && filter.trim() !== "" ? filter.toLowerCase().trim() : null;
    if (filterLower) {
      entries = entries.filter(([uri, info]) => {
        const haystack = [uri, info.label || "", info.sctid || ""]
          .join(" ")
          .toLowerCase();
        return exactMatch ? haystack === filterLower : haystack.includes(filterLower);
      });
    }

    entries.sort((a, b) => {
      const labelA = a[1].label || a[0];
      const labelB = b[1].label || b[0];
      return labelA.localeCompare(labelB);
    });

    const parsedLimit = Number.parseInt(limit, 10);
    const limitNum = Number.isFinite(parsedLimit) && parsedLimit > 0 ? parsedLimit : 100;
    const parsedOffset = Number.parseInt(offset, 10);
    const offsetNum = Number.isFinite(parsedOffset) && parsedOffset >= 0 ? parsedOffset : 0;

    const page = entries.slice(offsetNum, offsetNum + limitNum);
    const uriList = page.map(([uri]) => uri);

    const totalCount = entries.length;
    const query_types = uniqueCareActionTypes.map((type) =>
      type.replace("vocab:", "http://anonymous.org/vocab/")
    );
    const responseData = {
      status: "success",
      data: uriList,
      metadata: {
        total_count: totalCount,
        returned_count: uriList.length,
        limit: limitNum,
        offset: offsetNum,
        has_more: offsetNum + limitNum < totalCount,
        query_types,
        successful_queries: uniqueCareActionTypes.length,
        failed_queries: 0,
      },
      requestId,
      responseTime: Date.now() - startTime,
    };

    // Include additional metadata if requested
    if (include_metadata === "true" || include_metadata === true) {
      responseData.metadata.route_config = {
        description: routeConfig.description,
        category: routeConfig.category,
        subcategory: routeConfig.subcategory,
      };

      const details = page.reduce((acc, [uri, info]) => {
        acc[uri] = {
          label: info.label || null,
          sctid: info.sctid || null,
          type: info.type || null,
        };
        return acc;
      }, {});
      if (Object.keys(details).length > 0) {
        responseData.metadata.details = details;
      }
    }

    logger.info("Care actions query completed successfully", {
      requestId,
      route: req.route?.path,
      totalResults: totalCount,
      returnedResults: uriList.length,
      failedQueries: 0,
      responseTime: Date.now() - startTime,
    });

    res.status(StatusCodes.OK).json(responseData);
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Care actions query failed", {
      requestId,
      route: req.route?.path,
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
      message: "Internal server error occurred while processing query",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
}

/**
 * Health check endpoint
 */
router.get("/health", (req, res) => {
  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "care-actions-query",
    timestamp: new Date().toISOString(),
    version: "2.0.0",
    routes: Object.keys(ROUTE_CONFIGURATIONS).length,
    care_action_types: Object.keys(CARE_ACTION_TYPES).length,
  });
});

/**
 * Get available care action types and routes
 */
router.get("/info", (req, res) => {
  const routeInfo = Object.entries(ROUTE_CONFIGURATIONS).map(
    ([route, config]) => ({
      endpoint: route,
      method: "POST",
      description: config.description,
      category: config.category,
      subcategory: config.subcategory,
      types: config.types,
    })
  );

  const typeInfo = Object.entries(CARE_ACTION_TYPES).map(([name, uri]) => ({
    name,
    uri,
    category: name.includes("Drug")
      ? "drug"
      : name.includes("Vaccine")
      ? "vaccine"
      : name.includes("NonDrug")
      ? "nondrug"
      : "administration",
  }));

  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      available_routes: routeInfo,
      available_types: typeInfo,
      supported_query_params: [
        "limit (1-1000)",
        "offset (>=0)",
        "format (json, turtle, rdf, n3)",
        "include_metadata (boolean)",
        "filter (string)",
      ],
    },
  });
});

/**
 * Advanced search endpoint
 */
router.post(
  "/search",
  [
    queryLimiter,
    validateQueryParams,
    body("types")
      .optional()
      .isArray()
      .custom((types) => {
        return types.every((type) =>
          Object.values(CARE_ACTION_TYPES).includes(type)
        );
      })
      .withMessage("All types must be valid care action types"),

    body("query")
      .isString()
      .trim()
      .isLength({ min: 1, max: 200 })
      .matches(/^[a-zA-Z0-9_\s\-\.]*$/)
      .withMessage(
        "Query must be 1-200 characters, alphanumeric with spaces, underscores, hyphens, and periods"
      ),

    body("exact_match")
      .optional()
      .isBoolean()
      .withMessage("Exact match must be a boolean"),
  ],
  async (req, res) => {
    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const {
        types = Object.values(CARE_ACTION_TYPES),
        query: searchQuery,
        exact_match = false,
      } = req.body;

      // Create a mock route config for the search
      const searchConfig = {
        description: "Advanced search across care action types",
        category: "search",
        subcategory: "advanced",
      };

      // Add search query to the request for filtering
      req.query.filter = searchQuery;
      req.query.exact_match = exact_match;

      await handleSparqlQuery(types, searchConfig, req, res);
    } catch (error) {
      logger.error("Advanced search failed", {
        error: error.message,
        body: req.body,
        ip: req.ip,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Search operation failed",
      });
    }
  }
);

/**
 * Register all routes dynamically with enhanced middleware
 */
Object.entries(ROUTE_CONFIGURATIONS).forEach(([route, routeConfig]) => {
  const isLargeQuery = routeConfig.types.length > 3;
  const rateLimitMiddleware = isLargeQuery ? bulkQueryLimiter : queryLimiter;

  router.post(
    route,
    [rateLimitMiddleware, validateQueryParams],
    async (req, res) => {
      await handleSparqlQuery(routeConfig.types, routeConfig, req, res);
    }
  );

  logger.debug("Registered care actions route", {
    route,
    description: routeConfig.description,
    typeCount: routeConfig.types.length,
  });
});

// Export router and utilities for testing
module.exports = router;
module.exports.CARE_ACTION_TYPES = CARE_ACTION_TYPES;
module.exports.ROUTE_CONFIGURATIONS = ROUTE_CONFIGURATIONS;
