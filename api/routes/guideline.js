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
const { logStart, logSuccess, logWarn, logError } = require("../lib/requestLogger");
const { isValidId, escapeQuotes } = require("../lib/router_functs/route_helpers");
const { createCache, CACHE_CONFIG } = require("../lib/cacheUtils");
const {
  normalizeCigIdentifiers,
  normalizeDatasetId,
  resolveDatasetPersistenceFlag,
  resolveGuidelineIdentifiers,
  normalizeRecommendationIdentifiers,
  normalizeBeliefIdentifier,
  normalizeCareActionIdentifier,
  normalizePreconditionIdentifier,
  normalizeAuthorIdentifier,
  DATA_PREFIX,
  VOCAB_PREFIX,
  DATASET_PERSISTENCE_MAP,
} = require("../lib/guideline/identifierNormalizers");
const {
  buildRecommendationInsertStatement,
  buildRecommendationDeleteStatement,
  buildRecommendationsQuery,
  parseRecommendationBinding,
  deriveSuggestionFromStrength,
  cleanUriValue,
  splitGroupValues,
  parseBasedOnPairs,
  extractLocalName,
  wrapUriForValuesClause,
  RECOMMENDATION_GROUP_SEPARATOR,
  RECOMMENDATION_PAIR_SEPARATOR,
} = require("../lib/guideline/recommendationHandlers");

const parseGuidelineContent =
  typeof auxFuncts.parseGuidelineContent === "function"
    ? auxFuncts.parseGuidelineContent
    : async (guidelineText) => ({ raw: guidelineText });

// RECOMMENDATION_GROUP_SEPARATOR and RECOMMENDATION_PAIR_SEPARATOR now imported from recommendationHandlers
const RECOMMENDATION_STRENGTH_MAP = new Map([
  ["should", "vocab:should"],
  ["recommend", "vocab:should"],
  ["shouldrecommend", "vocab:should"],
  ["should-recommend", "vocab:should"],
  ["stronglyrecommend", "vocab:must"],
  ["must", "vocab:must"],
  ["mustrecommend", "vocab:must"],
  ["must-recommend", "vocab:must"],
  ["shouldnot", "vocab:shouldNot"],
  ["should_not", "vocab:shouldNot"],
  ["should-not", "vocab:shouldNot"],
  ["nonrecommend", "vocab:shouldNot"],
  ["notrecommend", "vocab:shouldNot"],
  ["discourage", "vocab:shouldNot"],
  ["mustnot", "vocab:mustNot"],
  ["must_not", "vocab:mustNot"],
  ["must-not", "vocab:mustNot"],
  ["contraindicated", "vocab:mustNot"],
]);
const CONTRIBUTION_VALUES = new Set(["positive", "negative", "neutral"]);
const CONTRIBUTION_FALLBACK = "positive";
const CARE_ACTION_RELATION_PROPERTIES = [
  "administrationOf",
  "applicationOf",
  "inoculationOf",
  "provisionOf",
  "vaccinationWith",
  "combinedParticipationOf",
];
const CARE_ACTION_RELATIONS_PATH = CARE_ACTION_RELATION_PROPERTIES.map(
  (prop) => `vocab:${prop}`
).join(" | ");
const URI_LIKE_PATTERN = /^[a-zA-Z][a-zA-Z0-9+.-]*:/;
// DATA_PREFIX, VOCAB_PREFIX, and DATASET_PERSISTENCE_MAP now imported from identifierNormalizers

// Constants and Configuration
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

// Enhanced caching system - 10 minutes for guidelines (longer due to complexity)
const cacheUtils = createCache({
  prefix: "guideline:",
  ttl: CACHE_CONFIG.COMPLEX_TTL,
  maxSize: 200,
});

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

// Identifier normalizer functions now imported from ../lib/guideline/identifierNormalizers.js

function coerceToArray(value) {
  if (Array.isArray(value)) {
    return value.filter((item) => item !== undefined && item !== null);
  }

  if (value === undefined || value === null) {
    return [];
  }

  if (typeof value === "string") {
    return value
      .split(",")
      .map((item) => item.trim())
      .filter((item) => item.length > 0);
  }

  if (value instanceof Set) {
    return Array.from(value).filter((item) => item !== undefined && item !== null);
  }

  return [String(value).trim()].filter((item) => item.length > 0);
}

function isLikelyUri(value) {
  return URI_LIKE_PATTERN.test(value);
}

function prepareUriOrLiteral(value) {
  if (value === undefined || value === null) {
    return null;
  }

  let text = String(value).trim();
  if (!text) {
    return null;
  }

  if (text.startsWith("<") && text.endsWith(">")) {
    return { value: text, isUri: true };
  }

  if (text.startsWith("\"") && text.endsWith("\"")) {
    return { value: text, isUri: false };
  }

  if (
    text.startsWith("data:") ||
    text.startsWith("vocab:") ||
    text.startsWith("tmr:") ||
    text.startsWith("tmrvocab:") ||
    text.startsWith("prov:") ||
    text.startsWith("rdf:") ||
    text.startsWith("rdfs:") ||
    text.startsWith("owl:")
  ) {
    return { value: text, isUri: true };
  }

  if (text.startsWith(DATA_PREFIX)) {
    return { value: `<${text}>`, isUri: true };
  }

  if (isLikelyUri(text)) {
    return { value: `<${text}>`, isUri: true };
  }

  return { value: `'''${escapeQuotes(text)}'''`, isUri: false };
}

function renderPreparedValue(entry) {
  if (!entry) {
    return null;
  }

  let output = entry.value;

  if (entry.isUri) {
    return cleanUriValue(output);
  }

  if (output.startsWith("'''") && output.endsWith("'''")) {
    output = output.slice(3, -3);
  }

  if (output.startsWith('"') && output.endsWith('"')) {
    output = output.slice(1, -1);
  }

  return output;
}

function resolveStrengthValue(strengthInput, isRecommendedInput) {
  if (typeof strengthInput === "string" && strengthInput.trim().length) {
    const normalized = strengthInput
      .trim()
      .toLowerCase()
      .replace(/[\s_-]+/g, "");

    if (RECOMMENDATION_STRENGTH_MAP.has(normalized)) {
      return RECOMMENDATION_STRENGTH_MAP.get(normalized);
    }

    if (normalized === "true" || normalized === "false") {
      isRecommendedInput = normalized === "true";
    }
  }

  if (typeof isRecommendedInput === "string") {
    const normalizedBool = isRecommendedInput.trim().toLowerCase();
    if (["true", "1", "yes", "y"].includes(normalizedBool)) {
      return "vocab:should";
    }
    if (["false", "0", "no", "n"].includes(normalizedBool)) {
      return "vocab:shouldNot";
    }
  } else if (typeof isRecommendedInput === "boolean") {
    return isRecommendedInput ? "vocab:should" : "vocab:shouldNot";
  }

  return "vocab:should";
}

function resolveContributionValue(value) {
  if (typeof value !== "string" || !value.trim()) {
    return CONTRIBUTION_FALLBACK;
  }

  const normalized = value.trim().toLowerCase();

  if (CONTRIBUTION_VALUES.has(normalized)) {
    return normalized;
  }

  if (["pos", "positiveeffect", "increase"].includes(normalized)) {
    return "positive";
  }

  if (["neg", "negativeeffect", "decrease"].includes(normalized)) {
    return "negative";
  }

  if (["neutral", "none", "noeffect"].includes(normalized)) {
    return "neutral";
  }

  return CONTRIBUTION_FALLBACK;
}

function parseDerivedSources(input) {
  const entries = coerceToArray(input);
  const unique = [];

  entries.forEach((item) => {
    const prepared = prepareUriOrLiteral(item);
    if (prepared && !unique.some((existing) => existing.value === prepared.value)) {
      unique.push(prepared);
    }
  });

  return unique;
}

// Recommendation handler functions now imported from ../lib/guideline/recommendationHandlers.js
// Imported: deriveSuggestionFromStrength, cleanUriValue, splitGroupValues, parseBasedOnPairs,
// extractLocalName, wrapUriForValuesClause, buildRecommendationInsertStatement,
// buildRecommendationDeleteStatement, buildRecommendationsQuery, parseRecommendationBinding

function filterRecommendationsByGuideline(recommendations, guidelineUris) {
  if (!guidelineUris || guidelineUris.length === 0) {
    return recommendations;
  }

  const normalized = new Set(guidelineUris.map((uri) => cleanUriValue(uri)).filter(Boolean));

  if (!normalized.size) {
    return recommendations;
  }

  const filtered = recommendations.filter((item) => {
    if (!item.partOf || item.partOf.length === 0) {
      return false;
    }

    return item.partOf.some((uri) => normalized.has(cleanUriValue(uri)));
  });

  return filtered.length > 0 ? filtered : recommendations;
}

async function fetchCareActionDetails(executionUris) {
  if (!executionUris || executionUris.length === 0) {
    return new Map();
  }

  const valuesClause = executionUris
    .map((uri) => `<${uri}>`)
    .join(" ");

  const query = `
    SELECT ?execution ?executionLabel ?resource ?resourceLabel
    WHERE {
      VALUES ?execution { ${valuesClause} }
      OPTIONAL { ?execution rdfs:label ?executionLabel . }
      OPTIONAL {
        ?execution (${CARE_ACTION_RELATIONS_PATH}) ?resource .
        OPTIONAL { ?resource rdfs:label ?resourceLabel . }
      }
    }
  `;

  try {
    const raw = await utils.sparqlJSONQuery("careActions", query);
    const bindings = raw?.results?.bindings ?? [];
    const details = new Map();

    bindings.forEach((binding) => {
      const execution = binding.execution?.value;
      if (!execution) {
        return;
      }

      const executionLabel = binding.executionLabel?.value || null;
      const resourceUri = binding.resource?.value || null;
      const resourceLabel = binding.resourceLabel?.value || null;

      const entry = details.get(execution) || {
        executionLabel: null,
        resource: null,
      };

      if (executionLabel) {
        entry.executionLabel = executionLabel;
      }

      if (resourceUri) {
        if (!entry.resource) {
          entry.resource = {
            uri: resourceUri,
            label: resourceLabel || null,
          };
        } else if (!entry.resource.label && resourceLabel) {
          entry.resource.label = resourceLabel;
        }
      }

      details.set(execution, entry);
    });

    return details;
  } catch (error) {
    logger.warn("Failed to fetch care action details", {
      executionUris,
      error: error.message,
    });
    return new Map();
  }
}

async function enrichCareActionDetails(recommendations) {
  const executionUris = Array.from(
    new Set(
      recommendations
        .map((item) => item.careAction?.execution?.uri)
        .filter(Boolean)
    )
  );

  if (!executionUris.length) {
    return;
  }

  const details = await fetchCareActionDetails(executionUris);

  recommendations.forEach((item) => {
    const executionUri = item.careAction?.execution?.uri;
    if (!executionUri) {
      return;
    }

    const entry = details.get(executionUri);
    if (!entry) {
      return;
    }

    if (entry.executionLabel) {
      item.careAction.execution.label = entry.executionLabel;
    }

    if (entry.resource) {
      item.careAction.resource = {
        uri: entry.resource.uri,
        id: extractLocalName(entry.resource.uri),
        label: entry.resource.label || null,
      };
      item.careAction.resourceLabel = entry.resource.label || null;
    }
  });
}

async function fetchRecommendationsFromDataset({
  datasetId,
  recUris = [],
  guidelineUris = [],
} = {}) {
  if (!datasetId) {
    return [];
  }

  const query = buildRecommendationsQuery(recUris);

  try {
    const raw = await utils.sparqlJSONQuery(datasetId, query);
    const bindings = raw?.results?.bindings ?? [];

    let recommendations = bindings.map((binding) =>
      parseRecommendationBinding(binding, datasetId)
    );

    if (guidelineUris.length) {
      recommendations = filterRecommendationsByGuideline(
        recommendations,
        guidelineUris
      );
    }

    await enrichCareActionDetails(recommendations);

    return recommendations;
  } catch (error) {
    logger.error("Failed to fetch recommendations from dataset", {
      datasetId,
      recUris,
      error: error.message,
    });
    return [];
  }
}

async function extractRecommendationsFallback(guideline, options = {}) {
  try {
    const identifiers = resolveGuidelineIdentifiers({
      guideline,
      cigId: options.cigId,
    });

    const recFilterValues = coerceToArray(options.recUris || options.recUri)
      .map((value) => {
        try {
          return normalizeRecommendationIdentifiers(value, identifiers).uri;
        } catch (error) {
          return cleanUriValue(value);
        }
      })
      .filter((uri) => uri && uri.length > 0);

    return fetchRecommendationsFromDataset({
      datasetId: identifiers.datasetId,
      guidelineUris: identifiers.resourceUris,
      recUris: recFilterValues,
    });
  } catch (error) {
    logger.warn("Unable to resolve guideline identifiers for recommendations", {
      guidelineId: guideline?.id,
      guidelineUri: guideline?.uri,
      cigId: options.cigId,
      error: error.message,
    });
    return [];
  }
}

const extractRecommendations =
  typeof auxFuncts.extractRecommendations === "function"
    ? async (guideline, options = {}) => {
        try {
          const customResult = await auxFuncts.extractRecommendations(
            guideline,
            options
          );
          if (Array.isArray(customResult) && customResult.length > 0) {
            return customResult;
          }
        } catch (error) {
          logger.warn("Custom extractRecommendations failed, using fallback", {
            error: error.message,
          });
        }
        return extractRecommendationsFallback(guideline, options);
      }
    : extractRecommendationsFallback;

function normalizeGuidelineMetadata(metadata = {}, { isUpdate = false } = {}) {
  const safeMeta =
    metadata && typeof metadata === "object" ? metadata : {};
  const hasOwn = (prop) => Object.prototype.hasOwnProperty.call(safeMeta, prop);
  const trim = (value) =>
    typeof value === "string" ? value.trim() || undefined : undefined;

  const provided = {
    version: hasOwn("version"),
    title: hasOwn("title"),
    author: hasOwn("author"),
    organization: hasOwn("organization"),
    label: hasOwn("label"),
    type: hasOwn("type"),
    createdAt: hasOwn("created_at") || hasOwn("createdAt"),
  };

  const values = {
    version: provided.version
      ? trim(safeMeta.version)
      : isUpdate
      ? undefined
      : "1.0.0",
    label: provided.label ? trim(safeMeta.label) : undefined,
    title: provided.title ? trim(safeMeta.title) : undefined,
    author: provided.author ? trim(safeMeta.author) : undefined,
    organization: provided.organization ? trim(safeMeta.organization) : undefined,
    createdAt: provided.createdAt
      ? trim(safeMeta.created_at || safeMeta.createdAt)
      : undefined,
    type: provided.type
      ? Array.isArray(safeMeta.type)
        ? safeMeta.type
            .map((item) =>
              typeof item === "string" ? item.trim() || undefined : undefined
            )
            .filter((item) => typeof item === "string")
        : typeof safeMeta.type === "string"
        ? [trim(safeMeta.type)].filter((item) => item !== undefined)
        : undefined
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
    if (!values.type || values.type.length === 0) {
      values.type = ["vocab:ClinicalGuideline"];
      provided.type = true;
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
  const typeValues = Array.isArray(metadataValues.type)
    ? metadataValues.type
    : metadataValues.type
    ? [metadataValues.type]
    : ["vocab:ClinicalGuideline"];
  const uniqueTypes = Array.from(
    new Set([
      ...typeValues.filter((type) => typeof type === "string" && type.length > 0),
      "owl:NamedIndividual",
    ])
  );
  const labelLiteral = metadataValues.label || guidelineId;
  const predicateObjects = [
    ["a", uniqueTypes.join(" , ")],
    ["vocab:hasLabel", `"${escapeQuotes(labelLiteral)}"`],
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

  if (metadataProvided.type) {
    deleteClauses.push(`${subject} a ?oldType .`);
    whereClauses.push(`OPTIONAL { ${subject} a ?oldType . }`);

    const typeValues = Array.isArray(metadataValues.type)
      ? metadataValues.type
      : metadataValues.type
      ? [metadataValues.type]
      : [];

    const typeSet = new Set(
      typeValues.filter(
        (type) => typeof type === "string" && type.trim().length > 0
      )
    );

    if (typeSet.size === 0) {
      typeSet.add("vocab:ClinicalGuideline");
    }

    typeSet.add("owl:NamedIndividual");

    const newTypes = Array.from(typeSet);

    predicateObjects.push(["a", newTypes.join(" , ")]);
  }

  if (metadataProvided.label) {
    deleteClauses.push(`${subject} vocab:hasLabel ?oldLabel .`);
    whereClauses.push(`OPTIONAL { ${subject} vocab:hasLabel ?oldLabel . }`);

    const labelLiteral = metadataValues.label || guidelineId;
    predicateObjects.push([
      "vocab:hasLabel",
      `"${escapeQuotes(labelLiteral)}"`,
    ]);
  }

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
        guideline.metadata.label = value;
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
        guideline.metadata.type = Array.from(
          new Set([...(guideline.metadata.type || []), value])
        );
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
    if (error?.statusCode === StatusCodes.NOT_FOUND) {
      logger.warn("Guideline dataset not found during fetch", {
        guidelineId,
      });
      return { status: StatusCodes.NOT_FOUND };
    }

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
    .bail()
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
    .exists({ checkNull: true })
    .withMessage("Metadata object is required")
    .bail()
    .isObject()
    .withMessage("Metadata must be an object"),

  body("metadata.label")
    .exists({ checkNull: true })
    .withMessage("Guideline label is required")
    .bail()
    .isString()
    .withMessage("Guideline label must be a string")
    .trim()
    .isLength({ min: 3, max: 200 })
    .withMessage("Guideline label must be between 3 and 200 characters"),

  body("metadata.type")
    .exists({ checkNull: true })
    .withMessage("Guideline type is required")
    .bail()
    .custom((value) => {
      if (typeof value === "string") {
        return value.trim().length > 0;
      }

      if (Array.isArray(value) && value.every((item) => typeof item === "string" && item.trim().length > 0)) {
        return true;
      }

      throw new Error("Guideline type must be a non-empty string or array of strings");
    })
    .withMessage("Guideline type is required and must be a non-empty string or array of strings"),

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
    .isInt({ min: 1, max: 200 })
    .withMessage("Limit must be an integer between 1 and 200"),

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

const recommendationAddValidation = [
  body("cig_id")
    .notEmpty()
    .withMessage("Guideline identifier (cig_id) is required")
    .bail()
    .isString()
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Guideline identifier must be between 1 and 100 characters"),
  body("id")
    .notEmpty()
    .withMessage("Recommendation id is required")
    .bail()
    .isString()
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Recommendation id must be between 1 and 100 characters"),
  body("belief_id")
    .notEmpty()
    .withMessage("Belief identifier is required")
    .bail()
    .isString()
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Belief identifier must be between 1 and 100 characters"),
  body("careAction_id")
    .notEmpty()
    .withMessage("Care action identifier is required")
    .bail()
    .isString()
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Care action identifier must be between 1 and 100 characters"),
  body("label")
    .notEmpty()
    .withMessage("Recommendation label is required")
    .bail()
    .isString()
    .bail()
    .trim()
    .isLength({ min: 3, max: 500 })
    .withMessage("Recommendation label must be between 3 and 500 characters"),
  body("strength")
    .optional()
    .isString()
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Strength must be a string up to 100 characters"),
  body("contribution")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 1, max: 50 })
    .withMessage("Contribution must be a string up to 50 characters"),
  body("author")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 1, max: 200 })
    .withMessage("Author must be a string up to 200 characters"),
  body("derivedFrom")
    .optional()
    .custom((value) => {
      if (value === undefined || value === null) {
        return true;
      }

      if (typeof value === "string" || Array.isArray(value) || value instanceof Set) {
        return true;
      }

      throw new Error("derivedFrom must be a string, array, or set of values");
    }),
  body("extractedFrom")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 1, max: 300 })
    .withMessage("extractedFrom must be a string up to 300 characters"),
  body("precondition_id")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("precondition_id must be a string up to 100 characters"),
  body("isRecommended")
    .optional()
    .isIn([true, false, "true", "false", "1", "0", 1, 0])
    .withMessage("isRecommended must be a boolean value"),
];

const recommendationPathValidation = [
  param("cigId")
    .notEmpty()
    .withMessage("Guideline identifier is required")
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Guideline identifier must be between 1 and 100 characters"),
  param("recId")
    .notEmpty()
    .withMessage("Recommendation identifier is required")
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("Recommendation identifier must be between 1 and 100 characters"),
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
        label: metadataValues.label || guidelineId,
        type: metadataValues.type,
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
  const startTime = logStart(req, "Guideline creation requested", {
    filename: req.body?.filename,
    hasMetadata: !!req.body?.metadata,
  });

  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logWarn(req, "Guideline creation validation failed", startTime, {
        errors: errors.array(),
      });

      const errorArray = errors.array();
      const missingFields = Array.from(
        new Set(
          errorArray
            .filter((err) => typeof err.msg === "string" && err.msg.toLowerCase().includes("required"))
            .map((err) => err.path)
        )
      );

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errorArray,
        missing_fields: missingFields,
      });
    }

    const { guideline, filename, metadata = {} } = req.body;

    // Parse and validate guideline structure
    let parsedGuideline;
    try {
      parsedGuideline = await parseGuidelineContent(guideline);
    } catch (parseError) {
      logError(req, "Guideline parsing failed", startTime, parseError);

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

    logSuccess(req, "Guideline created successfully", startTime, {
      guidelineId: createdIds.guidelineId,
      guidelineUri: createdIds.guidelineUri,
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
    logError(req, "Failed to create guideline", startTime, error);

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
      const errorArray = errors.array();
      const missingFields = Array.from(
        new Set(
          errorArray
            .filter((err) => typeof err.msg === "string" && err.msg.toLowerCase().includes("required"))
            .map((err) => err.path)
        )
      );

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errorArray,
        missing_fields: missingFields,
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
      const errorArray = errors.array();
      const missingFields = Array.from(
        new Set(
          errorArray
            .filter((err) => typeof err.msg === "string" && err.msg.toLowerCase().includes("required"))
            .map((err) => err.path)
        )
      );

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errorArray,
        missing_fields: missingFields,
      });
    }

    const { id } = req.params;
    const { guideline, metadata = {} } = req.body;

    logger.info("Updating guideline", {
      guidelineId: id,
      guidelineLength: typeof guideline === "string" ? guideline.length : null,
      metadata,
      ip: req.ip,
    });

    // Check if guideline exists
  const existsResult = await fetchGuidelineById(id);
  const { status: existsStatus } = existsResult;

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
  const cause = existsResult?.error ? String(existsResult.error) : "Unknown error";
      throw new ErrorHandler(
        existsStatus || StatusCodes.INTERNAL_SERVER_ERROR,
        `Failed to verify guideline existence (${existsStatus || "unknown"}): ${cause}`
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
            label: metadataValues.label,
            type: metadataValues.type,
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
 * Add a clinical recommendation to a guideline
 * Creates a recommendation nanopublication with belief, care action, and metadata
 */
router.post("/rec/add", [writeLimiter, recommendationAddValidation], async (req, res) => {
  const startTime = logStart(req, "Recommendation creation requested", {
    cig_id: req.body?.cig_id,
    rec_id: req.body?.id,
    belief_id: req.body?.belief_id,
  });

  try {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      logWarn(req, "Recommendation creation validation failed", startTime, {
        errors: errors.array(),
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

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
      extractedFrom,
      precondition_id,
      precondition,
    } = req.body;

    const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId: cig_id });
    const recommendationIdentifiers = normalizeRecommendationIdentifiers(
      id,
      guidelineIdentifiers
    );
    const beliefIdentifiers = normalizeBeliefIdentifier(belief_id);

    if (!beliefIdentifiers) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "Belief identifier could not be resolved"
      );
    }

    const careActionIdentifiers = normalizeCareActionIdentifier(careAction_id);
    const derivedEntries = parseDerivedSources(derivedFrom);
    const strengthValue = resolveStrengthValue(strength, isRecommended);
    const contributionValue = resolveContributionValue(contribution);
    const authorResource = normalizeAuthorIdentifier(author);
    const generatedAt = new Date().toISOString();
    const extractedEntry = extractedFrom
      ? prepareUriOrLiteral(extractedFrom)
      : null;
    const preconditionResource = normalizePreconditionIdentifier(
      precondition_id || precondition
    );

    const { sparql } = buildRecommendationInsertStatement({
      cigIdentifiers: guidelineIdentifiers,
      recommendation: recommendationIdentifiers,
      belief: beliefIdentifiers,
      careAction: careActionIdentifiers,
      label,
      strength: strengthValue,
      contribution: contributionValue,
      derivedFrom: derivedEntries,
      author: authorResource,
      generatedAt,
      extractedFrom: extractedEntry,
      precondition: preconditionResource,
    });

    const updateResponse = await utils.sparqlUpdate(
      guidelineIdentifiers.datasetId,
      sparql
    );

    if (updateResponse.status >= 400) {
      throw new ErrorHandler(
        updateResponse.status,
        `Failed to create recommendation: ${
          updateResponse.data || "Unknown error"
        }`
      );
    }

    cacheUtils.invalidatePattern(guidelineIdentifiers.localId);
    cacheUtils.invalidatePattern(guidelineIdentifiers.datasetId);
    cacheUtils.invalidatePattern(`/recommendations/${guidelineIdentifiers.localId}`);
    cacheUtils.invalidatePattern("list");

    logger.info("Clinical recommendation created successfully", {
      cig_id,
      recommendationId: recommendationIdentifiers.localId,
      dataset: guidelineIdentifiers.datasetId,
    });

    const derivedFromResponse = derivedEntries
      .map(renderPreparedValue)
      .filter((item) => item !== null);

    const responsePayload = {
      id: recommendationIdentifiers.localId,
      uri: recommendationIdentifiers.uri,
      dataset: guidelineIdentifiers.datasetId,
      graphs: {
        assertion: recommendationIdentifiers.assertionGraphUri,
        head: recommendationIdentifiers.headGraphUri,
        provenance: recommendationIdentifiers.provenanceGraphUri,
        publication: recommendationIdentifiers.publicationGraphUri,
      },
      label,
      strength: strengthValue,
      suggestion: deriveSuggestionFromStrength(strengthValue),
      contribution: contributionValue,
      belief: {
        id: beliefIdentifiers.localId,
        uri: beliefIdentifiers.uri,
      },
      careAction: {
        execution: {
          id: careActionIdentifiers.execution.localId,
          uri: careActionIdentifiers.execution.uri,
        },
        resource: careActionIdentifiers.resource
          ? {
              id: careActionIdentifiers.resource.localId,
              uri: careActionIdentifiers.resource.uri,
            }
          : null,
      },
      derivedFrom: derivedFromResponse,
      extractedFrom: extractedEntry ? renderPreparedValue(extractedEntry) : null,
      preconditions: preconditionResource
        ? [cleanUriValue(preconditionResource)]
        : [],
      provenance: {
        generatedAt,
        author: authorResource ? cleanUriValue(authorResource) : null,
      },
    };

    logSuccess(req, "Recommendation created successfully", startTime, {
      recommendationId: recommendationIdentifiers.localId,
      guidelineId: guidelineIdentifiers.datasetId,
    });

    res.status(StatusCodes.CREATED).json({
      status: "success",
      message: `Recommendation ${recommendationIdentifiers.localId} created successfully in guideline ${guidelineIdentifiers.datasetId}`,
      data: responsePayload,
    });
  } catch (error) {
    logError(req, "Failed to create clinical recommendation", startTime, error);

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
 * POST /subguideline/add - Create a subguideline within a guideline dataset
 */
router.post(
  "/subguideline/add",
  [
    writeLimiter,
    body("cig_id").trim().notEmpty().withMessage("cig_id is required"),
    body("subcig_id").trim().notEmpty().withMessage("subcig_id is required"),
    body("recs_ids").trim().notEmpty().withMessage("recs_ids is required"),
    body("description").optional().trim(),
  ],
  async (req, res) => {
    const startTime = Date.now();
    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logWarn(req, "Subguideline creation validation failed", startTime, {
          errors: errors.array(),
        });

        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { cig_id, subcig_id, recs_ids, description } = req.body;

      logStart(req, "Creating subguideline", {
        cig_id,
        subcig_id,
        recs_count: recs_ids.split(",").length,
      });

      // Resolve parent guideline identifiers
      const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId: cig_id });

      // Build subguideline URI (data:subCIG-DEMO_1)
      const subguidelineLocalId = `subCIG-${subcig_id}`;
      const subguidelinePrefixed = `data:${subguidelineLocalId}`;
      const subguidelineUri = `http://anonymous.org/data/${subguidelineLocalId}`;

      // Parse and normalize recommendation IDs
      const recIds = recs_ids.split(",").map((id) => id.trim()).filter((id) => id);
      const recUris = recIds.map((recId) => {
        // Check if it's already a full URI
        if (recId.startsWith("http://")) {
          return `<${recId}>`;
        }
        // Check if it's prefixed
        if (recId.startsWith("data:")) {
          return recId;
        }
        // Build the recommendation URI
        const recIdentifiers = normalizeRecommendationIdentifiers(
          recId,
          guidelineIdentifiers
        );
        return recIdentifiers.prefixed;
      });

      // Build SPARQL INSERT statement
      const subguidelineTriples = [
        `${subguidelinePrefixed} a owl:NamedIndividual , vocab:subGuideline .`,
        `${subguidelinePrefixed} vocab:isSubGuidelineOf ${guidelineIdentifiers.prefixedResourceUris[0]} .`,
      ];

      if (description) {
        subguidelineTriples.push(
          `${subguidelinePrefixed} rdfs:label """${description}"""@en .`
        );
      }

      // Add vocab:isPartOf triples for each recommendation
      const recTriples = recUris.map(
        (recUri) => `${recUri} vocab:isPartOf ${subguidelinePrefixed} .`
      );

      const sparqlUpdate = `
        INSERT DATA {
          ${subguidelineTriples.join("\n          ")}
          ${recTriples.join("\n          ")}
        }
      `;

      const updateResponse = await utils.sparqlUpdate(
        guidelineIdentifiers.datasetId,
        sparqlUpdate
      );

      if (updateResponse.status >= 400) {
        throw new ErrorHandler(
          updateResponse.status,
          `Failed to create subguideline: ${updateResponse.data || "Unknown error"}`
        );
      }

      // Invalidate cache
      cacheUtils.invalidatePattern(guidelineIdentifiers.localId);
      cacheUtils.invalidatePattern(guidelineIdentifiers.datasetId);

      logSuccess(req, "Subguideline created successfully", startTime, {
        cig_id,
        subcig_id,
        subguidelineUri,
        recommendations_count: recUris.length,
      });

      res.status(StatusCodes.CREATED).json({
        status: "success",
        message: `Subguideline ${subcig_id} created successfully in ${cig_id}`,
        data: {
          subguideline: {
            id: subcig_id,
            localId: subguidelineLocalId,
            uri: subguidelineUri,
            prefixed: subguidelinePrefixed,
          },
          parent_guideline: {
            id: cig_id,
            dataset: guidelineIdentifiers.datasetId,
          },
          recommendations: recUris,
          description: description || null,
        },
      });
    } catch (error) {
      logError(req, "Failed to create subguideline", startTime, error);

      if (error instanceof ErrorHandler) {
        return res.status(error.statusCode).json({
          status: "error",
          message: error.message,
        });
      }

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to create subguideline",
        error: process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * POST /subguideline/delete - Delete a subguideline from a guideline dataset
 */
router.post(
  "/subguideline/delete",
  [
    writeLimiter,
    body("cig_id").trim().notEmpty().withMessage("cig_id is required"),
    body("subcig_id").trim().notEmpty().withMessage("subcig_id is required"),
  ],
  async (req, res) => {
    const startTime = Date.now();
    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logWarn(req, "Subguideline deletion validation failed", startTime, {
          errors: errors.array(),
        });

        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { cig_id, subcig_id } = req.body;

      logStart(req, "Deleting subguideline", {
        cig_id,
        subcig_id,
      });

      // Resolve parent guideline identifiers
      const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId: cig_id });

      // Build subguideline URI (data:subCIG-DEMO_1)
      const subguidelineLocalId = `subCIG-${subcig_id}`;
      const subguidelinePrefixed = `data:${subguidelineLocalId}`;

      // Build SPARQL DELETE statement
      // 1. Delete all triples where subguideline is subject or object
      // 2. Delete all vocab:isPartOf references from recommendations to this subguideline
      const sparqlDelete = `
        DELETE WHERE {
          ${subguidelinePrefixed} ?p ?o .
        };
        DELETE WHERE {
          ?s ?p ${subguidelinePrefixed} .
        };
        DELETE WHERE {
          ?rec vocab:isPartOf ${subguidelinePrefixed} .
        }
      `;

      const updateResponse = await utils.sparqlUpdate(
        guidelineIdentifiers.datasetId,
        sparqlDelete
      );

      if (updateResponse.status >= 400) {
        throw new ErrorHandler(
          updateResponse.status,
          `Failed to delete subguideline: ${updateResponse.data || "Unknown error"}`
        );
      }

      // Invalidate cache
      cacheUtils.invalidatePattern(guidelineIdentifiers.localId);
      cacheUtils.invalidatePattern(guidelineIdentifiers.datasetId);

      logSuccess(req, "Subguideline deleted successfully", startTime, {
        cig_id,
        subcig_id,
        subguidelineUri: `http://anonymous.org/data/${subguidelineLocalId}`,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `Subguideline ${subcig_id} deleted successfully from ${cig_id}`,
        data: {
          subguideline: {
            id: subcig_id,
            localId: subguidelineLocalId,
            prefixed: subguidelinePrefixed,
          },
          parent_guideline: {
            id: cig_id,
            dataset: guidelineIdentifiers.datasetId,
          },
          deleted_at: new Date().toISOString(),
        },
      });
    } catch (error) {
      logError(req, "Failed to delete subguideline", startTime, error);

      if (error instanceof ErrorHandler) {
        return res.status(error.statusCode).json({
          status: "error",
          message: error.message,
        });
      }

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to delete subguideline",
        error: process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

/**
 * POST /add - Copy recommendation nanopubs (named graphs only) from one guideline dataset to another
 * - If subguidelines and/or recommendations are provided, only those recs are copied
 * - If neither is provided, all recommendations in the source dataset are copied
 * - Default graph is NOT copied
 */
router.post(
  "/add",
  [
    writeLimiter,
    body("cig_from").trim().notEmpty().withMessage("cig_from is required"),
    body("cig_to").trim().notEmpty().withMessage("cig_to is required"),
    body("subguidelines").optional().trim(),
    body("recommendations").optional().trim(),
  ],
  async (req, res) => {
    const startTime = Date.now();

    const constructPrefixes = `
      PREFIX vocab: <http://anonymous.org/vocab/>
      PREFIX data: <http://anonymous.org/data/>
      PREFIX nanopub: <http://www.nanopub.org/nschema#>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    `;

    const splitCsv = (value = "") =>
      value
        .split(",")
        .map((v) => v.trim())
        .filter((v) => v.length > 0);

    const normalizeSubguidelinePrefixed = (raw) => {
      if (!raw) return null;
      if (raw.startsWith("http://")) return `<${raw}>`;
      if (raw.startsWith("data:")) return raw;
      if (raw.startsWith("subCIG-")) return `data:${raw}`;
      return `data:subCIG-${raw}`;
    };

    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logWarn(req, "Guideline add (copy) validation failed", startTime, {
          errors: errors.array(),
        });

        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { cig_from, cig_to, subguidelines, recommendations } = req.body;

      // Resolve dataset IDs
      const sourceDatasetId = normalizeDatasetId(cig_from);
      const targetDatasetId = normalizeDatasetId(cig_to);

      // Resolve guideline identifiers (for rec normalization)
      const sourceGuidelineIds = resolveGuidelineIdentifiers({ cigId: cig_from });
      const targetGuidelinePrefixed = `data:${targetDatasetId}`;

      // Collect subguideline URIs (prefixed) if provided
      const subguidelineList = splitCsv(subguidelines);
      const subPrefixedList = subguidelineList
        .map(normalizeSubguidelinePrefixed)
        .filter(Boolean);

      // Collect recommendation URIs from explicit list
      const recList = splitCsv(recommendations);
      const recUriSet = new Set(
        recList.map((recId) => {
          if (recId.startsWith("http://")) return wrapUriForValuesClause(recId);
          if (recId.startsWith("data:")) return recId;
          const recIdentifiers = normalizeRecommendationIdentifiers(recId, sourceGuidelineIds);
          return wrapUriForValuesClause(recIdentifiers.uri);
        })
      );

      // If subguidelines specified, fetch recs linked via vocab:isPartOf
      if (subPrefixedList.length) {
        const valuesClause = subPrefixedList
          .map((uri) => wrapUriForValuesClause(uri))
          .filter(Boolean)
          .join(" ");

        const recsFromSubsQuery = `
          ${constructPrefixes}
          SELECT DISTINCT ?rec WHERE {
            VALUES ?sub { ${valuesClause} }
            ?rec (vocab:isPartOf | vocab:partOf) ?sub .
          }
        `;

        const subsResult = await utils.sparqlJSONQuery(
          sourceDatasetId,
          recsFromSubsQuery
        );

        const subRecUris = subsResult?.results?.bindings
          ?.map((b) => b.rec?.value)
          ?.filter(Boolean)
          ?.map((uri) => wrapUriForValuesClause(uri))
          ?.filter(Boolean) || [];

        subRecUris.forEach((u) => recUriSet.add(u));

        logger.info("Guideline add: subguideline recs collected", {
          subguidelines: subPrefixedList,
          subRecCount: subRecUris.length,
        });
      }

      // If no filters, copy all recommendations in the source dataset
      if (!subPrefixedList.length && !recUriSet.size) {
        const allRecsQuery = `
          ${constructPrefixes}
          SELECT DISTINCT ?rec WHERE {
            GRAPH ?g { ?rec a vocab:ClinicalRecommendation . }
          }
        `;

        const allRecs = await utils.sparqlJSONQuery(sourceDatasetId, allRecsQuery);
        const allRecUris = allRecs?.results?.bindings
          ?.map((b) => b.rec?.value)
          ?.filter(Boolean)
          ?.map((uri) => wrapUriForValuesClause(uri))
          ?.filter(Boolean) || [];

        allRecUris.forEach((u) => recUriSet.add(u));
      }

      if (!recUriSet.size) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "No recommendations found to copy",
        });
      }

      logger.info("Guideline add: recommendations to copy", {
        recCount: recUriSet.size,
        recs: Array.from(recUriSet),
        sourceDataset: sourceDatasetId,
        targetDataset: targetDatasetId,
      });

      // Build CONSTRUCT to pull named graphs for the selected recommendations
      const recValues = Array.from(recUriSet).join(" ");

      const constructQuery = `${constructPrefixes}
        CONSTRUCT {
          GRAPH ?head { ?head ?hp ?ho . }
          GRAPH ?assert {
            ?s ?pAll ?oAdjAll .
            ?rec vocab:copiedFrom ?sourceGuideline .
          }
          GRAPH ?prov { ?ps ?pp ?po . }
          GRAPH ?pub { ?us ?up ?uo . }
        }
        WHERE {
          VALUES ?rec { ${recValues} }
          VALUES (?sourceGuideline ?targetGuideline) { ( ${sourceGuidelineIds.prefixedResourceUris[0]} ${targetGuidelinePrefixed} ) }

          # Locate assertion graph(s) containing the recommendation as subject
          GRAPH ?assert { ?rec ?p ?o . }

          # Include all triples from the assertion graph
          GRAPH ?assert { ?s ?pAll ?oAll . }

          # Rewrite partOf/isPartOf objects only when the subject is the recommendation
          BIND(
            IF(?s = ?rec && ?pAll IN (vocab:isPartOf, vocab:partOf), ?targetGuideline, ?oAll)
            AS ?oAdjAll
          )

          OPTIONAL {
            GRAPH ?head {
              ?head nanopub:hasAssertion ?assert ;
                    nanopub:hasProvenance ?prov ;
                    nanopub:hasPublicationInfo ?pub .
            }

            GRAPH ?head   { ?head ?hp ?ho . }
            GRAPH ?prov   { ?ps ?pp ?po . }
            GRAPH ?pub    { ?us ?up ?uo . }
          }
        }
      `;

      const nquads = await utils.sparqlConstruct(sourceDatasetId, constructQuery);

      if (!nquads || !String(nquads).trim()) {
        logger.warn("Guideline add: construct returned empty", {
          recCount: recUriSet.size,
          sourceDataset: sourceDatasetId,
          targetDataset: targetDatasetId,
        });
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "No data found to copy",
        });
      }

      const postResp = await utils.postNQuads(targetDatasetId, nquads);

      logSuccess(req, "Guideline add (copy) completed", startTime, {
        cig_from,
        cig_to,
        rec_count: recUriSet.size,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: "Recommendations copied successfully (named graphs only)",
        data: {
          cig_from,
          cig_to,
          recommendations_copied: recUriSet.size,
          subguidelines_processed: subPrefixedList.length,
        },
      });
    } catch (error) {
      logError(req, "Failed to add (copy) guideline content", startTime, error);

      if (error instanceof ErrorHandler) {
        return res.status(error.statusCode).json({
          status: "error",
          message: error.message,
        });
      }

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to copy guideline content",
        error: process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

router.post(
  "/create",
  [
    writeLimiter,
    body("cig_id")
      .notEmpty()
      .withMessage("cig_id is required")
      .isString()
      .trim()
      .isLength({ min: 1, max: 100 })
      .withMessage("cig_id must be between 1 and 100 characters"),
    body("IsPersistent")
      .optional()
      .isString()
      .trim()
      .isLength({ max: 50 })
      .withMessage("IsPersistent must be a string up to 50 characters"),
    body("description")
      .optional()
      .isString()
      .trim()
      .isLength({ max: 500 })
      .withMessage("description must be a string up to 500 characters"),
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

      const { cig_id, IsPersistent, description } = req.body;
      const datasetId = normalizeDatasetId(cig_id);
      const dbType = resolveDatasetPersistenceFlag(IsPersistent);

      logger.info("Creating guideline dataset", {
        requestedId: cig_id,
        datasetId,
        persistence: dbType,
        description,
        ip: req.ip,
      });

      const response = await utils.sparqlDatasetUpdate(false, datasetId, dbType);

      if (response.status >= 400) {
        throw new ErrorHandler(
          response.status,
          `Failed to create guideline dataset: ${response.data || "Unknown error"}`
        );
      }

      const labelSource = description && description.trim() ? description : datasetId;
      const labelLiteral = escapeQuotes(labelSource.toUpperCase());
      const guidelineDefinition = `
        INSERT DATA {
          data:${datasetId} rdf:type vocab:ClinicalGuideline , owl:NamedIndividual ;
            rdfs:label "${labelLiteral}"@en .
        }
      `.trim();

      const definitionResponse = await utils.sparqlUpdate(datasetId, guidelineDefinition);

      if (definitionResponse.status >= StatusCodes.BAD_REQUEST) {
        throw new ErrorHandler(
          definitionResponse.status,
          `Dataset created but failed to register guideline definition: ${
            definitionResponse.data || "Unknown error"
          }`
        );
      }

      cacheUtils.invalidatePattern(datasetId);

      res.status(StatusCodes.CREATED).json({
        status: "success",
        message: `Guideline dataset ${datasetId} created successfully`,
        data: {
          dataset: datasetId,
          persistence: dbType,
          description: description || null,
          fusekiEndpoint: `${process.env.FUSEKI_URL || "http://localhost:3030"}/${datasetId}`,
        },
      });
    } catch (error) {
      logger.error("Failed to create guideline dataset", {
        cig_id: req.body?.cig_id,
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
        message: "Failed to create guideline dataset",
      });
    }
  }
);

router.get(
  "/rec/:cigId/:recId",
  [readLimiter, recommendationPathValidation],
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

      const { cigId, recId } = req.params;

      const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId });
      const recommendationIdentifiers = normalizeRecommendationIdentifiers(
        recId,
        guidelineIdentifiers
      );

      const recommendations = await fetchRecommendationsFromDataset({
        datasetId: guidelineIdentifiers.datasetId,
        recUris: [recommendationIdentifiers.uri],
        guidelineUris: guidelineIdentifiers.resourceUris,
      });

      if (!recommendations.length) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Recommendation not found",
        });
      }

      res.status(StatusCodes.OK).json({
        status: "success",
        data: recommendations[0],
      });
    } catch (error) {
      logger.error("Failed to retrieve recommendation", {
        cigId: req.params.cigId,
        recId: req.params.recId,
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
        message: "Failed to retrieve recommendation",
      });
    }
  }
);

/**
 * POST /rec/delete endpoint - Delete recommendation with body parameters
 */
router.post(
  "/rec/delete",
  [
    writeLimiter,
    body("cig_id").trim().notEmpty().withMessage("cig_id is required"),
    body("rec_id").trim().notEmpty().withMessage("rec_id is required"),
  ],
  async (req, res) => {
    const startTime = Date.now();
    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logWarn(req, "Recommendation deletion validation failed", startTime, {
          errors: errors.array(),
        });

        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { cig_id, rec_id } = req.body;

      logStart(req, "Deleting recommendation", {
        cig_id,
        rec_id,
      });

      const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId: cig_id });
      const recommendationIdentifiers = normalizeRecommendationIdentifiers(
        rec_id,
        guidelineIdentifiers
      );

      logger.info("Deleting recommendation - identifiers resolved", {
        cig_id,
        rec_id,
        guidelineIdentifiers: {
          localId: guidelineIdentifiers.localId,
          datasetId: guidelineIdentifiers.datasetId,
        },
        recommendationIdentifiers: {
          localId: recommendationIdentifiers.localId,
          uri: recommendationIdentifiers.uri,
          prefixed: recommendationIdentifiers.prefixed,
        },
      });

      const existing = await fetchRecommendationsFromDataset({
        datasetId: guidelineIdentifiers.datasetId,
        recUris: [recommendationIdentifiers.uri],
        guidelineUris: guidelineIdentifiers.resourceUris,
      });

      if (!existing.length) {
        logWarn(req, "Recommendation not found", startTime, {
          cig_id,
          rec_id,
          dataset: guidelineIdentifiers.datasetId,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: `Recommendation not found: ${rec_id}`,
        });
      }

      const deleteStatement = buildRecommendationDeleteStatement(
        recommendationIdentifiers,
        guidelineIdentifiers
      );

      const updateResponse = await utils.sparqlUpdate(
        guidelineIdentifiers.datasetId,
        deleteStatement
      );

      if (updateResponse.status >= 400) {
        throw new ErrorHandler(
          updateResponse.status,
          `Failed to delete recommendation: ${
            updateResponse.data || "Unknown error"
          }`
        );
      }

      cacheUtils.invalidatePattern(guidelineIdentifiers.localId);
      cacheUtils.invalidatePattern(guidelineIdentifiers.datasetId);
      cacheUtils.invalidatePattern(`/recommendations/${guidelineIdentifiers.localId}`);
      cacheUtils.invalidatePattern("list");

      logSuccess(req, "Recommendation deleted successfully", startTime, {
        cig_id,
        rec_id,
        dataset: guidelineIdentifiers.datasetId,
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        message: `Recommendation ${rec_id} deleted successfully`,
        data: {
          cig_id,
          rec_id,
          recommendation_id: recommendationIdentifiers.localId,
          dataset: guidelineIdentifiers.datasetId,
          deleted_at: new Date().toISOString(),
        },
      });
    } catch (error) {
      logError(req, "Failed to delete recommendation", startTime, error);

      if (error instanceof ErrorHandler) {
        return res.status(error.statusCode).json({
          status: "error",
          message: error.message,
        });
      }

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to delete recommendation",
        error: process.env.NODE_ENV === "development" ? error.message : undefined,
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
 * Get guideline and its recommendations by cig_id
 * Original endpoint: GET /get with cig_id parameter
 */
router.post(
  "/get",
  [readLimiter, [
    body("cig_id")
      .notEmpty()
      .withMessage("Guideline identifier (cig_id) is required")
      .isString()
      .trim()
      .isLength({ min: 1, max: 100 })
      .withMessage("cig_id must be between 1 and 100 characters"),
  ]],
  async (req, res) => {
    const startTime = logStart(req, "Get guideline by cig_id", {
      cig_id: req.body?.cig_id,
    });

    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logWarn(req, "Get guideline validation failed", startTime, {
          errors: errors.array(),
        });

        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { cig_id } = req.body;
      const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId: cig_id });
      const localId = guidelineIdentifiers.localId;

      logger.info("Retrieving guideline by cig_id", {
        cig_id,
        localId,
        dataset: guidelineIdentifiers.datasetId,
      });

      // Check cache
      const cacheKey = cacheUtils.generateKey(`/get/${localId}`);
      const cachedResult = cacheUtils.get(cacheKey);

      if (cachedResult) {
        logSuccess(req, "Guideline retrieved (cached)", startTime, {
          guidelineId: localId,
        });

        return res.status(StatusCodes.OK).json({
          ...cachedResult,
          cached: true,
        });
      }

      // Fetch guideline by resolved local ID
      const { status, guideline, error: retrievalError } =
        await fetchGuidelineById(localId);

      if (status === StatusCodes.NOT_FOUND) {
        logWarn(req, "Guideline not found", startTime, {
          guidelineId: localId,
        });

        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: `Guideline ${cig_id} not found`,
        });
      }

      if (status !== StatusCodes.OK) {
        throw new ErrorHandler(
          status || StatusCodes.INTERNAL_SERVER_ERROR,
          retrievalError || "Failed to retrieve guideline"
        );
      }

      // Extract recommendations from guideline
      const recommendations = await extractRecommendations(guideline);

      const responseData = {
        status: "success",
        data: {
          cig_id,
          guideline_id: localId,
          guideline,
          recommendations,
          count: recommendations.length,
        },
      };

      // Cache the result
      cacheUtils.set(cacheKey, responseData, CACHE_CONFIG.COMPLEX_TTL);

      logSuccess(req, "Guideline retrieved successfully", startTime, {
        guidelineId: localId,
        recommendationsCount: recommendations.length,
      });

      res.status(StatusCodes.OK).json({
        ...responseData,
        cached: false,
      });
    } catch (error) {
      logError(req, "Failed to retrieve guideline", startTime, error);

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
  }
);

/**
 * Get all recommendation URIs from a specific dataset (guideline)
 * Query endpoint to retrieve all recommendations for a given CIG
 */
router.post(
  "/recs/get",
  [readLimiter, [
    body("cig_id")
      .notEmpty()
      .withMessage("Guideline identifier (cig_id) is required")
      .isString()
      .trim()
      .isLength({ min: 1, max: 100 })
      .withMessage("cig_id must be between 1 and 100 characters"),
  ]],
  async (req, res) => {
    const startTime = logStart(req, "Query: Get all recommendation URIs", {
      cig_id: req.body?.cig_id,
    });

    try {
      const errors = validationResult(req);
      if (!errors.isEmpty()) {
        logWarn(req, "Get recommendations query validation failed", startTime, {
          errors: errors.array(),
        });

        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Validation failed",
          errors: errors.array(),
        });
      }

      const { cig_id } = req.body;
      const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId: cig_id });

      logger.info("Retrieving all recommendation URIs", {
        cig_id,
        dataset: guidelineIdentifiers.datasetId,
      });

      // Check cache
      const cacheKey = cacheUtils.generateKey(`/recs/get/${guidelineIdentifiers.datasetId}`);
      const cachedResult = cacheUtils.get(cacheKey);

      if (cachedResult) {
        logSuccess(req, "Recommendation URIs retrieved (cached)", startTime, {
          count: cachedResult.data.recommendations.length,
        });

        return res.status(StatusCodes.OK).json({
          ...cachedResult,
          cached: true,
        });
      }

      // Build SPARQL query to get recommendation URIs via nanopub heads
      const sparqlQuery = `
        PREFIX nanopub: <http://www.nanopub.org/nschema#>
        PREFIX vocab: <http://anonymous.org/vocab/>
        SELECT DISTINCT ?recommendation ?label
        WHERE {
          GRAPH ?head {
            ?head nanopub:hasAssertion ?assert .
          }
          FILTER regex(str(?head), "_head$")
          GRAPH ?assert {
            ?recommendation a vocab:ClinicalRecommendation .
            OPTIONAL { ?recommendation vocab:hasLabel ?label . }
          }
        }
        ORDER BY ?recommendation
      `;

      try {
        const queryResult = await utils.sparqlJSONQuery(guidelineIdentifiers.datasetId, sparqlQuery);

        const recommendations = queryResult.results.bindings.map((binding) => ({
          uri: binding.recommendation?.value,
          label: binding.label?.value,
        }));

        const responseData = {
          status: "success",
          data: {
            cig_id,
            dataset: guidelineIdentifiers.datasetId,
            recommendations,
            count: recommendations.length,
          },
        };

        // Cache the result
        cacheUtils.set(cacheKey, responseData, CACHE_CONFIG.QUERY_TTL);

        logSuccess(req, "Recommendation URIs retrieved successfully", startTime, {
          count: recommendations.length,
          dataset: guidelineIdentifiers.datasetId,
        });

        res.status(StatusCodes.OK).json({
          ...responseData,
          cached: false,
        });
      } catch (queryError) {
        logger.error("SPARQL query failed for recommendations", {
          cig_id,
          dataset: guidelineIdentifiers.datasetId,
          error: queryError.message,
        });

        throw new ErrorHandler(
          StatusCodes.INTERNAL_SERVER_ERROR,
          `Failed to query recommendations: ${queryError.message}`
        );
      }
    } catch (error) {
      logError(req, "Failed to retrieve recommendation URIs", startTime, error);

      if (error instanceof ErrorHandler) {
        return res.status(error.statusCode).json({
          status: "error",
          message: error.message,
        });
      }

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to retrieve recommendation URIs",
        error: process.env.NODE_ENV === "development" ? error.message : undefined,
      });
    }
  }
);

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

  // POST /delete endpoint (delete guideline)
  router.post(
    `${route}/delete`,
    writeLimiter,
    body("cig_id").trim().notEmpty().withMessage("cig_id is required"),
    async (req, res) => {
      const startTime = Date.now();
      try {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
          logWarn(req, "Guideline deletion validation failed", startTime, {
            errors: errors.array(),
          });

          return res.status(StatusCodes.BAD_REQUEST).json({
            status: "error",
            message: "Validation failed",
            errors: errors.array(),
          });
        }

        const { cig_id } = req.body;

        logStart(req, "Deleting guideline", {
          cig_id,
        });

        // Normalize the guideline identifier
        const guidelineIdentifiers = resolveGuidelineIdentifiers({ cigId: cig_id });

        // Delete the dataset from Fuseki
        const datasetId = guidelineIdentifiers.datasetId;
        
        try {
          const axios = require("axios");
          const jenaBaseUrl = `http://${process.env.JENA_HOST || "127.0.0.1"}:${process.env.JENA_PORT || "3030"}`;
          const auth = {
            username: process.env.FUSEKI_USER || "admin",
            password: process.env.FUSEKI_PASSWORD || "road2h",
          };

          await axios.delete(`${jenaBaseUrl}/$/datasets/${datasetId}`, {
            auth,
            headers: {
              "Content-Type": "application/x-www-form-urlencoded",
            },
          });
          
          logger.info("Dataset deleted from Fuseki", {
            datasetId,
          });
        } catch (deleteError) {
          if (deleteError.response?.status === 404) {
            logWarn(req, "Dataset not found", startTime, {
              cig_id,
              datasetId,
            });

            return res.status(StatusCodes.NOT_FOUND).json({
              status: "error",
              message: `Dataset not found: ${datasetId}`,
            });
          }

          logger.error("Failed to delete dataset from Fuseki", {
            datasetId,
            error: deleteError.message,
          });

          throw new ErrorHandler(
            StatusCodes.INTERNAL_SERVER_ERROR,
            `Failed to delete dataset: ${deleteError.message}`
          );
        }

        // Invalidate cache entries
        cacheUtils.invalidatePattern(guidelineIdentifiers.guidelineId);
        cacheUtils.invalidatePattern("list");
        cacheUtils.invalidatePattern("search");

        logSuccess(req, "Guideline deleted successfully", startTime, {
          cig_id,
          guidelineId: guidelineIdentifiers.guidelineId,
        });

        res.status(StatusCodes.OK).json({
          status: "success",
          message: `Guideline ${cig_id} deleted successfully`,
          data: {
            cig_id,
            guideline_id: guidelineIdentifiers.guidelineId,
            deleted_at: new Date().toISOString(),
          },
        });
      } catch (error) {
        logError(req, "Failed to delete guideline", startTime, error);

        if (error instanceof ErrorHandler) {
          return res.status(error.statusCode).json({
            status: "error",
            message: error.message,
          });
        }

        res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
          status: "error",
          message: "Failed to delete guideline",
          error: process.env.NODE_ENV === "development" ? error.message : undefined,
        });
      }
    }
  );
});

module.exports = router;
module.exports.cacheUtils = cacheUtils;
