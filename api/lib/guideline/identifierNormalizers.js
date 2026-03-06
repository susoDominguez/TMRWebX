/**
 * Identifier Normalizers for Guideline Routes
 * Provides consistent identifier normalization across guideline operations
 */

const logger = require("../../config/winston");

// Constants
const DATA_PREFIX = "http://anonymous.org/data/";
const VOCAB_PREFIX = "http://anonymous.org/vocab/";

const DATASET_PERSISTENCE_MAP = new Map([
  ["true", "tdb"],
  ["1", "tdb"],
  ["yes", "tdb"],
  ["y", "tdb"],
  ["persist", "tdb"],
  ["persistent", "tdb"],
]);

/**
 * Normalize Clinical Implementation Guideline identifiers
 * @param {string} rawId - Raw guideline identifier
 * @returns {Object} Normalized identifier details
 */
function normalizeCigIdentifiers(rawId) {
  if (rawId === undefined || rawId === null) {
    throw new Error("Guideline identifier is required");
  }

  let value = String(rawId).trim();
  if (!value) {
    throw new Error("Guideline identifier is required");
  }

  if (value.startsWith("<") && value.endsWith(">")) {
    value = value.slice(1, -1).trim();
  }

  if (value.startsWith(DATA_PREFIX)) {
    value = value.substring(DATA_PREFIX.length);
  }

  if (value.startsWith("data:")) {
    value = value.substring("data:".length);
  }

  let localId = value;

  if (localId.startsWith("CIG-")) {
    localId = localId.substring(4);
  } else if (localId.startsWith("CIG")) {
    localId = localId.substring(3);
  }

  localId = localId.replace(/^[-_]+/, "");

  if (!localId) {
    throw new Error("Unable to resolve guideline identifier");
  }

  const datasetId = `CIG-${localId}`;
  const resourceUris = Array.from(
    new Set([`${DATA_PREFIX}CIG-${localId}`, `${DATA_PREFIX}CIG${localId}`])
  );
  const prefixedResourceUris = Array.from(
    new Set([`data:CIG-${localId}`, `data:CIG${localId}`])
  );

  return {
    raw: rawId,
    localId,
    datasetId,
    resourceUris,
    prefixedResourceUris,
  };
}

/**
 * Normalize dataset identifier for Fuseki dataset operations
 * @param {string} rawId - Raw dataset identifier
 * @returns {string} Normalized dataset ID
 */
function normalizeDatasetId(rawId) {
  if (rawId === undefined || rawId === null) {
    throw new Error("Guideline identifier (cig_id) is required");
  }

  let value = String(rawId).trim();
  if (!value) {
    throw new Error("Guideline identifier (cig_id) is required");
  }

  if (value.startsWith("data:")) {
    value = value.substring("data:".length);
  }

  if (value.startsWith("CIG-")) {
    value = value.substring(4);
  } else if (value.toUpperCase().startsWith("CIG")) {
    value = value.substring(3);
  }

  value = value.replace(/^[-_]+/, "");
  value = value.replace(/[^A-Za-z0-9_\-]/g, "_");

  if (!value) {
    throw new Error("Guideline identifier must contain alphanumeric characters");
  }

  return `CIG-${value}`;
}

/**
 * Resolve dataset persistence flag from raw value
 * @param {string|boolean|number} rawValue - Raw persistence value
 * @returns {string} Database type ('tdb' or 'mem')
 */
function resolveDatasetPersistenceFlag(rawValue) {
  if (rawValue === undefined || rawValue === null) {
    return "mem";
  }

  const normalized = String(rawValue).trim().toLowerCase();
  return DATASET_PERSISTENCE_MAP.get(normalized) || "mem";
}

/**
 * Resolve guideline identifiers from multiple candidate sources
 * @param {Object} options - Options containing guideline and cigId
 * @returns {Object} Resolved identifier details
 */
function resolveGuidelineIdentifiers({ guideline, cigId } = {}) {
  const candidates = [];

  if (cigId) {
    candidates.push(cigId);
  }

  if (guideline?.id) {
    candidates.push(guideline.id);
  }

  if (guideline?.uri) {
    candidates.push(guideline.uri);
  }

  if (guideline?.label) {
    candidates.push(guideline.label);
  }

  for (const candidate of candidates) {
    try {
      return normalizeCigIdentifiers(candidate);
    } catch (error) {
      logger.debug("Failed to normalize guideline identifier candidate", {
        candidate,
        error: error.message,
      });
    }
  }

  throw new Error("Unable to resolve guideline identifiers");
}

/**
 * Normalize recommendation identifier with optional CIG context
 * @param {string} rawId - Raw recommendation identifier
 * @param {Object} cigContext - CIG context with localId
 * @returns {Object} Normalized recommendation identifiers
 */
function normalizeRecommendationIdentifiers(rawId, cigContext) {
  if (rawId === undefined || rawId === null) {
    throw new Error("Recommendation identifier is required");
  }

  let value = String(rawId).trim();
  if (!value) {
    throw new Error("Recommendation identifier is required");
  }

  if (value.startsWith("<") && value.endsWith(">")) {
    value = value.slice(1, -1).trim();
  }

  if (value.startsWith(DATA_PREFIX)) {
    value = value.substring(DATA_PREFIX.length);
  }

  if (value.startsWith("data:")) {
    value = value.substring("data:".length);
  }

  let localId = value;

  if (!localId.startsWith("Rec")) {
    const base = cigContext?.localId ? `${cigContext.localId}` : "";
    localId = base ? `Rec${base}-${localId}` : `Rec${localId}`;
  }

  const prefixed = `data:${localId}`;
  const uri = `${DATA_PREFIX}${localId}`;

  return {
    raw: rawId,
    localId,
    prefixed,
    uri,
    assertionGraph: prefixed,
    assertionGraphUri: uri,
    headGraph: `data:${localId}_head`,
    headGraphUri: `${DATA_PREFIX}${localId}_head`,
    provenanceGraph: `data:${localId}_provenance`,
    provenanceGraphUri: `${DATA_PREFIX}${localId}_provenance`,
    publicationGraph: `data:${localId}_publicationinfo`,
    publicationGraphUri: `${DATA_PREFIX}${localId}_publicationinfo`,
  };
}

/**
 * Normalize belief (causation belief) identifier
 * @param {string} rawId - Raw belief identifier
 * @returns {Object|null} Normalized belief identifiers
 */
function normalizeBeliefIdentifier(rawId) {
  if (rawId === undefined || rawId === null) {
    return null;
  }

  let value = String(rawId).trim();
  if (!value) {
    return null;
  }

  if (value.startsWith("<") && value.endsWith(">")) {
    value = value.slice(1, -1).trim();
  }

  if (value.startsWith(DATA_PREFIX)) {
    value = value.substring(DATA_PREFIX.length);
  }

  if (value.startsWith("data:")) {
    value = value.substring("data:".length);
  }

  if (!value.startsWith("CB")) {
    if (value.startsWith("CausationBelief")) {
      value = value.replace(/^CausationBelief/, "CB");
    } else {
      value = `CB${value}`;
    }
  }

  return {
    raw: rawId,
    localId: value,
    prefixed: `data:${value}`,
    uri: `${DATA_PREFIX}${value}`,
  };
}

/**
 * Normalize care action identifier
 * @param {string} rawId - Raw care action identifier
 * @returns {Object} Normalized care action identifiers
 */
function normalizeCareActionIdentifier(rawId) {
  if (rawId === undefined || rawId === null) {
    throw new Error("Care action identifier is required");
  }

  let value = String(rawId).trim();
  if (!value) {
    throw new Error("Care action identifier is required");
  }

  if (value.startsWith("<") && value.endsWith(">")) {
    value = value.slice(1, -1).trim();
  }

  if (value.startsWith(DATA_PREFIX)) {
    value = value.substring(DATA_PREFIX.length);
  }

  if (value.startsWith("data:")) {
    value = value.substring("data:".length);
  }

  let executionLocalId = value;

  if (!executionLocalId.startsWith("Act")) {
    executionLocalId = `ActAdminister${executionLocalId}`;
  }

  const resourceLocalId = executionLocalId.replace(/^Act[A-Za-z]+/, "");

  return {
    raw: rawId,
    execution: {
      localId: executionLocalId,
      prefixed: `data:${executionLocalId}`,
      uri: `${DATA_PREFIX}${executionLocalId}`,
    },
    resource: resourceLocalId
      ? {
          localId: resourceLocalId,
          prefixed: `data:${resourceLocalId}`,
          uri: `${DATA_PREFIX}${resourceLocalId}`,
        }
      : null,
  };
}

/**
 * Normalize precondition (situation) identifier
 * @param {string} rawId - Raw precondition identifier
 * @returns {string|null} Normalized precondition identifier
 */
function normalizePreconditionIdentifier(rawId) {
  if (rawId === undefined || rawId === null) {
    return null;
  }

  let value = String(rawId).trim();
  if (!value) {
    return null;
  }

  if (value.startsWith("<") && value.endsWith(">")) {
    return value;
  }

  if (value.startsWith("data:")) {
    return value;
  }

  if (value.startsWith(DATA_PREFIX)) {
    return `<${value}>`;
  }

  if (!value.startsWith("Sit")) {
    value = `Sit${value}`;
  }

  return `data:${value}`;
}

/**
 * Normalize author identifier
 * @param {string} rawAuthor - Raw author identifier or name
 * @returns {string|null} Normalized author identifier
 */
function normalizeAuthorIdentifier(rawAuthor) {
  if (rawAuthor === undefined || rawAuthor === null) {
    return null;
  }

  let value = String(rawAuthor).trim();
  if (!value) {
    return null;
  }

  if (value.startsWith("<") && value.endsWith(">")) {
    return value;
  }

  if (value.startsWith("data:")) {
    return value;
  }

  if (value.startsWith(DATA_PREFIX)) {
    return `<${value}>`;
  }

  const sanitized = value.replace(/[^A-Za-z0-9_\-]/g, "_");
  if (!sanitized) {
    return null;
  }

  return `data:${sanitized}`;
}

module.exports = {
  normalizeCigIdentifiers,
  normalizeDatasetId,
  resolveDatasetPersistenceFlag,
  resolveGuidelineIdentifiers,
  normalizeRecommendationIdentifiers,
  normalizeBeliefIdentifier,
  normalizeCareActionIdentifier,
  normalizePreconditionIdentifier,
  normalizeAuthorIdentifier,
  // Export constants for use in other modules
  DATA_PREFIX,
  VOCAB_PREFIX,
  DATASET_PERSISTENCE_MAP,
};
