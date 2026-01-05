/**
 * Recommendation Handlers Module
 * Provides SPARQL query building and parsing utilities for TMR clinical recommendations
 * Based on nanopublication pattern (head, assertion, provenance, publication)
 */

const logger = require("../../config/winston");
const { escapeQuotes } = require("../router_functs/route_helpers");
const { DATA_PREFIX } = require("./identifierNormalizers");

// Constants for SPARQL query building and parsing
const RECOMMENDATION_GROUP_SEPARATOR = "||";
const RECOMMENDATION_PAIR_SEPARATOR = "::";

/**
 * Build SPARQL INSERT statement for recommendation nanopublication
 * Creates four named graphs: head, assertion, provenance, publication
 * 
 * @param {Object} params - Recommendation parameters
 * @param {Object} params.cigIdentifiers - Normalized guideline identifiers
 * @param {Object} params.recommendation - Normalized recommendation identifiers
 * @param {Object} params.belief - Normalized belief identifiers (optional)
 * @param {Object} params.careAction - Normalized care action identifiers
 * @param {string} params.label - Recommendation label (required)
 * @param {string} params.strength - Strength URI (vocab:should, vocab:must, etc.)
 * @param {string} params.contribution - Belief contribution (positive/negative/neutral)
 * @param {Array} params.derivedFrom - Array of source URIs
 * @param {string} params.author - Author URI (optional)
 * @param {string} params.generatedAt - ISO timestamp
 * @param {Object} params.extractedFrom - Extracted source (optional)
 * @param {string} params.precondition - Precondition URI (optional)
 * @returns {Object} Object with sparql property containing INSERT DATA statement
 */
function buildRecommendationInsertStatement({
  cigIdentifiers,
  recommendation,
  belief,
  careAction,
  label,
  strength,
  contribution,
  derivedFrom,
  author,
  generatedAt,
  extractedFrom,
  precondition,
}) {
  if (!label || typeof label !== "string") {
    throw new Error("Recommendation label is required");
  }

  const predicateObjects = [
    ["a", "vocab:ClinicalRecommendation , owl:NamedIndividual"],
    ["rdfs:label", `'''${escapeQuotes(label)}'''@en`],
    ["vocab:aboutExecutionOf", careAction.execution.prefixed],
    ["vocab:strength", strength],
  ];

  const [primaryGuideline] = cigIdentifiers?.prefixedResourceUris || [];
  if (primaryGuideline) {
    predicateObjects.push(["vocab:partOf", primaryGuideline]);
  }

  if (belief) {
    predicateObjects.push(["vocab:basedOn", belief.prefixed]);
  }

  if (extractedFrom) {
    predicateObjects.push(["vocab:extractedFrom", extractedFrom.value]);
  }

  if (precondition) {
    predicateObjects.push(["vocab:hasFilterSituation", precondition]);
  }

  const assertionTriples = serializePredicates(
    recommendation.prefixed,
    predicateObjects
  );

  const beliefContributionTriple = belief
    ? `\n      ${belief.prefixed} vocab:contribution '''${escapeQuotes(
        contribution
      )}''' .`
    : "";

  const derivedTriples = derivedFrom.length
    ? `\n${derivedFrom
        .map(
          (entry) => `      ${recommendation.prefixed} prov:wasDerivedFrom ${entry.value} .`
        )
        .join("\n")}`
    : "";

  const headGraph = `
    GRAPH ${recommendation.headGraph} {
      ${recommendation.headGraph}
        a nanopub:Nanopublication ;
        nanopub:hasAssertion ${recommendation.assertionGraph} ;
        nanopub:hasProvenance ${recommendation.provenanceGraph} ;
        nanopub:hasPublicationInfo ${recommendation.publicationGraph} .
    }`;

  const assertionGraph = `
    GRAPH ${recommendation.assertionGraph} {
      ${assertionTriples}${beliefContributionTriple}
    }`;

  const provenanceGraph = `
    GRAPH ${recommendation.provenanceGraph} {
      ${recommendation.provenanceGraph} a oa:Annotation ;
        oa:hasBody ${recommendation.prefixed} .${derivedTriples}
    }`;

  const publicationStatements = [];
  publicationStatements.push(
    `${recommendation.headGraph} prov:generatedAtTime "${generatedAt}"^^xsd:dateTime`
  );

  if (author) {
    publicationStatements[0] += " ;";
    publicationStatements.push(`  prov:wasAttributedTo ${author}`);
  }

  const publicationGraph = `
    GRAPH ${recommendation.publicationGraph} {
      ${publicationStatements.join("\n      ")} .
    }`;

  // Default graph triple: recommendation isPartOf guideline
  const defaultGraphTriple = cigIdentifiers?.prefixedResourceUris?.[0]
    ? `    ${recommendation.prefixed} vocab:isPartOf ${cigIdentifiers.prefixedResourceUris[0]} .`
    : "";

  const sparql = `
  INSERT DATA {
${defaultGraphTriple}
${headGraph}
${assertionGraph}
${provenanceGraph}
${publicationGraph}
  }
`;

  return {
    sparql,
  };
}

/**
 * Helper function to serialize predicate-object pairs into SPARQL triples
 * Used by buildRecommendationInsertStatement
 * 
 * @param {string} subject - Subject URI (prefixed or full)
 * @param {Array<Array<string>>} predicateObjects - Array of [predicate, object] pairs
 * @returns {string} Formatted SPARQL triples with proper indentation
 */
function serializePredicates(subject, predicateObjects) {
  if (predicateObjects.length === 0) {
    return "";
  }

  const lines = predicateObjects.map(([predicate, object], index) => {
    const separator = index < predicateObjects.length - 1 ? " ;" : " .";
    return `${predicate} ${object}${separator}`;
  });

  lines[0] = `${subject} ${lines[0]}`;
  return lines.join("\n");
}

/**
 * Build SPARQL DELETE statement for recommendation nanopublication
 * Drops all four named graphs and deletes the recommendation resource
 * 
 * @param {Object} recommendation - Normalized recommendation identifiers
 * @returns {string} SPARQL DELETE statement
 */
function buildRecommendationDeleteStatement(recommendation, cigIdentifiers = null) {
  // Delete specific vocab:isPartOf triple from default graph if guideline is provided
  const defaultGraphDelete = cigIdentifiers?.prefixedResourceUris?.[0]
    ? `DELETE DATA { ${recommendation.prefixed} vocab:isPartOf ${cigIdentifiers.prefixedResourceUris[0]} . };`
    : "";

  return `
    ${defaultGraphDelete}
    DROP SILENT GRAPH ${recommendation.headGraph};
    DROP SILENT GRAPH ${recommendation.assertionGraph};
    DROP SILENT GRAPH ${recommendation.provenanceGraph};
    DROP SILENT GRAPH ${recommendation.publicationGraph};
    DELETE WHERE { ${recommendation.prefixed} ?p ?o . }
  `;
}

/**
 * Build SPARQL SELECT query to fetch recommendations
 * Retrieves all nanopublication components with optional URI filtering
 * 
 * @param {Array<string>} recUris - Array of recommendation URIs to filter (optional)
 * @returns {string} SPARQL SELECT query
 */
function buildRecommendationsQuery(recUris) {
  const valuesClause = recUris.length
    ? `VALUES ?recId { ${recUris
        .map((uri) => wrapUriForValuesClause(uri))
        .filter(Boolean)
        .join(" ")} }`
    : "";

  return `
    ${valuesClause}
    SELECT ?assertGraph ?recId ?label ?strength ?careAction ?head ?generatedAt ?attributedTo
           (GROUP_CONCAT(DISTINCT ?basedOnPair; SEPARATOR "${RECOMMENDATION_GROUP_SEPARATOR}") AS ?basedOnPairs)
           (GROUP_CONCAT(DISTINCT ?partOf; SEPARATOR "${RECOMMENDATION_GROUP_SEPARATOR}") AS ?partList)
           (GROUP_CONCAT(DISTINCT ?derivedVal; SEPARATOR "${RECOMMENDATION_GROUP_SEPARATOR}") AS ?derivedList)
           (GROUP_CONCAT(DISTINCT ?extractedFrom; SEPARATOR "${RECOMMENDATION_GROUP_SEPARATOR}") AS ?extractedList)
           (GROUP_CONCAT(DISTINCT ?precondition; SEPARATOR "${RECOMMENDATION_GROUP_SEPARATOR}") AS ?preconditionList)
    WHERE {
      GRAPH ?head {
        ?head nanopub:hasAssertion ?assertGraph ;
              nanopub:hasProvenance ?provGraph ;
              nanopub:hasPublicationInfo ?pubGraph .
      }
      GRAPH ?assertGraph {
        ?recId a vocab:ClinicalRecommendation ;
               rdfs:label ?label ;
               vocab:aboutExecutionOf ?careAction ;
               vocab:strength ?strength .
        OPTIONAL {
          ?recId vocab:basedOn ?basedOn .
          OPTIONAL { ?basedOn vocab:contribution ?contribution . }
          BIND(CONCAT(str(?basedOn), "${RECOMMENDATION_PAIR_SEPARATOR}", COALESCE(str(?contribution), "")) AS ?basedOnPair)
        }
        OPTIONAL { ?recId vocab:partOf ?partOf . }
        OPTIONAL { ?recId vocab:extractedFrom ?extractedFrom . }
        OPTIONAL { ?recId vocab:hasFilterSituation ?precondition . }
      }
      GRAPH ?provGraph {
        OPTIONAL { ?recId prov:wasDerivedFrom ?derivedVal . }
      }
      GRAPH ?pubGraph {
        OPTIONAL { ?head prov:generatedAtTime ?generatedAt . }
        OPTIONAL { ?head prov:wasAttributedTo ?attributedTo . }
      }
    }
    GROUP BY ?assertGraph ?recId ?label ?strength ?careAction ?head ?generatedAt ?attributedTo
    ORDER BY LCASE(str(?label))
  `;
}

/**
 * Parse SPARQL binding into recommendation object
 * Transforms raw RDF results into structured JavaScript object
 * 
 * @param {Object} binding - SPARQL query binding result
 * @param {string} datasetId - Dataset identifier (e.g., CIG-DB)
 * @returns {Object} Structured recommendation object with graphs, metadata, and relationships
 */
function parseRecommendationBinding(binding, datasetId) {
  const recUri = binding.recId?.value || null;
  const assertionGraph = binding.assertGraph?.value || recUri || null;
  const headGraph = binding.head?.value || null;
  const strength = binding.strength?.value || null;
  const suggestion = deriveSuggestionFromStrength(strength);

  const localId = extractLocalName(recUri);

  const partOf = splitGroupValues(binding.partList?.value);
  const derivedFrom = splitGroupValues(binding.derivedList?.value);
  const extractedFrom = splitGroupValues(binding.extractedList?.value);
  const preconditions = splitGroupValues(binding.preconditionList?.value);
  const basedOn = parseBasedOnPairs(binding.basedOnPairs?.value);

  const baseGraphUri = localId ? `${DATA_PREFIX}${localId}` : assertionGraph;

  const graphs = {
    assertion: assertionGraph,
    head: headGraph || (localId ? `${DATA_PREFIX}${localId}_head` : null),
    provenance: localId ? `${DATA_PREFIX}${localId}_provenance` : null,
    publication: localId ? `${DATA_PREFIX}${localId}_publicationinfo` : null,
  };

  const careActionExecutionUri = binding.careAction?.value || null;

  const recommendation = {
    datasetId,
    id: localId,
    uri: recUri,
    graphs,
    label: binding.label?.value || null,
    strength,
    suggestion,
    careAction: careActionExecutionUri
      ? {
          execution: {
            uri: careActionExecutionUri,
            id: extractLocalName(careActionExecutionUri),
            label: null,
          },
          resource: null,
          resourceLabel: null,
        }
      : null,
    basedOn,
    partOf,
    derivedFrom,
    extractedFrom,
    preconditions,
    metadata: {
      generatedAt: binding.generatedAt?.value || null,
      attributedTo: binding.attributedTo?.value || null,
    },
  };

  // Ensure graphs default to assertion when missing
  if (!recommendation.graphs.head && baseGraphUri) {
    recommendation.graphs.head = `${baseGraphUri}_head`;
  }
  if (!recommendation.graphs.provenance && baseGraphUri) {
    recommendation.graphs.provenance = `${baseGraphUri}_provenance`;
  }
  if (!recommendation.graphs.publication && baseGraphUri) {
    recommendation.graphs.publication = `${baseGraphUri}_publicationinfo`;
  }

  return recommendation;
}

/**
 * Derive human-readable suggestion from strength URI
 * Maps vocab:must/should/shouldNot/mustNot to recommend/contraindicated
 * 
 * @param {string} strengthUri - Full or prefixed strength URI
 * @returns {string|null} Suggestion string or null if unrecognized
 */
function deriveSuggestionFromStrength(strengthUri) {
  if (!strengthUri) {
    return null;
  }

  const text = strengthUri.toLowerCase();

  if (text.endsWith("mustnot")) {
    return "contraindicated";
  }

  if (text.endsWith("shouldnot")) {
    return "nonRecommend";
  }

  if (text.endsWith("must")) {
    return "strongRecommend";
  }

  if (text.endsWith("should")) {
    return "recommend";
  }

  return null;
}

/**
 * Clean URI or literal value from SPARQL binding
 * Removes angle brackets, quotes, and UNDEF placeholders
 * 
 * @param {string} value - Raw value from SPARQL result
 * @returns {string} Cleaned value or empty string
 */
function cleanUriValue(value) {
  if (!value) {
    return "";
  }

  let text = String(value).trim();
  if (!text || text === "UNDEF") {
    return "";
  }

  if (text.startsWith("<") && text.endsWith(">")) {
    text = text.slice(1, -1);
  }

  if (text.startsWith("\"") && text.endsWith("\"")) {
    text = text.slice(1, -1);
  }

  return text;
}

/**
 * Split GROUP_CONCAT values into unique array
 * Handles SPARQL aggregated values with custom separator
 * 
 * @param {string} rawString - Aggregated string from GROUP_CONCAT
 * @returns {Array<string>} Array of unique cleaned values
 */
function splitGroupValues(rawString) {
  if (!rawString) {
    return [];
  }

  return Array.from(
    new Set(
      rawString
        .split(RECOMMENDATION_GROUP_SEPARATOR)
        .map((item) => cleanUriValue(item))
        .filter((item) => item.length > 0)
    )
  );
}

/**
 * Parse basedOn pairs from GROUP_CONCAT result
 * Extracts belief URI and contribution value pairs
 * 
 * @param {string} rawString - Aggregated basedOn pairs string
 * @returns {Array<Object>} Array of {uri, id, contribution} objects
 */
function parseBasedOnPairs(rawString) {
  if (!rawString) {
    return [];
  }

  return rawString
    .split(RECOMMENDATION_GROUP_SEPARATOR)
    .map((item) => item.trim())
    .filter((item) => item.length > 0)
    .map((pair) => {
      const [uriPart, contributionPart = ""] = pair.split(
        RECOMMENDATION_PAIR_SEPARATOR
      );
      const uri = cleanUriValue(uriPart);
      if (!uri) {
        return null;
      }

      const contribution = cleanUriValue(contributionPart) || null;

      return {
        uri,
        id: extractLocalName(uri),
        contribution: contribution || null,
      };
    })
    .filter(Boolean);
}

/**
 * Extract local name from full URI
 * Handles data: prefix, hash fragments, and path segments
 * 
 * @param {string} uri - Full or prefixed URI
 * @returns {string|null} Local name or null if invalid
 */
function extractLocalName(uri) {
  if (!uri) {
    return null;
  }

  if (uri.startsWith(DATA_PREFIX)) {
    return uri.substring(DATA_PREFIX.length);
  }

  if (uri.startsWith("data:")) {
    return uri.substring("data:".length);
  }

  const hashIndex = uri.lastIndexOf("#");
  if (hashIndex !== -1) {
    return uri.substring(hashIndex + 1);
  }

  const slashIndex = uri.lastIndexOf("/");
  if (slashIndex !== -1) {
    return uri.substring(slashIndex + 1);
  }

  return uri;
}

/**
 * Wrap URI for SPARQL VALUES clause
 * Adds angle brackets if needed, preserves prefixes
 * 
 * @param {string} uri - URI to wrap
 * @returns {string|null} Wrapped URI or null if invalid
 */
function wrapUriForValuesClause(uri) {
  if (!uri) {
    return null;
  }

  if (uri.startsWith("<") && uri.endsWith(">")) {
    return uri;
  }

  if (uri.startsWith("data:") || uri.startsWith("vocab:")) {
    return uri;
  }

  return `<${uri}>`;
}

module.exports = {
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
};
