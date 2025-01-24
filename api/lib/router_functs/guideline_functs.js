const Promise = require("bluebird");
const utils = Promise.promisifyAll(require("../utils.js"));
const logger = require("../../config/winston.js");
const jsonata = require("jsonata");
//const { ErrorHandler } = require("../lib/errorHandler");
const config = require("../config");
const data_prefix = "http://anonymous.org/data/";
const data_prefix_short = "data:";
const sct_prefix = `http://snomed.info/sct/`;
const data_prefix_length = "http://anonymous.org/data/".length;
const vocab_prefix_length = "http://anonymous.org/vocab/".length;

function transformSPARQLResults(sparqlResults, schema, context = {}) {
  /**
   * Utility function to set a value in a nested object using a dot-separated path.
   */
  const setNestedValue = (obj, path, value) => {
    const keys = path.split(".");
    let current = obj;

    keys.forEach((key, index) => {
      if (index === keys.length - 1) {
        current[key] = value; // Set the final value
      } else {
        current[key] = current[key] || {}; // Ensure intermediate objects exist
        current = current[key];
      }
    });
  };

  /**
   * Process each binding in the SPARQL results and apply the schema.
   */
  return sparqlResults.results.bindings.map((binding) => {
    const result = {};

    for (const [sourceKey, config] of Object.entries(schema)) {
      // Check if the SPARQL result contains the sourceKey and handle missing fields
      if (binding[sourceKey] && binding[sourceKey].value !== undefined) {
        const value = binding[sourceKey].value;

        // Apply transform function if provided
        if (typeof config.transform === "function") {
          config.transform(value, result, context);
        } else {
          // Direct mapping to the target key
          setNestedValue(result, config.targetKey, value);
        }
      } else if (!binding[sourceKey]) {
        // Handle missing data gracefully
        if (config.defaultValue !== undefined) {
          // If a default value is defined in the schema, use it
          setNestedValue(result, config.targetKey, config.defaultValue);
        } else if (typeof config.onMissing === "function") {
          // Optionally, handle missing values with a custom onMissing function
          config.onMissing(result, context);
        }
      }
    }

    return result;
  });
}

const cb_tr_schema = {
  cbId: { targetKey: "id", transform: (value) => value },
  contribution: {
    targetKey: "contribution",
    transform: (value) => {
      const match = value.toLowerCase().match(/(positive|negative|neutral)$/i);
      return match ? match[1] : null; // Extract postfix or set to null if not found
    },
  },
  probability: {
    targetKey: "probability",
    transform: (value) => {
      const match = value
        .toLowerCase()
        .match(/(never|often|sometimes|rarely|always)$/i);
      return match ? match[1] : null; // Extract postfix or set to null if not found
    },
  },
  evidence: {
    targetKey: "evidence",
    transform: (value) => {
      const match = value.toLowerCase().match(/(high|medium|low)$/i);
      return match ? match[1] : null; // Extract postfix or set to null if not found
    },
  },
  actId: { targetKey: "careActionTypeRef", transform: (value) => value },
  trId: { targetKey: "transition.id", transform: (value) => value },
  derivedFromCB: { targetKey: "derivedFrom", transform: (value) => value },
  hasSourcesCB: { targetKey: "hasSource", transform: (value) => value },
  effect: { targetKey: "effect", transform: (value) => value.toLowerCase() },
  propUri: {
    targetKey: "transition.property",
    transform: (value, result) => {
      result.transition.property ??= { value: { coding: [] } };
      result.transition.property.id = value;
      const type = value.slice(data_prefix_length);
      result.transition.property.value.coding[0] ??= {};
      result.transition.property.value.coding[0].code = type;
      result.transition.property.value.coding[0].system = data_prefix;
    },
  },
  propLabel: {
    targetKey: "transition.property.value.coding[0].display",
    transform: (value, result) => {
      result.transition.property ??= { value: { coding: [] } };
      result.transition.property.value.coding[0] ??= {};
      result.transition.property.value.coding[0].display = value;
      result.transition.property.value.text ??= value;
    },
  },
  propUriSCT: {
    targetKey: "transition.property.value.coding[1].code",
    transform: (value, result) => {
      if (value) {
        result.property ??= { value: { coding: [] } };
        result.property.value.coding[1] ??= {};
        result.property.value.coding[1].code = value;
        result.property.value.coding[1].system = sct_prefix;
      }
    },
  },
  propLabelSCT: {
    targetKey: "transition.property.value.coding[1].display",
    transform: (value, result) => {
      if (value) {
        result.property ??= { value: { coding: [] } };
        result.property.value.coding[1] ??= {};
        result.property.value.coding[1].display = value;
      }
    },
  },
  sitFromId: {
    targetKey: "transition.situationTypes[0]",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[0].id = value;
      const type = value.slice(data_prefix_length);
      result.transition.situationTypes[0].value.coding[0] ??= {};
      result.transition.situationTypes[0].value.coding[0].code = type;
      result.transition.situationTypes[0].value.coding[0].system = data_prefix;
    },
  },
  sitFromLabel: {
    targetKey: "transition.situationTypes[0].value.coding[0].display",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[0].value.coding[0] ??= {};
      result.transition.situationTypes[0].value.coding[0].display = value;
      result.transition.situationTypes[0].value.text ??= value;
    },
  },
  sitFromIdSCT: {
    targetKey: "transition.situationTypes[0].value.coding[1].code",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[0].value.coding[1] ??= {};
      result.transition.situationTypes[0].value.coding[1].code = value;
      result.transition.situationTypes[0].value.coding[1].system = sct_prefix;
    },
  },
  sitFromLabelSCT: {
    targetKey: "transition.situationTypes[0].value.coding[1].display",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[0].value.coding[1] ??= {};
      result.transition.situationTypes[0].value.coding[1].display = value;
    },
  },
  sitToId: {
    targetKey: "transition.situationTypes[1]",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[1].id = value;
      const type = value.slice(data_prefix_length);
      result.transition.situationTypes[1].value.coding[0] ??= {};
      result.transition.situationTypes[1].value.coding[0].code = type;
      result.transition.situationTypes[1].value.coding[0].system = data_prefix;
    },
  },
  sitToLabel: {
    targetKey: "transition.situationTypes[1].value.coding[0].display",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[1].value.coding[0] ??= {};
      result.transition.situationTypes[1].value.coding[0].display = value;
      result.transition.situationTypes[1].value.text ??= value;
    },
  },
  sitToIdSCT: {
    targetKey: "transition.situationTypes[1].value.coding[1].code",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[1].value.coding[1] ??= {};
      result.transition.situationTypes[1].value.coding[1].code = value;
      result.transition.situationTypes[1].value.coding[1].system = sct_prefix;
    },
  },
  sitToLabelSCT: {
    targetKey: "transition.situationTypes[1].value.coding[1].display",
    transform: (value, result) => {
      result.transition.situationTypes ??= [];
      result.transition.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.transition.situationTypes[1].value.coding[1] ??= {};
      result.transition.situationTypes[1].value.coding[1].display = value;
    },
  },
};

const cb_schema = {
  cbId: { targetKey: "id", transform: (value) => value },
  contribution: {
    targetKey: "contribution",
    transform: (value) => {
      const match = value.toLowerCase().match(/(positive|negative|neutral)$/i);
      return match ? match[1] : null; // Extract postfix or set to null if not found
    },
  },
  probability: {
    targetKey: "probability",
    transform: (value) => {
      const match = value
        .toLowerCase()
        .match(/(never|often|sometimes|rarely|always)$/i);
      return match ? match[1] : null; // Extract postfix or set to null if not found
    },
  },
  evidence: {
    targetKey: "evidence",
    transform: (value) => {
      const match = value.toLowerCase().match(/(high|medium|low)$/i);
      return match ? match[1] : null; // Extract postfix or set to null if not found
    },
  },
  actId: { targetKey: "careActionTypeRef", transform: (value) => value },
  trId: { targetKey: "transitionTypeRef", transform: (value) => value },
  derivedFromCB: { targetKey: "derivedFrom", transform: (value) => value },
  hasSourcesCB: { targetKey: "hasSource", transform: (value) => value },
};

const recommendation_schema = {
  partOf: {
    targetKey: "partOf",
    transform: (value) => value.split("/").pop(),
  },
  isPartOf: {
    targetKey: "source_cig",
    transform: (value) => value.split("/").pop(),
  },
  extractedFrom: {
    targetKey: "extractedFrom",
    transform: (value) => value.split("/").pop(),
  },
  label: {
    targetKey: "text",
    transform: (value) => value,
  },
  strength: {
    targetKey: "suggestion",
    transform: (value) =>
      /should$|must$/i.test(value) ? "recommend" : "nonRecommend",
  },
  cbId: {
    targetKey: "causationBeliefTypeRef",
    transform: (value) => value,
  },
  contrib: {
    //contribution to the recommendation (redundant if only 1 CB type)
    targetKey: "causationBeliefType.contribution",
    transform: (value) => value,
  },
  derivedFrom: {
    targetKey: "derivedFrom",
    transform: (value) => value.split(","),
  },
  hasSources: {
    targetKey: "hasSource",
    transform: (value) => value.split(","),
  },
  attributedTo: {
    targetKey: "wasAttributedTo",
    transform: (value) => value,
  },
  generatedTime: {
    targetKey: "generatedAtTime",
    transform: (value) => value,
  },
  actId: {
    targetKey: "careActionTypeRef",
    transform: (value) => value,
  },
  of: {
    targetKey: "careActionType.requestType",
    transform: (value, result) => {
      const temp = value.split("/").pop();
      if (temp === "applicationOf") {
        result.careActionType = {
          requestType: 1,
          type: {
            sctId: "304541006",
            display: "Recommendation to perform treatment (procedure)",
          },
        };
      } else if (temp === "inoculationOf") {
        result.careActionType = {
          requestType: 2,
          type: {
            sctId: "830152006",
            display: "Recommendation regarding vaccination (procedure)",
          },
        };
      }
    },
  },
  precondId: {
    targetKey: "preconditions.id",
    transform: (value) => value,
  },
  precondSctid: {
    targetKey: "preconditions.value.coding[1].code",
    transform: (value, result) => {
      result.preconditions ??= { value: { coding: [] } };
      result.preconditions.value.coding[1] ??= {};
      result.preconditions.value.coding[1].code = value;
      result.preconditions.value.coding[1].system = sct_prefix;
    },
  },
  precondSctidLbl: {
    targetKey: "preconditions.value.coding[1].display",
    transform: (value, result) => {
      result.preconditions ??= { value: { coding: [] } };
      result.preconditions.value.coding[1] ??= {};
      result.preconditions.value.coding[1].display = value;
      result.preconditions.value.coding[1].text ??= value;
    },
  },
  precondT: {
    targetKey: "preconditions.value.coding[0].code",
    transform: (value, result) => {
      result.preconditions ??= { value: { coding: [] } };
      result.preconditions.value.coding[0] ??= {};
      result.preconditions.value.coding[0].code = value.slice(
        value.lastIndexOf("/") + 1
      );
      result.preconditions.value.coding[0].system = data_prefix;
    },
  },
  precondLbl: {
    targetKey: "preconditions.value.coding[0].display",
    transform: (value, result) => {
      result.preconditions ??= { value: { coding: [] } };
      result.preconditions.value.coding[0] ??= {};
      result.preconditions.value.coding[0].display = value;
      result.preconditions.value.coding[0].text ??= value;
    },
  },
};

const care_action_schema = {
  actId: { targetKey: "id", transform: (value) => value },
  adminLabel: { targetKey: "display", transform: (value) => value },
  subsumes: { targetKey: "subsumes", transform: (value) => value.split(", ") },
  hasGroupingCriteria: {
    targetKey: "has_grouping_criteria",
    transform: (value) => value.split(", "),
  },
  sameAs: { targetKey: "same_as", transform: (value) => value.split(", ") },
  adminT: {
    targetKey: "type",
    transform: (value) => value.slice(value.lastIndexOf("/") + 1),
  },
  drugTid: {
    targetKey: "administers",
    transform: (value, result) => {
      result.administers ??= {};
      result.administers.id = value;
      result.administers.value ??= { coding: [] };
      result.administers.value.coding[0] ??= {};
      result.administers.value.coding[0].code = value.slice(
        value.lastIndexOf("/") + 1
      );
      result.administers.value.coding[0].system = data_prefix;
    },
  },
  drugType: {
    targetKey: "administers.type",
    transform: (value) => value.slice(value.lastIndexOf("/") + 1),
  },
  drugLabel: {
    targetKey: "administers.value.coding[0].display",
    transform: (value, result) => {
      result.administers ??= {};
      result.administers.value ??= { coding: [] };
      result.administers.value.coding[0] ??= {};
      result.administers.value.coding[0].display = value;
      result.administers.value.coding[0].text ??= value;
    },
  },
  sctid: {
    targetKey: "administers.value.coding[1].code",
    transform: (value, result) => {
      result.administers ??= {};
      result.administers.value ??= { coding: [] };
      result.administers.value.coding[1] ??= {};
      result.administers.value.coding[1].code = value;
      result.administers.value.coding[1].system = sct_prefix;
    },
  },
  sctLbl: {
    targetKey: "administers.value.coding[1].display",
    transform: (value, result) => {
      result.administers ??= {};
      result.administers.value ??= { coding: [] };
      result.administers.value.coding[1] ??= {};
      result.administers.value.coding[1].display = value;
      result.administers.value.text ??= value;
    },
  },
  components: {
    targetKey: "administers.has_components",
    transform: (value) => value.split(", "),
  },
};

const transition_schema = {
  TrId: { targetKey: "id", transform: (value) => value },
  effect: { targetKey: "effect", transform: (value) => value.toLowerCase() },
  propUri: {
    targetKey: "property",
    transform: (value, result) => {
      result.property ??= { value: { coding: [] } };
      result.property.id = value;
      const type = value.slice(data_prefix_length);
      result.property.value.coding[0] ??= {};
      result.property.value.coding[0].code = type;
      result.property.value.coding[0].system = data_prefix;
    },
  },
  propLabel: {
    targetKey: "property.value.coding[0].display",
    transform: (value, result) => {
      result.property ??= { value: { coding: [] } };
      result.property.value.coding[0] ??= {};
      result.property.value.coding[0].display = value;
      result.property.value.text ??= value;
    },
  },
  propUriSCT: {
    targetKey: "property.value.coding[1].code",
    transform: (value, result) => {
      if (value) {
        result.property ??= { value: { coding: [] } };
        result.property.value.coding[1] ??= {};
        result.property.value.coding[1].code = value;
        result.property.value.coding[1].system = sct_prefix;
      }
    },
  },
  propLabelSCT: {
    targetKey: "property.value.coding[1].display",
    transform: (value, result) => {
      if (value) {
        result.property ??= { value: { coding: [] } };
        result.property.value.coding[1] ??= {};
        result.property.value.coding[1].display = value;
      }
    },
  },
  sitFromId: {
    targetKey: "situationTypes[0]",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.situationTypes[0].id = value;
      const type = value.slice(data_prefix_length);
      result.situationTypes[0].value.coding[0] ??= {};
      result.situationTypes[0].value.coding[0].code = type;
      result.situationTypes[0].value.coding[0].system = data_prefix;
    },
  },
  sitFromLabel: {
    targetKey: "situationTypes[0].value.coding[0].display",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.situationTypes[0].value.coding[0] ??= {};
      result.situationTypes[0].value.coding[0].display = value;
      result.situationTypes[0].value.text ??= value;
    },
  },
  sitFromIdSCT: {
    targetKey: "situationTypes[0].value.coding[1].code",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.situationTypes[0].value.coding[1] ??= {};
      result.situationTypes[0].value.coding[1].code = value;
      result.situationTypes[0].value.coding[1].system = sct_prefix;
    },
  },
  sitFromLabelSCT: {
    targetKey: "situationTypes[0].value.coding[1].display",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[0] ??= {
        type: "hasTransformableSituation",
        value: { coding: [] },
      };
      result.situationTypes[0].value.coding[1] ??= {};
      result.situationTypes[0].value.coding[1].display = value;
    },
  },
  sitToId: {
    targetKey: "situationTypes[1]",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.situationTypes[1].id = value;
      const type = value.slice(data_prefix_length);
      result.situationTypes[1].value.coding[0] ??= {};
      result.situationTypes[1].value.coding[0].code = type;
      result.situationTypes[1].value.coding[0].system = data_prefix;
    },
  },
  sitToLabel: {
    targetKey: "situationTypes[1].value.coding[0].display",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.situationTypes[1].value.coding[0] ??= {};
      result.situationTypes[1].value.coding[0].display = value;
      result.situationTypes[1].value.text ??= value;
    },
  },
  sitToIdSCT: {
    targetKey: "situationTypes[1].value.coding[1].code",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.situationTypes[1].value.coding[1] ??= {};
      result.situationTypes[1].value.coding[1].code = value;
      result.situationTypes[1].value.coding[1].system = sct_prefix;
    },
  },
  sitToLabelSCT: {
    targetKey: "situationTypes[1].value.coding[1].display",
    transform: (value, result) => {
      result.situationTypes ??= [];
      result.situationTypes[1] ??= {
        type: "hasExpectedSituation",
        value: { coding: [] },
      };
      result.situationTypes[1].value.coding[1] ??= {};
      result.situationTypes[1].value.coding[1].display = value;
    },
  },
};

const gpRec_schema = {
  wasDerivedFrom: {
    targetKey: "hasSource",
    transform: (value) => value,
  },
  partOf: {
    targetKey: "partOf",
    transform: (value) => value,
  },
  gpRecId: {
    targetKey: "id",
    transform: (value) => value,
  },
  extractedFrom: {
    targetKey: "extractedFrom",
    transform: (value) => value,
  },
  label: {
    targetKey: "title",
    transform: (value) => value,
  },
  stUris: {
    targetKey: "clinicalStatements",
    transform: (value) => value.split(","),
  },
};

const stData_schema = {
  st_id: {
    targetKey: "id",
    transform: (value) => value,
  },
  statementTitle: {
    targetKey: "hasStatementTitle",
    transform: (value) => value,
  },
  statementText: {
    targetKey: "hasStatementText",
    transform: (value) => value,
  },
  organizationName: {
    targetKey: "organization",
    transform: (value) => value.split(", "),
  },
  jurisdiction: {
    targetKey: "jurisdiction",
    transform: (value) => value.split(", "),
  },
  derivedFromSt: {
    targetKey: "derivedFrom",
    transform: (value) => value.split(", "),
  },
  hasSources: {
    targetKey: "hasTarget",
    transform: (value) => value.split(", "),
  },
};

///// create SPECIFIC TMR concepts in JSON

function get_care_action_data(sparqlResults, context = {}) {
  return transformSPARQLResults(sparqlResults, care_action_schema, context);
}

function get_transition_data(sparqlResults, context = {}) {
  return transformSPARQLResults(sparqlResults, transition_schema, context);
}

function get_causation_belief_data_short(sparqlResults, context = {}) {
  return transformSPARQLResults(sparqlResults, cb_schema, context);
}

function get_causation_belief_w_transition_data(sparqlResults, context = {}) {
  return transformSPARQLResults(sparqlResults, cb_tr_schema, context);
}

function get_recommendation_data_short(sparqlResults, context = {}) {
  return transformSPARQLResults(sparqlResults, recommendation_schema, context);
}

function get_recommendation_with_cb_data(sparqlResults, context = {}) {
  return transformSPARQLResults(
    sparqlResults,
    {}, //recommendation_cb_schema,
    context
  );
}

function set_cig_id(id, no_prefix = false) {
  if (!id) throw new Error("no CIG id has been added");

  if (id.startsWith("CIG-")) {
    //return id
    if (no_prefix) id = id.trim().substring(0, "CIG-".length);
  } else {
    //doesnt start with CIG- but it is required
    if (!no_prefix) id = `CIG-${id.trim()}`;
  }
  return id;
}

/**
 *
 * @param {String} label label representing the knowledge as a full URI or a shortened version
 * @param {String} prefix prefix to add to label
 * @param {Boolean} fullUri set the label argument as a full URI? otherwise, just the identifying section of the URI
 *  @param {Boolean} shortenedURI set the label argument as a shortened URI?
 */
function set_uri(label, prefix = null, fullUri = false, shortenedURI = false) {
  //if its already full URI, return
  if (label.includes(data_prefix)) return label.trim();

  let output = "";

  //if it is not full URI neither has the expected prefix, add prefix
  if (prefix && !label.startsWith(prefix)) output = prefix + "-";

  //add the label to the final output
  output += label.trim();

  //construe URI
  output = fullUri
    ? data_prefix + output
    : shortenedURI
    ? data_prefix_short + output
    : output;

  return output;
}

/**
 *
 * @param {String} label label representing the knowledge as a full URI or a shortened version
 * @param {String} prefix prefix to add to label
 * @param {Boolean} fullUri set the label argument as a full URI? otherwise, just the identifying section of the URI
 *  @param {Boolean} shortenedURI set the label argument as a shortened URI?
 */
function set_uri(label, prefix = null, fullUri = false, shortenedURI = false) {
  //if its already full URI, return
  if (label.includes(data_prefix)) return label;

  let output = "";

  //if it is not full URI neither has the expected prefix, add prefix
  if (prefix && !label.startsWith(prefix)) output = prefix + "-";

  //add the label to the final output
  output += label;

  //construe URI
  output = fullUri
    ? data_prefix + output
    : shortenedURI
    ? data_prefix_short + output
    : output;

  return output;
}

function mergeArraysToMap(keys = [], values = []) {
  if (keys.length !== values.length) {
    throw new Error("The number of keys and values must be the same.");
  }

  // Use reduce to create the key/value map
  const mergedMap = keys.reduce((map, key, index) => {
    map[key] = values[index];
    return map;
  }, {});

  return mergedMap;
}

/**
 *
 * @param {Array} bindings
 * @returns Map
 */
async function get_CB_uris_from_bindings(bindings = []) {
  const expr_uri = jsonata("cbUri.value[]");
  const expr_contrib = jsonata("contrib.value[]");

  const result = await Promise.all([
    expr_uri.evaluate(bindings),
    expr_contrib.evaluate(bindings),
  ]);

  const keys = result[0]; // CB URIs
  const values = result[1]; // CB contributions to given Rec

  const contribMap = (keys, values) => {
    const map = new Map();
    for (let i = 0; i < keys.length; i++) {
      map.set(keys[i], values[i]);
    }
    return map;
  };

  //create map
  return { uris: keys, contribs: contribMap(keys, values) };
}

function action_rec(req, res, insertOrDelete) {
  //data id for this rec
  const id = `data:Rec${req.body.cig_id}-${req.body.id}`;
  let sources = "";
  const date = new Date().toJSON();
  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = `${id} vocab:isPartOf data:CIG-${req.body.cig_id} .`;

  if (req.body.derivedFrom) {
    sources = `  prov:wasDerivedFrom  `;

    req.body.derivedFrom.split(",").forEach(function (code) {
      sources += ` <` + code + `> ,`;
    });
    //this removes the last coma
    sources = sources.substring(0, sources.length - 1);
  }

  // Guideline format:
  const $ =
    id +
    `_head { 
          ` +
    id +
    `_head
              a     nanopub:Nanopublication ;
              nanopub:hasAssertion        ` +
    id +
    ` ;
              nanopub:hasProvenance       ` +
    id +
    `_provenance ;
              nanopub:hasPublicationInfo  ` +
    id +
    `_publicationinfo .
    }`;

  const body =
    id +
    `_assertion {
      ` +
    id +
    `  a   vocab:ClinicalRecommendation ;
              rdfs:label  '''` +
    req.body.label +
    `'''@en ;
              vocab:aboutExecutionOf  data:ActAdminister` +
    req.body.careAction_id +
    ` ;
              vocab:partOf            data:CIG-` +
    req.body.cig_id +
    ` ;
              vocab:strength          vocab:` +
    req.body.strength +
    ` .
    }`;

  const provenance =
    id +
    `_provenance {
      ` +
    id +
    `_provenance
      a  oa:Annotation ;
      oa:hasBody  ` +
    id +
    ` ;
      oa:hasTarget                  [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
      ` +
    id +
    `
      ` +
    sources +
    ` .
      }`;

  const publication =
    id +
    `_publicationinfo {
        ` +
    id +
    `_head
        prov:generatedAtTime          "` +
    date +
    `"^^xsd:dateTime ;
        prov:wasAttributedTo          data:` +
    req.body.author +
    `.
      }`;

  return {
    id2CIG: id2CIG,
    query: `GRAPH ${head} GRAPH ${body} GRAPH ${provenance} GRAPH ${publication}`,
  };
  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,

    insertOrDelete,
    function (err, status) {
      if (status === 200) {
        //add assertion id to default graph as part of CIG
        utils.sparqlUpdate(
          "CIG-" + req.body.cig_id,
          id2CIG,
          insertOrDelete,
          function (err2, status2) {
            res.status(status2).end();
          }
        );
      } else {
        //didnt work. send first status back
        logger.error(err);
        res.status(status);
      }
    }
  );
}

function insert_CB_in_rec(req, res, insertOrDelete) {
  const recId = `data:Rec` + req.body.cig_id + `-` + req.body.rec_id;

  const body =
    recId +
    ` {
      ` +
    recId +
    ` vocab:basedOn data:CB` +
    req.body.belief_id +
    ` .
      data:CB` +
    req.body.belief_id +
    ` vocab:contribution vocab:` +
    req.body.contribution +
    `. }`;

  const graph = `GRAPH ${body}`;
  utils.sparqlUpdate("CIG-" + req.body.cig_id, graph);

  res.status(status).end();
}

function insert_precond_in_rec(req, res, insertOrDelete) {
  const recId = `data:Rec` + req.body.cig_id + `-` + req.body.rec_id;

  const body =
    recId +
    ` {
      ` +
    recId +
    ` vocab:hasFilterSituation data:Sit` +
    req.body.precond_id +
    ` .
    }`;

  const graph = `GRAPH ${body}`;
  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    graph,
    insertOrDelete,
    function (err, status) {
      if (err) {
        logger.debug(
          `error when updating recommendation with precondition: ${err}`
        );
      }
      res.status(status).end();
    }
  );
}

//filter out vocab types when unnecesary for the triggered router
function filter_vocab_rec_type(RecUris) {
  if (!Array.isArray(RecUris)) return RecUris;
  let result = RecUris.filter(
    (uri) =>
      !(
        uri === "http://anonymous.org/vocab/ClinicalRecommendation" ||
        uri === "http://anonymous.org/vocab/GoodPracticeRecommendation"
      )
  );
  return result;
}

async function get_rdf_atom_as_array(bindings) {
  let expr = jsonata("[**.value]");
  const result = await expr
    .evaluate(bindings)
    .catch((err) => logger.error(err));
  //logger.debug(result);
  return result;
}

async function get_sparqlquery_arr(arr_resp) {
  logger.debug(`arr_resp is ${JSON.stringify(arr_resp)}`);

  let expr = jsonata("[$.bindings.**.value]");
  const result = await expr
    .evaluate(arr_resp)
    .catch((err) => logger.error(err));

  return result;
}

function sparql_drop_named_graphs(ds_id, id) {
  let head_graph = `data:${id}_head`;
  let assert_graph = `data:${id}`;
  let prov_graph = `data:${id}_provenance`;
  let pubInfo_graph = `data:${id}_publicationinfo`;

  return ` DROP SILENT GRAPH ${head_graph} ;  DROP SILENT GRAPH ${assert_graph} ; DROP SILENT GRAPH ${prov_graph} ; DROP SILENT GRAPH ${pubInfo_graph} `;
}

/**
 *
 * @param {string} cigFrom original CIG
 * @param {string} cigTo destination CIG
 * @param {string} nanoHead
 * @param {string} nanoAssert
 * @param {string} nanoProv
 * @param {string} nanoPubInfo
 */
function addGraphsDataFromToCig(
  cigFrom,
  cigTo,
  nanoHead,
  nanoAssert,
  nanoProv,
  nanoPub
) {
  let insertGraphsData = ``;
  let graphs;
  let assertGraphs = ``;
  let provGraphs = ``;
  let headGraphs = ``;
  let nanopubGraphs = ``;
  let graphDescrDel = ``;
  let graphDescrIns = ``;

  let deleteTriples;

  const cigFromUrl =
    "http://" +
    config.JENA_HOST +
    ":" +
    config.JENA_PORT +
    "/" +
    cigFrom +
    "/query";

  assertGraphs = `\nGRAPH <` + nanoAssert + `> { ?a  ?b ?c } `;
  provGraphs = `\nGRAPH <` + nanoProv + `> { ?d  ?e ?f } `;
  nanopubGraphs = `\nGRAPH <` + nanoPub + `> { ?g ?h ?i } `;
  headGraphs = `\nGRAPH <` + nanoHead + `> { ?j ?k ?l } `;

  graphDescrDel +=
    `\nGRAPH <` +
    nanoAssert +
    `> { <` +
    nanoAssert +
    `> vocab:partOf data:` +
    cigFrom +
    ` } `;

  graphDescrIns +=
    `\nGRAPH <` +
    nanoAssert +
    `> { <${nanoAssert}> vocab:partOf data:${cigTo} ;
          vocab:extractedFrom data:${cigFrom} . } `;

  let graphDescrInsDef =
    `\n <` + nanoAssert + `> vocab:partOf data:` + cigTo + ` . `;

  insertGraphsData =
    `\nINSERT {` +
    graphDescrInsDef +
    headGraphs +
    assertGraphs +
    graphDescrIns +
    nanopubGraphs +
    provGraphs +
    `} \nWHERE { SERVICE <` +
    cigFromUrl +
    `> { ` +
    headGraphs +
    assertGraphs +
    nanopubGraphs +
    provGraphs +
    ` } } ; `;

  deleteTriples = `\nDELETE WHERE { ` + graphDescrDel + ` } ; `;

  ///UPDATE GRAPH STORE///

  return insertGraphsData + deleteTriples;
}

module.exports = {
  filter_vocab_rec_type,
  insert_precond_in_rec,
  insert_CB_in_rec,
  action_rec,
  get_care_action_data,
  get_causation_belief_data_short,
  get_causation_belief_w_transition_data,
  get_recommendation_data_short,
  setUri: set_uri,
  get_rdf_atom_as_array,
  sparql_drop_named_graphs,
  addGraphsDataFromToCig,
  get_sparqlquery_arr,
  get_CB_uris_from_bindings,
  set_cig_id,
  set_uri,
};
