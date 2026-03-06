const express = require("express");
const { body, validationResult } = require("express-validator");
const { StatusCodes, ReasonPhrases } = require("http-status-codes");
const router = express.Router();
const rateLimit = require("express-rate-limit");

// Core dependencies
const config = require("../lib/config");
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFunct = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

// Constants and Configuration
const SNOMED_PREFIX = "http://snomed.info/sct/";
const DATA_PREFIX = "http://anonymous.org/data/";
const VOCAB_PREFIX = "http://anonymous.org/vocab/";
const { isValidId, parseIdsInput, escapeQuotes } = require("../lib/router_functs/route_helpers");

// Rate limiting for create/delete operations
const createLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 50, // limit each IP to 50 create requests per windowMs
  message: {
    status: "error",
    message: "Too many care action creation requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

const deleteLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 20, // limit each IP to 20 delete requests per windowMs
  message: {
    status: "error",
    message: "Too many care action deletion requests, please try again later.",
  },
  standardHeaders: true,
  legacyHeaders: false,
});

// Route mappings with metadata
const ROUTE_MAPPINGS = Object.freeze({
  "/property": {
    resourceType: auxFunct.ResourceTypes.PropertyType,
    description: "Individual property type",
    requiresComponents: false,
    maxLabelLength: 200,
  },
  "/situation": {
    resourceType: auxFunct.ResourceTypes.SituationType,
    description: "Individual situation type",
    requiresComponents: false,
    maxLabelLength: 200,
  },
  "/situation/compound": {
    resourceType: auxFunct.ResourceTypes.CompoundSituationType,
    description: "Compound situation type",
    requiresComponents: true,
    maxLabelLength: 200,
  },
  "": {
    resourceType: auxFunct.ResourceTypes.TransitionType,
    description: "Individual transition type",
    requiresComponents: false,
    maxLabelLength: 200,
  }
});

/**
 * Helper function to get friendly resource type name
 */
function getResourceTypeName(resourceType) {
  const typeNames = {
    [auxFunct.ResourceTypes.PropertyType]: "property",
    [auxFunct.ResourceTypes.SituationType]: "situation",
    [auxFunct.ResourceTypes.CompoundSituationType]: "compound situation",
    [auxFunct.ResourceTypes.TransitionType]: "transition",
  };
  return typeNames[resourceType] || "resource";
}

/**
 * Enhanced validation rules for resource creation
 */
const getValidationRules = (routeConfig) => {
  const isTransition = routeConfig.resourceType === auxFunct.ResourceTypes.TransitionType;

  const rules =[
  body("id")
    .isString()
    .trim()
    .isLength({ min: 2, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "ID must be 1-50 alphanumeric characters, underscores, or hyphens"
    ),
  
  body("pre_situation_id")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 2, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "ID must be 1-50 alphanumeric characters, underscores, or hyphens"
  ),

  body("post_situation_id")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 2, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "ID must be 2-50 alphanumeric characters, underscores, or hyphens"
  ),

  body("affected_property_id")
    .optional()
    .isString()
    .trim()
    .isLength({ min: 2, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "ID must be 2-50 alphanumeric characters, underscores, or hyphens"
  ),

  body("derivative")
    .optional()
    .isString()
    .trim()
    .customSanitizer((v) => (typeof v === "string" ? v.toLowerCase() : v))
    .isIn(["increase", "decrease", "maintain"])
    .withMessage("Derivative must be one of: 'increase', 'decrease', 'maintain'"),

  body("connective")
    .optional()
    .isString()
    .trim()
    .customSanitizer((v) => (typeof v === "string" ? v.toLowerCase() : v))
    .isIn(["and", "or", "neg"])
    .withMessage("Connective must be one of: 'and', 'or', 'neg'"),

  // SNOMED CT codes validation (optional but if provided must be valid)
  body("sctid")
    .optional()
    .isString()
    .trim()
    .matches(/^\d+$/)
    .withMessage("SNOMED CT drug code must be numeric"),

  body("sctid_label")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 300 })
    .withMessage("SNOMED CT drug label must be maximum 300 characters"),

  body("stateOfproperty")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 20 })
    .withMessage("State of property must be maximum 20 characters"),

  // Array fields validation
  body("situation_id_list")
    .optional()
    .custom((value) => {
      if (typeof value === "string") {
        const ids = value.split(",").map((id) => id.trim());
        return ids.every((id) => /^[a-zA-Z0-9_-]+$/.test(id));
      }
      return (
        Array.isArray(value) &&
        value.every(
          (id) => typeof id === "string" && /^[a-zA-Z0-9_-]+$/.test(id)
        )
      );
    })
    .withMessage("Situation IDs must be valid identifiers"),

  ];

    // Label: required for everything except TransitionType (label is derived for transitions)
  if (isTransition) {
    rules.push(
      body("label")
        .optional()
        .isString()
        .trim()
        .isLength({ max: routeConfig.maxLabelLength })
        .withMessage(`Label must be maximum ${routeConfig.maxLabelLength} characters`)
    );
  } else {
    rules.push(
      body("label")
        .isString()
        .trim()
        .isLength({ min: 1, max: routeConfig.maxLabelLength })
        .withMessage(`Label must be 1-${routeConfig.maxLabelLength} characters`)
    );
  }

  return rules;
};

/**
 * Enhanced SNOMED CT resource definition with validation
 */
function createSnomedResourceDefinition(dataId, sctCode, sctLabel) {
  if (!dataId) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      "Data ID is required for SNOMED resource"
    );
  }

  if (!sctCode || !auxFunct.isValidArgument(sctCode)) {
    return ""; // SNOMED is optional
  }

  // Validate SNOMED code format
  if (!/^\d+$/.test(sctCode)) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      `Invalid SNOMED CT code format: ${sctCode}`
    );
  }
  let sctDefinition = `\n${dataId} vocab:hasSctId snomed:${sctCode} ;\n`;

  if (auxFunct.isValidArgument(sctLabel)) {
    sctDefinition += ` vocab:hasSctLbl "${escapeQuotes(sctLabel)}"@en .\n`;
  } else {
    sctDefinition = sctDefinition.slice(0, -2) + " .\n"; // Remove semicolon, add period
  }

  return sctDefinition;
}

/**
 * Specializes Transition type
 * Ensures proper prefixes are added to property and situation IDs
 */
// Use dataId (e.g. "data:MyTransition") as subject
function speciliazeTransitionType(
  dataId,
  affects,
  derivative,
  preSituation,
  postSituationId
){
  if (!dataId || !affects || !derivative || !preSituation || !postSituationId) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      "All parameters are required for Transition specialization"
    );
  }

  // Add proper prefixes if not already present
  // Properties should have "Prop" prefix
  const affectedProperty = affects.startsWith('Prop') || affects.startsWith('CompSit') 
    ? affects 
    : `Prop${affects}`;
  
  // Situations should have "Sit" or "CompSit" prefix
  const preSituationWithPrefix = preSituation.startsWith('Sit') || preSituation.startsWith('CompSit')
    ? preSituation 
    : `Sit${preSituation}`;
  
  const postSituationWithPrefix = postSituationId.startsWith('Sit') || postSituationId.startsWith('CompSit')
    ? postSituationId 
    : `Sit${postSituationId}`;

  return `
    ${dataId} vocab:affects data:${affectedProperty} ;
        vocab:derivative "${escapeQuotes(derivative)}" ;
        vocab:hasExpectedSituation data:${preSituationWithPrefix} ;
        vocab:hasTransformableSituation data:${postSituationWithPrefix} .
  `;
}

/**
 * Enhanced resource type definition with validation
 */
function createResourceTypeDefinition(
  type,
  typeClass,
  dataId,
  label,
  stateOfproperty,
  connective,
  componentsIds = []
) {
  // Basic required params
  if (!type || !dataId || !typeClass) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      "Type class and data ID are required"
    );
  }

  // Build list of predicate triples (without the subject). We'll join them later.
  const triples = [];

  // Only add a label for non-TransitionType resources. TransitionType must omit rdfs:label.
  if (type !== auxFunct.ResourceTypes.TransitionType) {
    if (!label) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "Label is required for this resource type"
      );
    }
    const escapedLabel = escapeQuotes(label || "");
    triples.push(`rdfs:label "${escapedLabel}"@en`);
  }

  // Coerce componentsIds to an array (accept comma-separated string or array)
  const normalizedComponents = parseIdsInput(componentsIds);

  // Add components for Compound Situation type (and|or|neg)
  if (type === auxFunct.ResourceTypes.CompoundSituationType) {
    if (!normalizedComponents || normalizedComponents.length < 2) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "Compound Situation type requires at least 2 components"
      );
    }

    // Validate situation IDs and build the list
    const validatedComponents = normalizedComponents.map((componentId) => {
      const trimmedId = String(componentId).trim();
      if (!isValidId(trimmedId)) {
        throw new ErrorHandler(
          StatusCodes.BAD_REQUEST,
          `Invalid situation ID: ${trimmedId}`
        );
      }
      return `data:${trimmedId}`;
    });

    triples.push(`rdf:${connective} ${validatedComponents.join(" , ")}`);
  }

  if (type === auxFunct.ResourceTypes.SituationType) {
    if (stateOfproperty && auxFunct.isValidArgument(stateOfproperty)) {
      triples.push(`vocab:stateOf "${escapeQuotes(stateOfproperty)}"@en`);
    }
  }

  // Assemble definition
  let definition = `\n${dataId} a vocab:${typeClass} , owl:NamedIndividual`;
  if (triples.length > 0) {
    definition += ` ;\n` + triples.join(" ;\n");
  }
  definition += " .\n";
  return definition;
}


/**
 * Main function to create complete transition type definition
 */
function createCompleteTransitionDefinition(requestBody, routeConfig) {
  try {
    const {
      id,
      label,
      sctid,
      sctid_label,
      stateOfproperty,
      connective,
      pre_situation_id,
      post_situation_id,
      affected_property_id,
      derivative,
      situation_id_list,
    } = requestBody;

    const { resourceType } = routeConfig;

    // Convert string arrays to proper arrays if needed
    const processedSituationsIds = situation_id_list
      ? typeof situation_id_list === "string"
        ? situation_id_list.split(",").map((id) => id.trim())
        : situation_id_list
      : [];

    // Here, actionTp is undefined
    const { postfixTp, actionTp, adminTp } = auxFunct.getTypeDetails(resourceType);

    // Validate required components for combination types
    if (
      routeConfig.requiresComponents &&
      (!processedSituationsIds || processedSituationsIds.length < 1)
    ) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "Situation Compound Precondition types require at least 1 Situation ID"
      );
    }

    // Build resource URIs
    const dataId = `data:${postfixTp}${id}`;
    // dataType same as resourceType except for CompoundSituationType
    // which uses SituationType as adminTp
    const dataType = adminTp;

    // Create the TMR resource type definition
    let resourceDefinition = createResourceTypeDefinition(
      resourceType,
      dataType,
      dataId,
      label,
      stateOfproperty,
      connective,
      processedSituationsIds
    );

    let transitionSpecialization = '';
    if (resourceType === auxFunct.ResourceTypes.TransitionType) {
      // pass the full dataId (data:...) so the specialization uses the correct subject
      transitionSpecialization = speciliazeTransitionType(
        dataId,
        affected_property_id,
        derivative,
        pre_situation_id,
        post_situation_id
      );
    }

    // Add SNOMED CT details for resource
    resourceDefinition += createSnomedResourceDefinition(
      dataId,
      sctid,
      sctid_label
    );

    const completeDefinition = `${resourceDefinition}\n${transitionSpecialization}`;

    logger.debug("Generated RDF definition", {
      id,
      resourceType,
      definitionLength: completeDefinition.length,
    });

    return {
      definition: completeDefinition,
      createdIds: {
        resourceId: id, // Just the user-provided ID
        resourceUri: `http://anonymous.org/${dataId.replace(":", "/")}`, // Convert colon to slash
      },
    };
  } catch (error) {
    const resourceTypeName = getResourceTypeName(routeConfig.resourceType);
    logger.error(`Failed to create ${resourceTypeName} definition`, {
      error: error.message,
      requestBody: requestBody,
      routeConfig: routeConfig,
    });
    throw error;
  }
}

/**
 * Enhanced SPARQL execution with better error handling
 */
async function executeCareActionOperation(
  sparqlQuery,
  operationType,
  resourceType,
  id
) {
  try {
    let sparqlStatement;

    if (operationType === config.INSERT) {
      sparqlStatement = `INSERT DATA { ${sparqlQuery} }`;
    } else if (operationType === config.DELETE) {
      const { postfixTp } = auxFunct.getTypeDetails(resourceType);
      const deletePattern = `?s ?p ?o`;
      const whereClause = `
        ${deletePattern} .
        FILTER (?s = data:${postfixTp}${id})
      `;
      sparqlStatement = `DELETE { ${deletePattern} } WHERE { ${whereClause} }`;
    } else {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        `Invalid operation type: ${operationType}`
      );
    }

    logger.debug("Executing SPARQL operation", {
      operation: operationType,
      resourceType,
      id,
      queryLength: sparqlStatement.length,
    });

  const result = await utils.sparqlUpdate("transitions", sparqlStatement);

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
      resourceType,
      id,
      error: error.message,
    });
    throw error;
  }
}

/**
 * Enhanced add handler with comprehensive validation and error handling
 */
const createAddHandler = (route, routeConfig) => async (req, res) => {
  try {
    // Check validation results
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      const resourceTypeName = getResourceTypeName(routeConfig.resourceType);
      logger.warn(`Validation failed for ${resourceTypeName} creation`, {
        route,
        errors: errors.array(),
        body: req.body,
      });

      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const { resourceType } = routeConfig;
    const resourceTypeName = getResourceTypeName(resourceType);

    logger.info(`Creating ${resourceTypeName}`, {
      route,
      resourceType,
      id: req.body.id,
      ip: req.ip,
    });

    // Generate SPARQL definition
    const { definition: sparqlQuery, createdIds } =
      createCompleteTransitionDefinition(req.body, routeConfig);

    // Execute the operation
    const { status, data } = await executeCareActionOperation(
      sparqlQuery,
      config.INSERT,
      resourceType,
      req.body.id
    );

    logger.info(`${resourceTypeName.charAt(0).toUpperCase() + resourceTypeName.slice(1)} created successfully`, {
      route,
      resourceType,
      id: req.body.id,
      status,
    });

    res.status(status).json({
      status: "success",
      message: `${resourceTypeName.charAt(0).toUpperCase() + resourceTypeName.slice(1)} ${req.body.id} created successfully`,
      data: {
        operation: data || "Operation completed",
        createdResources: createdIds,
      },
    });
  } catch (error) {
    const resourceTypeName = getResourceTypeName(routeConfig.resourceType);
    logger.error(`Failed to create ${resourceTypeName}`, {
      route,
      resourceType: routeConfig.resourceType,
      id: req.body?.id,
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
      message: `Failed to create ${resourceTypeName}`,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
};

/**
 * Enhanced delete handler with validation
 */
const createDeleteHandler = (route, routeConfig) => async (req, res) => {
  try {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Validation failed",
        errors: errors.array(),
      });
    }

    const { resourceType } = routeConfig;
    const { id } = req.body;
    const resourceTypeName = getResourceTypeName(resourceType);

    logger.info(`Deleting ${resourceTypeName}`, {
      route,
      resourceType,
      id,
      ip: req.ip,
    });

    const { status, data } = await executeCareActionOperation(
      null,
      config.DELETE,
      resourceType,
      id
    );

    logger.info(`${resourceTypeName.charAt(0).toUpperCase() + resourceTypeName.slice(1)} deleted successfully`, {
      route,
      resourceType,
      id,
      status,
    });

    res.status(status).json({
      status: "success",
      message: `${resourceTypeName.charAt(0).toUpperCase() + resourceTypeName.slice(1)} ${id} deleted successfully`,
      data: data || "Operation completed",
    });
  } catch (error) {
    const resourceTypeName = getResourceTypeName(routeConfig.resourceType);
    logger.error(`Failed to delete ${resourceTypeName}`, {
      route,
      resourceType: routeConfig.resourceType,
      id: req.body?.id,
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
      message: `Failed to delete ${resourceTypeName}`,
    });
  }
};

/**
 * Simple GET endpoint to retrieve raw RDF data for any resource
 */
router.get("/:id", async (req, res) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    const { id } = req.params;

    if (!isValidId(id)) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "error",
        message: "Invalid ID format",
        requestId,
      });
    }

    const fullUri = `${DATA_PREFIX}${id}`;

    logger.info("Retrieving resource by ID", {
      requestId,
      id,
      uri: fullUri,
      ip: req.ip,
    });

    // Use DatabaseService to query directly
    const db = req.app.locals.db;
    const query = `
      SELECT ?p ?o WHERE {
        <${fullUri}> ?p ?o
      }
    `;

    let sparqlResults;
    try {
      sparqlResults = await db.sparqlQuery("transitions", query);
    } catch (error) {
      logger.error("SPARQL query failed", {
        requestId,
        error: error.message,
      });
      throw error;
    }

    if (!sparqlResults?.bindings || sparqlResults.bindings.length === 0) {
      logger.warn("No data found for resource", {
        requestId,
        id,
        uri: fullUri,
      });
      
      return res.status(StatusCodes.NOT_FOUND).json({
        status: "error",
        message: "Resource not found",
        requestId,
      });
    }

    // Transform bindings to a more readable format
    const properties = {};
    const types = [];
    
    sparqlResults.bindings.forEach((binding) => {
      const predicate = binding.p.value;
      const predicateName = predicate.split("/").pop().split("#").pop();
      const object = binding.o;

      // Handle rdf:type specially
      if (predicate === "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") {
        types.push(object.value);
      } else {
        // Store other properties
        if (!properties[predicateName]) {
          properties[predicateName] = [];
        }
        
        properties[predicateName].push({
          value: object.value,
          type: object.type,
          lang: object["xml:lang"] || object.lang,
          datatype: object.datatype,
        });
      }
    });

    const responseTime = Date.now() - startTime;

    logger.info("Resource retrieved successfully", {
      requestId,
      id,
      propertiesCount: Object.keys(properties).length,
      typesCount: types.length,
      responseTime,
    });

    res.status(StatusCodes.OK).json({
      status: "success",
      data: {
        id,
        uri: fullUri,
        types,
        properties,
      },
      requestId,
      responseTime,
    });
  } catch (error) {
    const responseTime = Date.now() - startTime;

    logger.error("Failed to retrieve resource", {
      requestId,
      id: req.params?.id,
      error: error.message,
      stack: error.stack,
      responseTime,
    });

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "error",
      message: "Failed to retrieve resource",
      requestId,
      responseTime,
      error: process.env.NODE_ENV === "development" ? error.message : undefined,
    });
  }
});

/**
 * Enhanced get transition/situation/property handler (using structured schema)
 */
router.post(
  "/all/get",
  [
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

      const { id, uri } = req.body;

      if (!id && !uri) {
        return res.status(StatusCodes.BAD_REQUEST).json({
          status: "error",
          message: "Either ID or URI must be provided",
        });
      }

      // Construct full URI if only ID provided
      const fullUri = uri || `${DATA_PREFIX}${id}`;

      logger.info("Retrieving transition/situation/property", { 
        id, 
        uri: fullUri, 
        ip: req.ip 
      });

      // Query using the transitions dataset
      const sparqlResults = await utils.getTransitionData(
        "transitions",
        `<${fullUri}>`
      );

      logger.debug("SPARQL Results received", {
        hasResults: !!sparqlResults,
        hasResultsProperty: !!sparqlResults?.results,
        bindingsLength: sparqlResults?.results?.bindings?.length || 0,
      });

      if (
        !sparqlResults ||
        !sparqlResults.results ||
        !sparqlResults.results.bindings ||
        sparqlResults.results.bindings.length === 0
      ) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Transition/situation/property not found",
        });
      }

      const data = auxFunct.get_transition_data(sparqlResults, {});

      logger.info("Transition/situation/property retrieved successfully", { 
        id, 
        uri: fullUri 
      });

      res.status(StatusCodes.OK).json({
        status: "success",
        data,
      });
    } catch (error) {
      logger.error("Failed to retrieve transition/situation/property", {
        id: req.body?.id,
        uri: req.body?.uri,
        error: error.message,
        stack: error.stack,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to retrieve transition/situation/property",
      });
    }
  }
);

/**
 * Health check endpoint
 */
router.get("/health", (req, res) => {
  res.status(StatusCodes.OK).json({
    status: "healthy",
    service: "transitions",
    timestamp: new Date().toISOString(),
    routes: Object.keys(ROUTE_MAPPINGS).length,
  });
});

/**
 * Get available transition types
 */
router.get("/types", (req, res) => {
  const types = Object.entries(ROUTE_MAPPINGS).map(([route, config]) => ({
    route,
    resourceType: config.resourceType,
    description: config.description,
    requiresComponents: config.requiresComponents,
  }));

  res.status(StatusCodes.OK).json({
    status: "success",
    data: {
      available_types: types,
      total_count: types.length,
    },
  });
});

/**
 * Register all routes dynamically with enhanced validation
 */
Object.entries(ROUTE_MAPPINGS).forEach(([route, routeConfig]) => {
  const validationRules = getValidationRules(routeConfig);

  // Add endpoints
  router.post(
    `${route}/add`,
    createLimiter,
    validationRules,
    createAddHandler(route, routeConfig)
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
    createDeleteHandler(route, routeConfig)
  );
});

module.exports = router;
