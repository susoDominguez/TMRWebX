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
  "/drug/individual": {
    resourceType: auxFunct.ResourceTypes.DrugType,
    description: "Individual drug type",
    requiresComponents: false,
    maxLabelLength: 200,
  },
  "/drug/category": {
    resourceType: auxFunct.ResourceTypes.DrugCategory,
    description: "Drug category",
    requiresComponents: false,
    maxLabelLength: 200,
  },
  "/drug/combination": {
    resourceType: auxFunct.ResourceTypes.DrugCombinationType,
    description: "Drug combination type",
    requiresComponents: true,
    maxLabelLength: 200,
  },
  "/nondrug/individual": {
    resourceType: auxFunct.ResourceTypes.NonDrugType,
    description: "Non-drug treatment type",
    requiresComponents: false,
    maxLabelLength: 200,
  },
  "/vaccine/individual": {
    resourceType: auxFunct.ResourceTypes.VaccineType,
    description: "Individual vaccine type",
    requiresComponents: false,
    maxLabelLength: 200,
  },
  "/vaccine/category": {
    resourceType: auxFunct.ResourceTypes.VaccineCategory,
    description: "Vaccine category",
    requiresComponents: false,
    maxLabelLength: 200,
  },
});

/**
 * Enhanced validation rules for care action creation
 */
const getValidationRules = (routeConfig) => [
  body("id")
    .isString()
    .trim()
    .isLength({ min: 1, max: 50 })
    .matches(/^[a-zA-Z0-9_-]+$/)
    .withMessage(
      "ID must be 1-50 alphanumeric characters, underscores, or hyphens"
    ),

  body("drug_label")
    .isString()
    .trim()
    .isLength({ min: 1, max: routeConfig.maxLabelLength })
    .withMessage(
      `Drug label must be 1-${routeConfig.maxLabelLength} characters`
    ),

  body("action_label")
    .isString()
    .trim()
    .isLength({ min: 1, max: routeConfig.maxLabelLength })
    .withMessage(
      `Action label must be 1-${routeConfig.maxLabelLength} characters`
    ),

  // SNOMED CT codes validation (optional but if provided must be valid)
  body("sct_drug_code")
    .optional()
    .isString()
    .trim()
    .matches(/^\d+$/)
    .withMessage("SNOMED CT drug code must be numeric"),

  body("sct_action_code")
    .optional()
    .isString()
    .trim()
    .matches(/^\d+$/)
    .withMessage("SNOMED CT action code must be numeric"),

  body("sct_drug_label")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 300 })
    .withMessage("SNOMED CT drug label must be maximum 300 characters"),

  body("sct_action_label")
    .optional()
    .isString()
    .trim()
    .isLength({ max: 300 })
    .withMessage("SNOMED CT action label must be maximum 300 characters"),

  // Array fields validation
  body("subsumed_ids")
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
    .withMessage("Subsumed IDs must be valid identifiers"),

  body("grouping_criteria_ids")
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
    .withMessage("Grouping criteria IDs must be valid identifiers"),

  // Components validation for combination types
  ...(routeConfig.requiresComponents
    ? [
        body("components_ids")
          .notEmpty()
          .custom((value) => {
            if (typeof value === "string") {
              const ids = value.split(",").map((id) => id.trim());
              return (
                ids.length >= 2 &&
                ids.every((id) => /^[a-zA-Z0-9_-]+$/.test(id))
              );
            }
            return (
              Array.isArray(value) &&
              value.length >= 2 &&
              value.every(
                (id) => typeof id === "string" && /^[a-zA-Z0-9_-]+$/.test(id)
              )
            );
          })
          .withMessage(
            "Combination types require at least 2 valid component IDs"
          ),
      ]
    : []),
];

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
    // Escape quotes in label
    const escapedLabel = sctLabel.replace(/"/g, '\\"');
    sctDefinition += ` vocab:hasSctLbl "${escapedLabel}"@en .\n`;
  } else {
    sctDefinition = sctDefinition.slice(0, -2) + " .\n"; // Remove semicolon, add period
  }

  return sctDefinition;
}

/**
 * Enhanced resource type definition with validation
 */
function createResourceTypeDefinition(
  typeClass,
  dataId,
  label,
  componentsIds = []
) {
  if (!typeClass || !dataId || !label) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      "Type class, data ID, and label are required"
    );
  }

  // Escape quotes in label
  const escapedLabel = label.replace(/"/g, '\\"');

  let definition =
    `\n${dataId} a vocab:${typeClass} , owl:NamedIndividual ;\n` +
    `rdfs:label "${escapedLabel}"@en `;

  // Add components for Drug Combination type
  if (typeClass === auxFunct.ResourceTypes.DrugCombinationType) {
    if (!componentsIds || componentsIds.length < 2) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "Drug combination type requires at least 2 components"
      );
    }

    definition += `;\n vocab:hasComponent `;

    // Validate component IDs and build the definition
    const validatedComponents = componentsIds.map((componentId) => {
      const trimmedId = componentId.trim();
      if (!/^[a-zA-Z0-9_-]+$/.test(trimmedId)) {
        throw new ErrorHandler(
          StatusCodes.BAD_REQUEST,
          `Invalid component ID: ${trimmedId}`
        );
      }
      return `data:${trimmedId}`;
    });

    definition += validatedComponents.join(" , ");
  }

  definition += " .\n";
  return definition;
}

/**
 * Enhanced care action type definition with comprehensive validation
 */
function createCareActionTypeDefinition(
  dataId,
  careActionTypeId,
  adminTypeId,
  actionType,
  actionLabel,
  subsumedIds,
  groupingCriteriaIds
) {
  if (
    !dataId ||
    !careActionTypeId ||
    !adminTypeId ||
    !actionType ||
    !actionLabel
  ) {
    throw new ErrorHandler(
      StatusCodes.BAD_REQUEST,
      "All care action parameters are required"
    );
  }

  // Escape quotes in label
  const escapedLabel = actionLabel.replace(/"/g, '\\"');

  // Define the TMR care action type
  let definition =
    ` ${careActionTypeId} a vocab:${adminTypeId}, owl:NamedIndividual ;\n` +
    ` rdfs:label "${escapedLabel}"@en ;\n` +
    ` vocab:${actionType} ${dataId} .\n`;

  // Parse and validate lists
  const parseAndValidateIds = (idsInput) => {
    if (!idsInput) return [];

    const ids =
      typeof idsInput === "string"
        ? idsInput.split(",").map((id) => id.trim())
        : Array.isArray(idsInput)
        ? idsInput
        : [];

    // Validate each ID
    ids.forEach((id) => {
      if (!/^[a-zA-Z0-9_-]+$/.test(id)) {
        throw new ErrorHandler(
          StatusCodes.BAD_REQUEST,
          `Invalid ID format: ${id}`
        );
      }
    });

    return ids;
  };

  const subsumptionList = parseAndValidateIds(subsumedIds);
  const groupingCriteriaList = parseAndValidateIds(groupingCriteriaIds);

  // Add subsumption relationships
  subsumptionList.forEach((itemId) => {
    definition += `\n${careActionTypeId} vocab:subsumes data:ActAdminister${itemId} .\n`;
  });

  // Add grouping criteria relationships
  groupingCriteriaList.forEach((itemId) => {
    definition += `\n${careActionTypeId} vocab:hasGroupingCriteria data:${itemId} .\n`;
  });

  return definition;
}

/**
 * Main function to create complete care action type definition
 */
function createCompleteCareActionDefinition(requestBody, routeConfig) {
  try {
    const {
      id,
      drug_label,
      action_label,
      sct_drug_label,
      sct_action_label,
      sct_drug_code,
      sct_action_code,
      subsumed_ids = [],
      grouping_criteria_ids = [],
      components_ids,
    } = requestBody;

    const { resourceType } = routeConfig;
    const typeDetails = auxFunct.getTypeDetails(resourceType);

    if (!typeDetails) {
      throw new ErrorHandler(
        StatusCodes.INTERNAL_SERVER_ERROR,
        `Unknown resource type: ${resourceType}`
      );
    }

    const { postfixTp, actionTp, adminTp } = typeDetails;

    // Validate required components for combination types
    if (
      routeConfig.requiresComponents &&
      (!components_ids || components_ids.length < 2)
    ) {
      throw new ErrorHandler(
        StatusCodes.BAD_REQUEST,
        "Combination types require at least 2 component IDs"
      );
    }

    // Build resource URIs
    const dataId = `data:${postfixTp}${id}`;
    const careActionTypeId = `data:ActAdminister${id}`;

    // Create the TMR resource type definition
    let resourceDefinition = createResourceTypeDefinition(
      resourceType,
      dataId,
      drug_label,
      components_ids
    );

    // Add SNOMED CT details for resource
    resourceDefinition += createSnomedResourceDefinition(
      dataId,
      sct_drug_code,
      sct_drug_label
    );

    // Create care action type definition
    let actionDefinition = createCareActionTypeDefinition(
      dataId,
      careActionTypeId,
      adminTp,
      actionTp,
      action_label,
      subsumed_ids,
      grouping_criteria_ids
    );

    // Add SNOMED CT details for action
    actionDefinition += createSnomedResourceDefinition(
      careActionTypeId,
      sct_action_code,
      sct_action_label
    );

    const completeDefinition = `${resourceDefinition}\n${actionDefinition}`;

    logger.debug("Generated RDF definition", {
      id,
      resourceType,
      definitionLength: completeDefinition.length,
    });

    return completeDefinition;
  } catch (error) {
    logger.error("Failed to create care action definition", {
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
      const deletePattern = `
        ?s ?p ?o .
        FILTER (?s = data:${postfixTp}${id} || ?s = data:ActAdminister${id})
      `;
      sparqlStatement = `DELETE { ${deletePattern} } WHERE { ${deletePattern} }`;
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

    const result = await utils.sparqlUpdate("careActions", sparqlStatement);

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
      logger.warn("Validation failed for care action creation", {
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

    logger.info("Creating care action", {
      route,
      resourceType,
      id: req.body.id,
      ip: req.ip,
    });

    // Generate SPARQL definition
    const sparqlQuery = createCompleteCareActionDefinition(
      req.body,
      routeConfig
    );

    // Execute the operation
    const { status, data } = await executeCareActionOperation(
      sparqlQuery,
      config.INSERT,
      resourceType,
      req.body.id
    );

    logger.info("Care action created successfully", {
      route,
      resourceType,
      id: req.body.id,
      status,
    });

    res.status(status).json({
      status: "success",
      message: `Care action ${req.body.id} created successfully`,
      data: data || "Operation completed",
    });
  } catch (error) {
    logger.error("Failed to create care action", {
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
      message: "Failed to create care action",
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

    logger.info("Deleting care action", {
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

    logger.info("Care action deleted successfully", {
      route,
      resourceType,
      id,
      status,
    });

    res.status(status).json({
      status: "success",
      message: `Care action ${id} deleted successfully`,
      data: data || "Operation completed",
    });
  } catch (error) {
    logger.error("Failed to delete care action", {
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
      message: "Failed to delete care action",
    });
  }
};

/**
 * Enhanced get care action handler
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

      logger.info("Retrieving care action", { id, uri, ip: req.ip });

      const sparqlResults = await utils.getCareActionData(
        "careActions",
        id,
        uri
      );

      if (
        !sparqlResults ||
        (sparqlResults.bindings && sparqlResults.bindings.length === 0)
      ) {
        return res.status(StatusCodes.NOT_FOUND).json({
          status: "error",
          message: "Care action not found",
        });
      }

      const data = auxFunct.get_care_action_data(sparqlResults, {});

      logger.info("Care action retrieved successfully", { id, uri });

      res.status(StatusCodes.OK).json({
        status: "success",
        data,
      });
    } catch (error) {
      logger.error("Failed to retrieve care action", {
        id: req.body?.id,
        uri: req.body?.uri,
        error: error.message,
        stack: error.stack,
      });

      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "error",
        message: "Failed to retrieve care action",
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
    service: "care-actions",
    timestamp: new Date().toISOString(),
    routes: Object.keys(ROUTE_MAPPINGS).length,
  });
});

/**
 * Get available care action types
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
