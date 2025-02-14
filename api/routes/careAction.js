const express = require("express");
const { body, validationResult } = require("express-validator");
const { StatusCodes, ReasonPhrases } = require("http-status-codes");
const router = express.Router();
const config = require("../lib/config");
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFunct = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

const routes = {
  "/drug/individual": auxFunct.ResourceTypes.DrugType,
  "/drug/category": auxFunct.ResourceTypes.DrugCategory,
  "/nondrug/individual": auxFunct.ResourceTypes.NonDrugType,
  "/vaccine/individual": auxFunct.ResourceTypes.VaccineType,
  "/vaccine/category": auxFunct.ResourceTypes.VaccineCategory,
};

/**
 *
 * @param {string} dataId
 * @param {string} sct_code
 * @param {string} sct_label
 * @returns
 */
function definedSctResource(dataId, sct_code, sct_label) {
  if (!dataId || !auxFunct.isValidArgument(sct_code)) return "";

  let sctDef = `${dataId} vocab:hasSctId snomed:${sct_code} .`;

  if (auxFunct.isValidArgument(sct_label)) {
    sctDef += `${dataId} vocab:hasSctLbl "${sct_label}"@en .`;
  }

  return sctDef;
}

// SPARQL Query Helper
async function postCareAction(sparqlQuery, operationType, type = "", id = "") {
  const deleteTemplate = `data:${type}${id} ?p ?o .`;
  const content =
    operationType === config.INSERT ? sparqlQuery : deleteTemplate;
  const sparqlStr = `${operationType} ${
    operationType === config.INSERT ? "DATA" : ""
  } { ${content} } ${
    operationType === config.DELETE ? `WHERE { ${deleteTemplate} }` : ""
  }`;
  return utils.sparqlUpdate("careActions", sparqlStr);
}

/**
 *
 * @param {string} typeClass resource type
 * @param {string} dataId resource URI
 * @param {string} label resource label
 * @param {string} sct_id Optional SCT identifier
 * @param {string} sct_label Optional SCT english representation
 * @param {string} components_ids list of resources that compose a drug combination type
 * @returns RDF representation of resource of type string
 */
function defineResourceType(
  typeClass,
  dataId,
  label,
  sct_id,
  sct_label,
  components_ids = []
) {
  let def = `${dataId} a vocab:${typeClass} , owl:NamedIndividual ;
                          rdfs:label "${label}"@en .`;

  //Add components types for Drug Combination type
  if (typeClass === auxFunct.ResourceTypes.DrugCombinationType) {
    def += components_ids
      .map((type) => `${dataId} vocab:hasComponent ${type} .`)
      .join("");
  }

  // add SCT details
  def += definedSctResource(dataId, sct_id, sct_label);

  return def;
}

/**
 *
 * @param {string} typeClass resource type
 * @param {string} id resource path id
 * @param {string} postfixTp resource type shortened
 * @param {string} actionTp
 * @param {string} label
 * @param {string} sct_action_id
 * @param {string} sct_action_label
 * @param {string} subsumed_ids_str
 * @param {string} grouping_criteria_ids_str
 * @returns
 */
function defineCareActionType(
  typeClass,
  id,
  dataId,
  actionTp,
  label,
  sct_action_id,
  sct_action_label,
  subsumed_ids_str,
  grouping_criteria_ids_str
) {
  const dataActId = `data:ActAdminister${id}`;

  const parseList = (input) =>
    auxFunct.isValidArgument(input)
      ? input.split(",").map((item) => item.trim())
      : [];

  let def = `${dataActId}  a  vocab:${typeClass},  owl:NamedIndividual ;
            rdfs:label "${label}"@en ;
            vocab:${actionTp} ${dataId} .`;

  const subsumptionList = parseList(subsumed_ids_str);
  const groupingCriteriaList = parseList(grouping_criteria_ids_str);

  subsumptionList.forEach((item) => {
    def += `${dataActId} vocab:subsumes data:ActAdminister${item} .`;
  });

  groupingCriteriaList.forEach((item) => {
    def += `${dataActId} vocab:hasGroupingCriteria data:${item} .`;
  });

  def += definedSctResource(dataId, sct_action_id, sct_action_label);
  // TODO: skos:broader SCT?

  return def;
}

/**
 *
 * @param {string} body
 * @param {string} typeClass
 * @param {string} postfixTp
 * @param {string} actionTp
 * @returns
 */
function careActionTypeDefinition(body, typeClass, postfixTp, actionTp) {
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
    components_ids, // not given default as must contain elems if part of a combination type
  } = body;

  const coreFields = [
    id,
    drug_label,
    action_label,
    typeClass,
    postfixTp,
    actionTp,
  ];
  if (typeClass == auxFunct.ResourceTypes.DrugCombinationType)
    coreFields.push(components_ids);

  //validate core arguments for all types
  const invalidField = auxFunct.findInvalidField();

  if (invalidField) {
    throw new ErrorHandler(
      400,
      `Missing or invalid required field: ${invalidField}`
    );
  }

  // resource URI
  const dataId = `data:${postfixTp}${id}`;

  //define the TMR resource type
  let definition = defineResourceType(
    typeClass,
    dataId,
    drug_label,
    sct_drug_code,
    sct_drug_label,
    components_ids //this is about combining drug types, hence added to resource type
  );

  //TODO: action combination type (towards a 1-2-1 mapping of CGs to CIGs)

  const adminDefinition = defineCareActionType(
    typeClass,
    id,
    postfixTp,
    actionTp,
    action_label,
    sct_action_code,
    sct_action_label,
    subsumed_ids,
    grouping_criteria_ids
  );

  return `${definition} . \n${adminDefinition}`;
}

//////
//
// Routes
//
//////

const addHandler = (resourceType) => async (req, res) => {
  try {
    const { postfixTp, actionTp } = auxFunct.getTypeDetails(resourceType);

    const sparqlQuery = careActionTypeDefinition(
      req.body,
      resourceType,
      postfixTp,
      actionTp
    );

    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);

    res.status(status).send(data);
  } catch (err) {
    const errorMessage = `Error adding care action (Type: ${resourceType}) via path ${
      req.path
    }: ${err.message || err}`;

    logger.error(errorMessage);

    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.INTERNAL_SERVER_ERROR,
      error: err.message,
    });
  }
};

////////////
/// DELETE
///////////

// DELETE Filters
function deleteFilter(typeClass, id) {
  const { postfixTp, actionTp } = auxFunct.getTypeDetails(typeClass);
  return `FILTER (?s = data:${postfixTp}${id} || ?s = data:ActAdminister${id})`;
}

const deleteHandler = (resourceType) => async (req, res) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return res.status(StatusCodes.BAD_REQUEST).json({
      status: "Error",
      message: errors.array(),
    });
  }

  try {
    const { id } = req.body;
    const filter = deleteFilter(resourceType, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error deleting care action (Type: ${resourceType}, ID: ${req.body.id}) via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.INTERNAL_SERVER_ERROR,
    });
  }
};

//////////////////
// GET  care action type
/////////////////

router.post(
  "/all/get",
  [
    body("id").optional().isString().withMessage("ID must be a string."),
    body("uri").optional().isString().withMessage("URI must be a string."),
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(StatusCodes.BAD_REQUEST).json({
        status: "Error",
        message: errors.array(),
      });
    }

    try {
      const { id, uri } = req.body;
      const sparqlResults = await utils.getCareActionData(
        "careActions",
        id,
        uri
      );
      const data = auxFunct.get_care_action_data(sparqlResults, {});
      res.status(StatusCodes.OK).send(data);
    } catch (err) {
      logger.error(
        `Error retrieving all care actions via path ${req.path}: ${err.message}`,
        { stack: err.stack }
      );
      res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
        status: "Error",
        message: ReasonPhrases.INTERNAL_SERVER_ERROR,
      });
    }
  }
);

/////////////////
// Routes Handler
////////////////

Object.entries(routes).forEach(([route, resourceType]) => {
  router.post(route + "/add", addHandler(resourceType));
});

Object.entries(routes).forEach(([route, resourceType]) => {
  router.post(
    route + "/delete",
    [
      body("id")
        .isString()
        .notEmpty()
        .withMessage("ID is required and must be a string."),
    ],
    deleteHandler(resourceType)
  );
});

module.exports = router;
