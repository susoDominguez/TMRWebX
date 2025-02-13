const express = require("express");
const { StatusCodes, ReasonPhrases } = require("http-status-codes");
const router = express.Router();
const config = require("../lib/config");
const utils = require("../lib/utils");
const { ErrorHandler, handleError } = require("../lib/errorHandler");
const auxFunct = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

/**
 *
 * @param {string} dataId
 * @param {string} sct_code
 * @param {string} sct_label
 * @returns
 */
function definedSctResource(dataId, sct_code, sct_label) {
  let sctDef = "";

  if (dataId && auxFunct.isValidArgument(sct_code)) {
    sctDef += `${dataId} vocab:hasSctId snomed:${sct_code} .`;
    if (auxFunct.isValidArgument(sct_label)) {
      sctDef += `${dataId} vocab:hasSctLbl "${sct_label}"@en .`;
    }
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
 * @param {string} id resource identifier
 * @param {string} label resource label
 * @param {string} sct_id Optional SCT identifier
 * @param {string} sct_label Optional SCT english representation
 * @returns RDF representation of resource of type string
 */
function defineResourceType(typeClass, typeId, id, label, sct_id, sct_label) {
  const dataId = `data:${typeId}${id}`;

  let def = `${dataId} a vocab:${typeClass} , owl:NamedIndividual ;
                          rdfs:label "${label}"@en .`;

  def += definedSctResource(dataId, sct_id, sct_label);

  return def;
}

function defineCareActionType(
  typeClass,
  id,
  label,
  sct_id,
  sct_label,
  subsumptionList = [],
  groupingCriteriaList = []
) {
  const TypeId = typePostfix(typeClass);
  const relationship = typeRelationship(typeClass);
  const dataId = `data:${TypeId}${id}`;

  let def = `data:ActAdminister${id} a vocab:${typeClass}, owl:NamedIndividual ;
                  rdfs:label "${label}"@en ;
                  vocab:${relationship} ${dataId} .`;

  for (let i = 0; i < groupingCriteriaList.length; i++) {
    def += `data:ActAdminister${id} vocab:hasGroupingCriteria data:${groupingCriteriaList[i]} .`;
  }

  for (let i = 0; i < subsumptionList.length; i++) {
    def += `data:ActAdminister${id} vocab:subsumes data:ActAdminister${subsumptionList[i]} .`;
  }

  def += definedSctResource(dataId, sct_id, sct_label);
  // TODO: skos:broader ?

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
function careActionDefinition(body, typeClass, postfixTp, actionTp) {
  // body req

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
    components_ids = [],
  } = body;

  //validate core arguments for all types
  const invalidField = findInvalidField([
    id,
    drug_label,
    action_label,
    typeClass,
    postfixTp,
    actionTp,
  ]);

  if (invalidField) {
    throw new ErrorHandler(
      400,
      `Missing or invalid required field: ${invalidField}`
    );
  }

  //define the TMR resource type
  let definition = defineResourceType(
    typeClass,
    id,
    drug_label,
    sct_drug_code,
    sct_drug_label,
    components_ids
  );

  let subsumptionList = auxFunct.isValidArgument(subsumed_ids)
    ? subsumed_ids.split(",").map((item) => item.trim())
    : [];
  let groupingCriteriaList = auxFunct.isValidArgument(grouping_criteria_ids)
    ? subsumed_ids.split(",").map((item) => item.trim())
    : [];

  const adminDefinition = defineCareActionType(
    typeClass,
    id,
    action_label,
    sct_action_code,
    sct_action_label,
    subsumptionList,
    groupingCriteriaList
  );

  return `${definition} . \n${adminDefinition}`;
}

// DELETE Filters
function deleteFilter(typeClass, id) {
  const Type = typePostfix(typeClass);
  return `FILTER (?s = data:${Type}${id} || ?s = data:ActAdminister${id})`;
}

//////
//
// Routes
//
//////

/**
 * Drug type: Add
 */
router.post("/drug/individual/add", async function (req, res) {
  try {
    const { postfixTp, actionTp } = getTypeDetails(
      auxFunct.ResourceTypes.DrugType
    );

    const sparqlQuery = careActionDefinition(
      req.body,
      auxFunct.ResourceTypes.DrugType,
      postfixTp,
      actionTp
    );

    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error adding individual drug care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

/**
 * Non-drug types: Add
 */
router.post("/nondrug/individual/add", async function (req, res) {
  try {
    const { postfixTp, actionTp } = getTypeDetails(
      auxFunct.ResourceTypes.NonDrugType
    );
    const sparqlQuery = careActionDefinition(
      req.body,
      auxFunct.ResourceTypes.NonDrugType,
      postfixTp,
      actionTp
    );
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error adding individual non-drug care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

/**
 * Drug Category type: Add
 */
router.post("/drug/category/add", async (req, res) => {
  try {
    const { postfixTp, actionTp } = getTypeDetails(
      auxFunct.ResourceTypes.DrugCategory
    );
    const sparqlQuery = careActionDefinition(
      req.body,
      auxFunct.ResourceTypes.DrugCategory,
      postfixTp,
      actionTp
    );
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding drug category via path ${req.path}: ${err}`);
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

/**
 * Drug Combination: Add
 */
router.post("/drug/combination/add", async (req, res) => {
  try {
    const { postfixTp, actionTp } = getTypeDetails(
      auxFunct.ResourceTypes.DrugCombinationType
    );
    const sparqlQuery = careActionDefinition(
      req.body,
      auxFunct.ResourceTypes.DrugCombinationType,
      postfixTp,
      actionTp
    );
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding drug combination via path ${req.path}: ${err}`);
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

/**
 * Vaccine type: Add
 */
router.post("/vaccine/individual/add", async (req, res) => {
  try {
    const { postfixTp, actionTp } = getTypeDetails(
      auxFunct.ResourceTypes.VaccineType
    );
    const sparqlQuery = careActionDefinition(
      req.body,
      auxFunct.ResourceTypes.VaccineType,
      postfixTp,
      actionTp
    );
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error adding individual vaccine via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

/**
 * Vaccine Category type: Add
 */
router.post("/vaccine/category/add", async (req, res) => {
  try {
    const { postfixTp, actionTp } = getTypeDetails(
      auxFunct.ResourceTypes.VaccineCategory
    );
    const sparqlQuery = careActionDefinition(
      req.body,
      auxFunct.ResourceTypes.VaccineCategory,
      postfixTp,
      actionTp
    );
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error adding vaccine category care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

////////////
/// DELETE
///////////

router.post("/drug/individual/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(auxFunct.ResourceTypes.DrugType, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error deleting individual drug care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

router.post("/drug/category/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(auxFunct.ResourceTypes.DrugCategory, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error deleting category drug care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

router.post("/nondrug/individual/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(auxFunct.ResourceTypes.NonDrugType, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error deleting individual nondrug care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

router.post("/vaccine/individual/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(auxFunct.ResourceTypes.VaccineType, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error deleting individual vaccine care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

router.post("/vaccine/category/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(auxFunct.ResourceTypes.VaccineCategory, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(
      `Error deleting drug category care action via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

//////////////////
// GET  care action type
/////////////////

router.post("/all/get", async (req, res) => {
  try {
    const { id, uri } = req.body;
    const sparqlResults = await utils.getCareActionData("careActions", id, uri);
    const data = auxFunct.get_care_action_data(sparqlResults, {});
    res.status(200).send(data);
  } catch (err) {
    logger.error(
      `Error retrieving all care actions via path ${req.path}: ${err}`
    );
    res.status(StatusCodes.INTERNAL_SERVER_ERROR).json({
      status: "Error",
      message: ReasonPhrases.BAD_REQUEST,
    });
  }
});

module.exports = router;
