const express = require("express");
const router = express.Router();
const config = require("../lib/config");
const utils = require("../lib/utils");
const auxFunct = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");

// Constants for Care Action Types
const Types = {
  DrugT: "DrugT",
  DrugType: "DrugType",
  NonDrugT: "NonDrugT",
  NonDrugType: "NonDrugType",
  DrugCat: "DrugCat",
  DrugCategory: "DrugCategory",
  DrugCombT: "DrugCombT",
  DrugCombinationType: "DrugCombinationType",
  VacT: "VacT",
  VaccineType: "VaccineType",
  VacCat: "VacCat",
  VaccineCategory: "VaccineCategory",
  DrugAdminT: "DrugAdministrationType",
  //combinedAdminOf: "combinedAdministrationOf",
  vaccWith: "vaccinationWith",
  adminOf: "administrationOf",
  applyOf: "applicationOf"
};

function typePostfix(typeClass) {
  return typeClass === Types.NonDrugType 
          ? Types.NonDrugT 
          : typeClass === Types.VacCat 
            ? Types.VaccineCategory
            : typeClass === VaccineType
              ? Types.VacT
              : typeClass === Types.DrugCategory
                ? Types.DrugCat
                : typeClass === Types.DrugCombinationType
                ? Types.DrugCombT
                : Types.DrugT ;
} ;

function typeRelationship(typeClass) {
  return typeClass === Types.NonDrugType 
          ? Types.applyOf
          : (typeClass === Types.VacCat || typeClass === VaccineType)
              ? Types.vaccWith
              : Types.adminOf ;
} ;

function definedSct(dataId, sctid, sctid_label){
  let sctDef = "";
  
  if(auxFunct.isValidArgument(sctid)){
    sctDef += `${dataId} vocab:hasSctid snomed:${sctid} .`;
    if(auxFunct.isValidArgument(sctid_label)){
      sctDef += `${dataId} skos:prefLabel "${sctid_label}"@en .`
    }
  }
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

// RDF Definitions
function defineIntervention(typeClass, id, label, sctid, label_sctid) {
  const TypeId = typePostfix(typeClass);
  const dataId = `data:${TypeId}${id}`;

  let def = `${dataId}  a  vocab:${typeClass} , owl:NamedIndividual ;
                        rdfs:label "${label}"@en .` ;

      def += definedSct(dataId, sctid, label_sctid);

  return def ;
}

function defineCareAction(typeClass, id, label, sctid, label_sctid, subsumptionList=[], groupingCriteriaList=[]) {
  const TypeId = typePostfix(typeClass);
  const relationship = typeRelationship(typeClass);
  const dataId = `data:${TypeId}${id}`;

  let def = `data:ActAdminister${id} a vocab:${typeClass}, owl:NamedIndividual ;
                  rdfs:label "${label}"@en ;
                  vocab:${relationship} ${dataId} .`;
    
  for (let i = 0; i < groupingCriteriaList.length; i++) {
          def += `data:ActAdminister${id} vocab:hasGroupingCriteria data:${groupingCriteriaList[i]} .`);
  }

  for (let i = 0; i < subsumptionList.length; i++) {
    def += `data:ActAdminister${id} vocab:subsumes data:ActAdminister${subsumptionList[i]} .`);
  }

  def += definedSct(dataId, sctid, label_sctid);
   // TODO: skos:broader ?

  return def;
}

// Define Care Actions
function careActionDefinition(req, typeClass) {

  const { id, label_drug, label_action, label_sctid_drug, label_sctid_action, sctid_drug, sctid_action, subsumed_ids, grouping_criteria_ids, components_ids } = req.body;
  //validate core arguments
  if([id, label_drug,label_action].some(field => ! auxFunct.isValidArgument(field))) return res.status(400).json({ error: 'Missing or invalid required fields' });

  let definition = defineIntervention(typeClass, id, label_drug, sctid_drug, label_sctid_drug, components_ids);

  let subsumptionList = auxFunct.isValidArgument(subsumed_ids) ? subsumed_ids.split(",").map(item => item.trim()) : [];
  let groupingCriteriaList = auxFunct.isValidArgument(grouping_criteria_ids) ? subsumed_ids.split(",").map(item => item.trim()) : [];

  const adminDefinition = defineCareAction(
    typeClass,
    id,
    label_action,
    sctid_action,
    label_sctid_action,
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

// Routes


// drug type
router.post("/drug/individual/add", async function (req, res) {
  try {
    const sparqlQuery = careActionDefinition(req, Types.DrugType);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding individual care action: ${err}`);
    res.status(500).send({ error: "Failed to add individual care action" });
  }
});

router.post("/nondrug/individual/add", async function (req, res) {
  try {
    const sparqlQuery = careActionDefinition(req, Types.NonDrugType);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding non-drug related care action: ${err}`);
    res
      .status(500)
      .send({ error: "Failed to add non-drug related  care action" });
  }
});

router.post("/drug/category/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.DrugCategory);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding drug category care action: ${err}`);
    res.status(500).send({ error: "Failed to add drug category care action" });
  }
});

router.post("/drug/combination/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.DrugCombinationType);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding drug combination care action: ${err}`);
    res
      .status(500)
      .send({ error: "Failed to add drug combination care action" });
  }
});

router.post("/vaccine/individual/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.VaccineType);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding care action: ${err}`);
    res.status(500).send({ error: "Failed to add care action" });
  }
});

router.post("/vaccine/category/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.VaccineCategory);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding care action: ${err}`);
    res.status(500).send({ error: "Failed to add care action" });
  }
});

////////////
/// DELETE
///////////

router.post("/drug/individual/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(Types.DrugType, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error deleting individual care action: ${err}`);
    res.status(500).send({ error: "Failed to delete individual care action" });
  }
});

router.post("/drug/category/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(Types.DrugCategory, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error deleting care action: ${err}`);
    res.status(500).send({ error: "Failed to delete care action" });
  }
});

router.post("/nondrug/individual/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(Types.NonDrugType, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error deleting non-drug-related care action: ${err}`);
    res
      .status(500)
      .send({ error: "Failed to delete non-drug-related care action" });
  }
});

router.post("/vaccine/individual/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(Types.VaccineType, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error deleting care action: ${err}`);
    res.status(500).send({ error: "Failed to delete care action" });
  }
});

router.post("/vaccine/category/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(Types.VaccineCategory, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error deleting care action: ${err}`);
    res.status(500).send({ error: "Failed to delete care action" });
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
    logger.error(`Error retrieving care action: ${err}`);
    res.status(500).send({ error: "Failed to retrieve care action" });
  }
});

module.exports = router;
