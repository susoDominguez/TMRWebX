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
  DrugAdminT : "DrugAdministrationType",
  combinedAdminOf: "combinedAdministrationOf",
  vaccWith: "vaccinationWith",
  adminOf: "administrationOf"
};

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
function defineDrug(type, id, text) {
  const typeClass =
    type === Types.DrugCat
      ? Types.DrugCategory
      : type === Types.DrugCombT
      ? Types.DrugCombinationType
      : Types.DrugType;
    
  return  `data:${type}${id} a vocab:${typeClass}, owl:NamedIndividual ; 
            vocab:label "${text}"@en  `  ;
}

function defineVaccine(type, id, text) {
  const typeClass =
    type === Types.VacCat ? Types.VaccineCategory : Types.VaccineType;
  return `data:${type}${id} a vocab:${typeClass}, owl:NamedIndividual ;
                            vocab:label "${text}"@en `
}

function defineNonDrug(type, id, text) {
    return `data:${type}${id} a vocab:${Types.NonDrugType}, owl:NamedIndividual ; 
                             vocab:label "${text}"@en  `
  } ;

function defineAdminAction(type, id, action_text, action_sctid, action_label) {
  const typeClass =
    type === Types.VacT || type === Types.VacCat
      ? Types.VaccineType
      : type === Types.DrugCombT
      ? Types.DrugAdminT
      : Types.DrugAdminT;

  const relationship =
    type === Types.DrugCombT
      ? Types.combinedAdminOf
      : type === Types.VacT || type === Types.VacCat
      ? Types.vaccWith
      : Types.adminOf;

      let data = `data:ActAdminister${id} a vocab:${typeClass}, owl:NamedIndividual ;
                  vocab:label "${action_text}"@en ;
                  vocab:${relationship} data:${type}${id}  `
      
      if(action_sctid) {
        data += ` ; vocab:sctid  "${action_sctid}"^^xsd:string  `
      }

      if(action_sctid && action_label){
        data+= ` ;  rdfs:label  "${action_label}"@en `
      } else {
        data+= ` ; rdfs:label  "${action_text}"@en `
      }

      data+= ` .`
      
  return   data ;
  }

// Define Care Actions
function careActionDefinition(req, type) {
  const { 
    id, 
    text = id,
    action_text = id,
    label = undefined, 
    action_label = undefined,
    sctid,
    action_sctid
  } = req.body;
  
  let definition ;

  if (type === Types.VacT || type === Types.VacCat) {
    definition = defineVaccine(type, id, text, sctid, label);
  } else if (type === Types.NonDrugT) {
    definition = defineNonDrug(type, id, text, sctid, label);
  } else {
    definition = defineDrug(type, id, text, sctid, label);
  }

  //add SCT code
  if(sctid) {
    definition += ` ; vocab:sctid  "${sctid}"^^xsd:string  `
  }

  if(sctid && label){
    definition+= `  ; rdfs:label  "${action_label}"@en `
  } else {
    definition+= ` ; rdfs:label  "${action_text}"@en `
  }

  const adminDefinition = defineAdminAction(type, id, action_text, action_sctid, action_label);

  return `${definition} . \n${adminDefinition}`;
}

// DELETE Filters
function deleteFilter(type, id) {
  return `FILTER (?s = data:${type}${id} || ?s = data:ActAdminister${id})`;
}

// Routes
// drug type
router.post("/drug/individual/add", async function (req, res) {
  try {
    const sparqlQuery = careActionDefinition(req, Types.DrugT);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding individual care action: ${err}`);
    res.status(500).send({ error: "Failed to add individual care action" });
  }
});

router.post("/nondrug/individual/add", async function (req, res) {
  try {
    const sparqlQuery = careActionDefinition(req, Types.NonDrugT);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding non-drug related care action: ${err}`);
    res.status(500).send({ error: "Failed to add non-drug related  care action" });
  }
});


router.post("/drug/category/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.DrugCat);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding drug category care action: ${err}`);
    res.status(500).send({ error: "Failed to add drug category care action" });
  }
});

router.post("/drug/combination/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.DrugCombT);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding drug combination care action: ${err}`);
    res.status(500).send({ error: "Failed to add drug combination care action" });
  }
});

router.post("/vaccine/individual/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.VacT);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding care action: ${err}`);
    res.status(500).send({ error: "Failed to add care action" });
  }
});

router.post("/vaccine/category/add", async (req, res) => {
  try {
    const sparqlQuery = careActionDefinition(req, Types.VacCat);
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
    const filter = deleteFilter(Types.DrugT, id);
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
    const filter = deleteFilter(Types.DrugCat, id);
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
    const filter = deleteFilter(Types.NonDrugT, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error deleting non-drug-related care action: ${err}`);
    res.status(500).send({ error: "Failed to delete non-drug-related care action" });
  }
});

router.post("/vaccine/individual/delete", async (req, res) => {
  try {
    const { id } = req.body;
    const filter = deleteFilter(Types.VacT, id);
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
    const filter = deleteFilter(Types.VacCat, id);
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

/*

curl --location 'http://localhost:8888/tmrweb/careAction/drug/individual/add' \
--header 'Content-Type: application/x-www-form-urlencoded' \
--data-urlencode 'id=Simvastatin40mg' \
--data-urlencode 'label=label' \
--data-urlencode 'text=text' \
--data-urlencode 'action_label=administer product containing Simvastatin 40mg tablets' \
--data-urlencode 'action_text=action text' \
--data-urlencode 'subsumed_ids=drug_A, drug_B' \
--data-urlencode 'sctid=42382311000001100' \
--data-urlencode 'action_sctid=423823'

*/