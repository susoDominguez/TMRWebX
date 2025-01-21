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
  CombDrugT: "CombDrugT",
  DrugCombinationType: "DrugCombinationType",
  VacT: "VacT",
  VaccineType: "VaccineType",
  VacCat: "VacCat",
  VaccineCategory: "VaccineCategory",
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
function defineDrug(type, id, label) {
  const typeClass =
    type === Types.DrugCat
      ? Types.DrugCategory
      : type === Types.CombDrugT
      ? Types.DrugCombinationType
      : Types.DrugType;
  return `data:${type}${id} a vocab:${typeClass}, owl:NamedIndividual ;
          rdfs:label "${label}"@en .`;
}

function defineVaccine(type, id, label) {
  const typeClass =
    type === Types.VacCat ? Types.VaccineCategory : Types.VaccineType;
  return `data:${type}${id} a vocab:${typeClass}, owl:NamedIndividual ;
          rdfs:label "${label}"@en .`;
}

function defineNonDrug(type, id, label) {
  return `data:${type}${id} a vocab:${Types.NonDrugType}, owl:NamedIndividual ;
          rdfs:label "${label}"@en .`;
}

function defineAdminAction(type, id, label) {
  const typeClass =
    type === Types.VacT || type === Types.VacCat
      ? Types.VaccineType
      : type === Types.CombDrugT
      ? "DrugAdministrationType"
      : "DrugAdministrationType";
  const relationship =
    type === Types.CombDrugT
      ? "combinedAdministrationOf"
      : type === Types.VacT || type === Types.VacCat
      ? "vaccinationWith"
      : "administrationOf";
  return `data:ActAdminister${id} a vocab:${typeClass}, owl:NamedIndividual ;
          rdfs:label "administer ${label}"@en ;
          vocab:${relationship} data:${type}${id} .`;
}

// Insert Clinical Codes
// Helper function for appending a single code to definitions
function appendCodes(req) {
  let codes = "";
  const codeFields = ["sctid"];
  const codeLblFields = ["sctid_label"];

  codeFields.forEach((field) => {
    if (req.body[field]) {
      codes += `vocab:${field} "${req.body[field].trim()}"^^xsd:string ;\n`;
    }
  });

  codes.replace(/;\n$/, ""); // Remove the trailing semicolon and newline

  codeLblFields.forEach((field) => {
    if (req.body[field]) {
      codes += `vocab:${field} "${req.body[field].trim()}"^^xsd:string ;\n`;
    }
  });

  return codes.replace(/;\n$/, ""); // Remove the trailing semicolon and newline
}

// Define Care Actions
function careActionDefinition(req, type) {
  const { id, label = id, action_label = label } = req.body;
  let definition;

  if (type === Types.VacT || type === Types.VacCat) {
    definition = defineVaccine(type, id, label);
  } else if (type === Types.NonDrugT) {
    definition = defineNonDrug(type, id, label);
  } else {
    definition = defineDrug(type, id, label);
  }

  const adminDefinition = defineAdminAction(type, id, action_label);
  const clinicalCodes = appendCodes(req);

  return `${definition}\n${adminDefinition}${
    clinicalCodes ? `\n${clinicalCodes}` : ""
  }`;
}

// DELETE Filters
function deleteFilter(type, id) {
  return `FILTER (?s = data:${type}${id} || ?s = data:ActAdminister${id})`;
}

// Routes
router.post("/add", async (req, res) => {
  try {
    const { type } = req.body;
    const sparqlQuery = careActionDefinition(req, type);
    const { status, data } = await postCareAction(sparqlQuery, config.INSERT);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error adding care action: ${err}`);
    res.status(500).send({ error: "Failed to add care action" });
  }
});

router.post("/delete", async (req, res) => {
  try {
    const { type, id } = req.body;
    const filter = deleteFilter(type, id);
    const { status, data } = await postCareAction(filter, config.DELETE);
    res.status(status).send(data);
  } catch (err) {
    logger.error(`Error deleting care action: ${err}`);
    res.status(500).send({ error: "Failed to delete care action" });
  }
});

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
