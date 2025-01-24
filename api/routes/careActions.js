const express = require("express");
const router = express.Router();
const auxFuncts = require("../lib/router_functs/guideline_functs");
const logger = require("../config/winston");
const utils = require("../lib/utils");

const CARE_ACTION_TYPES = {
  DrugCombinationType: "vocab:DrugCombinationType",
  DrugCategory: "vocab:DrugCategory",
  DrugType: "vocab:DrugType",
  VaccineCategory: "vocab:VaccineCategory",
  VaccineType: "vocab:VaccineType",
  NonDrugType: "vocab:NonDrugType",
  NonDrugAdministrationType: "vocab:NonDrugAdministrationType",
  DrugAdministrationType: "vocab:DrugAdministrationType",
  VaccinationType: "vocab:VaccinationType",
  DrugCombinationAdministrationType: "vocab:DrugAdministrationType",
};

/**
 * Generic function to handle SPARQL queries and responses.
 * @param {Array<string>} careActionTypes - List of care action types to query.
 * @param {Response} res - Express response object.
 */
async function handleSparqlQuery(careActionTypes, res) {
  try {
    const sparqlResults = await Promise.all(
      careActionTypes.map((type) =>
        utils.sparqlGetSubjectDefaultGraph("careActions", type)
      )
    );
    const parsedResults = await auxFuncts.get_sparqlquery_arr(sparqlResults);
    res.status(200).json(parsedResults);
  } catch (error) {
    logger.error(`Error during SPARQL query: ${JSON.stringify(error)}`);
    res.status(500).json([]);
  }
}

/**
 * Get all individual drug types and vaccines.
 */
router.post("/drugs/individual/get", (req, res) => {
  handleSparqlQuery(
    [CARE_ACTION_TYPES.VaccineType, CARE_ACTION_TYPES.DrugType],
    res
  );
});

/**
 * Get all drug-related care actions including combinations, categories, and types.
 */
router.post("/drugs/get", (req, res) => {
  handleSparqlQuery(
    [
      CARE_ACTION_TYPES.DrugCombinationType,
      CARE_ACTION_TYPES.DrugCategory,
      CARE_ACTION_TYPES.DrugType,
      CARE_ACTION_TYPES.VaccineCategory,
      CARE_ACTION_TYPES.VaccineType,
    ],
    res
  );
});

/**
 * Get all non-drug care actions.
 */
router.post("/nondrugs/get", (req, res) => {
  handleSparqlQuery([CARE_ACTION_TYPES.NonDrugType], res);
});

/**
 * Get all vaccine-related care actions.
 */
router.post("/drugs/vaccines/get", (req, res) => {
  handleSparqlQuery([CARE_ACTION_TYPES.VaccineType], res);
});

/**
 * Get all drug categories.
 */
router.post("/drugs/category/get", (req, res) => {
  handleSparqlQuery([CARE_ACTION_TYPES.DrugCategory], res);
});

/**
 * Get all vaccine categories.
 */
router.post("/drugs/vaccines/category/get", (req, res) => {
  handleSparqlQuery([CARE_ACTION_TYPES.VaccineCategory], res);
});

/**
 * Get all drug combinations.
 */
router.post("/drugs/combination/get", (req, res) => {
  handleSparqlQuery([CARE_ACTION_TYPES.DrugCombinationType], res);
});

/**
 * Get all care actions, including drugs, vaccines, and non-drug administrations.
 */
router.post("/get", (req, res) => {
  handleSparqlQuery(
    [
      CARE_ACTION_TYPES.NonDrugAdministrationType,
      CARE_ACTION_TYPES.DrugAdministrationType,
      CARE_ACTION_TYPES.VaccinationType,
      CARE_ACTION_TYPES.DrugCombinationAdministrationType,
    ],
    res
  );
});

module.exports = router;
