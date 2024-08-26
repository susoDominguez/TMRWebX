const express = require('express');
const router = express.Router();
const auxFuncts = require('../lib/router_functs/guideline_functs');
const logger = require('../config/winston');

//const config = require('../lib/config');
//const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

const DrugT = 'DrugT';
const DrugType = 'DrugType';
const NonDrugT = 'NonDrugT';
const NonDrugType = 'NonDrugType';
const DrugCat = 'DrugCat';
const DrugCategory = 'DrugCategory';
const CombDrugT = 'CombDrugT';
const DrugCombinationType = 'DrugCombinationType';
const VacT = 'VacT';
const VaccineType = 'VaccineType';
const VacCat = 'VacCat';
const VaccineCategory = 'VaccineType';
const VaccinationType = 'VaccinationType';
const DrugCombinationAdministrationType = 'DrugCombinationAdministrationType';
const DrugAdministrationType = 'DrugAdministrationType';
const combinedAdministrationOf = 'combinedAdministrationOf';
const administrationOf = 'administrationOf';
const vaccinationWith = 'vaccinationWith';


router.post('/medications/individual/get', async function(req, res, next) {
  try {

    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" + VaccineType),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" + DrugType)
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all individual drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//get all drug types URIS
router.post('/medications/get', async function(req, res, next) {
  try {
    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" + DrugCombinationType),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" + DrugCategory),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" + DrugType),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" + VaccineCategory),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" + VaccineType)
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//Get all non drug types URIs
router.post('/nondrugs/get', async function(req, res, next) {
  try {
    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:" +  NonDrugType)
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all non drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//Get all vaccine types URIs
router.post('/vaccines/get', async function(req, res, next) {
  try {
    let arr_res =  await Promise.all([
     utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType")
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all vaccine drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//Get all drug categories types URIs
router.post('/drugs/category/get', async function(req, res, next) {
  try {
    let arr_res =  await Promise.all([
     utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugCategory")
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all category drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//Get all drug categories types URIs
router.post('/vaccines/category/get', async function(req, res, next) {
  try {
    let arr_res =  await Promise.all([
     utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineCategory")
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all category vaccine types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//Get all drug and non drug combinations types URIs
router.post('/drugs/combination/get', async function(req, res, next) {
  try {

    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugCombinationType")
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all combination drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});


//Get all care actions URIs
router.post('/get', async function(req, res, next) {
  try {

  let arr_res =  await Promise.all([
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:NonDrugAdministrationType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugAdministrationType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccinationType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugCombinationAdministrationType")
  ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));

  return res.status(200).json(arr_res);
   
} catch(err){
  logger.error(`Error when retrieving list of all care actions: ${JSON.stringify(err)}`);
  return res.status(500).json([]);
}
});


module.exports = router;
