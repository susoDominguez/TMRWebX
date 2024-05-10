const express = require('express');
const router = express.Router();
const auxFuncts = require('../lib/router_functs/guideline_functs');
const logger = require('../config/winston');

//const config = require('../lib/config');
//const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

router.post('/drugs/individual/get', async function(req, res, next) {
  try {

    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType"),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugType")
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all individual drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//get all drug types URIS
router.post('/drugs/get', async function(req, res, next) {
  try {
    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:CombinedCareType"),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugCategory"),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType"),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugType")
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});

//Get all non drug types URIs
router.post('/nonDrugs/get', async function(req, res, next) {
  try {
    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:NonDrugType")
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
router.post('/drugCategories/get', async function(req, res, next) {
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

//Get all drug and non drug combinations types URIs
router.post('/combinations/get', async function(req, res, next) {
  try {

    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:CombinedCareType")
    ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));
  
    return res.status(200).json(arr_res);
     
  } catch(err){
    logger.error(`Error when retrieving list of all combination drug types: ${JSON.stringify(err)}`);
    return res.status(500).json([]);
  }
});


//Get all drug and non drug combinations types URIs
router.post('/get', async function(req, res, next) {
  try {

  let arr_res =  await Promise.all([
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:NonDrugAdministrationType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugAdministrationType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccinationType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:CombinedCareActionType")
  ]).then( (arr_resp) => auxFuncts.get_sparqlquery_arr(arr_resp));

  return res.status(200).json(arr_res);
   
} catch(err){
  logger.error(`Error when retrieving list of all care actions: ${JSON.stringify(err)}`);
  return res.status(500).json([]);
}
});


module.exports = router;
