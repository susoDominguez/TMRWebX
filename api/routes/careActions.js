const express = require('express');
const router = express.Router();
//const request = require('request');
const logger = require('../config/winston');

//const config = require('../lib/config');
//const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

router.post('/drugs/individual/get', async function(req, res, next) {
  try{
    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType"),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugType")
    ]);
  
    let arrs= new Array();
  
    for (const {data, status}  of arr_res) {
      
      let aList = Array.isArray(data)? data : new Array(data);
      logger.debug(aList)
      arrs.push(aList);
    }
    //flatten then convert to set and back to array to remove duplicates
    let anArr = [...new Set(arrs.flat(1))];
    return res.json(anArr);
  } catch(err){
    return res.status(500).json(err);
  }
});

//get all drug types URIS
router.post('/drugs/get', async function(req, res, next) {
  try{
    let arr_res =  await Promise.all([
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugCategory"),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType"),
      utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugType")
    ]);
  
    let arrs= new Array();
  
    for (const {data, status}  of arr_res) {
      
      let aList = Array.isArray(data)? data : new Array(data);
      logger.debug(aList)
      arrs.push(aList);
    }
    //flatten then convert to set and back to array to remove duplicates
    let anArr = [...new Set(arrs.flat(1))];
    return res.json(anArr);
  } catch(err){
    return res.status(500).json(err);
  }
});

//Get all non drug types URIs
router.post('/nonDrugs/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:NonDrugType").catch(err => logger.error('Error: router careactions nonDrugs/get '+err));
  let aList = Array.isArray(data)? data : new Array(data);
  return res.status(status).send(aList);
});

//Get all vaccine types URIs
router.post('/vaccines/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType").catch(err => logger.error('Error: router careactions vaccines/get '+err));
  let aList = Array.isArray(data)? data : new Array(data);
  return res.status(status).send(aList);
});

//Get all drug categories types URIs
router.post('/drugCategories/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugCategory").catch(err => logger.error('Error: router careactions combinations/get '+err));
  let aList = Array.isArray(data)? data : new Array(data);
  return res.status(status).json(aList);
});

//Get all drug and non drug combinations types URIs
router.post('/combinations/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:CombinedCareType").catch(err => logger.error('Error: router careactions combinations/get '+err));
  let aList = Array.isArray(data)? data : new Array(data);
  return res.status(status).send(aList);
});


//Get all drug and non drug combinations types URIs
router.post('/get', async function(req, res, next) {
  try{
  let arr_res =  await Promise.all([
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:CombinedCareType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugCategory"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:NonDrugType"),
    utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugType")
  ]);

  let arrs= new Array();

  for (const {data, status}  of arr_res) {
    
    let aList = Array.isArray(data)? data : new Array(data);
    logger.debug(aList)
    arrs.push(aList);
  }
  //flatten then convert to set and back to array to remove duplicates
  let anArr = [...new Set(arrs.flat(1))];
  return res.json(anArr);
} catch(err){
  return res.status(500).json(err);
}
});


module.exports = router;
