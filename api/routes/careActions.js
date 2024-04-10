const express = require('express');
const router = express.Router();
//const request = require('request');
const logger = require('../config/winston');

//const config = require('../lib/config');
//const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

//get all drug types URIS
router.post('/drugs/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:DrugType").catch(err => logger.error('Error: drugs/get '+err));
  return res.status(status).send(data);
});

//Get all non drug types URIs
router.post('/nonDrugs/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:NonDrugType").catch(err => logger.error('Error: drugs/get '+err));
  return res.status(status).send(data);
});

//Get all vaccine types URIs
router.post('/vaccines/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:VaccineType").catch(err => logger.error('Error: drugs/get '+err));
  return res.status(status).send(data);
});

//Get all drug categories types URIs
router.post('/drugCategories/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:DrugCategory").catch(err => logger.error('Error: drugs/get '+err));
  return res.status(status).send(data);
});

//Get all drug and non drug combinations types URIs
router.post('/combinations/get', async function(req, res, next) {
  let {status = 500, data = [] } =  await utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:CompoundType").catch(err => logger.error('Error: drugs/get '+err));
  return res.status(status).send(data);
});

//TODO: review
//Get all drug and non drug combinations types URIs
router.post('/get', async function(req, res, next) {

  let results = await Promise.all(utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:DrugAdministrationType"), utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:NonDrugAdministrationType")).catch(err => logger.error('Error: careActions/get '+err));
  let status1 = results[0].status ?? 500;
  let status2 = results[1].status ?? 500;
   status2 = status2 < status1 ? status2 : status1;
  let data1 = results[0].data ?? [];
  let data2 = results[1].data ?? [];
  return res.status(status2).send(data1 + data2);
});


module.exports = router;
