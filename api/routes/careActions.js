const express = require('express');
const router = express.Router();
const request = require('request');
const logger = require('../config/winston');

//const config = require('../lib/config');
//const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

//get all drug types URIS
router.post('/drugs/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:DrugType", function(err, list) {

    if(err) {
      res.status(404).send(list);
      return;
    }
    res.status(200).send(list);
  });

});

//Get all non drug types URIs
router.post('/nonDrugs/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:NonDrugType", function(err, list) {

    if(err) {
      res.status(404).send(list);
      return;
    } else {
      res.status(200).send(list);
      return;
    }
  });

});

//Get all vaccine types URIs
router.post('/vaccines/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "tmr:VaccineType", function(err, list) {
    if(err) {
      res.status(404).send(list);
      return;
    } else {
      res.status(200).send(list);
    }
  });

});

module.exports = router;
