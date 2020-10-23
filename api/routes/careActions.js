const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

//get all drug types URIS
router.post('/drugs/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugType", function(err, list) {

    if(err) {
      res.sendStatus(404).send(list);
      return;
    }
    res.sendStatus(200).send(list);
  });

});

//Get all non drug types URIs
router.post('/nonDrugs/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:NonDrugType", function(err, list) {

    if(err) {
      res.sendStatus(404).send(list);
      return;
    }
    res.sendStatus(200).send(list);
  });

});

//Get all vaccine types URIs
router.post('/vaccines/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType", function(err, list) {
    if(err) {
      res.sendStatus(404).send(list);
      return;
    }
    res.sendStatus(200).send(list);
  });

});

module.exports = router;
