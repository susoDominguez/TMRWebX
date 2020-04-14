const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

//get all drug types URIS
router.post('/drugs/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:DrugType", function(uris) {

    res.send(uris);

  });

});

//Get all non drug types URIs
router.post('/nonDrugs/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:NonDrugType", function(uris) {

    res.send(uris);

  });

});

//Get all vaccine types URIs
router.post('/vaccines/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("careActions", "vocab:VaccineType", function(uris) {

    res.send(uris);

  });

});

module.exports = router;
