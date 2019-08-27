const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

//to be tested
router.post('/drugs/get', function(req, res, next) {

  utils.sparqlInstanceOf("careActions", "<http://anonymous.org/vocab/DrugType>", function(uris) {

    res.send(uris);

  });

});

//to be tested
router.post('/nonDrugs/get', function(req, res, next) {

  utils.sparqlInstanceOf("careActions", "<http://anonymous.org/vocab/NonDrugType>", function(uris) {

    res.send(uris);

  });

});

module.exports = router;
