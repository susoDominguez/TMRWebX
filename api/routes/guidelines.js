const express = require('express');
const router = express.Router();
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar.js");
//const request = require('request');
// Create a Parser object from our grammar.
const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
const utils = require('../lib/utils');
const logger = require('../config/winston');

router.post('/interactions', function (req, res, next) {

  //guideline is now not strict to prefix CIG-
  const cigId = (req.body.guideline_id) ? (`CIG-` + req.body.guideline_id) : req.body.dataset_id;

  var postData = require('querystring').stringify({
    //Jena dataset name
    'guideline_id': cigId,
  });

  logger.info("Determining interactions with data: " + JSON.stringify(postData));

  utils.callPrologServer("interactions", postData, res, function (data) {

    if (!data) {
      res.sendStatus(400);
    } else {
      //use grammar to parse response into a JSON object
      try {
        parser.feed(data);
      } catch (err) {
        console.log("Error at character " + parseError.offset);
      }
      parser.results[0];
      res.send(JSON.parse(data));
    }

  });

});

router.post('/rec/get', function (req, res, next) {

  const guideline_id = (req.body.guideline_id) ? ("CIG-" + req.body.guideline_id) : req.body.dataset_id;

  utils.sparqlGetSubjectAllNamedGraphs(guideline_id, "vocab:ClinicalRecommendation", function (uris) {

    res.send(uris);

  });

});

module.exports = router;
