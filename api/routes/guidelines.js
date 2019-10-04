const express = require('express');
const router = express.Router();
//const request = require('request');

const utils = require('../lib/utils');
const logger = require('../config/winston');

router.post('/interactions', function(req, res, next) {

  //guideline is now not strict to prefix CIG-
  const guidelineId = (req.body.guideline_id)? (`CIG-` + req.body.guideline_id) : req.body.dataset_id; 
  var postData = require('querystring').stringify({
      'guideline_id' : guidelineId,
  });

  logger.info("Determining interactions with data: " + JSON.stringify(postData));

  utils.callPrologServer("interactions", postData, res, function(data) {

    if (!data) {
      res.sendStatus(400);
    } else {
      res.send(data);
    }

  });

});

router.post('/rec/get', function(req, res, next) {

  const guideline_id = (req.body.guideline_id) ? ("CIG-" + req.body.guideline_id) : req.body.dataset_id;

  utils.sparqlGetSubjectAllNamedGraphs( guideline_id, "vocab:ClinicalRecommendation", function(uris) {

    res.send(uris);

  });

});

module.exports = router;
