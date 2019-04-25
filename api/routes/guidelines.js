const express = require('express');
const router = express.Router();
const request = require('request');

const utils = require('../lib/utils').default;
const logger = require('../config/winston');

router.post('/interactions', function(req, res, next) {

  var postData = require('querystring').stringify({
      'guideline_id' : `CIG-` + req.body.guideline_id,
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

router.post('/get', function(req, res, next) {

  utils.sparqlGraphInstanceOf("CIG-" + req.body.guideline_id, "<http://anonymous.org/vocab/ClinicalRecommendation>", function(uris) {

    res.send(uris);

  });

});

module.exports = router;
