const express = require('express');
const router = express.Router();
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar.js");
//const request = require('request');
// Create a Parser object from our grammar.
const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
const utils = require('../lib/utils');
const logger = require('../config/winston');

router.post('/interactions', function (req, res) {

  if (req.body.cig_id) {

    var cigId = JSON.stringify(req.body.cig_id);
    cigId = (cigId.startsWith(`CIG-`)) ? cigId : (`CIG-` + cigId);

    var postData = require('querystring').stringify({
      //Jena dataset name
      'guideline_id': cigId,
    });

    logger.info("Determining interactions with data: " + JSON.stringify(postData));

    //Call prolog server and convert response to JSON object
    utils.callPrologServer("interactions", postData, function (data) {

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
  }
  //not found as it has not been provided with a CIG id
  res.sendStatus(404);
});

/**
 * get URIs of all Recommendations in a given CIG
 */
router.post('/rec/get', function (req, res) {

  if (req.body.cig_id) {

    var cigId = JSON.stringify(req.body.cig_id);
    cigId = (cigId.startsWith(`CIG-`)) ? cigId : (`CIG-` + cigId);

    utils.sparqlGetSubjectAllNamedGraphs(cigId, "vocab:ClinicalRecommendation", function (RecUris) {

      (recUris) ? res.send(RecUris) : res.sendStatus(400);

    });
  } else {
    res.sendStatus(400);
  }

});

/**
 * add nanopub graphs from one existing CIG to another
 */
router.post('/add', function (req, res) {

  if(req.body.cig_from && req.body.cig_to){ //&& req.body.subguidelines){

    if(!req.body.cig_from.startsWith(`CIG-`)){
      req.body.cig_from = `CIG-` + req.body.cig_from;
    }
    if(!req.body.cig_to.startsWith(`CIG-`)) {
      req.body.cig_to = `CIG-` + req.body.cig_to;
    }

    var filterString = ``;
		if(req.body.subguidelines) {
			req.body.subguidelines.split(",").forEach(function (SubId) {

				filterString += (`?sg = data:` + SubId.trim() + ` || `);
		});
			
			//remove last operator and whitespace
			filterString = filterString.substring(0, filterString.length - 4);
			filterString = `FILTER(`+ filterString +`)`;
    }
    
    //select nanopub URIs from subguidelines
    utils.sparqlGetNamedNanopubFromSubguidelines(req.body.cig_from,filterString, function(assertionList){
      
    var nanoHeadList = [];
    var nanoProbList = [];
    var nanoPubList = [];

    //for each assertion URI, add the rest of the related nano graphs
		for (var index in assertionList) {
      var uri = assertionList[index];
      logger.info(uri);
      nanoHeadList.push(uri + `_head`);
      nanoProbList.push(uri + `_provenance`);
      nanoPubList.push(uri + `_publicationinfo`);
    }

      if(assertionList){
        utils.addGraphsDataFromToCig(req.body.cig_from, req.body.cig_to, nanoHeadList, assertionList, nanoProbList, nanoPubList, function(status){
          res.sendStatus(status);
        });
      } else {
        res.sendStatus(400);
      }
    });
    } else {
    res.sendStatus(400);
  }
});

module.exports = router;
