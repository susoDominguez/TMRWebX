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

    var cigId = (req.body.cig_id.startsWith(`CIG-`)) ? req.body.cig_id : (`CIG-` + req.body.cig_id);

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
          console.info("data sent to grammar parser is \n" + data);
          parser.feed( data.toString());
          //console.info("\ninteractions data is:\n" + data);
          if(parser.results.length <= 1){  
            data = parser.results[0];
            //convert type of first recommendation to secondary when type of interaction is repairable
            for(let val of data){
              if(val.type === 'repairable'){
                val.interactionNorms[0].type = 'secondary';
              }
            }
          } else {
            console.info("parsed data has " + parser.results.length + " result(s):\n" + parser.results);
            data = null;
          }
          
        } catch (err) {
          console.log("Error at character " + err.offset);
          data = null; //this should not happen
        }

        if(data) {
          res.send(data);
        } else {
          res.sendStatus(500);
        }
        
      }

    });
  } else {
     //not found as it has not been provided with a CIG id
  res.sendStatus(404);
  }
});

router.post('/drug', function (req, res) {

  if (req.body.cig_id && req.body.rec_URI) {

    var cigId = (req.body.cig_id.startsWith(`CIG-`)) ? req.body.cig_id : (`CIG-` + req.body.cig_id);

    var postData = require('querystring').stringify({
      //Jena dataset name
      'guideline_id': cigId, 'recommendation_uri': req.body.rec_URI
    });

    logger.info("Determining care action part of recommendation with data: " + JSON.stringify(postData));

    //Call prolog server and convert response to JSON object
    utils.callPrologServer("drug", postData, function (data) {

      if (!data) {
        res.sendStatus(400);
      } else {
        console.info(data)
        res.send(data);
      }
    });
  } else {
     //not found as it has not been provided with a CIG id
  res.sendStatus(404);
  }
});

router.post('/drugeffects', function (req, res) {

  if (req.body.careAction_URI) {

    var postData = require('querystring').stringify({
      'drug_URI': req.body.careAction_URI,
    });

    logger.info("Determining effects of care action application with care action: " + JSON.stringify(postData));

    //Call prolog server and convert response to JSON object
    utils.callPrologServer("drugeffects", postData, function (data) {

      if (!data) {
        res.sendStatus(400);
      } else {
        console.info(data)
        res.send(data);
      }
    });
  } else {
  res.sendStatus(404);
  }
});

/**
 * get URIs of all Recommendations in a given CIG
 */
router.post('/rec/get', function (req, res) {

  if (req.body.cig_id) {

    var cigId = req.body.cig_id;

    cigId = (cigId.startsWith(`CIG-`)) ? cigId : (`CIG-` + cigId);

    console.info(`cigId is ` + cigId);

    utils.sparqlGetSubjectAllNamedGraphs(cigId, "vocab:ClinicalRecommendation", function (RecUris) {

      (RecUris) ? res.send(RecUris) : res.sendStatus(400);

    });
  } else {
    res.sendStatus(400);
  }

});

/**
 * add nanopub graphs from one existing CIG to another
 */
router.post('/add', function (req, res) {
  
  if (req.body.cig_from && req.body.cig_to) {

    if (!req.body.cig_from.startsWith(`CIG-`)) {
      req.body.cig_from = `CIG-` + req.body.cig_from;
    }

    if (!req.body.cig_to.startsWith(`CIG-`)) {
      req.body.cig_to = `CIG-` + req.body.cig_to;
    }
  
    var filterString = ``;
    if(req.body.subguidelines) {
      req.body.subguidelines.split(",").forEach(function (SubId) {
        filterString += (`?sg = data:` + SubId.trim() + ` || `);
      });
       //remove last operator and whitespace
    filterString = filterString.substring(0, filterString.length - 4);
    } else {
      filterString = (` ?sg = data:` + req.body.cig_from);
    }

    filterString = `FILTER(` + filterString + `)`;
    console.info(filterString)

    //select nanopub URIs from subguidelines
    utils.sparqlGetNamedNanopubFromSubguidelines(req.body.cig_from, filterString, function (assertionList) {

      var nanoHeadList = [];
      var nanoProbList = [];
      var nanoPubList = [];

      //for each assertion URI, add the rest of the related nano graphs
      for (var index in assertionList) {
        var uri = assertionList[index];
        //logger.info(uri);
        nanoHeadList.push(uri + `_head`);
        nanoProbList.push(uri + `_provenance`);
        nanoPubList.push(uri + `_publicationinfo`);
      }

      if (assertionList) {
        utils.addGraphsDataFromToCig(req.body.cig_from, req.body.cig_to, nanoHeadList, assertionList, nanoProbList, nanoPubList, function (status) {
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
