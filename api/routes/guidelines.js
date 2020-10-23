const express = require("express");
const router = express.Router();
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar.js");
//const request = require('request');
// Create a Parser object from our grammar.
const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
const util = require("util");
const utils = require("../lib/utils");
const logger = require("../config/winston");
const { ErrorHandler } = require('../lib/errorHandler.js');

/*
//handler of async for errors when used with express
const asyncHandler = fn => (req, res, next) =>
  Promise
    .resolve(fn(req, res, next)) 
    .catch(next)


const middleware1 =  (req, res, next) => {

  //set header for json response
  //    res.setHeader("Content-Type", "application/json");

  if (!req.body.cig_id) {
    //pass error to Express
     return next(new ErrorHandler(404, `Missing required value for parameter 'cig_id'`));
  }
    //done once for all middleware
    req.body.cig_id = req.body.cig_id.startsWith(`CIG-`)
      ? req.body.cig_id
      : `CIG-` + req.body.cig_id;

    let cigId = req.body.cig_id;
    let postData = require("querystring").stringify({
      //Jena dataset name
      guideline_id: cigId,
    });

    logger.info(
      "Determining interactions with data: " + JSON.stringify(postData)
    );

    //try {
      //promisify Prolog call
      //const swiplCall = util.promisify(utils.callPrologServer);
    //Call prolog server and convert response to JSON object
    utils.callPrologServer("interactions", postData, (err, interactions) => {
  
          logger.info("data sent to grammar parser is \n" + interactions);

          if(err) return next(new ErrorHandler(500, `callPrologServer failed in route cig_analytics`));
          
     try {
          parser.feed(interactions);
          
          if (parser.results.length <= 1) {
            let result = parser.results[0];

            //convert type of first recommendation to secondary when type of interaction is repairable
            for (let val of result) {
              if (val.type === "repairable") {
                val.interactionNorms[0].type = "secondary";
              }
            }
            //add result to response before sending to middleware
            res.locals.interactions = result;
            
          } else {
            logger.error(
              "parsed data has " +
                parser.results.length +
                " result(s):\n" +
                parser.results
            );
            return next(new ErrorHandler(500, "route cig_analytics: parsed data has " +
            parser.results.length +
            " result(s):\n" +
            parser.results));
          }
        } catch (err) {
          //logger.error("Error at character " + err.offset);
           return next(new ErrorHandler(500, "route cig_analytics: parser error at character " + err.offset));
        }
        //next middleware
        next();
    }//end of callback
    );
};

const middleware2 =  (req, res, next) => {

  let  cigId = req.body.cig_id;

    utils.sparqlGetSubjectAllNamedGraphs(
      cigId,
      "vocab:ClinicalRecommendation",
      function (err, recUriList) {

        if(err) return next(new ErrorHandler(500, `route cig_analytics: function sparqlGetSubjectAllNamedGraphs has failed with error: ${err}`));
        //add list of Rec URIs to locals
        res.locals.recList = recUriList;
        //next middleware
        next();
      }
    );  
};

const middleware3 = async (req, res, next) => {

  
}

router.post("/cig_analytics", middleware1, middleware2, asyncHandler(middleware3), (req, res) => {
  res.status(200).json(res.locals.recList);
});
*/

router.post("/interactions", function (req, res) {

  if (!req.body.cig_id) {
    res.sendStatus(406).send({error: 'cig_id param missing'});
    return;
  }
    let cigId = req.body.cig_id.startsWith(`CIG-`)
      ? req.body.cig_id
      : `CIG-` + req.body.cig_id;

    let postData = require("querystring").stringify({
      //Jena dataset name
      guideline_id: cigId,
    });

    logger.info(
      "Determining interactions with data: " + JSON.stringify(postData)
    );

    //Call prolog server and convert response to JSON object
    utils.callPrologServer("interactions", postData, function (err, data) {
      if (err) {
        res.sendStatus(400);
        return;
      }
        //use grammar to parse response into a JSON object
        try {
          console.info("data sent to grammar parser is \n" + data);
          parser.feed(data.toString());
          //console.info("\ninteractions data is:\n" + data);
          if (parser.results.length <= 1) {
            data = parser.results[0];
            //convert type of first recommendation to secondary when type of interaction is repairable
            for (let val of data) {
              if (val.type === "repairable") {
                val.interactionNorms[0].type = "secondary";
              }
            }
          } else {
            console.info(
              "parsed data has " +
                parser.results.length +
                " result(s):\n" +
                parser.results
            );
            data = null;
          }
        } catch (err) {
          console.logger("Error at character " + err.offset);
          data = null; //this should not happen
        }

        if (data) {
          res.send(data);
        } else {
          res.sendStatus(500);
        }
    });
});

router.post("/drug", function (req, res) {
  if (!req.body.cig_id || !req.body.rec_URI) {
     res.sendStatus(406);
     return;
  }
    var cigId = req.body.cig_id.startsWith(`CIG-`)
      ? req.body.cig_id
      : `CIG-` + req.body.cig_id;

    var postData = require("querystring").stringify({
      //Jena dataset name
      guideline_id: cigId,
      recommendation_uri: req.body.rec_URI,
    });

    logger.info(
      "Determining care action part of recommendation with data: " +
        JSON.stringify(postData)
    );

    //Call prolog server and convert response to JSON object
    utils.callPrologServer("drug", postData, function (err,data) {
      if (err) {
        res.sendStatus(400);
      } else {
        //console.info(data);
        res.send(data);
      }
    });
});

router.post("/drugeffects", function (req, res) {
  if (req.body.careAction_URI) {
    var postData = require("querystring").stringify({
      drug_URI: req.body.careAction_URI,
    });

    logger.info(
      "Determining effects of care action application with care action: " +
        JSON.stringify(postData)
    );

    //Call prolog server and convert response to JSON object
    utils.callPrologServer("drugeffects", postData, function (err,data) {
      if (err) {
        res.sendStatus(400);
      } else {
        //console.info(data);
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
router.post("/rec/get", function (req, res) {
  if (req.body.cig_id) {
    var cigId = req.body.cig_id;

    cigId = cigId.startsWith(`CIG-`) ? cigId : `CIG-` + cigId;

    utils.sparqlGetSubjectAllNamedGraphs(
      cigId,
      "vocab:ClinicalRecommendation",
      function (err, RecUris) {
        err ?  res.sendStatus(400) : res.send(RecUris);
      }
    );
  } else {
    res.sendStatus(400);
  }
});

/**
 * add nanopub graphs from one existing CIG to another
 */
router.post("/add", function (req, res) {

  if (!req.body.cig_from || !req.body.cig_to) {
    res.sendStatus(406);
    return;
  }
    if (!req.body.cig_from.startsWith(`CIG-`)) {
      req.body.cig_from = `CIG-` + req.body.cig_from;
    }

    if (!req.body.cig_to.startsWith(`CIG-`)) {
      req.body.cig_to = `CIG-` + req.body.cig_to;
    }

    var filterString = ``;
    if (req.body.subguidelines) {
      req.body.subguidelines.split(",").forEach(function (SubId) {
        filterString += `?sg = data:` + SubId.trim() + ` || `;
      });
      //remove last operator and whitespace
      filterString = filterString.substring(0, filterString.length - 4);
    } else {
      filterString = ` ?sg = data:` + req.body.cig_from;
    }

    filterString = `FILTER(` + filterString + `)`;
    //console.info(filterString)

    //select nanopub URIs from subguidelines
    utils.sparqlGetNamedNanopubFromSubguidelines(
      req.body.cig_from,
      filterString,
      function (err, assertionList) {

         if (err){
           res.sendStatus(400);
           return;
         }

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
          utils.addGraphsDataFromToCig(
            req.body.cig_from,
            req.body.cig_to,
            nanoHeadList,
            assertionList,
            nanoProbList,
            nanoPubList,
            function (err, status) {
              res.sendStatus(status);
            }
          );
        } 
      }
    );
});

module.exports = router;
