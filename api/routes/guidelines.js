const express = require("express");
const router = express.Router();
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar");
const { reportError } = require("../lib/parser/parserError");
//const request = require('request');
//const util = require("util");
const utils = require("../lib/utils");
const logger = require("../config/winston");
const { ErrorHandler } = require("../lib/errorHandler");

router.post("/interactions", function (req, res) {
  if (!req.body.cig_id) {
    res.sendStatus(406).send({ error: "cig_id param missing" });
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

   // logger.info("data sent to grammar parser is: " + data);

    //use grammar to parse response into a JSON object
    const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar), {
      keepHistory: true,
    });

    let result;
    try {

      parser.feed(data);

        result = parser.results[0];
        //convert type of first recommendation to secondary when type of interaction is repairable
        for (let val of result) {
          if (val.type === "repairable") {
            val.interactionNorms[0].type = "secondary";
          }
        }
    } catch (e) {
      reportError(e, parser);
      result = null;
    }
      
    if (result) {
      res.send(result);
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
  utils.callPrologServer("drug", postData, function (err, data) {
    if (err) {
      res.sendStatus(400);
    } else {
      //logger.info(data);
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
    utils.callPrologServer("drugeffects", postData, function (err, data) {
      if (err) {
        res.sendStatus(400);
      } else {
        //logger.info(data);
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
        err ? res.sendStatus(400) : res.send(RecUris);
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
  //logger.info(filterString)

  //select nanopub URIs from subguidelines
  utils.sparqlGetNamedNanopubFromSubguidelines(
    req.body.cig_from,
    filterString,
    function (err, assertionList) {
      if (err) {
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
