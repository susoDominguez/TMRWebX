const express = require("express");
const router = express.Router();
const Promise = require("bluebird");
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar");
const { reportError } = require("../lib/parser/parserError");
const utils = Promise.promisifyAll(require("../lib/utils"));
const logger = require("../config/winston");
const { ErrorHandler } = require("../lib/errorHandler");
const e = require("express");
const { throws } = require("assert");

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
      filterString += `?sg = data:subCIG-` + SubId.trim() + ` || `;
    });
    //remove last operator and whitespace
    filterString = filterString.substring(0, filterString.length - 4);
  } else {
    filterString = ` ?sg = data:` + req.body.cig_from;
  }

  filterString = `FILTER(` + filterString + `)`;

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
        
        let uri = assertionList[index];

        logger.info('uri is ' + uri);
        //nanoHeadList.push(uri + `_head`);
        //nanoProbList.push(uri + `_provenance`);
        //nanoPubList.push(uri + `_publicationinfo`);

          utils.addGraphsDataFromToCig(
            req.body.cig_from,
            req.body.cig_to,
            uri + `_head`,//nanoHeadList,
            uri, //assertionList,
            uri + `_provenance`, //nanoProbList,
            uri + `_publicationinfo`,//nanoPubList,
            function (err, status) {
  
              if (err) {
                res.sendStatus(400);
                return;
              }
            }
          );
      }
      res.sendStatus(200);
    }
  );
});

/**
 * get URIs of all Recommendations in a given CIG
 */
router.post("/cig/get",  function (req, res, next) {

  if (!req.body.cig_id) next("parameter cig_id missing in /cig/get");

  //label of CIG{
  let cigId = req.body.cig_id;

  cigId = cigId.startsWith(`CIG-`) ? cigId : `CIG-` + cigId;

  //promise to deliver list of Ids
  utils
    .sparqlGetSubjectAllNamedGraphsAsync(cigId, "vocab:ClinicalRecommendation")
    .map((recURI) => {
      //get sparql data for the given recommendation URI
      return utils
        .getRecDataAsync(cigId, recURI, "beliefs", "transitions", "careActions")
        .then( (guidelineData) => {
          if (
            !guidelineData ||
            guidelineData.constructor !== Object ||
            Object.entries(guidelineData).length === 0
          )
            throw new SyntaxError(
              "guidelineData at RecURI: " +
                recURI +
                " in /cig/get does not have the expected properties for CIG " +
                cigId
            );

          let recData = {
            id: recURI,
            causationBeliefs: [],
          };
          let cbData = {
            author: "JDA",
          };
          let actData = {};
          let TrData = {
            situationTypes: [
              {
                type: "hasTransformableSituation",
                value: {},
              },
              {
                type: "hasExpectedSituation",
                value: {},
              },
            ],
            property: {},
          };

          let vars = guidelineData.head.vars;
          let bindings = guidelineData.results.bindings;

          if (bindings.length === 0)
            throw new SyntaxError(
              "guidelineData at RecURI: " +
                recURI +
                " in /cig/get does not have any (expected) bindings for CIG " +
                cigId
            );

          //format data by looping through results
          for (let pos in bindings) {
            let bind = bindings[pos];
            //console.info(bind);

            for (let varPos in vars) {
              let value = vars[varPos] in bind ? bind[vars[varPos]].value : "";
              let type;

              //for each heading, add a field
              switch (vars[varPos]) {
                case "partOf":
                  let extractedFrom = value.slice(30);
                  recData.partOf = extractedFrom;
                  break;
                case "text":
                  recData.text = value;
                  break;
                case "strength":
                  recData.suggestion =
                    value == "should" ? "recommend" : "nonRecommend";
                  break;
                case "sourceOfRec":
                  recData.derivedFrom = value;
                  break;
                case "contrib":
                  cbData.contribution = value;
                  break;
                case "cbUri": //data from main contribution
                  cbData.id = value;
                  break;
                case "freq":
                  cbData.probability = value;
                  break;
                case "evidence":
                  cbData.evidence = value;
                  break;
                case "TrUri":
                  TrData.id = value;
                  break;
                case "sitFromId":
                  TrData.situationTypes[0].id = value;
                  //extract code
                  type = value.slice(26);
                  TrData.situationTypes[0].value.code = type;
                  break;
                case "sitToId":
                  TrData.situationTypes[1].id = value;
                  //extract code
                  type = value.slice(26);
                  TrData.situationTypes[1].value.code = type;
                  break;
                case "PropUri":
                  TrData.property.id = value;
                  //extract code
                  type = value.slice(26);
                  TrData.property.code = type;
                  break;
                case "sitFromLabel":
                  TrData.situationTypes[0].value.display = value;
                  break;
                case "sitToLabel":
                  TrData.situationTypes[1].value.display = value;
                  break;
                case "propTxt":
                  TrData.property.display = value;
                  break;
                case "deriv":
                  TrData.effect = value;
                  break;
                case "actId":
                  actData.id = value;
                  break;
                case "adminLabel":
                  actData.display = value;
                  break;
                case "actLabel":
                  actData.code = value;
                  break;
                case "actType":
                  //extract code
                  type = value.slice(27);
                  //actData.code = type;
                  actData.requestType = 0; //for drugT and DrugCat
                  //check for therapy
                  if (type.startsWith("NonDrugT")) {
                    actData.requestType = 1;
                  } else {
                    //check for vaccine
                    if (type.startsWith("VacT")) {
                      actData.requestType = 2;
                    }
                  }
                  break;
              }
            } //end of for vars
          } //end of for bindings

          //join data together
          cbData.transition = TrData;
          recData.careActionType = actData;
          recData.causationBeliefs[0] = cbData;

          return recData;
        }) //TODO: if it fails early, does it go to the outer catch?
    })
    .then((cigData) => res.send(cigData))
    .catch((err) => next( new ErrorHandler(404, `thrown when converting Sparql (Recommendation) data into Json data: ${err}.`)));
});

module.exports = router;
