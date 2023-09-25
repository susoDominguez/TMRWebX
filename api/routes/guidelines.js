const express = require("express");
const router = express.Router();
const Promise = require("bluebird");
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar");
const { reportError } = require("../lib/parser/parserError");
const utils = Promise.promisifyAll(require("../lib/utils"));
const logger = require("../config/winston");
const { ErrorHandler } = require("../lib/errorHandler");
//const e = require("express");
//const { throws } = require("assert");

//filter out TMR types when unnecesary for the triggered router 
function filterTMRtype(RecUris) {
  if(!Array.isArray(RecUris)) return RecUris;
   let result = RecUris.filter( uri => !(uri ===  "http://anonymous.org/tmr/ClinicalRecommendation" || uri ===  "http://anonymous.org/tmr/GoodPracticeRecommendation"));
   return result;
 }

router.post("/interactions", function (req, res, next) {
  if (!req.body.cig_id) {
    res.status(406).send({ error: "cig_id param missing" });
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

  utils
    .callPrologServerAsync("interactions", postData)
    .then((data) => {
      logger.info("data sent to grammar parser is: " + data);

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
        throw ErrorHandler(400, "result of parsing failed");
      }

      if (result) {
        return res.status(200).json(result);
      } else {
        throw ErrorHandler(400, "result of parsing failed");
      }
    })
    .catch((err) => {
      res.status(400).json({
        status: "error",
        error: err,
      });
    });
});

/**
 * get URIs of all Recommendations in a given CIG
 */
router.post("/rec/get", function (req, res, next) {
  if (req.body.cig_id) {
    var cigId = req.body.cig_id;

    cigId = cigId.startsWith(`CIG-`) ? cigId : `CIG-` + cigId;

    utils.sparqlGetSubjectAllNamedGraphs(
      cigId,
      "tmr:ClinicalRecommendation",
      function (err, RecUris) {
        err ? res.status(400).end() : res.send(filterTMRtype(RecUris));
      }
    );
  } else {
    res.status(400).end();
  }
});


/**
 * add nanopub graphs from one existing CIG to another
 */
router.post("/add", async function (req, res, next) {
  if (!req.body.cig_from || !req.body.cig_to) {
    return res.status(406).json({
      status: "error",
      error: "parameters cig_from or cig_to cannot be empty",
    });
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
      filterString +=
        `?sg = data:` +
        (SubId.startsWith(`subCIG-`) ? "" : `subCIG-`) +
        SubId.trim() +
        ` || `;
    });
    //remove last operator and whitespace
    filterString = filterString.substring(0, filterString.length - 4);
  } else {
    filterString = ` ?sg = data:` + req.body.cig_from;
  }

  filterString = `FILTER(` + filterString + `)`;
  //logger.info(filterString);

  try {
    //select nanopub URIs from subguidelines
    const assertionList =
      await utils.sparqlGetNamedNanopubFromSubguidelinesAsync(
        req.body.cig_from,
        filterString
      );

    //throw catch if no data
    if (assertionList == null || assertionList == null || assertionList == [])
      throw Error(
        "Failure retrieving recommendations referenced in subguidelines from database"
      );

    //for each assertion URI, add the rest of the related nano graphs
    const promises = assertionList.map((uri) => {
      // logger.info(uri);
      return utils.addGraphsDataFromToCigAsync(
        req.body.cig_from,
        req.body.cig_to,
        uri + `_head`, //nanoHeadList,
        uri, //assertionList,
        uri + `_provenance`, //nanoProbList,
        uri + `_publicationinfo` //nanoPubList,
      );
    });

    let result = await Promise.all(promises);

    return res.status(204).end();
  } catch (error) {
    res.status(406).json({
      status: "error",
      error: error,
    });
  }
});

/**
 * get URIs of all Recommendations in a given CIG
 */
router.post("/cig/get", function (req, res, next) {

  if (!req.body.cig_id) {
    return res.status(406).json({
      status: "error",
      error: "parameter cig_id is missing.",
    });
  }

  //label of CIG{
  let cigId = req.body.cig_id.trim();

  cigId = cigId.startsWith(`CIG-`) ? cigId : `CIG-` + cigId;

  //promise to deliver list of Ids
  utils
    .sparqlGetSubjectAllNamedGraphsAsync(cigId, [
      "tmr:ClinicalRecommendation",
      "tmr:GoodPracticeRecommendation",
    ])
    .then( async (uriList) => {

      try {
        let promises = uriList.map( async (recURI) => {
          if (recURI.startsWith(`http://anonymous.org/tmr/data/GPRec`)) {
            return utils
              .getRecStmntDataAsync(
                cigId,
                recURI,
                "statements",
                "transitions",
                "careActions"
              )
              .then((guidelineData) => {
                //if  data found in Object (we check), begin
                if (
                  guidelineData &&
                  guidelineData.constructor === Object &&
                  Object.entries(guidelineData).length != 0
                ) {
                  try {
                    return getStatementData(recURI, guidelineData);
                  } catch (err) {
                    //silently log error?
                    logger.debug(
                      `Error at getStatementData with recURI ${recURI} and guidelinedata: ${JSON.stringify(
                        guidelineData
                      )}`
                    );
                    throw new ErrorHandler(500, err);
                  }
                }
              });
          }

          if (recURI.startsWith(`http://anonymous.org/tmr/data/Rec`)) {
            return utils
              .getRecDataAsync(
                cigId,
                recURI,
                "beliefs",
                "transitions",
                "careActions"
              )
              .then((guidelineData) => {
                //if  data found in Object (we check), begin
                if (
                  guidelineData &&
                  guidelineData.constructor === Object &&
                  Object.entries(guidelineData).length != 0
                ) {
                  try {
                    return getRecJsonData(recURI, guidelineData);
                  } catch (err) {
                    //silently log error?
                    logger.debug(
                      `Error at getRecJsonData with recURI ${recURI} and guidelinedata: ${JSON.stringify(
                        guidelineData
                      )}`
                    );
                    throw new ErrorHandler(500, err);
                  }
                }
              });
          }

          return null;
        }); //end of map

        let recJsonResult = await Promise.all(promises);
        //filter out array items of form {}
        recJsonResult = recJsonResult.filter( (item) => item !== null );

        return res.status(200).json(recJsonResult);
      } catch (error) {
        //end loop if error found
        logger.info(recURI);
        logger.error(error);
        throw error;
      }
    })
    .catch((error) => {
      new ErrorHandler(406, JSON.stringify(error));
    });
});

function getRecJsonData(recURI, guidelineData) {
  //recommendation template object
  let recData = {
    id: recURI,
    partOf: undefined, //combined dataset or original
    extractedFrom: undefined, //original dataset
    type: {
      sctId: "306807008",
      display: "Recommendation to start drug treatment (procedure)",
    },
    careActionType: {
      id: undefined,
      requestType: 0, //for drug treatments
      code: undefined,
      display: undefined,
      drugLabel: undefined,
      sctId: undefined,
      hasComponents: undefined,
    },
    causationBeliefs: [],
    derivedFrom: undefined,
    hasSource: undefined,
    wasAttributedTo: undefined,
    generatedAtTime: undefined,
    hasFilterSituation: undefined,
  };
  let precond = {
    id: undefined,
    sctId: undefined,
    display: undefined,
    composedOf: undefined,
  };

  let headVars = guidelineData.head.vars;
  let bindingsList = guidelineData.results.bindings;

  //logger.debug(JSON.stringify(headVars));

  for (const index in bindingsList) {
    //results object
    const bindingObj = bindingsList[index];
    //one TR and CB object per CB id encountered
    //one object per causation belief
    const cbData = {
      id: undefined,
      author: undefined,
      contribution: undefined,
      derivedFrom: undefined,
      hasSource: undefined,
      wasAttributedTo: undefined,
      generatedAtTime: undefined,
      probability: undefined,
      evidence: undefined,
      transition: {
        id: undefined,
        effect: undefined,
        property: {
          sctId: undefined,
          display: undefined,
          code: undefined,
          id: undefined,
        },
        situationTypes: [
          {
            id: undefined,
            type: "hasTransformableSituation",
            value: {
              stateOfProp: undefined,
              sctId: undefined,
              display: undefined,
              code: undefined,
            },
          },
          {
            id: undefined,
            type: "hasExpectedSituation",
            value: {
              stateOfProp: undefined,
              sctId: undefined,
              display: undefined,
              code: undefined,
            },
          },
        ],
      },
    };

    //format data by looping through head vars
    for (const pos in headVars) {
      //variable name
      let headVar = headVars[pos];

      //check there is a corresponding binding, if not, next head var
      if (!(headVar && bindingObj.hasOwnProperty(headVar))) continue;

      //otherwise, retrieve value
      let value = bindingObj[headVar].value;
      //temporary var
      let temp;

      //for each CB
      switch (headVar) {
        case "derivedFromCB":
          temp = value.split(",");
          cbData.derivedFrom = temp;
          break;
        case "hasSourcesCB":
          temp = value.split(",");
          cbData.hasSource = temp;
          break;
        case "contrb":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.contribution = temp.toLowerCase();
          break;
        case "cbUri":
          cbData.id = value;
          break;
        case "freq":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.probability = temp.toLowerCase();
          break;
        case "evidence":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.evidence = temp.toLowerCase();
          break;
        case "TrUri":
          cbData.transition.id = value;
          break;
        case "propTxt":
          cbData.transition.property.display = value;
          break;
        case "sctProp":
          cbData.transition.property.sctId = value;
          break;
        case "PropUri":
          cbData.transition.property.id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.property.code = temp;
          break;
        case "sitFromId":
          cbData.transition.situationTypes[0].id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.situationTypes[0].value.code = temp;
          break;
        case "sitToId":
          cbData.transition.situationTypes[1].id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.situationTypes[1].value.code = temp;
          break;
        case "sitFromLabel":
          cbData.transition.situationTypes[0].value.display = value;
          break;
        case "stateOfPreSit":
          cbData.transition.situationTypes[0].value.stateOfProp = value;
          break;
        case "sctPreSit":
          cbData.transition.situationTypes[0].value.sctId = value;
          break;
        case "sitToLabel":
          cbData.transition.situationTypes[1].value.display = value;
          break;
        case "stateOfPostSit":
          cbData.transition.situationTypes[1].value.stateOfProp = value;
          break;
        case "sctPostSit":
          cbData.transition.situationTypes[1].value.sctId = value;
          break;
        case "deriv":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.effect = temp.toLowerCase();
          break;
      }

      //take the first binding object and
      //create the outer content of recoomend minus CBs
      if (index < 1) {
        switch (headVar) {
          //precondition
          case "precond":
            precond.id = value;
            break;
          case "sctPrecond":
            precond.sctId = value;
            break;
          case "precondLbl":
            precond.display = value;
            break;
          case "compoundSituation": //TODO
            // precond.composedOf = value;
            break;
          //recommendation
          case "partOf":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.partOf = temp;
            break;
          case "extractedFrom":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.extractedFrom = temp;
            break;
          case "label":
            recData.text = value;
            break;
          case "strength":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.suggestion =
              temp == "Should"
                ? "recommend"
                : temp == "Should_not"
                ? "nonRecommend"
                : temp.toLowerCase();
            break;
          case "derivedFrom":
            temp = value.split(",");
            recData.derivedFrom = temp;
            break;
          case "hasSources":
            temp = value.split(",");
            recData.hasSource = temp;
            break;
          case "attributedTo":
            recData.wasAttributedTo = value;
            break;
          case "generatedTime":
            recData.generatedAtTime = value;
            break;
          //care action
          case "actId":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.careActionType.id = value;
            recData.careActionType.code = temp;
            break;
          case "adminLabel":
            recData.careActionType.display = value;
            break;
          case "actLabel":
            recData.careActionType.drugLabel = value;
            break;
          case "components":
            temp = value.split(",");
            recData.careActionType.hasComponents = temp;
            break;
          case "sctDrg":
            recData.careActionType.sctId = value;
            break;
          case "of":
            //extract code
            temp = value.split("/");
            temp = temp[temp.length - 1];
            //check for therapy
            if (temp === "applicationOf") {
              recData.careActionType.requestType = 1;
              recData.type.sctId = "304541006";
              recData.type.display =
                "Recommendation to perform treatment (procedure)";
            }
            //check for vaccine
            if (temp === "inoculationOf") {
              recData.careActionType.requestType = 2;
              (recData.type.sctId = "830152006"),
                (recData.type.display =
                  "Recommendation regarding vaccination (procedure)");
            }
            break;
        }
        if (precond.id) recData.hasFilterSituation = precond;
      }

      //test values of each binding object
      //logger.debug(`head var is ${headVar} and value is ${value}`);
    } //endOf headVars
    //add one CB object to Rec
    recData.causationBeliefs.push(cbData);
  } //endOf bindingObj

  return recData;
}

function getStatementData(recURI, guidelineData) {
  //recommendation template object
  let recData = {
    id: recURI,
    partOf: undefined, //combined dataset or original
    extractedFrom: undefined, //original dataset
    type: {
      sctId: "223464006",
      display: "Procedure education (procedure)",
    },
    derivedFrom: undefined,
    hasSource: undefined,
    wasAttributedTo: undefined,
    generatedAtTime: undefined,
    hasFilterSituation: undefined,
    title: undefined,
    clinicalStatements: [],
  };

  let precond = {
    id: undefined,
    sctId: undefined,
    display: undefined,
    composedOf: undefined,
  };

  let headVars = guidelineData.head.vars;
  let bindingsList = guidelineData.results.bindings;
  logger.debug(bindingsList.length);

  for (const index in bindingsList) {
    //results object
    const bindingObj = bindingsList[index];
    //one TR and CB object per CB id encountered
    //one object per causation belief
    const stData = {
      id: undefined,
      author: undefined,
      hasStatementTitle: undefined,
      hasStatementText: undefined,
      organization: undefined,
      jurisdiction: undefined,
      derivedFrom: undefined,
      hasSource: undefined,
      wasAttributedTo: undefined,
      generatedAtTime: undefined,
    };

    //format data by looping through head vars
    for (const pos in headVars) {
      //variable name
      let headVar = headVars[pos];

      //check there is a corresponding binding, if not, next head var
      if (!(headVar && headVar in bindingObj)) continue;

      //otherwise, retrieve value
      let value = bindingObj[headVar].value;
      //temporary var
      let temp;

      //for each CB
      switch (headVar) {
        case "derivedFromSt":
          temp = value.split(",");
          stData.derivedFrom = temp;
          break;
        case "hasSourcesSt":
          temp = value.split(",");
          stData.hasSource = temp;
          break;
        case "orgNmsSt":
          temp = value.split(",");
          stData.organization = temp;
          break;
        case "stUri":
          stData.id = value;
          break;
        case "orgJursSt":
          temp = value.split(",");
          stData.jurisdiction = temp;
          break;
        case "stTxt":
          stData.hasStatementText = value;
          break;
        case "stTtl":
          stData.hasStatementTitle = value;
          break;
      }

      //take the first binding object and
      //create the outer content of recoomend minus CBs
      if (index < 1) {
        switch (headVar) {
          //precondition
          case "precond":
            precond.id = value;
            break;
          case "sctPrecond":
            precond.sctId = value;
            break;
          case "precondLbl":
            precond.display = value;
            break;
          case "compoundSituation": //TODO
            // precond.composedOf = value;
            break;
          //recommendation
          case "partOf":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.partOf = temp;
            break;
          case "extractedFrom":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.extractedFrom = temp;
            break;
          case "label":
            recData.title = value;
            break;
          case "derivedFrom":
            temp = value.split(",");
            recData.derivedFrom = temp;
            break;
          case "hasSources":
            temp = value.split(",");
            recData.hasSource = temp;
            break;
          case "attributedTo":
            recData.wasAttributedTo = value;
            break;
          case "generatedTime":
            recData.generatedAtTime = value;
            break;
          case "orgNms":
            recData.organization = value;
            break;
          case "orgJurs":
            recData.jurisdiction = value;
            break;
        }
        if (precond.id) recData.hasFilterSituation = precond;
      }

      //test values of each binding object
      //logger.debug(`head var is ${headVar} and value is ${value}`);
    } //endOf headVars
    //add one CB object to Rec
    recData.clinicalStatements.push(stData);
  } //endOf bindingObj

  return recData;
}

module.exports = router;
