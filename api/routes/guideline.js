const express = require("express");
const router = express.Router();
const request = require("request");
const bodyParser = require("body-parser");

const config = require("../lib/config");
const guidelines = require("../lib/prefixes");
const utils = require("../lib/utils");
const { handleError, ErrorHandler } = require("../lib/errorHandler");
const logger = require("../config/winston");
const e = require("express");

const tmrDataUri = "http://anonymous.org/tmr/data/";
/**
 * Create a persistent or in-memory CIG and return label of CIG
 */
router.post("/create", bodyParser.json(), function (req, res, next) {
  let id = req.body.cig_id ? req.body.cig_id : new Date().toISOString();

  cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

  const dbType = req.body.IsPersistent ? `tdb` : `mem`;

  request.post(
    {
      url:
        "http://" +
        config.JENA_HOST +
        ":" +
        config.JENA_PORT +
        "/$/datasets?dbType=" +
        dbType +
        "&dbName=" +
        cigId,
      headers: {
        Authorization:
          "Basic " +
          new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
      },
    },
    function (error, response, body) {
      //send error and end
      if (error) {
        res.status(404).send({ error: error });
        return;
      }

      if (!req.body.description) {
        req.body.description = `Guideline ` + cigId;
      }

      const description = `data:${cigId} rdf:type tmr:ClinicalGuideline, owl:NamedIndividual ;
                         rdfs:label '''${req.body.description}'''@en .`;

      utils.sparqlUpdate(
        cigId,
        description,
        config.INSERT,
        function (err, status) {
          if (err)
            return next(
              new ErrorHandler(
                status,
                `sparql has returned an error when creating clinical guideline dataset.`
              )
            );
          //otherwise
          res.json({ cig_id: cigId });
        }
      );
    }
  );
});

/**
 * Delete a persistent or in-memory CIG
 */
router.post("/delete", function (req, res, next) {
  let id = req.body.cig_id;

  if (!id)
    return next(
      new ErrorHandler(
        400,
        `guideline could not be deleted due to internal error.`
      )
    );
  //otherwise

  const cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

  request.delete(
    {
      url:
        "http://" +
        config.JENA_HOST +
        ":" +
        config.JENA_PORT +
        "/$/datasets/" +
        cigId,
      headers: {
        Authorization:
          "Basic " +
          new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
      },
    },
    function (error, response, body) {
      if (error) {
        res.status(400).send(error);
      } else {
        res.status(200).send(response);
      }
    }
  );
});

function action(req, res, insertOrDelete) {
  //data id for this rec
  const id = `data:Rec${req.body.cig_id}-${req.body.rec_id}`;
  let sources = "";
  const date = new Date().toJSON();
  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = `${id} tmr:isPartOf data:CIG-${req.body.cig_id} .`;

  if (req.body.derivedFrom) {
    sources = `  prov:wasDerivedFrom  `;

    req.body.derivedFrom.split(",").forEach(function (code) {
      sources += ` <` + code + `> ,`;
    });
    //this removes the last coma
    sources = sources.substring(0, sources.length - 1);
  }

  // Guideline format:
  const head =
    id +
    `_head { 
        ` +
    id +
    `_head
            a     nanopub:Nanopublication ;
            nanopub:hasAssertion        ` +
    id +
    ` ;
            nanopub:hasProvenance       ` +
    id +
    `_provenance ;
            nanopub:hasPublicationInfo  ` +
    id +
    `_publicationinfo .
  }`;

  const body =
    id +
    ` {
    ` +
    id +
    `  a   tmr:ClinicalRecommendation ;
            rdfs:label  '''` +
    req.body.label +
    `'''@en ;
            tmr:aboutExecutionOf  data:ActAdminister` +
    req.body.careAction_id +
    ` ;
            tmr:partOf            data:CIG-` +
    req.body.cig_id +
    ` ;
            tmr:strength          tmr:` +
    req.body.strength +
    ` .
  }`;

  const provenance =
    id +
    `_provenance {
    ` +
    id +
    `_provenance
    a  oa:Annotation ;
    oa:hasBody  ` +
    id +
    ` ;
    oa:hasTarget                  [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
    ` +
    id +
    `
    ` +
    sources +
    ` .
    }`;

  const publication =
    id +
    `_publicationinfo {
      ` +
    id +
    `_head
      prov:generatedAtTime          "` +
    date +
    `"^^xsd:dateTime ;
      prov:wasAttributedTo          data:` +
    req.body.author +
    `.
    }`;

  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    "GRAPH " +
      head +
      "\nGRAPH " +
      body +
      "\nGRAPH " +
      provenance +
      "\nGRAPH " +
      publication,
    insertOrDelete,
    function (err, status) {
      if (status === 200) {
        //add assertion id to default graph as part of CIG
        utils.sparqlUpdate(
          "CIG-" + req.body.cig_id,
          id2CIG,
          insertOrDelete,
          function (err2, status2) {
            res.status(status2).end();
          }
        );
      } else {
        //didnt work. send first status back
        logger.error(err);
        res.status(status);
      }
    }
  );
}

function insertCB(req, res, insertOrDelete) {
  const recId = `data:Rec` + req.body.cig_id + `-` + req.body.rec_id;

  const body =
    recId +
    ` {
    ` +
    recId +
    ` tmr:basedOn data:CB` +
    req.body.belief_id +
    ` .
    data:CB` +
    req.body.belief_id +
    ` tmr:contribution tmr:` +
    req.body.contribution +
    `.
  }`;

  const graph = `GRAPH ${body}`;
  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    graph,
    insertOrDelete,
    function (err, status) {
      if (err) {
        logger.debug(`error when updating recommendation with belief: ${err}`);
      }
      res.status(status).end();
    }
  );
}

function insertPrecond(req, res, insertOrDelete) {
  const recId = `data:Rec` + req.body.cig_id + `-` + req.body.rec_id;

  const body =
    recId +
    ` {
    ` +
    recId +
    ` tmr:hasFilterSituation data:Sit` +
    req.body.precond_id +
    ` .
  }`;

  const graph = `GRAPH ${body}`;
  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    graph,
    insertOrDelete,
    function (err, status) {
      if (err) {
        logger.debug(
          `error when updating recommendation with precondition: ${err}`
        );
      }
      res.status(status).end();
    }
  );
}

function action_gprec(req, res, insertOrDelete) {
  //data id for this rec
  const id = `data:GPRec` + req.body.cig_id + `-` + req.body.gpRec_id;

  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = id + ` tmr:isPartOf data:CIG-` + req.body.cig_id + ` .`;

  // Graph format:
  const head =
    id +
    `_head { 
        ` +
    id +
    `_head
            a     nanopub:Nanopublication ;
            nanopub:hasAssertion        ` +
    id +
    ` ;
            nanopub:hasProvenance       ` +
    id +
    `_provenance ;
            nanopub:hasPublicationInfo  ` +
    id +
    `_publicationinfo .
  }`;

  const body =
    id +
    ` {
    ` +
    id +
    `
            a   tmr:GoodPracticeRecommendation ;
            rdfs:label  '''${req.body.gpRec_label}'''@en ;
            tmr:aboutNotificationOf  data:ST` +
    req.body.statement_id +
    ` ;
            tmr:partOf            data:CIG-` +
    req.body.cig_id +
    ` ;

  }`;

  const provenance =
    id +
    `_provenance {
    ` +
    id +
    `
            prov:wasDerivedFrom  <` +
    (req.body.source ? req.body.source : "unknown") +
    `> .

    ` +
    id +
    `_provenance
            a             oa:Annotation ;
            oa:hasBody    ` +
    id +
    ` ;
            oa:hasTarget  [ oa:hasSource  <http://hdl.handle.net/10222/43703> ] .
  }`;

  const publication =
    id +
    `_publicationinfo {
      ` +
    id +
    `_head
            prov:generatedAtTime  "2020-03-01"^^xsd:dateTime ;
            prov:wasAttributedTo  data:` +
    req.body.author +
    ` .
  }`;
  const graph = `GRAPH ${head} \n GRAPH ${body} \n GRAPH ${provenance} \n GRAPH ${publication}`;

  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    graph,
    insertOrDelete,
    function (err, status) {
      if (err) {
        logger.debug(
          `error when updating good practice recommendation: ${err}`
        );
        res.status(status).end();
      } else {
        //add assertion id to default graph as part of CIG
        if (status === 200) {
          utils.sparqlUpdate(
            "CIG-" + req.body.cig_id,
            id2CIG,
            insertOrDelete,
            function (err2, status2) {
              if (err2) logger.debug(`action_gprec Error: ${err2}`);

              res.status(status2).end();
            }
          );
        }
      }
    }
  );
}

router.post("/gprec/add", function (req, res, next) {
  action_gprec(req, res, config.INSERT);
});

router.post("/gprec/delete", function (req, res, next) {
  var id = req.body.cig_id;
  var idCig;

  if (id.startsWith(`CIG-`)) {
    idCig = id;
    id = id.substring(`CIG-`.length - 1);
  } else {
    idCig = `CIG-` + id;
  }

  const gpRecUri = req.body.gprec_uri
    ? req.body.gprec_uri
    : "data:GPRec" + id + "-" + req.body.gprec_id;

  utils.sparqlDropGraphs(idCig, gpRecUri, function (err, status) {
    res.sendStatus(err, status);
  });
});

router.post("/rec/add", function (req, res, next) {
  action(req, res, config.INSERT);
});

router.post("/belief/add", function (req, res, next) {
  insertCB(req, res, config.INSERT);
});

router.post("/precond/add", function (req, res, next) {
  insertPrecond(req, res, config.INSERT);
});

router.post("/rec/delete", function (req, res, next) {
  var id = req.body.cig_id;
  var idCig;

  if (id.startsWith(`CIG-`)) {
    idCig = id;
    id = id.substring(`CIG-`.length - 1);
  } else {
    idCig = `CIG-` + id;
  }

  const recUri = req.body.rec_uri
    ? req.body.rec_uri
    : "data:Rec" + id + "-" + req.body.rec_id;

  utils.sparqlDropGraphs(idCig, recUri, function (err, status) {
    res.sendStatus(err, status);
  });
});

////

router.post("/careAction/get", function (req, res, next) {
  var postData = "";

  if (req.body.rec_id) {
    postData = require("querystring").stringify({
      guideline_id: `CIG-` + req.body.guideline_id,
      rec_id:
        "http://anonymous.org/tmr/data/Rec" +
        req.body.guideline_id +
        "-" +
        req.body.rec_id,
    });
  } else {
    //this one will be more accurate when combining guidelines
    postData = require("querystring").stringify({
      guideline_id: `CIG-` + req.body.guideline_id,
      rec_URI: req.body.rec_URI,
    });
  }

  utils.callPrologServer("drug", postData, res, function (err, data) {
    if (err) {
      res.sendStatus(400);
      return;
    }

    res.send(data);
  });
});

router.post("/rec/all/get/", function (req, res, next) {
  let id = req.body.cig_id.trim();
  let idCig;

  //separate lable id from dataset id
  if (id.startsWith(`CIG-`)) {
    idCig = id;
    //remove it
    id = id.substring(`CIG-`.length);
  } else {
    idCig = `CIG-` + id;
  }
  logger.debug(`id is ${id} and cigID isw ${idCig}`);

  const recURI = req.body.uri
    ? req.body.uri.trim()
    : tmrDataUri + "Rec" + id + "-" + req.body.id.trim();

  utils.getRecData_multiple_CBs(
    idCig,
    recURI,
    "beliefs",
    "transitions",
    "careActions",
    function (err, guidelineData) {
      if (err) return next(new ErrorHandler(500, err));
      //for submission
      let data = {};

      //if  data found in Object (we check), begin
      if (
        guidelineData &&
        guidelineData.constructor === Object &&
        Object.entries(guidelineData).length != 0
      ) data = getRecData(recURI, guidelineData);

      res.send(data);
    }
  );
});

router.post("/gprec/all/get/", function (req, res, next) {
  
  var id = req.body.cig_id.trim();
  var idCig;

  //separate lable id from dataset id
  if (id.startsWith(`CIG-`)) {
    idCig = id;
    //remove it
    id = id.substring(`CIG-`.length);
  } else {
    idCig = `CIG-` + id;
  }

  const recURI = req.body.uri
    ? req.body.uri.trim()
    : req.body.id ?   `${tmrDataUri}GPRec${id}-${req.body.id.trim()}` : undefined ;

  utils.getRecStmntData(
    idCig,
    recURI,
    "statements",
    "transitions",
    "careActions",
    function (err, guidelineData) {
        if (err) return next(new ErrorHandler(500, err));
        //for submission
        let data = {};

        //if  data found in Object (we check), begin
        if (
          guidelineData &&
          guidelineData.constructor === Object &&
          Object.entries(guidelineData).length != 0
        ) {
          try { 
            data = getStatementData(recURI, guidelineData);
          } catch(err) {
            throw new ErrorHandler(500, err);
          }
        }
        res.send(data);
      }
  );
});

///create subguideline by referencing assertion  resources which are same as assertion graph names in main guideline
router.post("/subGuideline/add", function (req, res, next) {
  actionSubguideline(req, res, config.INSERT);
});

//delete subguideline along with its components
router.post("/subGuideline/delete", function (req, res, next) {
  actionSubguideline(req, res, config.DELETE);
});

function actionSubguideline(req, res, insertOrDelete) {
  if (!req.body.description) {
    req.body.description = "subGuideline " + req.body.subGuideline_id;
  }

  // SubGuideline declaration:
  const description =
    `data:subCIG-` +
    req.body.subGuideline_id +
    ` rdf:type tmr:subGuideline, owl:NamedIndividual ;
                         rdfs:label "` +
    req.body.description +
    `"@en ;
                         tmr:isSubGuidelineOf  data:CIG-` +
    req.body.guideline_id +
    ` .`;

  //var to construct the assignment of recs to a subguideline. initial whitespace to be kept
  var recDeclaration = " ";

  if (req.body.recs_ids) {
    //nanopublication is part of this subGuideline. contains  pred and object of resource
    const isPartOf =
      ` tmr:isPartOf   data:subCIG-` + req.body.subGuideline_id + ` .\n`;

    req.body.recs_ids.split(",").forEach(function (recId) {
      recDeclaration +=
        `data:Rec` + req.body.guideline_id + `-` + recId.trim() + isPartOf;
    });
  }

  utils.sparqlUpdate(
    "CIG-" + req.body.guideline_id,
    description + recDeclaration,
    insertOrDelete,
    function (err, status) {
      res.sendStatus(status);
    }
  );
}

function getRecData(recURI, guidelineData){
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
  logger.debug(bindingsList.length);

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
      if (!(headVar && bindingObj.hasOwnProperty(headVar)) ) continue;

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
    title:undefined,
    clinicalStatements: []
  };

  let precond = {
    id: undefined,
    sctId: undefined,
    display: undefined,
    composedOf: undefined,
  };

  let headVars = guidelineData.head.vars;
  let bindingsList = guidelineData.results.bindings;
  //logger.debug(bindingsList.length);

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
      generatedAtTime: undefined
    };

    //format data by looping through head vars
    for (const pos in headVars) {
      //variable name
      let headVar = headVars[pos];

      //check there is a corresponding binding, if not, next head var
      if (!(headVar && bindingObj.hasOwnProperty(headVar)) ) continue;

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
