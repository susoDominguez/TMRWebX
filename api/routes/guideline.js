const express = require("express");
const router = express.Router();
const request = require("request");
const bodyParser = require("body-parser");

const config = require("../lib/config");
const guidelines = require("../lib/guidelines");
const utils = require("../lib/utils");

/**
 * Create a persistent or in-memory CIG
 */
router.post("/create", bodyParser.json(), function (req, res, next) {
  var id = req.body.cig_id;

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
      if (error) {
        res.sendStatus(404).send({ error: error });
        return;
      }

      if (!req.body.description) {
        req.body.description = `Guideline ` + cigId;
      }

      const description =
        `data:` +
        cigId +
        ` rdf:type vocab:ClinicalGuideline, owl:NamedIndividual ;
                     rdfs:label "` +
        req.body.description +
        `"@en .`;

      utils.sparqlUpdate(cigId, description, config.INSERT, function (
        err,
        status
      ) {
        //if err -> status is fixed to 400 in sparqlUpdate
        res.sendStatus(status);
      });
    }
  );
});

/**
 * Delete a persistent or in-memory CIG
 */
router.post("/delete", function (req, res, next) {
  var id = req.body.cig_id;

  if (id) {
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
          res.sendStatus(400).send(error);
        } else {
          //console.log(body);
          res.sendStatus(200).send(body);
        }
      }
    );
  } else {
    res.sendStatus(404);
  }
});

function action(req, res, insertOrDelete) {
  //data id for this rec
  const id = `data:Rec` + req.body.cig_id + `-` + req.body.rec_id;

  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = id + ` vocab:isPartOf data:CIG-` + req.body.cig_id + ` .`;

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
    `
            a   vocab:ClinicalRecommendation ;
            rdfs:label              "` +
    req.body.label +
    `"@en ;
            vocab:aboutExecutionOf  data:ActAdminister` +
    req.body.careAction_id +
    ` ;
            vocab:basedOn           data:CB` +
    req.body.belief_id +
    ` ;
            vocab:partOf            data:CIG-` +
    req.body.cig_id +
    ` ;
            vocab:strength          "` +
    (req.body.isRecommended == "true" ? `should` : `should-not`) +
    `" ;
            vocab:motivation        "` +
    (req.body.motivation ? req.body.motivation : "") +
    `"@en .
            data:CB` +
    req.body.belief_id +
    ` vocab:contribution     "` +
    req.body.contribution +
    `" .
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
    `_nanopub
            prov:generatedAtTime  "2020-03-01"^^xsd:dateTime ;
            prov:wasAttributedTo  data:` +
    req.body.author +
    ` .
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
            res.sendStatus(status2);
          }
        );
      } else {
        //didnt work. send first status back
        res.sendStatus(status);
      }
    }
  );
}

router.post("/rec/add", function (req, res, next) {
  action(req, res, config.INSERT);
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

  utils.sparqlDropGraphs(idCig, recUri, function (err,status) {
    res.sendStatus(err,status);
  });
});

////

router.post("/careAction/get", function (req, res, next) {
  var postData = "";

  if (req.body.rec_id) {
    postData = require("querystring").stringify({
      guideline_id: `CIG-` + req.body.guideline_id,
      rec_id:
        "http://anonymous.org/data/Rec" +
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
  var id = req.body.cig_id;
  var idCig;

  //separate lable id from dataset id
  if (id.startsWith(`CIG-`)) {
    idCig = id;
    id = id.substring(`CIG-`.length - 1);
  } else {
    idCig = `CIG-` + id;
  }

  const recURI = req.body.rec_URI
    ? req.body.rec_URI
    : "data:Rec" + id + "-" + req.body.rec_id;

  utils.getRecData(
    idCig,
    recURI,
    "beliefs",
    "transitions",
    "careActions",
    function (err, guidelineData) {
      if (err) {
        return next(err);
      }

      //if  data found in Object (we check), begin
      if (
        guidelineData.constructor === Object &&
        Object.entries(guidelineData).length != 0
      ) {
        //console.info(guidelineData);
        var recData = {
          id: req.body.rec_URI ? req.body.rec_URI : recURI,
          causationBeliefs: [],
        };
        var cbData = {
          author: "JDA",
        };
        var actData = {};
        var TrData = {
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

        var vars = guidelineData.head.vars;
        var bindings = guidelineData.results.bindings;

        if (bindings.length != 0) {
          //format data by looping through results
          for (let pos in bindings) {
            var bind = bindings[pos];
            //console.info(bind);

            for (var varPos in vars) {
              var value = vars[varPos] in bind ? bind[vars[varPos]].value : "";

              //for each heading, add a field
              switch (vars[varPos]) {
                case "partOf":
                  var extractedFrom = value.slice(30);
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
                  var type = value.slice(26);
                  TrData.situationTypes[0].value.code = type;
                  break;
                case "sitToId":
                  TrData.situationTypes[1].id = value;
                  //extract code
                  var type = value.slice(26);
                  TrData.situationTypes[1].value.code = type;
                  break;
                case "PropUri":
                  TrData.property.id = value;
                  //extract code
                  var type = value.slice(26);
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
                  var type = value.slice(27);
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
            }
          }

          //join data together
          cbData.transition = TrData;
          recData.careActionType = actData;
          recData.causationBeliefs[0] = cbData;

          res.send(recData);
        } else {
          res.send({});
        }
      } else {
        res.send({});
      }
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
    ` rdf:type vocab:subGuideline, owl:NamedIndividual ;
                         rdfs:label "` +
    req.body.description +
    `"@en ;
                         vocab:isSubGuidelineOf  data:CIG-` +
    req.body.guideline_id +
    ` .`;

  //var to construct the assignment of recs to a subguideline. initial whitespace to be kept
  var recDeclaration = " ";

  if (req.body.recs_ids) {
    //nanopublication is part of this subGuideline. contains  pred and object of resource
    const isPartOf =
      ` vocab:isPartOf   data:subCIG-` + req.body.subGuideline_id + ` .\n`;

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

module.exports = router;
