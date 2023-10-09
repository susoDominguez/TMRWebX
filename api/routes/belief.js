const express = require("express");
const router = express.Router();
const logger = require("../config/winston");
const config = require("../lib/config");
const { ErrorHandler } = require("../lib/errorHandler");
const utils = require("../lib/utils");

function action(req, res, insertOrDelete) {
  //data id for this belief
  const id = `data:CB` + req.body.belief_id;
  const date = new Date().toJSON();
  let sources = "";

  // Belief format:
  const head =
    id +
    `_head {
       ` +
    id +
    `_head
              a            nanopub:Nanopublication ;
              nanopub:hasAssertion          ` +
    id +
    ` ;
              nanopub:hasProvenance         ` +
    id +
    `_provenance ;
              nanopub:hasPublicationInfo    ` +
    id +
    `_publicationinfo .
  }`;

  const body =
    id +
    ` {
      data:ActAdminister` +
    req.body.careAct_cause_id +
    `
          tmr:causes 									data:Tr` +
    req.body.transition_effect_id +
    ` .
          ` +
    id +
    `
          a                             tmr:CausationBelief ;
          tmr:strength                tmr:` +
    req.body.strength +
    ` ;
          tmr:frequency               tmr:` +
    req.body.frequency +
    ` .
  }`;

  if (req.body.derivedFrom) {
    sources = `  prov:wasDerivedFrom  `;

    req.body.derivedFrom.split(",").forEach(function (code) {
      sources += ` <` + code + `> ,`;
    });
    //this removes the last coma
    sources = sources.substring(0, sources.length - 1);
  }

  const provenance =
    id +
    `_provenance {
          ` +
    id +
    `_provenance
          a                             oa:Annotation ;
          oa:hasBody                    ` +
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
    "beliefs",
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
      res.status(status).end();
    }
  );
}

router.post("/add", function (req, res) {
  action(req, res, config.INSERT);
});

router.post("/delete", function (req, res) {
  //action(req, res, config.DELETE);
  const belief_URI = req.body.belief_id
    ? req.body.belief_id
    : "data:CB" + req.body.belief_id;
  const dataset_id = "beliefs";

  utils.sparqlDropGraphs(dataset_id, belief_URI, function (err, status) {
    res.status(status).end();
  });
});

router.post("/all/get/", function (req, res, next) {
  const prefix = "http://anonymous.org/tmr/data/";
  const id = req.body.uri ? req.body.uri : prefix + req.body.id;

  //belief URI
  utils.getBeliefData(
    "beliefs",
    `<${id}>`,
    "transitions",
    "careActions",
    function (err, beliefData) {
      if (
        err ||
        !beliefData ||
        beliefData.constructor !== Object ||
        Object.entries(beliefData).length === 0
      ) {
        next(new ErrorHandler(500, err));
        return;
      }
      //otherwise

      var cbData = {
        id: id
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

      var vars = beliefData.head.vars;
      let bindings = beliefData.results.bindings[0];
      //throw error if no values in bindings
      if(!bindings) next( new ErrorHandler(500, 'no bindings were sent back as result to the current SPARQL query'));

      //format data by looping through head vars
      for (let pos in vars) {

          //variable name
          let headVar = vars[pos];

          //check there is a corresponding binding, if not, next head var
          if (!(headVar && bindings.hasOwnProperty(headVar)) ) continue;

          //otherwise, retrieve value
          let value = bindings[headVar].value;
          let type;

          //for each heading, add a field
          switch (headVar) {
            case "freq":
               type = value.slice(25);
              cbData.probability = type;
              break;
            case "strength":
               type = value.slice(25);
              cbData.evidence = type;
              break;
            case "TrUri":
              TrData.id = value;
              break;
            case "sitFromId":
              TrData.situationTypes[0].id = value;
              //extract code
               type = value.slice(30);
              TrData.situationTypes[0].value.code = type;
              break;
            case "sitToId":
              TrData.situationTypes[1].id = value;
              //extract code
               type = value.slice(30);
              TrData.situationTypes[1].value.code = type;
              break;
            case "propUri":
              //extract code
               type = value.slice(30);
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
              type = value.slice(25);
              TrData.effect = type;
              break;
            case "actId":
              actData.id = value;
              break;
            case "adminLabel":
              actData.display = value;
              break;
            case "actType":
              //extract code
               type = value.slice(25);
              actData.code = type;
              actData.requestType = 0; //for drugT and DrugCat
              //check for therapy
              if (type.startsWith("NonDrugT")) {
                actData.requestType = 1;
              } else {
                //check for vaccine
                if (type.startsWith("VaccineT")) {
                  actData.requestType = 2;
                }
              }
              break;
            case "actLabel":
              actData.drugLabel = value;
              break;
          }

          //join data together
          cbData.transition = TrData;
          cbData.careActionType = actData;
          

      } //end forloop
      res.send(cbData);
    }
  );
});

module.exports = router;
