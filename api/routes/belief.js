const express = require('express');
const router = express.Router();

const config = require('../lib/config');
const { ErrorHandler } = require('../lib/errorHandler');
const utils = require('../lib/utils');

function action(req, res, insertOrDelete) {

  //data id for this belief
  const id = `data:CB` + req.body.belief_id;

  // Belief format:
  const head = id + `_head {
       ` + id + `_head
              a            nanopub:Nanopublication ;
              nanopub:hasAssertion          `+ id + ` ;
              nanopub:hasProvenance         `+ id + `_provenance ;
              nanopub:hasPublicationInfo    `+ id + `_publicationinfo .
  }`;


  const body = id + ` {
      data:ActAdminister` + req.body.careAct_cause_id + `
          vocab:causes 									data:Tr` + req.body.transition_effect_id + ` .
          `+ id + `
          a                             vocab:CausationBelief ;
          vocab:strength                "` + req.body.strength + `"^^xsd:string;
          vocab:frequency               "always"^^xsd:string.
  }`;

  const provenance = id + `_provenance {
          `+ id + `_provenance
          a                             oa:Annotation ;
          oa:hasBody                    `+ id + ` ;
          oa:hasTarget                  [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
          `+ id + `
          prov:wasDerivedFrom           <http://hdl.handle.net/10222/43703> .
  }`;

  const publication = id + `_publicationinfo {
          `+ id + `_head
          prov:generatedAtTime          "2019-01-01T13:14:15"^^xsd:dateTime ;
          prov:wasAttributedTo          data:` + req.body.author + `.
  }`;

  utils.sparqlUpdate("beliefs", "GRAPH " + head + "\nGRAPH " + body + "\nGRAPH " + provenance + "\nGRAPH " + publication, insertOrDelete, function (err, status) {

    res.status(status).end();

  });
}

router.post('/add', function (req, res) {

  action(req, res, config.INSERT);

});

router.post('/delete', function (req, res) {

  //action(req, res, config.DELETE);
  const belief_URI = (req.body.belief_id ? req.body.belief_id : "data:CB" + req.body.belief_id);
  const dataset_id = "beliefs"

  utils.sparqlDropGraphs(dataset_id, belief_URI, function (err, status) {

    res.status(status).end();

  });

});

router.post('/all/get/', function (req, res, next) {

  //belief_URI
  utils.getBeliefData("beliefs", ( req.body.belief_URI ? "<" + req.body.belief_URI + ">" : "data:CB" + req.body.belief_id), "transitions", "careActions", function (err, beliefData) {
    
    if (err || !beliefData || beliefData.constructor !== Object || Object.entries(beliefData).length === 0) {
      next(err);
      return;
    }
    //otherwise
  try {
    
      var cbData = {
        id: req.body.belief_URI,
        author: "JDA"
      };
      var actData = {};
      var TrData = {
        situationTypes: [
          {
            "type": "hasTransformableSituation",
            "value": {}
          },
          {
            "type": "hasExpectedSituation",
            "value": {}
          }
        ],
        property: {}
      };

      var vars = beliefData.head.vars;
      var bindings = beliefData.results.bindings;

      //format data by looping through results
      for (let pos in bindings) {

        var bind = bindings[pos];

        for (var varPos in vars) {

          var value = bind[vars[varPos]].value;

          //for each heading, add a field
          switch (vars[varPos]) {
            case "freq":
              cbData.probability = value;
              break;
            case "strength":
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
            case "propUri":
              //extract code
              var type = value.slice(30);
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
            case "actType":
              //extract code
              var type = value.slice(27);
              actData.code = type;
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
            case "actLabel":
              actData.drugLabel = value;
              break;
          }
        }
      }

      //join data together
      cbData.transition = TrData;
      cbData.careActionType = actData;

      res.send(cbData);
    } catch (error) {
      next(new ErrorHandler(500, err));
    }
  });
});

module.exports = router;
