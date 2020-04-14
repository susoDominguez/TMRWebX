const express = require('express');
const router = express.Router();

const config = require('../lib/config');
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

  utils.sparqlUpdate("beliefs", "GRAPH " + head + "\nGRAPH " + body + "\nGRAPH " + provenance + "\nGRAPH " + publication, insertOrDelete, function (status) {

    res.sendStatus(status);

  });

}

router.post('/add', function (req, res) {

  action(req, res, config.INSERT);

});

router.post('/delete', function (req, res) {

  //action(req, res, config.DELETE);
  const belief_URI = "CB" + req.body.belief_id;
  const dataset_id = "beliefs"

  utils.sparqlDropGraphs(dataset_id, belief_URI, function (status) {

    res.sendStatus(status);

  });

});

router.post('/all/get/', function (req, res) {

  if (req.body.belief_id) {

    utils.sparqlGetResourcesFromNamedGraph("beliefs", "data:CB" + req.body.belief_id, function (beliefData) {

      res.send(beliefData);

    });
  } else {

    if (req.body.belief_URI) {
      //belief_URI
      utils.getBeliefData("beliefs", req.body.belief_URI, function (beliefData) {
        //if  data found in Object (we check), begin
        if (transitionData.constructor === Object && Object.entries(beliefData).length != 0) {

          var data = {
            id: req.body.belief_URI,
            author: "JDA"
          };

          var vars = actionResults.head.vars;
          var bindings = actionResults.results.bindings;

          //format data by looping through results
          for (let pos in bindings) {

            var bind = bindings[pos];

            for (var varPos in vars) {

              var value = bind[vars[varPos]].value;

              //for each heading, add a field
              switch (vars[varPos]) {
                case "freq":
                  data.probability = value;
                  break;
                case "strength":
                  data.evidence = value;
                  break;
                case "actAdmin":
                  data.actAdminUri = value;
                  break;
                case "Tr":
                  data.transitionUri = value;
                  break;
              }
            }
          }
          res.send(data);
        } else {
          res.send({});
        }
      });
    }
  }


});

module.exports = router;
