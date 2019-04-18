const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

function action(req, res, insertOrDelete) {

  // Belief format:
  const head = `data:CB` + req.body.belief_id + `_head {
      data:CB` + req.body.belief_id + `_nanopub
          a                             nanopub:Nanopublication ;
          nanopub:hasAssertion          data:CB` + req.body.belief_id + ` ;
          nanopub:hasProvenance         data:CB` + req.body.belief_id + `_provenance ;
          nanopub:hasPublicationInfo    data:CB` + req.body.belief_id + `_publicationinfo .
  }`;

  const body = `data:CB` + req.body.belief_id + ` {
      data:ActAdminister` + req.body.drug_cause_id + `
          vocab:causes 									data:Tr` + req.body.transition_effect_id + ` .
      data:CB` + req.body.belief_id + `
          a                             vocab:CausationBelief ;
          vocab:strength                "` + req.body.strength + `"^^xsd:string;
          vocab:frequency               "` + req.body.frequency + `"^^xsd:string.
  }`;

  const provenance = `data:CB` + req.body.belief_id + `_provenance {
      data:CB` + req.body.belief_id + `_provenance
          a                             oa:Annotation ;
          oa:hasBody                    data:CB` + req.body.belief_id + ` ;
          oa:hasTarget                  [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
      data:CB` + req.body.belief_id + `
          prov:wasDerivedFrom           <http://hdl.handle.net/10222/43703> .
  }`;

  const publication = `data:CB` + req.body.belief_id + `_publicationinfo {
      data:CB` + req.body.belief_id + `_nanopub
          prov:generatedAtTime          "1922-12-28"^^xsd:dateTime ;
          prov:wasAttributedTo          data:` + req.body.author + `.
  }`;

  utils.sparqlUpdate("beliefs", head + " " + body + " " + provenance + " " + publication, insertOrDelete, function(status) {

    res.sendStatus(status);

  });

}

router.post('/add', function(req, res, next) {

  action(req, res, config.INSERT);

});

router.post('/delete', function(req, res, next) {

  action(req, res, config.DELETE);

});

router.post('/all/get/', function(req, res, next) {

  utils.sparqlGraph("beliefs", req.body.belief_full_id, function(beliefData) {

    res.send(beliefData);

  });

});

module.exports = router;
