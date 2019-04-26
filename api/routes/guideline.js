const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

router.post('/create', function(req, res, next) {

  request.post({

    url: "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/$/datasets?dbType=tdb&dbName=CIG-" + req.body.guideline_id,
    headers: {
      Authorization: "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
    },

  }, function (error, response, body) {

    if(!req.body.description){
      req.body.description = `CIG-` + req.body.guideline_id
    }

    const description = `:CIG-` + req.body.guideline_id + ` {
        :CIG-` + req.body.guideline_id + ` rdf:type vocab:ClinicalGuideline, owl:NamedIndividual ;
            rdfs:label "` + req.body.description + `"@en .
    }`;

    utils.sparqlUpdate("CIG-" + req.body.guideline_id, description, config.INSERT, function(body) {

      console.log(body);
      res.sendStatus(200);

    });

  });

});

function action(req, res, insertOrDelete) {

  // Guideline format:
  const head = `:Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_head {
    :Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_nanopub
            a                           nanopub:Nanopublication ;
            nanopub:hasAssertion        :Rec` + req.body.guideline_id + `-` + req.body.rec_id + ` ;
            nanopub:hasProvenance       :Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_provenance ;
            nanopub:hasPublicationInfo  :Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_publicationinfo .
  }`

  const body = `:Rec` + req.body.guideline_id + `-` + req.body.rec_id + ` {
    :Rec` + req.body.guideline_id + `-` + req.body.rec_id + `
            a                       vocab:ClinicalRecommendation ;
            rdfs:label              "` + req.body.label  + `"@en ;
            vocab:aboutExecutionOf  :ActAdminister` + req.body.drug_id + ` ;
            vocab:basedOn           :CB` + req.body.belief_id + ` ;
            vocab:partOf            :CIG-` + req.body.guideline_id + ` ;
            vocab:strength          "` + req.body.should_or_shouldnot + `" .
  }`

  const provenance = `:Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_provenance {
    :Rec` + req.body.guideline_id + `-` + req.body.rec_id + `
            prov:wasDerivedFrom  <http://hdl.handle.net/10222/43703> .

    :Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_provenance
            a             oa:Annotation ;
            oa:hasBody    :Rec` + req.body.guideline_id + `-` + req.body.rec_id + ` ;
            oa:hasTarget  [ oa:hasSource  <http://hdl.handle.net/10222/43703> ] .
  }`

  const publication = `:Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_publicationinfo {
    :Rec` + req.body.guideline_id + `-` + req.body.rec_id + `_nanopub
            prov:generatedAtTime  "1922-12-28"^^xsd:dateTime ;
            prov:wasAttributedTo  :` + req.body.author + ` .
  }`

  utils.sparqlUpdate("CIG-" + req.body.guideline_id, head + " " + body + " " + provenance + " " + publication, insertOrDelete, function(status) {

    res.sendStatus(status);

  });

}

router.post('/add', function(req, res, next) {

  action(req, res, config.INSERT);

});

router.post('/delete', function(req, res, next) {

  action(req, res, config.DELETE);

});

//

router.post('/drug/get', function(req, res, next) {

  var postData=""
  
  if(req.body.rec_id)
  {
    postData = require('querystring').stringify({
      'guideline_id' : `CIG-` + req.body.guideline_id,
      'rec_id' : "http://anonymous.org/data/Rec"+req.body.guideline_id+"-"+req.body.rec_id
    });
  } else
  {
    postData = require('querystring').stringify({
      'guideline_id' : `CIG-` + req.body.guideline_id,
      'rec_URI' : req.body.rec_URI
    });
  }

  utils.callPrologServer("drug", postData, res, function(data) {

    if (!data) {
      res.sendStatus(400);
    } else {
      res.send(data);
    }

  });

});

router.post('/all/get/', function(req, res, next) {

  if(req.body.rec_id){
    utils.sparqlGraph("CIG-" + req.body.guideline_id, 
        "http://anonymous.org/data/Rec"+req.body.guideline_id+"-"+req.body.rec_id, function(guidelineData) {

    res.send(guidelineData);

    });
  } else {
    utils.sparqlGraph("CIG-" + req.body.guideline_id, req.body.rec_URI, function(guidelineData) {

    res.send(guidelineData);

  });
  }
  

});

module.exports = router;
