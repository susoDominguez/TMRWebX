const express = require('express');
const router = express.Router();
const request = require('request');
const bodyParser = require('body-parser')

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

/**
 * Create a persistent or in-memory CIG
 */
router.post('/create', bodyParser.json(), function(req, res, next) {

  request.post({

    url: "http://" + config.JENA_HOST + ":" + config.JENA_PORT + 
          (req.body.IsPersistent ? "/$/datasets?dbType=tdb&dbName=CIG-" : "/$/datasets?dbType=mem&dbName=CIG-")
           + req.body.guideline_id,
    headers: {
      Authorization: "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
    },

  }, function (error, response, body) {

    if(error){
      console.log(error);
    }

    if(!req.body.description){
      req.body.description = `Guideline CIG-` + req.body.guideline_id
    }

    const description = `data:CIG-` + req.body.guideline_id + ` rdf:type vocab:ClinicalGuideline, owl:NamedIndividual ;
                     rdfs:label "` + req.body.description + `"@en .`;

    utils.sparqlUpdate("CIG-" + req.body.guideline_id, description, config.INSERT, function(body) {

      //console.log(response);
      console.log(body);
      res.sendStatus(200);

    });

  });

});

/**
 * Create a persistent or in-memory CIG
 */
router.post('/delete', function(req, res, next) {

  request.delete({

    url: "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/$/datasets/CIG-" + req.body.guideline_id,
    headers: {
      Authorization: "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
    },

  }, function (error, response, body) {

    if(error){
      console.log(error);
    } else {
      console.log(body);
      res.sendStatus(200);
    }

    /*
    utils.sparqlUpdate("CIG-" + req.body.guideline_id, "", config.DELETE, function(body) {

      //console.log(response);
      console.log(body);
      res.sendStatus(200);

    });*/

  });

});


function action(req, res, insertOrDelete) {

  //data id for this rec
  const id = `data:Rec` + req.body.guideline_id + `-` + req.body.rec_id ;

  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = id + ` vocab:isPartOf data:CIG-`+ req.body.guideline_id + ` .`;

  // Guideline format:
  const head = id + `_head { 
        ` + id + `_head
            a                           nanopub:Nanopublication ;
            nanopub:hasAssertion        `+ id  + ` ;
            nanopub:hasProvenance       `+ id  + `_provenance ;
            nanopub:hasPublicationInfo  `+ id  + `_publicationinfo .
  }`;

    
  const body = id + ` {
    ` + id + `
            a                       vocab:ClinicalRecommendation ;
            rdfs:label              "` + req.body.label  + `"@en ;
            vocab:aboutExecutionOf  data:ActAdminister` + req.body.careAction_id + ` ;
            vocab:basedOn           data:CB` + req.body.belief_id + ` ;
            vocab:partOf            data:CIG-` + req.body.guideline_id + ` ;
            vocab:strength          "` + req.body.should_or_shouldnot + `" ;
            vocab:motivation        "` + ( (!req.body.motivation) ? "" : req.body.motivation) + `"@en .
            data:CB` + req.body.belief_id +
            ` vocab:contribution     "` + req.body.contribution+ `" .
  }`;

  const provenance = id + `_provenance {
    ` + id + `
            prov:wasDerivedFrom  <` + (req.body.source? req.body.source : "unknown")+`> .

    ` + id + `_provenance
            a             oa:Annotation ;
            oa:hasBody    ` + id + ` ;
            oa:hasTarget  [ oa:hasSource  <http://hdl.handle.net/10222/43703> ] .
  }`;

  const publication = id + `_publicationinfo {
      ` + id + `_nanopub
            prov:generatedAtTime  "2020-03-01"^^xsd:dateTime ;
            prov:wasAttributedTo  data:` + req.body.author + ` .
  }`;

  utils.sparqlUpdate("CIG-" + req.body.guideline_id, "GRAPH " +head + "\nGRAPH " + body + "\nGRAPH " + provenance + "\nGRAPH " + publication,
   insertOrDelete, function(status) {
      if(status === 200){
        //add assertion id to default graph as part of CIG
        utils.sparqlUpdate("CIG-" + req.body.guideline_id, id2CIG,
                    insertOrDelete, function(status2) {
                      res.sendStatus(status2);  
              });
      } else {
        //didnt work. send first status back
        res.sendStatus(status);
      }
  });

}

router.post('/add', function(req, res, next) {

  action(req, res, config.INSERT);

});

router.post('/delete', function(req, res, next) {

  //action(req, res, config.DELETE);
  const guideline_URI = "CIG-" + req.body.guideline_id;
  const rec_URI = "Rec" + req.body.guideline_id + "-" + req.body.rec_id;

  utils.sparqlDropGraphs( guideline_URI, rec_URI, function(status) {
    
      res.sendStatus(status);
    
 });

});

////

router.post('/careAction/get', function(req, res, next) {

  var postData=""
  
  if(req.body.rec_id)
  {
    postData = require('querystring').stringify({
      'guideline_id' : `CIG-` + req.body.guideline_id,
      'rec_id' : "http://anonymous.org/data/Rec"+req.body.guideline_id+"-"+req.body.rec_id
    });
  } else
  {  //this one will be more accurate when combining guidelines
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

router.post('/rec/all/get/', function(req, res, next) {

  const cigId = (!req.body.guideline_id) ? req.body.cig_id : ("CIG-" + req.body.cig_label);
 
  const recURI = (!req.body.rec_id) ? req.body.rec_URI : ("data:Rec"+req.body.cig_label+"-"+req.body.rec_id);

  utils.getRecData(cigId, recURI, function(guidelineData) {
    res.send(guidelineData);
  });
});

///create subguideline by referencing assertion  resources which are same as assertion graph names in main guideline
router.post('/subGuideline/add', function(req, res, next) {
  actionSubguideline(req, res, config.INSERT);
});

//delete subguideline along with its components
router.post('/subGuideline/delete', function(req, res, next) {
  actionSubguideline(req, res, config.DELETE);
});

function actionSubguideline(req, res, insertOrDelete) {

  if(!req.body.description) { req.body.description = "subGuideline " + req.body.subGuideline_id}

    // SubGuideline declaration:
    const description = `data:subCIG-` + req.body.subGuideline_id + ` rdf:type vocab:subGuideline, owl:NamedIndividual ;
                         rdfs:label "` + req.body.description + `"@en ;
                         vocab:isSubGuidelineOf  data:CIG-` + req.body.guideline_id + ` .`;
     
    //var to construct the assignment of recs to a subguideline. initial whitespace to be kept
    var recDeclaration =  " ";

    if(req.body.recs_ids){
      
      //nanopublication is part of this subGuideline. contains  pred and object of resource
      const isPartOf = ` vocab:isPartOf   data:subCIG-` +  req.body.subGuideline_id + ` .\n`;
    
      req.body.recs_ids.split(",").forEach(function(recId) {

        recDeclaration += (`data:Rec` + req.body.guideline_id + `-`+ recId.trim() + isPartOf);
      });
    }

  
    utils.sparqlUpdate("CIG-" + req.body.guideline_id, description + recDeclaration, insertOrDelete, function(status) {
      res.sendStatus(status);
    });
  
  }

module.exports = router;
