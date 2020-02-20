const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

function postTransition(transitionData, res, insertOrDelete) {

  utils.sparqlUpdate('transitions', transitionData, insertOrDelete, function(status) {

    res.sendStatus(status);

  });

}

function action(req, res, insertOrDelete) {

  const transition = `data:Tr` + req.body.transition_id + ` rdf:type vocab:TransitionType, owl:NamedIndividual ;
                  vocab:hasTransformableSituation data:Sit` + req.body.pre_situation_id + ` ;
                  vocab:hasExpectedSituation data:Sit` + req.body.post_situation_id + ` ;
                  vocab:derivative   "` + req.body.derivative + `" ;
                  vocab:affects data:Prop` + req.body.affected_property_id + ` .`

  postTransition(transition, res, insertOrDelete);

}

router.post('/add', function(req, res, next) {

  action(req, res, config.INSERT);

});

router.post('/delete', function(req, res, next) {

  action(req, res, config.DELETE);

});

function actionSituation(req, res, insertOrDelete) {

  var situationDef = `data:Sit` + req.body.situation_id + ` rdf:type vocab:SituationType, owl:NamedIndividual;
              rdfs:label "` + req.body.situation_label + `"@en ; 
               vocab:stateOf  "`+ req.body.stateOfproperty +`" `

  if ( req.body.umlsCodes ) {

    situationDef += `;`;

    req.body.umlsCodes.split(",").forEach(function(code) {

      situationDef += `
      vocab:umlsCode   "` + code.trim() + `"^^xsd:string ;`

    });
     //this removes the last semicolon
    situationDef = situationDef.substring(0, situationDef.length - 1);
  }

  if ( req.body.atcCodes ) {

    situationDef += `;`;

    req.body.atcCodes.split(",").forEach(function(code) {

      situationDef += `
      vocab:atcCode   "` + code.trim() + `"^^xsd:string ;`

    });
    //this removes the last semicolon
    situationDef = situationDef.substring(0, situationDef.length - 1);
  }

  if ( req.body.icd10Codes ) {

    situationDef += `;`;

    req.body.icd10Codes.split(",").forEach(function(code) {

      situationDef += `
      vocab:icd10Code   "` + code.trim() + `"^^xsd:string ;`

    });
    //this removes the last semicolon
    situationDef = situationDef.substring(0, situationDef.length - 1);
  }

  if ( req.body.snomedCodes ) {

    situationDef += `;`;

    req.body.snomedCodes.split(",").forEach(function(code) {

      situationDef += `
      vocab:snomedCodes   "` + code.trim() + `"^^xsd:string ;`

    });
    //this removes the last semicolon
    situationDef = situationDef.substring(0, situationDef.length - 1);
  }

  situationDef += `.`

  postTransition(situationDef, res, insertOrDelete);

}

router.post('/situation/add', function(req, res, next) {

  actionSituation(req, res, config.INSERT);

});

router.post('/situation/delete', function(req, res, next) {

  actionSituation(req, res, config.DELETE);

});

//////////////
//////////////

function actionProperty(req, res, insertOrDelete) {

  const property = `data:Prop` + req.body.property_id + ` rdf:type  vocab:TropeType, owl:NamedIndividual ;
                    rdfs:label "` + req.body.property_label + `"@en `;

  if ( req.body.icd10Codes ) {
      property += `;`;
                  
      req.body.icd10Codes.split(",").forEach(function(code) {
                  
        property += `
          vocab:icd10Code   "` + code.trim() + `"^^xsd:string ;`
                  
      });
        //this removes the last semicolon
      property = property.substring(0, property.length - 1);
  }

  if ( req.body.snomedCodes ) {
    property += `;`;
                
    req.body.snomedCodes.split(",").forEach(function(code) {
                
      property += `
        vocab:snomedCodes   "` + code.trim() + `"^^xsd:string ;`
                
    });
      //this removes the last semicolon
    property = property.substring(0, property.length - 1);
}
  
  //add final dot on RDF          
    property += `.`

  postTransition(property, res, insertOrDelete);

}

router.post('/property/add', function(req, res, next) {

  actionProperty(req, res, config.INSERT);

});

router.post('/property/delete', function(req, res, next) {

  actionProperty(req, res, config.DELETE);

});

//

router.post('/all/get/', function(req, res, next) {

  if(req.body.transition_URI){
    utils.sparqlGetPreds_Objcts("transitions", ":"+req.body.transition_URI, function(transitionData) {

      res.send(transitionData);
  
    });
  } else{ 
    utils.sparqlGetPreds_Objcts("transitions", "data:Tr"+req.body.transition_id, function(transitionData) {

      res.send(transitionData);
    });
  }
});
  



router.post('/situation/all/get/', function(req, res, next) {

  if(req.body.situation_URI){
    utils.sparqlGetPreds_Objcts("transitions", ":"+req.body.situation_URI, function(situationData) {

      res.send(situationData);
  
    });
  } else{
    utils.sparqlGetPreds_Objcts("transitions", "data:Sit"+req.body.situation_id, function(situationData) {

      res.send(situationData);
  
    });
  }
  

});

router.post('/property/all/get/', function(req, res, next) {

  if(req.body.property_URI){
    utils.sparqlGetPreds_Objcts("transitions", ":"+req.body.property_URI, function(propertyData) {

      res.send(propertyData);
  
    });
  } else{
    utils.sparqlGetPreds_Objcts("transitions", "data:Sit"+req.body.property_id, function(propertyData) {

      res.send(propertyData);
  
    });
  }
  

});

module.exports = router;
