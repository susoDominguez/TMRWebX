const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');

function postTransition(transitionData, res, insertOrDelete) {

  utils.sparqlUpdate('transitions', transitionData, insertOrDelete, function(err, status) {

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
      vocab:snomedCode   "` + code.trim() + `"^^xsd:string ;`

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

  var property = `data:Prop` + req.body.property_id + ` rdf:type  vocab:TropeType, owl:NamedIndividual ;
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
        vocab:snomedCode   "` + code.trim() + `"^^xsd:string ;`
                
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

    utils.getTransitionData("transitions", ( req.body.transition_URI ? "<"+req.body.transition_URI+">" : "data:Tr"+req.body.transition_id), function(err, transitionData) {

      if(err) {
        res.sendStatus(404).send(transitionData);
        return;
      }

      //if  data found in Object (we check), begin
      if(transitionData.constructor === Object && Object.entries(transitionData).length != 0) {

      var data = { id: req.body.transition_URI,
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
                    property: {} };

      var vars = transitionData.head.vars;
      var bindings = transitionData.results.bindings;
      
      //format data by looping through results
      for(let pos in bindings){

        var bind = bindings[pos];

        for(var varPos in vars){

          var value = bind[vars[varPos]].value;

          //for each heading, add a field
          switch (vars[varPos]) {
            case "sitFromId":
              data.situationTypes[0].id = value;
              //extract code
              var type = value.slice(26);
              data.situationTypes[0].value.code= type;
              break;
            case "sitToId":
              data.situationTypes[1].id = value;
              //extract code
              var type = value.slice(26);
              data.situationTypes[1].value.code= type;
              break;
            case "propUri":
              //extract code
              var type = value.slice(30);
              data.property.code = type;
              break;
            case "sitFromLabel":
              data.situationTypes[0].value.display =  value;
              break;
            case "sitToLabel":
              data.situationTypes[1].value.display = value;
              break;
            case "propTxt":
              data.property.display = value;
              break;
            case "deriv":
              data.effect = value;
              break;
          }
        }
    }
    res.send(data);
      } else {
        res.send({});
      }
      
    });

});
  



router.post( '/situation/all/get/', function(req, res, next) {

    utils.sparqlGetPreds_Objcts("transitions", ( req.body.situation_URI ? "<"+req.body.situation_URI+">" : "data:Sit"+req.body.situation_id), 
     function(err, situationData) {

      res.send(situationData);
  
    });
});

router.post('/property/all/get/', function(req, res, next) {

    utils.sparqlGetPreds_Objcts("transitions", ( req.body.property_URI ? "<"+req.body.property_URI+">" : "data:Prop"+req.body.property_id ), 
      function(err,propertyData) {

      res.send(propertyData);
  
    });
  

});

module.exports = router;
