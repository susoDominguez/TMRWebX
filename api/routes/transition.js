const express = require('express');
const router = express.Router();
//const request = require('request');

const config = require('../lib/config');
//const guidelines = require('../lib/prefixes');
const utils = require('../lib/utils');
const { ErrorHandler } = require('../lib/errorHandler');

router.post('/property/add', async function(req, res, next) {

  let def = actionProperty(req);
  let {data, status} = await postTransition(def, config.INSERT);

  return res.status(status).send(data);

});

router.post('/property/delete', async function(req, res, next) {
  let {data, status} = await postTransition(req.body.id, config.DELETE, 'Prop');

  return res.status(status).send(data);

});

router.post('/all/get/', function(req, res, next) {

  const data = 'http://anonymous.org/tmr/data/';
  
  const id = req.body.uri ? "<"+req.body.uri+">" : "<"+ data + req.body.id+">";

    utils.getTransitionData("transitions",id, function(err, transitionData) {

      if(err) {
        res.status(404).send(transitionData);
        return;
      }

      //if  data found in Object (we check), begin
      if(transitionData.constructor === Object && Object.entries(transitionData).length != 0) {

      var data = { id: id  ,
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
              var type = value.slice(30);
              data.situationTypes[0].value.code= type;
              break;
            case "sitToId":
              data.situationTypes[1].id = value;
              //extract code
              var type = value.slice(30);
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
              var type = value.slice(25);
              data.effect = type;
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

  utils.sparqlGetPreds_Objcts("transitions", ( req.body.uri ? "<"+req.body.uri+">" : "data:Sit"+req.body.id), 
   function(err, situationData) {
    if(err){
      next(new ErrorHandler(500, err));
    } else {
      res.send(situationData);
    }

  });
});

router.post('/property/all/get/', function(req, res, next) {

  utils.sparqlGetPreds_Objcts("transitions", ( req.body.property_URI ? "<"+req.body.property_URI+">" : "data:Prop"+req.body.property_id ), 
    function(err,propertyData) {

    res.send(propertyData);

  });


});

router.post('/situation/compound/add', async function(req, res, next) {

  let def = actionSituationComplex(req);;

  let {data, status} = await postTransition(def, config.INSERT);

  return res.status(status).send(data);

});

router.post('/situation/add', async function(req, res, next) {

  let def = actionSituation(req, res, config.INSERT);

  let {data, status} = await postTransition(def, config.INSERT);

  return res.status(status).send(data);

});

router.post('/situation/delete', async function(req, res, next) {

  let {data, status} = await postTransition(req.body.id, config.DELETE, 'Sit');

  return res.status(status).send(data);

});

router.post('/situation/compound/delete', async function(req, res, next) {

  let {data, status} = await postTransition(req.body.id, config.DELETE, 'Sit');

  return res.status(status).send(data);

});

router.post('/add', async function(req, res, next) {

  let def = action(req, res, config.INSERT);

  let {data, status} = await postTransition(def, config.INSERT);

  return res.status(status).send(data);

});

router.post('/delete', async function(req, res, next) {

  let {data, status} = await postTransition(req.body.id, config.DELETE, 'Tr');

  return res.status(status).send(data);

});

async function postTransition(transitionData, insertOrDelete, type = '') {

  const del_str = ` data:${type+transitionData} ?p ?o . `;

  let content = insertOrDelete === config.INSERT ? transitionData : del_str;
  
  let sparql_query_str = `${insertOrDelete} ${insertOrDelete === config.INSERT ? "DATA" : ""} { ${content} } `;

  if(insertOrDelete === config.DELETE){
    let query_post_str = ` WHERE {  ${del_str}  }`;
    sparql_query_str += query_post_str ;
  }

   return await utils.sparqlUpdate('transitions', sparql_query_str); 

}

function action(req, res, insertOrDelete) {

  const transition = `data:Tr` + req.body.id + ` rdf:type vocab:TransitionType, owl:NamedIndividual ;
                  vocab:hasTransformableSituation data:Sit` + req.body.pre_situation_id + ` ;
                  vocab:hasExpectedSituation data:Sit` + req.body.post_situation_id + ` ;
                  vocab:derivative   vocab:` + req.body.derivative + ` ;
                  vocab:affects data:Prop` + req.body.affected_property_id + ` .`

  return transition;

}

function actionSituation(req) {

  var situationDef = `data:Sit` + req.body.id + ` rdf:type vocab:SituationType, owl:NamedIndividual ;
               rdfs:label "` + req.body.label + `"@en ; 
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

  if ( req.body.snomedCode ) {

    situationDef += `;`;

    req.body.snomedCodes.split(",").forEach(function(code) {

      situationDef += `
      vocab:snomedCode   "` + code.trim() + `"^^xsd:string ;`

    });
    //this removes the last semicolon
    situationDef = situationDef.substring(0, situationDef.length - 1);
  }

  situationDef += `.`

  return situationDef ;

}

function actionSituationComplex(req) {

  let situationDef = `data:Sit` + req.body.id + ` rdf:type vocab:CompoundSituationType, owl:NamedIndividual;
               rdfs:label "` + req.body.label + `"@en ; `
  
  let compoundList = ` 
               vocab:`+req.body.connective+` `;

  if ( req.body.situation_id_list ) {

    req.body.situation_id_list.split(",").forEach(function(code) {

      compoundList += `data:Sit`+code.trim() + ` ,`

    });
     //this removes the last comma
     compoundList = compoundList.substring(0, compoundList.length - 1);
  } else {
    throw ErrorHandler(500, `list of situations missing`);
  }

  compoundList += `.`

  return situationDef + compoundList;

}



//////////////
//////////////

function actionProperty(req) {

  var property = `data:Prop` + req.body.id + ` rdf:type  vocab:TropeType, owl:NamedIndividual ;
                    rdfs:label "` + req.body.label + `"@en `;

  if ( req.body.icd10Code ) {
      property += `;`;
                  
      req.body.icd10Codes.split(",").forEach(function(code) {
                  
        property += `
          vocab:icd10Code   "` + code.trim() + `"^^xsd:string ;`
                  
      });
        //this removes the last semicolon
      property = property.substring(0, property.length - 1);
  }

  if ( req.body.snomedCode ) {
    property += `;`;
                
    req.body.snomedCode.split(",").forEach(function(code) {
                
      property += `
        vocab:snomedCode   "` + code.trim() + `"^^xsd:string ;`
                
    });
      //this removes the last semicolon
    property = property.substring(0, property.length - 1);
}
  
  //add final dot on RDF          
    property += `.`

  return property;

}


module.exports = router;
