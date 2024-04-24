const express = require('express');
const router = express.Router();
//const request = require('request');

const config = require('../lib/config');
//const guidelines = require('../lib/prefixes');
const utils = require('../lib/utils');
const auxFunct = require('../lib/router_functs/guideline_functs')
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

/// GET 
router.post('/all/get/', async function(req, res, next) {

  const id = req.body.id ? "data:Tr"+req.body.id : "<"+req.body.uri+">" ;

  let {status,head_vars,bindings} = await utils.getTransitionData("transitions", id);

  if(status < 400 && bindings.length > 0) {
    let data = auxFunct.get_transition_object(head_vars, bindings[0]);
    return res.status(status).json(data);
  } else {
    return res.status(status).json({});
  }

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

  //, owl:NamedIndividual 
  const transition = `data:Tr` + req.body.id + ` rdf:type vocab:TransitionType ;
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

    req.body.snomedCode.split(",").forEach(function(code) {

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

  if ( req.body.icd10Code ) {
    compoundList += `;`;
                
    req.body.icd10Codes.split(",").forEach(function(code) {
                
      compoundList += `
        vocab:icd10Code   "` + code.trim() + `"^^xsd:string ;`
                
    });
      //this removes the last semicolon
      compoundList = compoundList.substring(0, compoundList.length - 1);
}

if ( req.body.snomedCode ) {
  compoundList += `;`;
              
  req.body.snomedCode.split(",").forEach(function(code) {
              
    compoundList += `
      vocab:snomedCode   "` + code.trim() + `"^^xsd:string ;`
              
  });
    //this removes the last semicolon
    compoundList = compoundList.substring(0, compoundList.length - 1);
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
