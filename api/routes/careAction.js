const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');


function postDrugs(careActData, insertOrDelete, callback) {

  utils.sparqlUpdate('careActions', careActData, insertOrDelete, function(status) {

    callback(status);

  });

}

router.post('/drug/individual/add', function(req, res) {

  careActDef(req, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/nondrug/individual/add', function(req, res) {

  careActDef(req, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/individual/delete', function(req, res) {

  careActDef(req, config.DELETE, function(status) {

    res.sendStatus(status);

  });

});

router.post('/drug/category/add', function(req, res) {

  drugCatCareAction(req, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/drug/category/delete', function(req, res) {

  drugCatCareAction(req, config.DELETE, function(status) {

    res.sendStatus(status);

  });

});

router.post('/effect/get', function(req, res) {

  var postData = ""

  if(req.body.drugCat_id){
    postData = require('querystring').stringify({
     'drugCat_id' : "http://anonymous.org/data/DrugCat"+req.body.drugCat_id
     });
  } 
  else
  {
     if (req.body.drugT_id){
        postData = require('querystring').stringify({
          'drugT_id' : "http://anonymous.org/data/DrugT"+req.body.drugT_id
          });
    } 
    else {
      if (req.body.nonDrugT_id) {
        postData = require('querystring').stringify({
          'nonDrugT_id' : "http://anonymous.org/data/NonDrugT"+req.body.nonDrugT_id
          });
      } else {
        postData = require('querystring').stringify({
          'act_URI' : req.body.act_URI
            });
        }
      }
         
  }

  utils.callPrologServer("drugeffects", postData, res, function(data) {

    res.send(data);

  });

});

router.post('/all/get/', function(req, res) {

  if(req.body.drugCat_id){

    utils.sparqlGetPreds_Objcts("careActions", "http://anonymous.org/data/DrugCat"+req.body.drugCat_id, function(drugData) {

    res.send(drugData);

    });
  } else
  { 
    if (req.body.drugT_id){
    utils.sparqlGetPreds_Objcts("careActions", "http://anonymous.org/data/DrugT"+req.body.drugT_id, function(drugData) {

      res.send(drugData);
  
      });
    } else {
      if (req.body.nonDrugT_id) {
        utils.sparqlGetPreds_Objcts("careActions", "http://anonymous.org/data/NonDrugT"+req.body.nonDrugT_id, function(drugData) {

      res.send(drugData);
  
      });
      } else {
        utils.sparqlGetPreds_Objcts("careActions", req.body.drug_URI, function(drugData) {

          res.send(drugData);
      
          });
      }
     
    }
  } 
  

});

//////////////////////////////

//Defines drug types and categories, providing an english label. 
function drugDef(typeOrCat, id, label) {
  return drug = 
  `data:Drug` + typeOrCat  + id + ` a vocab:DrugType, owl:NamedIndividual ;
                                     rdfs:label "` + label + `"@en `;
                  
}

//Defines non-drug related care actions
function nonDrugDef(typeOrCat, id, label) {
  return action = 
  `data:NonDrug` + typeOrCat + id + ` a vocab:NonDrugType, owl:NamedIndividual ;
                                     rdfs:label "` + label + `"@en `
}

////////////////

//Administration action care general to both drug type and category
function drugAdminActDef(typeOrCat, id, label) {

  var drugAdministration =
   `data:ActAdminister` + id + ` a vocab:DrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "Administer ` + label + `"@en ;
                               vocab:administrationOf data:Drug` + typeOrCat + id;

  return drugAdministration;
}

// Administration Action Care specialisation of a drugT from a drugT.
function adminActSub(id) {
  return ` ;
             vocab:subsumes data:ActAdminister` + id;
}

//////
////////////////

//Administration non drug action care 
function nonDrugAdminActDef(typeOrCat, id, actLabel) {

  var nonDrugAdmin =
   `data:ActAdminister` + id + ` a vocab:NonDrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "` + actLabel + `"@en ;
                               vocab:applicationOf data:NonDrug` + typeOrCat + id;

  return nonDrugAdmin;
}

//defines the insertion of clinical codes both in drugT and nonDrugT. Also in drug categories
function insertCodes(req){
  careAction="";

  if ( req.body.icd10Codes ) {
    careAction += `;`;
                
    req.body.icd10Codes.split(",").forEach(function(code) {
                
      careAction += `
        vocab:icd10Code   "` + code.trim() + `"^^xsd:string ;`
                
    });
      //this removes the last semicolon
      careAction = careAction.substring(0, careAction.length - 1);
}

  return careAction;
}

//defines ALL DrugT and also DrugT Admin. Also non drug careActT and careActT Admin
function careActDef(req, insertOrDelete, callback) {
  var action=""

  var careAdmin = ""


  if(req.body.nonDrugAct_label){
    //it is not a drug
    if (req.body.nonDrug_label) {
       action = nonDrugDef("T", req.body.nonDrug_id, req.body.nonDrug_label) 
    } else {
       action = nonDrugDef("T", req.body.nonDrug_id, req.body.nonDrug_id)
    }

    action += insertCodes(req) + ` .`
    
     careAdmin = nonDrugAdminActDef("T", req.body.nonDrug_id, req.body.nonDrugAct_label);

    if ( req.body.subsumed_nonDrug_id ) {

      careAdmin += adminActSub(req.body.subsumed_nonDrug_id);
    }
  } else {
     // Individual drug format:
     if (req.body.drug_label) {
          action = drugDef("T", req.body.drug_id, req.body.drug_label) 
                  + insertCodes(req) + ` .`

           careAdmin = drugAdminActDef("T", req.body.drug_id, req.body.drug_label);
     } else {
       action = drugDef("T", req.body.drug_id, req.body.drug_id)   + insertCodes(req) + ` .`;

       careAdmin = drugAdminActDef("T", req.body.drug_id, req.body.drug_id);
     }

    if ( req.body.subsumed_drug_id ) {

      careAdmin += adminActSub(req.body.subsumed_drug_id);
    }
  }
 
  careAdmin += ` .`

  const actionType = action

  postDrugs(actionType + " " + careAdmin, insertOrDelete, callback);

}

////////////////////////////

//add one or more drugTypes as part of the grouping criteria of a drug category
function addGroupingCriteria(groupingCriteriaIds) {

  var groupingCriteria = ` ;
                            vocab:hasGroupingCriteria  `;

  groupingCriteriaIds.split(",").forEach(function(criteriaId) {

    groupingCriteria += (`data:Tr` + criteriaId.trim() + `, `);

  });
  //remove last comma and whitespace
  groupingCriteria = groupingCriteria.substring(0, groupingCriteria.length - 2);

  return groupingCriteria;
}

//Specify multiple drug subsumptions via the administrationOf triple.
function adminActSubs(drugIds) {

  var adminSubs = ` ;
                      vocab:subsumes  `;

  drugIds.split(",").forEach(function(elem) {

    adminSubs += (`data:ActAdminister` + elem.trim() + `, `);

  });
  //return after removing the last comma and whitespace
  return adminSubs.substring(0, adminSubs.length - 2);
}

//defines a drug category and its corresponding care action type
function drugCatCareAction(req, insertOrDelete, callback) {
  var drugCat =""

  var adminActDrugCat=""

if(req.body.drugCat_label){
    // Drug category format:
     drugCat = drugDef("Cat", req.body.drugCat_id, req.body.drugCat_label) + insertCodes(req) 

     adminActDrugCat = drugAdminActDef("Cat", req.body.drugCat_id, req.body.drugCat_label);
} else{
    // Drug category format:
     drugCat = drugDef("Cat", req.body.drugCat_id, req.body.drugCat_id) + insertCodes(req)

     adminActDrugCat = drugAdminActDef("Cat", req.body.drugCat_id, req.body.drugCat_id);
}

  //add grouping criteria
  if ( req.body.grouping_criteria_ids ) {

    drugCat += addGroupingCriteria(req.body.grouping_criteria_ids);
  } 
  drugCat += ` .`

  //add subsumed action care types
  if ( req.body.subsumed_drug_ids ) {
    adminActDrugCat += adminActSubs(req.body.subsumed_drug_ids);
  }
  adminActDrugCat += ` .`

  postDrugs(drugCat + " " + adminActDrugCat, insertOrDelete, callback);
}

module.exports = router;
