const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const utils = require('../lib/utils');


function postDrugs(drugData, insertOrDelete, callback) {

  utils.sparqlUpdate('drugs', drugData, insertOrDelete, function(status) {

    callback(status);

  });

}

router.post('/individual/add', function(req, res) {

  drugTCareAction(req, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/individual/delete', function(req, res) {

  drugTCareAction(req, config.DELETE, function(status) {

    res.sendStatus(status);

  });

});

router.post('/category/add', function(req, res) {

  drugCatCareAction(req, config.INSERT, function(status) {

    res.sendStatus(status);

  });

});

router.post('/category/delete', function(req, res) {

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
          postData = require('querystring').stringify({
            'drug_URI' : req.body.drug_URI
              });
          }
  }

  utils.callPrologServer("drugeffects", postData, res, function(data) {

    res.send(data);

  });

});

router.post('/all/get/', function(req, res) {

  if(req.body.drugCat_id){

    utils.sparqlSubject("drugs", "http://anonymous.org/data/DrugCat"+req.body.drugCat_id, function(drugData) {

    res.send(drugData);

    });
  } else
  { 
    if (req.body.drugT_id){
    utils.sparqlSubject("drugs", "http://anonymous.org/data/DrugT"+req.body.drugT_id, function(drugData) {

      res.send(drugData);
  
      });
    } else {
      utils.sparqlSubject("drugs", req.body.drug_URI, function(drugData) {

      res.send(drugData);
  
      });
    }
  } 
  

});

//////////////////////////////

//Defines drug types and categories, providing an english label. 
function drugDef(typeOrCat, id) {
  return drug = 
  `:Drug` + typeOrCat + id + ` a vocab:DrugType, owl:NamedIndividual ;
                                     rdfs:label "` + id + `"@en `
}

////////////////

//Administration action care general to both drug type and category
function drugAdminActDef(typeOrCat, id) {

  var drugAdministration =
   `:ActAdminister` + id + ` a vocab:DrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "Administer ` + id + `"@en ;
                               vocab:administrationOf :Drug` + typeOrCat + id;

  return drugAdministration;
}

// Administration Action Care specialisation of a drugT from a drugT.
function adminActSub(id) {

  return ` ;
             vocab:subsumes :ActAdminister` + id;
}

//defines DrugT and also DrugT Admin
function drugTCareAction(req, insertOrDelete, callback) {

  // Individual drug format:
  const drug = drugDef("T", req.body.drug_id) + ` .`

  var drugAdministration = drugAdminActDef("T", req.body.drug_id);

  if ( req.body.subsumed_drug_id ) {

    drugAdministration += adminActSub(req.body.subsumed_drug_id);
  }
  drugAdministration += ` .`

  postDrugs(drug + " " + drugAdministration, insertOrDelete, callback);

}

////////////////////////////

//add one or more drugTypes as part of the grouping criteria of a drug category
function addGroupingCriteria(groupingCriteriaIds) {

  var groupingCriteria = ` ;
                            vocab:hasGroupingCriteria  `;

  groupingCriteriaIds.split(",").forEach(function(criteriaId) {

    groupingCriteria += (`:` + criteriaId.trim() + `, `);

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

    adminSubs += (`:ActAdminister` + elem.trim() + `, `);

  });
  //return after removing the last comma and whitespace
  return adminSubs.substring(0, adminSubs.length - 2);
}

//defines a drug category and its corresponding care action type
function drugCatCareAction(req, insertOrDelete, callback) {

  // Drug category format:
  var drugCat = drugDef("Cat", req.body.drug_category_id)

  var adminActDrugCat = drugAdminActDef("Cat", req.body.drug_category_id);
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
