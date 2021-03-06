const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const logger = require('../config/winston');
const utils = require('../lib/utils');


function postDrugs(careActData, insertOrDelete, callback) {

  utils.sparqlUpdate('careActions', careActData, insertOrDelete, callback);

}

router.post('/drug/individual/add', function (req, res) {

  careActDef(req, config.INSERT, (err, status) => res.status(status).end());

});

router.post('/nondrug/individual/add', function (req, res) {

  careActDef(req, config.INSERT, (err, status) => res.status(status).end());

});

router.post('/individual/delete', function (req, res) {

  careActDef(req, config.DELETE, (err, status) => res.status(status).end());

});

router.post('/drug/category/add', function (req, res) {

  drugCatCareAction(req, config.INSERT, (err, status) => res.status(status).end());

});

router.post('/drug/category/delete', function (req, res) {

  drugCatCareAction(req, config.DELETE, (err, status) => res.status(status).end());

});

router.post('/effect/get', function (req, res) {

  let postData = ""

  if (req.body.drugCat_id) {
    postData = require('querystring').stringify({
      'drugCat_id': "http://anonymous.org/data/DrugCat" + req.body.drugCat_id
    });
  }
  else {
    if (req.body.drugT_id) {
      postData = require('querystring').stringify({
        'drugT_id': "http://anonymous.org/data/DrugT" + req.body.drugT_id
      });
    }
    else {
      if (req.body.nonDrugT_id) {
        postData = require('querystring').stringify({
          'nonDrugT_id': "http://anonymous.org/data/NonDrugT" + req.body.nonDrugT_id
        });
      } else {
        postData = require('querystring').stringify({
          'act_URI': req.body.act_URI
        });
      }
    }

  }

  utils.callPrologServer("drugeffects", postData, res, function (err, data) {

    if(err){
      res.status(404).end();
      return;
    }

    res.status(200).send(data);

  });

});

//TODO: revise
router.post('/all/get/', function (req, res) {

  if (req.body.uri) {

    utils.getCareActionData("careActions", req.body.uri, function (err, actionResults) {

      if(err) {
        res.status(404).send(err);
        return;
      }

      let data = {};
      let vars = actionResults.head.vars;
      let bindings = actionResults.results.bindings;

      //format data by looping through results
      for (let pos in bindings) {

        let bind = bindings[pos];

        for (let varPos in vars) {

          let value = bind[vars[varPos]].value;

          //for each heading, add a field
          switch (vars[varPos]) {
            case "actId":
              data.id = value;
              break;
            case "adminLabel":
              data.display = value;
              break;
            case "actType":
              //extract code
              let type = value.slice(27);
              data.code = type;
              data.requestType = 0; //for drugT and DrugCat
              //check for therapy
              if (type.startsWith("NonDrugT")) {
                data.requestType = 1;
              } else {
                //check for vaccine
                if (type.startsWith("VacT")) {
                  data.requestType = 2;
                }
              }
              break;
            case "actLabel":
              data.drugLabel = value;
              break;
            case "snomed":
              data.snomedCode = value;
              break;
          }
        }
      }
      res.send(data);
    });

  } else {
    res.status(406).send({});
  }
});

/*
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
  

});*/

//////////////////////////////

//Defines drug types and categories, providing an english label. 
function drugDef(typeOrCat, id, label) {
  return drug =
    `data:Drug` + typeOrCat + id + ` a vocab:DrugType, owl:NamedIndividual ;
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

  let drugAdministration =
    `data:ActAdminister` + id + ` a vocab:DrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "administer ` + label + `"@en ;
                               vocab:administrationOf data:Drug` + typeOrCat + id;

  return drugAdministration;
}

//Administration non drug action care 
function nonDrugAdminActDef(typeOrCat, id, actLabel) {

  let nonDrugAdmin =
    `data:ActAdminister` + id + ` a vocab:NonDrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "` + actLabel + `"@en ;
                               vocab:applicationOf data:NonDrug` + typeOrCat + id;

  return nonDrugAdmin;
}

// Administration Action Care specialisation of a drugT from a drugT.
function adminActSub(id) {
  return ` ;
             vocab:subsumes data:ActAdminister` + id;
}

//defines the insertion of clinical codes both in drugT and nonDrugT. Also in drug categories
function insertCodes(req) {
 
  careAction = "";

  if (req.body.icd10Codes) {
    careAction += `;`;

    req.body.icd10Codes.split(",").forEach(function (code) {

      careAction += `
        vocab:icd10Code   "` + code.trim() + `"^^xsd:string ;`

    });
    //this removes the last semicolon
    careAction = careAction.substring(0, careAction.length - 1);
  }
  if (req.body.snomedCodes) {
    careAction += `;`;

    req.body.snomedCodes.split(",").forEach(function (code) {

      careAction += `
      vocab:snomedCode   "` + code.trim() + `"^^xsd:string ;`

    });
    //this removes the last semicolon
    careAction = careAction.substring(0, careAction.length - 1);
  }

  if (req.body.umlsCodes) {
    careAction += `;`;

    req.body.umlsCodes.split(",").forEach(function (code) {

      careAction += `
      vocab:umlsCode   "` + code.trim() + `"^^xsd:string ;`

    });
    //this removes the last semicolon
    careAction = careAction.substring(0, careAction.length - 1);
  }

  return careAction;
}

//defines ALL DrugT and also DrugT Admin. Also non drug careActT and careActT Admin
function careActDef(req, insertOrDelete, callback) {
  let action = "";
  let careAdmin = "";
  //if not  drug
  if (req.body.nonDrugAct_label) {
    //it is not a drug
    if (req.body.nonDrug_label) {
      action = nonDrugDef("T", req.body.nonDrug_id, req.body.nonDrug_label)
    } else {
      action = nonDrugDef("T", req.body.nonDrug_id, req.body.nonDrug_id)
    }

    action += insertCodes(req) + ` .`

    careAdmin = nonDrugAdminActDef("T", req.body.nonDrug_id, req.body.nonDrugAct_label);

    if (req.body.subsumed_nonDrug_id) {

      careAdmin += adminActSub(req.body.subsumed_nonDrug_id);
    }
  } else {
    // Individual drug format
    action = drugDef("T", req.body.drug_id, (req.body.drug_label ? req.body.drug_label : req.body.drug_id))
      + insertCodes(req) + ` .`

    careAdmin = drugAdminActDef("T", req.body.drug_id, 
      ( req.body.action_label ? req.body.action_label : (req.body.drug_label ? req.body.drug_label : req.body.drug_id ) ) );

    if (req.body.subsumed_drug_id) {
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

  let groupingCriteria = ` ;
                            vocab:hasGroupingCriteria  `;

  groupingCriteriaIds.split(",").forEach(function (criteriaId) {

    groupingCriteria += (`data:Tr` + criteriaId.trim() + `, `);

  });
  //remove last comma and whitespace
  groupingCriteria = groupingCriteria.substring(0, groupingCriteria.length - 2);

  return groupingCriteria;
}

//Specify multiple drug subsumptions via the administrationOf triple.
function adminActSubs(drugIds) {

  let adminSubs = ` ;
                      vocab:subsumes  `;

  drugIds.split(",").forEach(function (elem) {

    adminSubs += (`data:ActAdminister` + elem.trim() + `, `);

  });
  //return after removing the last comma and whitespace
  return adminSubs.substring(0, adminSubs.length - 2);
}

//defines a drug category and its corresponding care action type
function drugCatCareAction(req, insertOrDelete, callback) {

    // Drug category format:
   let drugCat = drugDef("Cat", req.body.drugCat_id, 
      ( req.body.drugCat_label ? req.body.drugCat_label : req.body.drugCat_id ) ) +
       insertCodes(req);

    let adminActDrugCat = drugAdminActDef("Cat", req.body.drugCat_id, 
      ( req.body.action_label ? req.body.action_label : ( req.body.drugCat_label ? req.body.drugCat_label : req.body.drugCat_id ) ) ) ;

  //add grouping criteria
  if (req.body.grouping_criteria_ids) {

    drugCat += addGroupingCriteria(req.body.grouping_criteria_ids);
  }
  
  drugCat += ` .`

  //add subsumed action care types
  if (req.body.subsumed_drug_ids) {
    adminActDrugCat += adminActSubs(req.body.subsumed_drug_ids);
  }
  adminActDrugCat += ` .`

  postDrugs(drugCat + " " + adminActDrugCat, insertOrDelete, callback);
}

module.exports = router;
