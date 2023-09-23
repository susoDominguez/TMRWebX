const express = require("express");
const router = express.Router();
const request = require("request");

const config = require("../lib/config");
const guidelines = require("../lib/prefixes");
const logger = require("../config/winston");
const utils = require("../lib/utils");

function postDrugs(careActData, insertOrDelete, callback) {
  utils.sparqlUpdate("careActions", careActData, insertOrDelete, callback);
}

router.post("/drug/individual/add", function (req, res) {
  careActDef(req, config.INSERT, (err, status) => res.status(status).end());
});

router.post("/nondrug/individual/add", function (req, res) {
  careActDefNonDrug(req, config.INSERT, (err, status) =>
    res.status(status).end()
  );
});

router.post("/individual/delete", function (req, res) {
  careActDef(req, config.DELETE, (err, status) => res.status(status).end());
});

router.post("/drug/category/add", function (req, res) {
  drugCatCareAction(req, config.INSERT, (err, status) =>
    res.status(status).end()
  );
});

router.post("/drug/category/delete", function (req, res) {
  drugCatCareAction(req, config.DELETE, (err, status) =>
    res.status(status).end()
  );
});

router.post("/effect/get", function (req, res) {
  let postData = "";

  if (req.body.drugCat_id) {
    postData = require("querystring").stringify({
      drugCat_id: "http://anonymous.org/tmr/data/DrugCat" + req.body.drugCat_id,
    });
  } else {
    if (req.body.drugT_id) {
      postData = require("querystring").stringify({
        drugT_id: "http://anonymous.org/tmr/data/DrugT" + req.body.drugT_id,
      });
    } else {
      if (req.body.nonDrugT_id) {
        postData = require("querystring").stringify({
          nonDrugT_id:
            "http://anonymous.org/tmr/data/NonDrugT" + req.body.nonDrugT_id,
        });
      } else {
        postData = require("querystring").stringify({
          act_URI: req.body.act_URI,
        });
      }
    }
  }

  utils.callPrologServer("drugeffects", postData, res, function (err, data) {
    if (err) {
      res.status(404).end();
      return;
    }

    res.status(200).send(data);
  });
});


router.post("/all/get/", function (req, res) {
  const prefix = `http://anonymous.org/tmr/data/`;

  if (req.body.uri || req.body.id) {
    utils.getCareActionData(
      "careActions",
      req.body.id ? prefix + req.body.id : req.body.uri,
      function (err, actionResults) {

        if (err) {
          res.status(404).send(err);
          return;
        }

        let data = {};
        let vars = actionResults.head.vars;
        let bindings = actionResults.results.bindings[0];

        //format data by looping through head vars
        for (let pos in vars) {
          //variable name
          let headVar = vars[pos];

          //check there is a corresponding binding, if not, next head var
        if (!bindings.hasOwnProperty(headVar)) continue;
         
          //otherwise, retrieve value
          let value = bindings[headVar].value;
            
              //for each heading, add a field
              switch (headVar) {
                case "actId":
                  data.id = value;
                  break;
                case "adminLabel":
                  data.display = value;
                  break;
                case "actType":
                  //extract code
                  let type = value.slice(25);
                  data.code = type;
                  data.requestType = 0; //for drug related types
                  //check for therapy
                  if (type.startsWith("NonDrugT")) {
                    data.requestType = 1;
                  } else {
                    //check for vaccine
                    if (type.startsWith("VaccineT")) {
                      data.requestType = 2;
                    }
                  }
                  break;
                case "actLabel":
                  data.drugLabel = value;
                  data.sct_trm = value;
                  break;
                case "snomed":
                  data.snomedCode = value;
                  data.sct_id = value;
                  break;
                case "sameAs":
                  data.sameAs = value.split(', ');
                  break;
                case "hasGroupingCriteria":
                  data.hasGroupingCriteria = value.split(', ');
                  break;
                case "subsumes":
                  data.subsumes = value.split(', ');
                  break;
              }
            }

        res.send(data);
      }
    );
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
  return (drug =
    `data:Drug` +
    typeOrCat +
    id +
    ` a tmr:DrugType, owl:NamedIndividual ;
                                     rdfs:label "` +
    label +
    `"@en `);
}

//Defines non-drug related care actions
function nonDrugDef(typeOrCat, id, label) {
  return (action =
    `data:NonDrug` +
    typeOrCat +
    id +
    ` a tmr:NonDrugType, owl:NamedIndividual ;
                                     rdfs:label "` +
    label +
    `"@en `);
}

////////////////

//Administration action care general to both drug type and category
function drugAdminActDef(typeOrCat, id, label) {
  let drugAdministration =
    `data:ActAdminister` +
    id +
    ` a tmr:DrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "administer ` +
    label +
    `"@en ;
                               tmr:administrationOf data:Drug` +
    typeOrCat +
    id;

  return drugAdministration;
}

//Administration non drug action care
function nonDrugAdminActDef(typeOrCat, id, actLabel) {
  let nonDrugAdmin =
    `data:ActAdminister` +
    id +
    ` a tmr:NonDrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "` +
    actLabel +
    `"@en ;
                               tmr:applicationOf data:NonDrug` +
    typeOrCat +
    id;

  return nonDrugAdmin;
}

// Administration Action Care specialisation of a drugT from a drugT.
function adminActSub(id) {
  return (
    ` ;
             tmr:subsumes data:ActAdminister` + id
  );
}

//defines the insertion of clinical codes both in drugT and nonDrugT. Also in drug categories
function insertCodes(req) {
  careAction = "";

  if (req.body.icd10Codes) {
    careAction += `;`;

    req.body.icd10Codes.split(",").forEach(function (code) {
      careAction +=
        `
        tmr:icd10Code   "` +
        code.trim() +
        `"^^xsd:string ;`;
    });
    //this removes the last semicolon
    careAction = careAction.substring(0, careAction.length - 1);
  }
  if (req.body.snomedCodes) {
    careAction += `;`;

    req.body.snomedCodes.split(",").forEach(function (code) {
      careAction +=
        `
      tmr:snomedCode   "` +
        code.trim() +
        `"^^xsd:string ;`;
    });
    //this removes the last semicolon
    careAction = careAction.substring(0, careAction.length - 1);
  }

  if (req.body.umlsCodes) {
    careAction += `;`;

    req.body.umlsCodes.split(",").forEach(function (code) {
      careAction +=
        `
      tmr:umlsCode   "` +
        code.trim() +
        `"^^xsd:string ;`;
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
  // Individual drug format
  action =
    drugDef("T", req.body.id, req.body.label ? req.body.label : req.body.id) +
    insertCodes(req) +
    ` .`;

  careAdmin = drugAdminActDef(
    "T",
    req.body.id,
    req.body.action_label
      ? req.body.action_label
      : req.body.label
      ? req.body.label
      : req.body.id
  );

  if (req.body.subsumed_id) {
    careAdmin += adminActSubs(req.body.subsumed_id);
  }

  careAdmin += ` .`;

  const actionType = action;

  postDrugs(actionType + " " + careAdmin, insertOrDelete, callback);
}

//defines non drug careActT and careActT Admin
function careActDefNonDrug(req, insertOrDelete, callback) {
  let action = "";
  let careAdmin = "";

  action = nonDrugDef(
    "T",
    req.body.id,
    req.body.label ? req.body.label : req.body.id
  );

  action += insertCodes(req) + ` .`;

  careAdmin = nonDrugAdminActDef(
    "T",
    req.body.id,
    req.body.action_label
      ? req.body.action_label
      : req.body.label
      ? req.body.label
      : req.body.id
  );

  if (req.body.subsumed_id) {
    careAdmin += adminActSubs(req.body.subsumed_id);
  }

  careAdmin += ` .`;

  const actionType = action;

  postDrugs(actionType + " " + careAdmin, insertOrDelete, callback);
}

////////////////////////////

//add one or more drugTypes as part of the grouping criteria of a drug category
function addGroupingCriteria(groupingCriteriaIds) {
  let groupingCriteria = ` ;
                            tmr:hasGroupingCriteria  `;

  groupingCriteriaIds.split(",").forEach(function (criteriaId) {
    groupingCriteria += `data:Tr` + criteriaId.trim() + `, `;
  });
  //remove last comma and whitespace
  groupingCriteria = groupingCriteria.substring(0, groupingCriteria.length - 2);

  return groupingCriteria;
}

//Specify multiple drug subsumptions via the administrationOf triple.
function adminActSubs(drugIds) {
  let adminSubs = ` ;
                      tmr:subsumes  `;

  drugIds.split(",").forEach(function (elem) {
    adminSubs += `data:ActAdminister` + elem.trim() + `, `;
  });
  //return after removing the last comma and whitespace
  return adminSubs.substring(0, adminSubs.length - 2);
}

//defines a drug category and its corresponding care action type
function drugCatCareAction(req, insertOrDelete, callback) {
  // Drug category format:
  let drugCat =
    drugDef(
      "Cat",
      req.body.drugCat_id,
      req.body.drugCat_label ? req.body.drugCat_label : req.body.drugCat_id
    ) + insertCodes(req);

  let adminActDrugCat = drugAdminActDef(
    "Cat",
    req.body.drugCat_id,
    req.body.action_label
      ? req.body.action_label
      : req.body.drugCat_label
      ? req.body.drugCat_label
      : req.body.drugCat_id
  );

  //add grouping criteria
  if (req.body.grouping_criteria_ids) {
    drugCat += addGroupingCriteria(req.body.grouping_criteria_ids);
  }

  drugCat += ` .`;

  //add subsumed action care types
  if (req.body.subsumed_drug_ids) {
    adminActDrugCat += adminActSubs(req.body.subsumed_drug_ids);
  }
  adminActDrugCat += ` .`;

  postDrugs(drugCat + " " + adminActDrugCat, insertOrDelete, callback);
}

module.exports = router;
