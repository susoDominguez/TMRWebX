const express = require("express");
const router = express.Router();
//const request = require("request");

const config = require("../lib/config");
const auxFunct = require("../lib/router_functs/guideline_functs");
//const guidelines = require("../lib/prefixes");
//const logger = require("../config/winston");
const utils = require("../lib/utils");
const logger = require("../config/winston");
//const { ErrorHandler } = require("../lib/errorHandler");

const DrugT = "DrugT";
const DrugType = "DrugType";
const NonDrugT = "NonDrugT";
const NonDrugType = "NonDrugType";
const DrugCat = "DrugCat";
const DrugCategory = "DrugCategory";
const CombDrugT = "CombDrugT";
const DrugCombinationType = "DrugCombinationType";
const VacT = "VacT";
const VaccineType = "VaccineType";
const VacCat = "VacCat";
const VaccineCategory = "VaccineType";
const VaccinationType = "VaccinationType";
const DrugCombinationAdministrationType = "DrugCombinationAdministrationType";
const DrugAdministrationType = "DrugAdministrationType";
const combinedAdministrationOf = "combinedAdministrationOf";
const administrationOf = "administrationOf";
const vaccinationWith = "vaccinationWith";

// CREATE

// drug type
router.post("/drug/individual/add", async function (req, res) {
  let sprql_str = careActDef(req, DrugT);
  let { status, data } = await postDrugs(sprql_str, config.INSERT);
  res.status(status).send(data);
});

// non drug type (therapies) but not administrative actions yet
router.post("/nondrug/individual/add", async function (req, res) {
  let sprql_str = careActDefNonDrug(req, NonDrugT);
  let { status, data } = await postDrugs(sprql_str, config.INSERT);
  res.status(status).send(data);
});

// Drug category
router.post("/drug/category/add", async function (req, res) {
  let sprql_str = careActDef(req, DrugCat);
  logger.debug(`sprql_str is ${sprql_str}`);
  let { status, data } = await postDrugs(sprql_str, config.INSERT);
  res.status(status).send(data);
});

// Drug combination type
router.post("drug/combination/add", async function (req, res) {
  let sprql_str = careActDef(req, CombDrugT);
  let { status, data } = await postDrugs(sprql_str, config.INSERT);
  res.status(status).send(data);
});

// Vaccine type
router.post("/vaccine/individual/add", async function (req, res) {
  let sprql_str = careActDef(req, VacT);
  let { status, data } = await postDrugs(sprql_str, config.INSERT);
  res.status(status).send(data);
});

// Vaccine category
router.post("/vaccine/category/add", async function (req, res) {
  let sprql_str = careActDef(req, VacCat);
  logger.debug(`sprql_str is ${sprql_str}`);
  let { status, data } = await postDrugs(sprql_str, config.INSERT);
  res.status(status).send(data);
});

/////////////////////////////////////////

//DELETE

router.post("/drug/individual/delete", async function (req, res) {
  let sprql_str = delete_def(DrugT, req.body.id);
  let { status, data } = await postDrugs(
    sprql_str,
    config.DELETE,
    DrugT,
    req.body.id
  );
  res.status(status ? status : "500").send(data ? data : "Error");
});

router.post("/drug/category/delete", async function (req, res) {
  let sprql_str = delete_def(DrugCat, req.body.id);
  let { status, data } = await postDrugs(
    sprql_str,
    config.DELETE,
    DrugCat,
    req.body.id
  );
  res.status(status).send(data);
});

router.post("/nondrug/individual/delete", async function (req, res) {
  let sprql_str = delete_def(NonDrugT, req.body.id);
  let { status, data } = await postDrugs(
    sprql_str,
    config.DELETE,
    NonDrugT,
    req.body.id
  );
  res.status(status).send(data);
});

router.post("/drug/combination/delete", async function (req, res) {
  let sprql_str = delete_def(CombDrugT, req.body.id);
  let { status, data } = await postDrugs(
    sprql_str,
    config.DELETE,
    CombDrugT,
    req.body.id
  );
  res.status(status).send(data);
});

router.post("/vaccine/individual/delete", async function (req, res) {
  let sprql_str = delete_def(VacT, req.body.id);
  let { status, data } = await postDrugs(
    sprql_str,
    config.DELETE,
    VacT,
    req.body.id
  );
  res.status(status).send(data);
});

router.post("/vaccine/category/delete", async function (req, res) {
  let sprql_str = delete_def(VacCat, req.body.id);
  let { status, data } = await postDrugs(
    sprql_str,
    config.DELETE,
    VacCat,
    req.body.id
  );
  res.status(status).send(data);
});

/////////////////////////////////////////


//get all RDF data on a single care action
router.post("/all/get/", async function (req, res) {
  //no params
  if (!(req.body.uri || req.body.id))
    return res.status(406).send("Missing Id or URI parameter.");

  let { status, head_vars, bindings } = await utils.getCareActionData(
    "careActions",
    req.body.id,
    req.body.uri
  );

  if (status < 400) {
    let data = auxFunct.get_care_action(head_vars, bindings[0]);
    return res.status(status).json(data);
  } else {
    return res.status(status).end();
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

async function postDrugs(careActData, insertOrDelete, type, id) {
  const del_str = ` ?s ?p ?o `;

  let content = insertOrDelete === config.INSERT ? careActData : del_str;

  let sparql_query_str = `${insertOrDelete} ${
    insertOrDelete === config.INSERT ? "DATA" : ""
  } { ${content} } `;

  if (insertOrDelete === config.DELETE) {
    let query_post_str = ` WHERE {  ${del_str} . ${careActData} }`;
    sparql_query_str += query_post_str;
  }

  return await utils.sparqlUpdate("careActions", sparql_query_str);
}

//Defines drug types and categories, providing an english label.
function drugDef(type, id, englishLbl) {
  englishLbl =
    type !== CombDrugT ? englishLbl : englishLbl + " combination therapy";

  return (
    `data:` +
    type +
    id +
    ` a vocab:${
      type === DrugCat
        ? DrugCategory
        : type === CombDrugT
        ? DrugCombinationType
        : DrugType
    }, owl:NamedIndividual ; rdfs:label "` +
    englishLbl +
    `"@en `
  );
}

function vaccineDef(type, id, englishLbl) {
  return (
    `data:` +
    type +
    id +
    ` a vocab:${
      type === VacCat ? VaccineCategory : VaccineType
    }, owl:NamedIndividual ; rdfs:label "` +
    englishLbl +
    `"@en `
  );
}

//Defines non-drug related care actions
function nonDrugDef(type, id, label) {
  return `data:${type + id} a vocab:${NonDrugType} , owl:NamedIndividual ;
                      rdfs:label "${label}"@en `;
}

////////////////

//Administration action care general to vaccine, drug type and category
function drugAdminActDef(type, id, englishLabel) {
  let drugAdministration = `data:ActAdminister${id} a vocab:${
    type === VacT || type == VacCat
      ? VaccinationType
      : type === CombDrugT
      ? DrugCombinationAdministrationType
      : DrugAdministrationType
  }, owl:NamedIndividual ;
                      rdfs:label "administer ${englishLabel} "@en ;
                      vocab:${
                        type === CombDrugT
                          ? combinedAdministrationOf
                          : type == VacT || type == VacCat
                          ? vaccinationWith
                          : administrationOf
                      } data:${type + id} `;

  return drugAdministration;
}

//Administration non drug action care
function nonDrugAdminActDef(type, id, actLabel) {
  return `data:ActAdminister${id}  a vocab:NonDrugAdministrationType , owl:NamedIndividual ;
                              rdfs:label "${actLabel}"@en ;
                              vocab:applicationOf data:${type + id} `;
}

//defines the insertion of clinical codes both in drugT and nonDrugT. Also in drug categories & combinations
function insertCodes(req) {
  careAction = "";

  if (req.body.icd10Codes) {
    careAction += `;`;

    req.body.icd10Codes.split(",").forEach(function (code) {
      careAction +=
        `
        vocab:icd10Code   "` +
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
      vocab:sctid   "` +
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
      vocab:umlsCode   "` +
        code.trim() +
        `"^^xsd:string ;`;
    });
    //this removes the last semicolon
    careAction = careAction.substring(0, careAction.length - 1);
  }

  return careAction;
}

//defines ALL DrugT and also DrugT Admin. Also non drug careActT and careActT Admin
function careActDef(req, drug_type) {
  //retrieve values from params
  let id = req.body.id;
  let label = req.body.label ? req.body.label : req.body.id;
  let action_label = req.body.action_label
    ? req.body.action_label
    : req.body.label;
  let grouping_criteria = req.body.grouping_criteria_ids || null;
  let subsumed_list = req.body.subsumed_ids || null;
  let components_ids = req.body.components_ids || null;

  let drugT_def =
    type === VacCat || type === VacT
      ? vaccineDef(drug_type, id, label)
      : drugDef(drug_type, id, label);

  drugT_def += insertCodes(req);

  let careAct_def = drugAdminActDef(drug_type, id, action_label);
  //extras:

  //add grouping criteria or else components in combined drug types
  if (grouping_criteria) {
    drugT_def += addGroupingCriteria(grouping_criteria);
  }

  //add components in combined drug types or else subsumed drugs
  if (drug_type === CombDrugT && components_ids) {
    careAct_def += addComponents(components_ids);
  } else {
    if (subsumed_list) {
      careAct_def += adminActSubs(subsumed_list);
    }
  }

  //final period
  careAct_def += ` .`;
  drugT_def += ` .`;

  return drugT_def + " " + careAct_def;
}

//defines non drug careActT and careActT Admin
function careActDefNonDrug(req, type) {
  let action = nonDrugDef(
    type,
    req.body.id,
    req.body.label ? req.body.label : req.body.id
  );

  action += insertCodes(req) + ` .`;

  let careAdmin = nonDrugAdminActDef(
    type,
    req.body.id,
    req.body.action_label
      ? req.body.action_label
      : req.body.label
      ? req.body.label
      : req.body.id
  );

  if (req.body.subsumed_ids) {
    careAdmin += adminActSubs(req.body.subsumed_ids);
  }

  careAdmin += ` .`;

  return action + "\n" + careAdmin;
}

////////////////////////////

//add one or more drugTypes as part of the grouping criteria of a drug category
function addGroupingCriteria(groupingCriteriaIds) {
  let groupingCriteria = ` ;
                            vocab:hasGroupingCriteria  `;

  groupingCriteriaIds.split(",").forEach(function (criteriaId) {
    groupingCriteria += `data:` + criteriaId.trim() + `, `;
  });
  //remove last comma and whitespace
  groupingCriteria = groupingCriteria.substring(0, groupingCriteria.length - 2);

  return groupingCriteria;
}

function addComponents(componentsStr) {
  let components_part = ` ;
                            vocab:hasComponent  `;

  componentsStr.split(",").forEach(function (compId) {
    components_part += `data:ActAdminister` + compId.trim() + `, `;
  });
  //remove last comma and whitespace
  components_part = components_part.substring(0, components_part.length - 2);

  return components_part;
}

//Specify multiple drug subsumptions via the administrationOf triple.
function adminActSubs(drugIds) {
  let adminSubs = ` ;
                      vocab:subsumes  `;

  drugIds.split(",").forEach(function (elem) {
    adminSubs += `data:ActAdminister` + elem.trim() + `, `;
  });
  //return after removing the last comma and whitespace
  return adminSubs.substring(0, adminSubs.length - 2);
}

function delete_def(type, id) {
  return ` FILTER (?s = data:${type + id} || ?s = data:ActAdminister${id} ) `;
}

module.exports = router;
