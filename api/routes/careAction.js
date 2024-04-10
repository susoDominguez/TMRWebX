const express = require("express");
const router = express.Router();
//const request = require("request");

const config = require("../lib/config");
const auxFunct = require("../lib/router_functs/guideline_functs");
//const guidelines = require("../lib/prefixes");
//const logger = require("../config/winston");
const utils = require("../lib/utils");
//const { ErrorHandler } = require("../lib/errorHandler");

//create
router.post("/drug/individual/add", async function (req, res) {
  let sprql_str = careActDef(req, "DrugT");
   let {status, data} = await postDrugs(sprql_str, config.INSERT);
     res.status(status).send(data);
});

router.post("/nondrug/individual/add", async function (req, res) {
  let sprql_str = careActDefNonDrug(req, "NonDrugT");
   let {status, data} = await postDrugs(sprql_str, config.INSERT);
     res.status(status).send(data);
});

router.post("/drug/category/add", async function (req, res) {
  let sprql_str = careActDef(req, "DrugCat");
   let {status, data} = await postDrugs(sprql_str, config.INSERT);
     res.status(status).send(data);
});

router.post("/drug/vaccine/individual/add", async function (req, res) {
  let sprql_str = careActDef(req, "VacT");
   let {status, data} = await postDrugs(sprql_str, config.INSERT);
     res.status(status).send(data);
});

router.post("/combination/add", async function (req, res) {
  let sprql_str = careActDef(req, "CombT");
   let {status, data} = await postDrugs(sprql_str, config.INSERT);
     res.status(status).send(data);
});

/* 
/TO BE DONE
router.post("/nondrug/category/add", function (req, res) {
  nonDrugCatCareAction(req, config.INSERT, (err, status) =>
    res.status(status).end()
  );
});
*/

/////////////////////////////////////////

//DELETE
router.post("/drug/individual/delete", async function (req, res) {
  let sprql_str = delete_def("DrugT", req.body.id);
  let {status, data} = await postDrugs(sprql_str, config.DELETE, "DrugT"+req.body.id, req.body.id) ;
   res.status(status? status : '500').send(data? data : 'Error');
});

router.post("/drug/category/delete", async function (req, res) {
  let sprql_str = delete_def("DrugCat", req.body.id);
  let {status, data} = await postDrugs(sprql_str, config.DELETE, "DrugCat"+req.body.id, req.body.id) ;
   res.status(status).send(data);
});

router.post("/nondrug/individual/delete", async function (req, res) {
  let sprql_str = delete_def("NonDrugT", req.body.id);
  let {status, data} = await postDrugs(sprql_str, config.DELETE, "NonDrugT"+req.body.id, req.body.id) ;
   res.status(status).send(data);
});

router.post("/combination/delete", async function (req, res) {
  let sprql_str = delete_def("CombT", req.body.id);
  let {status, data} = await postDrugs(sprql_str, config.DELETE, "CombT"+req.body.id, req.body.id) ;
   res.status(status).send(data);
});

router.post("/drug/vaccine/individual/delete", async function (req, res) {
  let sprql_str = delete_def("VacT", req.body.id);
  let {status, data} = await postDrugs(sprql_str, config.DELETE, "VacT"+req.body.id, req.body.id) ;
   res.status(status).send(data);
});

/////////////////////////////////////////

router.post("/effect/get", async function (req, res) {
  let postData = "";

  if (req.body.drug_category_id) {
    postData = require("querystring").stringify({
      drugCat_id: "http://anonymous.org/tmr/data/DrugCat" + req.body.drugCat_id,
    });
  } else {
    if (req.body.drug_type_id) {
      postData = require("querystring").stringify({
        drugT_id: "http://anonymous.org/tmr/data/DrugT" + req.body.drugT_id,
      });
    } else {
      if (req.body.nonDrug_type_id) {
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

//get all RDF data on a single care action
router.post("/all/get/", async function (req, res) {
  //no params
  if (!(req.body.uri || req.body.id)) return res.status(406).send('Missing Id or URI parameter.');

  //else
  const prefix = `http://anonymous.org/tmr/data/`;


    let {status = 500,head_vars=[], bindings=[]} = await utils.getCareActionData(
      "careActions",
      req.body.id ? prefix + req.body.id : req.body.uri);

    let data = auxFunct.careAction_rdf2json(head_vars, bindings); //TODO:

    return res.status(status).json(data);
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

  const del_str = ` data:${type} ?p ?o . data:ActAdminister${id} ?p1 ?o1 . `;

  let content = insertOrDelete === config.INSERT ? careActData : del_str;
  
  let sparql_query_str = `${insertOrDelete} ${insertOrDelete === config.INSERT ? "DATA" : ""} { ${content} } `;

  if(insertOrDelete === config.DELETE){
    let query_post_str = ` WHERE {  ${del_str}  }`;
    sparql_query_str += query_post_str ;
  }

   return await  utils.sparqlUpdate('careActions', sparql_query_str); 
}

//Defines drug types and categories, providing an english label.
function drugDef(type, id, englishLbl) {
  englishLbl = type !== "CombT" ? englishLbl : "a combination of " + englishLbl;

  return (drug =
    `data:` +
    type +
    id +
    ` a tmr:${
      type === "DrugCat"
        ? "DrugCategory"
        : type === "CombT"
        ? "CompoundType"
        : type === "VacT"
        ? "VaccineType"
        : "DrugType"
    }, owl:NamedIndividual ;
        rdfs:label "` +
    englishLbl +
    `"@en `);
}

//Defines non-drug related care actions
function nonDrugDef(type, id, label) {
  return `data:${type + id} a tmr:NonDrugType , owl:NamedIndividual ;
                      rdfs:label "${label}"@en `;
}

////////////////

//Administration action care general to both drug type and category
function drugAdminActDef(type, id, englishLabel) {
  let drugAdministration = `data:ActAdminister${id} a tmr:${
    type === "VacT"
      ? "VaccinationType"
      : type === "CombT"
      ? "CombinedCareActionType"
      : "DrugAdministrationType"
  }, owl:NamedIndividual ;
                      rdfs:label "administer ${englishLabel} "@en ;
                      tmr:administrationOf data:${type + id} `;

  return drugAdministration;
}

//Administration non drug action care
function nonDrugAdminActDef(type, id, actLabel) {

  return `data:ActAdminister${id}  a tmr:NonDrugAdministrationType , owl:NamedIndividual ;
                              rdfs:label "${actLabel}"@en ;
                              tmr:applicationOf data:${type + id} `;
}

//defines the insertion of clinical codes both in drugT and nonDrugT. Also in drug categories & combinations
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

  let drugT_def = drugDef(drug_type, id, label) + insertCodes(req);
  let careAct_def = drugAdminActDef(drug_type, id, action_label);
  //extras:

  //add grouping criteria or else components in combined drug types
  if (grouping_criteria) {
    drugT_def += addGroupingCriteria(grouping_criteria);
  }

  //add components in combined drug types or else subsumed drugs
  if (drug_type === 'CombT' && components_ids) {
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
                            tmr:hasGroupingCriteria  `;

  groupingCriteriaIds.split(",").forEach(function (criteriaId) {
    groupingCriteria += `data:Tr` + criteriaId.trim() + `, `;
  });
  //remove last comma and whitespace
  groupingCriteria = groupingCriteria.substring(0, groupingCriteria.length - 2);

  return groupingCriteria;
}

function addComponents(componentsStr) {
  let components_part = ` ;
                            tmr:hasComponent  `;

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
                      tmr:subsumes  `;

  drugIds.split(",").forEach(function (elem) {
    adminSubs += `data:ActAdminister` + elem.trim() + `, `;
  });
  //return after removing the last comma and whitespace
  return adminSubs.substring(0, adminSubs.length - 2);
}

function delete_def(type, id) {
  return `data:${type + id} ?p ?o .
   data:ActAdminister${id} ?p1 ?o1 .`;
}

module.exports = router;
