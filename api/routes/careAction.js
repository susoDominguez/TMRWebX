const express = require("express");
const router = express.Router();
const request = require("request");

const config = require("../lib/config");
const guidelines = require("../lib/namespace_PREFIX");
const logger = require("../config/winston");
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");

function postDrugs(careActData, insertOrDelete, callback) {
  utils.sparqlUpdate("careActions", careActData, insertOrDelete, callback);
}

router.post("/drug/individual/add", function (req, res) {
  careActDef("SingleDrugType", req, config.INSERT, (err, status) => {
    if (err) {
      logger.error(`careActDef: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

//both for drug and nondrug individuals
router.post("/drug/individual/delete", function (req, res) {
  careActDef("SingleDrugType", req, config.DELETE, (err, status) => {
    if (err) {
      logger.error(`careActDef: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/drug/vaccine/add", function (req, res) {
  careActDef("VaccineType", req, config.INSERT, (err, status) => {
    if (err) {
      logger.error(`careActDef:vaccine: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/drug/vaccine/delete", function (req, res) {
  careActDef("VaccineType", req, config.DELETE, (err, status) => {
    if (err) {
      logger.error(`careActDef: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/nondrug/individual/add", function (req, res) {
  careActDef("NonDrugType", req, config.INSERT, (err, status) => {
    if (err) {
      logger.error(`careActDef: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/nondrug/individual/delete", function (req, res) {
  careActDef("NonDrugType", req, config.DELETE, (err, status) => {
    if (err) {
      logger.error(`careActDef: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/drug/category/add", function (req, res) {
  careActDef("DrugCategory", req, config.INSERT, (err, status) => {
    if (err) {
      logger.error(`drugCatCareAction: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/drug/category/delete", function (req, res) {
  careActDef("DrugCategory", req, config.DELETE, (err, status) => {
    if (err) {
      logger.error(`drugCatCareAction: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/drug/combination/add", function (req, res) {
  careActDef("CombinedDrugType", req, config.INSERT, (err, status) => {
    if (err) {
      logger.error(`drugTCombCareAction: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/drug/combination/delete", function (req, res) {
  careActDef("CombinedDrugType", req, config.DELETE, (err, status) => {
    if (err) {
      logger.error(`drugTCombCareAction: error ${JSON.stringify(err)}`);
      return res.status(500).end();
    } else {
      res.status(status).end();
    }
  });
});

router.post("/effect/get", function (req, res) {
  let postData = "";

  if (req.body.drugCat_id) {
    postData = require("querystring").stringify({
      drugCat_id: "http://anonymous.org/data/DrugCat" + req.body.drugCat_id,
    });
  } else {
    if (req.body.drugT_id) {
      postData = require("querystring").stringify({
        drugT_id: "http://anonymous.org/data/DrugT" + req.body.drugT_id,
      });
    } else {
      if (req.body.nonDrugT_id) {
        postData = require("querystring").stringify({
          nonDrugT_id:
            "http://anonymous.org/data/NonDrugT" + req.body.nonDrugT_id,
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

//TODO: revise
router.post("/all/get/", function (req, res) {
  if (req.body.uri) {
    utils.getCareActionData(
      "careActions",
      req.body.uri,
      function (err, actionResults) {
        if (err) {
          logger.error(`getCareActionData: error ${JSON.stringify(err)}`);
          return res.status(404).end(err);
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
        return res.send(data);
      }
    );
  } else {
    return res.status(406).end();
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
  return `data:Drug${typeOrCat}${id} a vocab:${
    typeOrCat === "T" ? "SingleDrugType" : "DrugCategory"
  }, owl:NamedIndividual ;
                                      rdfs:label "${label}"@en `;
}

//Defines non-drug related care actions
function nonDrugDef(typeOrCat, id, label) {
  return `data:NonDrug${typeOrCat}${id} a vocab:NonDrugType, owl:NamedIndividual ;
                                  rdfs:label "${label}"@en `;
}

//defines drug type combinations, providing an english label
function drugTCombDef(id, label) {
  return `data:DrugCombT${id} a vocab:CombinedDrugType, owl:NamedIndividual ;
                          rdfs:label "${label}"@en `;
}

//defines drug type combinations, providing an english label
function vaccineDef(id, label) {
  return `data:VaccineT${id} a vocab:VaccineType, owl:NamedIndividual ;
                          rdfs:label "${label}"@en `;
}

////////////////

//Administration action care general to both drug type and category
function drugAdminActDef(typeOrCat, id, label) {
  let drugAdministration =
    `data:ActAdminister` +
    id +
    ` a vocab:DrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "administer ` +
    label +
    `"@en ;
                               vocab:administrationOf data:Drug` +
    typeOrCat +
    id;

  return drugAdministration;
}

//administration action care to drug combination type
function drugCombTAdminActDef(id, label) {
  let drugAdministration = `data:ActAdminister${id} a vocab:DrugAdministrationType, owl:NamedIndividual ;
                            rdfs:label "administer ${label}"@en ;
                           vocab:administrationOf data:DrugCombT${id} `;

  return drugAdministration;
}

//administration of vaccination
function vaccineAdminActDef(id, label) {
  let drugAdministration =
    `data:ActAdminister` +
    id +
    ` a vocab:VaccinationType, owl:NamedIndividual ;
                               rdfs:label "administer ` +
    label +
    `"@en ;
                               vocab:inoculationOf data:VaccineType${id}`;

  return drugAdministration;
}

//Administration non drug action care
function nonDrugAdminActDef(typeOrCat, id, actLabel) {
  let nonDrugAdmin =
    `data:ActAdminister` +
    id +
    ` a vocab:NonDrugAdministrationType, owl:NamedIndividual ;
                               rdfs:label "` +
    actLabel +
    `"@en ;
                               vocab:applicationOf data:NonDrug` +
    typeOrCat +
    id;

  return nonDrugAdmin;
}

// Administration Action Care specialisation of a drugT from a drugT.
function adminActSub(id) {
  return ` ;
    vocab:subsumes data:ActAdminister${id}`;
}

//defines the insertion of clinical codes both in all drugs and nondrugs categories
function insertCodes(req) {
  const SCTcodingSystem = "http://snomed.info/sct";

  let insertCodesStatement = "";

  //icd 10 code
  if (req.body.icd10Codes) {
    insertCodesStatement += `;`;

    req.body.icd10Codes.split(",").forEach(function (code) {
      insertCodesStatement += `
        vocab:icd10Code   ${code.trim()}^^xsd:string ;`;
    });
    //this removes the last semicolon
    insertCodesStatement = insertCodesStatement.substring(
      0,
      insertCodesStatement.length - 1
    );
  }
  //ONE sct code
  if (req.body.snomedCode) {
    insertCodesStatement += ` ;
      vocab:codeSystem "${SCTcodingSystem}" ; `;

    //only simple expressions
    let [sctID, term] = req.body.snomedCode.split("|");

    //add code ID
    if (sctID) {
      insertCodesStatement += `
       vocab:snomedCode "${sctID.trim()}"^^xsd:string ;`;
    }
    //add term
    if (term) {
      insertCodesStatement += `
       vocab:snomedTerm "${term.trim()}"^^xsd:string ;`;
    }
    //this removes the last semicolon
    insertCodesStatement = insertCodesStatement.substring(
      0,
      insertCodesStatement.length - 1
    );
  }
  //umls code
  if (req.body.umlsCodes) {
    insertCodesStatement += `;`;

    req.body.umlsCodes.split(",").forEach(function (code) {
      insertCodesStatement +=
        `
      vocab:umlsCode   "` +
        code.trim() +
        `"^^xsd:string ;`;
    });
    //this removes the last semicolon
    insertCodesStatement = insertCodesStatement.substring(
      0,
      insertCodesStatement.length - 1
    );
  }

  return insertCodesStatement;
}

function insertDrugTList(req) {
  let drugTList = "";

  if (req.body.listOfDrugTypeComponents) {
    drugTList += `; 
     vocab:hasComponent`;

    req.body.listOfDrugTypeComponents.split(",").forEach(function (drugT) {
      drugTList += ` data:DrugT${drugT.trim()} ,`;
    });
    //this removes the last comma
    drugTList = drugTList.substring(0, drugTList.length - 1);
  } else {
    throw ErrorHandler(
      500,
      `function insertDrugTList: 2 or more drug types are required to define a combination of drug types.`
    );
  }

  return drugTList;
}

//defines ALL DrugT and also DrugT Admin. Also non drug careActT and careActT Admin
function careActDef(actionType, req, insertOrDelete, callback) {
  let action = "";
  let careAdmin = "";

  switch (actionType) {
    case "CombinedDrugType":
      // Drugs combination format:
      let action =
        drugTCombDef(
          req.body.id,
          req.body.label ? req.body.label : req.body.id
        ) +
        insertDrugTList(req) +
        addGroupingCriteria(req.body.grouping_criteria_id) +
        insertCodes(req);

      //end statement
      action += ` .`;

      //administration of drugs combination format
      let careAdmin = drugCombTAdminActDef(
        req.body.id,
        req.body.action_label
          ? req.body.action_label
          : req.body.label
          ? req.body.label
          : req.body.id
      );
      //end statement
      careAdmin += ` .`;
      break;
    case "NonDrugType":
      action = nonDrugDef(
        "T",
        req.body.id,
        req.body.label ? req.body.label : req.body.id
      );
      action += insertCodes(req) + ` .`;

      careAdmin = nonDrugAdminActDef("T", req.body.id, req.body.label);

      if (req.body.subsumed_id) {
        careAdmin += adminActSub(req.body.subsumed_id);
      }
      break;
    case "SingleDrugType":
      action =
        drugDef(
          "T",
          req.body.id,
          req.body.label ? req.body.label : req.body.id
        ) +
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
        careAdmin += adminActSub(req.body.subsumed_id);
      }
    case "DrugCategory":
      action =
        drugDef(
          "Cat",
          req.body.id,
          req.body.label ? req.body.label : req.body.id
        ) + insertCodes(req);

      //add grouping criteria
      if (req.body.grouping_criteria_id) {
        action += addGroupingCriteria(req.body.grouping_criteria_id);
      }

      //end statement
      action += ` .`;

      careAdmin = drugAdminActDef(
        "Cat",
        req.body.id,
        req.body.action_label
          ? req.body.action_label
          : req.body.label
          ? req.body.label
          : req.body.id
      );

      //add subsumed action care types
      if (req.body.subsumed_id) {
        careAdmin += adminActSubs(req.body.subsumed_id);
      }
      //end statement
      careAdmin += ` .`;
      break;
    case "VaccineType":
      action =
        vaccineDef(req.body.id, req.body.label ? req.body.label : req.body.id) +
        insertCodes(req) +
        ` .`;

      careAdmin = vaccineAdminActDef(
        req.body.id,
        req.body.action_label
          ? req.body.action_label
          : req.body.label
          ? req.body.label
          : req.body.id
      );

      if (req.body.subsumed_id) {
        careAdmin += adminActSub(req.body.subsumed_id);
      }
      break;
  } //endOf switch

  postDrugs(`${action} ${careAdmin}`, insertOrDelete, callback);
}

////////////////////////////

//add one or more drugTypes as part of the grouping criteria of a drug category
function addGroupingCriteria(groupingCriteriaIds) {
  let vocab = ` ;
    vocab:hasGroupingCriteria`;
  let groupingCriteria = "";

  groupingCriteriaIds.split(",").forEach(function (criteriaId) {
    groupingCriteria += ` data:Tr${criteriaId.trim()} ,`;
  });
  //remove last comma and whitespace
  groupingCriteria = groupingCriteria.substring(0, groupingCriteria.length - 1);

  return groupingCriteria.length > 0
    ? vocab + groupingCriteria
    : groupingCriteria;
}

//Specify multiple drug subsumptions via the administrationOf triple.
function adminActSubs(drugIds) {
  let subsumes = ` ;
                      vocab:subsumes`;
  let adminSubs = "";

  drugIds.split(",").forEach(function (elem) {
    adminSubs += ` data:ActAdminister ${elem.trim()} ,`;
  });

  //return after removing the last comma and whitespace
  return adminSubs.length > 0
    ? subsumes + adminSubs.substring(0, adminSubs.length - 1)
    : adminSubs;
}

module.exports = router;
