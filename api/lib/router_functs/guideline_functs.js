const Promise = require("bluebird");
const utils = Promise.promisifyAll(require("../utils.js"));
const logger = require("../../config/winston.js");
//const { ErrorHandler } = require("../lib/errorHandler");

const tmrDataUri = "http://anonymous.org/tmr/data/";
const tmrDataUri_short = "data:";
const sctUri = `http://snomed.info/sct/`;

/**
 * 
 * @param {String} label label representing the knowledge as a full URI or a shortened version
 * @param {String} prefix prefix to add to label
 * @param {Boolean} fullUri set the label argument as a full URI? otherwise, just the identifying section of the URI
 */
function setUri(label, prefix, fullUri, shortenedURI){

  //if its already full URI, return
  if(label.includes(tmrDataUri)) return label;
  
  let output = "";

  //if it is not full URI neither has the expected prefix, add prefix
  if(prefix && !label.startsWith(prefix))  output = prefix + "-";

  //add the label to the final output
  output += label;
 

  //construe URI
  output = fullUri ? (tmrDataUri + output) : shortenedURI ? (tmrDataUri_short + output) : output ;

  return output
}

function actionSubguideline(req, res, insertOrDelete) {
  if (!req.body.description) {
    req.body.description = "subGuideline " + req.body.subGuideline_id;
  }

  // SubGuideline declaration:
  const description =
    `data:subCIG-` +
    req.body.subGuideline_id +
    ` rdf:type tmr:subGuideline, owl:NamedIndividual ;
                           rdfs:label "` +
    req.body.description +
    `"@en ;
                           tmr:isSubGuidelineOf  data:CIG-` +
    req.body.guideline_id +
    ` .`;

  //var to construct the assignment of recs to a subguideline. initial whitespace to be kept
  var recDeclaration = " ";

  if (req.body.recs_ids) {
    //nanopublication is part of this subGuideline. contains  pred and object of resource
    const isPartOf =
      ` tmr:isPartOf   data:subCIG-` + req.body.subGuideline_id + ` .\n`;

    req.body.recs_ids.split(",").forEach(function (recId) {
      recDeclaration +=
        `data:Rec` + req.body.guideline_id + `-` + recId.trim() + isPartOf;
    });
  }

  utils.sparqlUpdate(
    "CIG-" + req.body.guideline_id,
    description + recDeclaration,
    insertOrDelete,
    function (err, status) {
      res.sendStatus(status);
    }
  );
}

/*
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
    if (!(headVar && bindings.hasOwnProperty(headVar))) continue;

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
        data.sameAs = value.split(", ");
        break;
      case "hasGroupingCriteria":
        data.hasGroupingCriteria = value.split(", ");
        break;
      case "subsumes":
        data.subsumes = value.split(", ");
        break;
    }
  }

  res.send(data);
}
*/

function get_rec_data(recURI, guidelineData, type) {
  //recommendation template object
  let recData = {
    id: recURI,
    partOf: undefined, //combined dataset or original
    extractedFrom: undefined, //original dataset
    type: {
      sctId: ( type === 'nonDrugType' ? "304541006" : type === 'vaccineType' ? '830152006' : "306807008" ),
      display: ( type === 'nonDrugType' ? "Recommendation to perform treatment (procedure)" : type === 'vaccineType' ? 'Recommendation regarding vaccination (procedure)' : "Recommendation to start drug treatment (procedure)" ),
    },
    careActionType: {
      id: undefined,
      requestType: 0, //for drug treatments
      code: undefined,
      display: undefined,
      drugLabel: undefined,
      sctId: undefined,
      hasComponents: undefined,
    },
    causationBeliefs: [],
    derivedFrom: undefined,
    hasSource: undefined,
    wasAttributedTo: undefined,
    generatedAtTime: undefined,
    hasFilterSituation: undefined,
  };
  let precond = {
    id: undefined,
    sctId: undefined,
    display: undefined,
    composedOf: undefined,
  };

  let headVars = guidelineData.head.vars;
  let bindingsList = guidelineData.results.bindings;
  logger.debug(bindingsList.length);

  for (const index in bindingsList) {
    //results object
    const bindingObj = bindingsList[index];
    //one TR and CB object per CB id encountered
    //one object per causation belief
    const cbData = {
      id: undefined,
      author: undefined,
      contribution: undefined,
      derivedFrom: undefined,
      hasSource: undefined,
      wasAttributedTo: undefined,
      generatedAtTime: undefined,
      probability: undefined,
      evidence: undefined,
      transition: {
        id: undefined,
        effect: undefined,
        property: {
          sctId: undefined,
          display: undefined,
          code: undefined,
          id: undefined,
        },
        situationTypes: [
          {
            id: undefined,
            type: "hasTransformableSituation",
            value: {
              stateOfProp: undefined,
              sctId: undefined,
              display: undefined,
              code: undefined,
            },
          },
          {
            id: undefined,
            type: "hasExpectedSituation",
            value: {
              stateOfProp: undefined,
              sctId: undefined,
              display: undefined,
              code: undefined,
            },
          },
        ],
      },
    };

    //format data by looping through head vars
    for (const pos in headVars) {
      //variable name
      let headVar = headVars[pos];

      //check there is a corresponding binding, if not, next head var
      if (!(headVar && bindingObj.hasOwnProperty(headVar))) continue;

      //otherwise, retrieve value
      let value = bindingObj[headVar].value;
      //temporary var
      let temp;

      //for each CB
      switch (headVar) {
        case "derivedFromCB":
          temp = value.split(",");
          cbData.derivedFrom = temp;
          break;
        case "hasSourcesCB":
          temp = value.split(",");
          cbData.hasSource = temp;
          break;
        case "contrb":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.contribution = temp.toLowerCase();
          break;
        case "cbUri":
          cbData.id = value;
          break;
        case "freq":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.probability = temp.toLowerCase();
          break;
        case "evidence":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.evidence = temp.toLowerCase();
          break;
        case "TrUri":
          cbData.transition.id = value;
          break;
        case "propTxt":
          cbData.transition.property.display = value;
          break;
        case "sctProp":
          cbData.transition.property.sctId = value;
          break;
        case "PropUri":
          cbData.transition.property.id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.property.code = temp;
          break;
        case "sitFromId":
          cbData.transition.situationTypes[0].id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.situationTypes[0].value.code = temp;
          break;
        case "sitToId":
          cbData.transition.situationTypes[1].id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.situationTypes[1].value.code = temp;
          break;
        case "sitFromLabel":
          cbData.transition.situationTypes[0].value.display = value;
          break;
        case "stateOfPreSit":
          cbData.transition.situationTypes[0].value.stateOfProp = value;
          break;
        case "sctPreSit":
          cbData.transition.situationTypes[0].value.sctId = value;
          break;
        case "sitToLabel":
          cbData.transition.situationTypes[1].value.display = value;
          break;
        case "stateOfPostSit":
          cbData.transition.situationTypes[1].value.stateOfProp = value;
          break;
        case "sctPostSit":
          cbData.transition.situationTypes[1].value.sctId = value;
          break;
        case "deriv":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.effect = temp.toLowerCase();
          break;
      }

      //take the first binding object and
      //create the outer content of recoomend minus CBs
      if (index < 1) {
        switch (headVar) {
          //precondition
          case "precond":
            precond.id = value;
            break;
          case "sctPrecond":
            precond.sctId = value;
            break;
          case "precondLbl":
            precond.display = value;
            break;
          case "compoundSituation": //TODO
            // precond.composedOf = value;
            break;
          //recommendation
          case "partOf":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.partOf = temp;
            break;
          case "extractedFrom":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.extractedFrom = temp;
            break;
          case "label":
            recData.text = value;
            break;
          case "strength":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.suggestion =
              temp == "Should"
                ? "recommend"
                : temp == "Should_not"
                ? "nonRecommend"
                : temp.toLowerCase();
            break;
          case "derivedFrom":
            temp = value.split(",");
            recData.derivedFrom = temp;
            break;
          case "hasSources":
            temp = value.split(",");
            recData.hasSource = temp;
            break;
          case "attributedTo":
            recData.wasAttributedTo = value;
            break;
          case "generatedTime":
            recData.generatedAtTime = value;
            break;
          //care action
          case "actId":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.careActionType.id = value;
            recData.careActionType.code = temp;
            break;
          case "adminLabel":
            recData.careActionType.display = value;
            break;
          case "actLabel":
            recData.careActionType.drugLabel = value;
            break;
          case "components":
            temp = value.split(",");
            recData.careActionType.hasComponents = temp;
            break;
          case "sctDrg":
            recData.careActionType.sctId = value;
            break;
          case "of":
            //extract code
            temp = value.split("/");
            temp = temp[temp.length - 1];
            //check for therapy
            if (temp === "applicationOf") {
              recData.careActionType.requestType = 1;
              recData.type.sctId = "304541006";
              recData.type.display =
                "Recommendation to perform treatment (procedure)";
            }
            //check for vaccine
            if (temp === "inoculationOf") {
              recData.careActionType.requestType = 2;
              (recData.type.sctId = "830152006"),
                (recData.type.display =
                  "Recommendation regarding vaccination (procedure)");
            }
            break;
        }
        if (precond.id) recData.hasFilterSituation = precond;
      }

      //test values of each binding object
      //logger.debug(`head var is ${headVar} and value is ${value}`);
    } //endOf headVars
    //add one CB object to Rec
    recData.causationBeliefs.push(cbData);
  } //endOf bindingObj

  return recData;
}

function get_statement_data(recURI, guidelineData) {
  //recommendation template object
  let recData = {
    id: recURI,
    partOf: undefined, //combined dataset or original
    extractedFrom: undefined, //original dataset
    type: {
      sctId: "223464006",
      display: "Procedure education (procedure)",
    },
    derivedFrom: undefined,
    hasSource: undefined,
    wasAttributedTo: undefined,
    generatedAtTime: undefined,
    hasFilterSituation: undefined,
    title: undefined,
    clinicalStatements: [],
  };

  let precond = {
    id: undefined,
    sctId: undefined,
    display: undefined,
    composedOf: undefined,
  };

  let headVars = guidelineData.head.vars;
  let bindingsList = guidelineData.results.bindings;
  //logger.debug(bindingsList.length);

  for (const index in bindingsList) {
    //results object
    const bindingObj = bindingsList[index];
    //one TR and CB object per CB id encountered
    //one object per causation belief
    const stData = {
      id: undefined,
      author: undefined,
      hasStatementTitle: undefined,
      hasStatementText: undefined,
      organization: undefined,
      jurisdiction: undefined,
      derivedFrom: undefined,
      hasSource: undefined,
      wasAttributedTo: undefined,
      generatedAtTime: undefined,
    };

    //format data by looping through head vars
    for (const pos in headVars) {
      //variable name
      let headVar = headVars[pos];

      //check there is a corresponding binding, if not, next head var
      if (!(headVar && bindingObj.hasOwnProperty(headVar))) continue;

      //otherwise, retrieve value
      let value = bindingObj[headVar].value;
      //temporary var
      let temp;

      //for each CB
      switch (headVar) {
        case "derivedFromSt":
          temp = value.split(",");
          stData.derivedFrom = temp;
          break;
        case "hasSourcesSt":
          temp = value.split(",");
          stData.hasSource = temp;
          break;
        case "orgNmsSt":
          temp = value.split(",");
          stData.organization = temp;
          break;
        case "stUri":
          stData.id = value;
          break;
        case "orgJursSt":
          temp = value.split(",");
          stData.jurisdiction = temp;
          break;
        case "stTxt":
          stData.hasStatementText = value;
          break;
        case "stTtl":
          stData.hasStatementTitle = value;
          break;
      }

      //take the first binding object and
      //create the outer content of recoomend minus CBs
      if (index < 1) {
        switch (headVar) {
          //precondition
          case "precond":
            precond.id = value;
            break;
          case "sctPrecond":
            precond.sctId = value;
            break;
          case "precondLbl":
            precond.display = value;
            break;
          case "compoundSituation": //TODO
            // precond.composedOf = value;
            break;
          //recommendation
          case "partOf":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.partOf = temp;
            break;
          case "extractedFrom":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.extractedFrom = temp;
            break;
          case "label":
            recData.title = value;
            break;
          case "derivedFrom":
            temp = value.split(",");
            recData.derivedFrom = temp;
            break;
          case "hasSources":
            temp = value.split(",");
            recData.hasSource = temp;
            break;
          case "attributedTo":
            recData.wasAttributedTo = value;
            break;
          case "generatedTime":
            recData.generatedAtTime = value;
            break;
          case "orgNms":
            recData.organization = value;
            break;
          case "orgJurs":
            recData.jurisdiction = value;
            break;
        }
        if (precond.id) recData.hasFilterSituation = precond;
      }

      //test values of each binding object
      //logger.debug(`head var is ${headVar} and value is ${value}`);
    } //endOf headVars
    //add one CB object to Rec
    recData.clinicalStatements.push(stData);
  } //endOf bindingObj

  return recData;
}

function get_rec_json_data(recURI, guidelineData,type) {
  //recommendation template object
  let recData = {
    id: recURI,
    partOf: undefined, //combined dataset or original
    extractedFrom: undefined, //original dataset
    label: undefined,
    type: {
      sctId: ( type === 'nonDrugType' ? "304541006" : type === 'vaccineType' ? '830152006' : "306807008" ),
      display: ( type === 'nonDrugType' ? "Recommendation to perform treatment (procedure)" : type === 'vaccineType' ? 'Recommendation regarding vaccination (procedure)' : "Recommendation to start drug treatment (procedure)" ),
    },
    derivedFrom: undefined,
    hasSource: undefined,
    wasAttributedTo: undefined,
    generatedAtTime: undefined,
    hasFilterSituation: undefined,
    careActionType: {
      id: undefined,
      requestType: 0, //for drug treatments
      code: undefined,
      label: undefined,
      drug: {
        system: undefined,
        sctId: undefined,
        display: undefined,
        hasComponents: undefined,
      },
    },
    causationBeliefs: [],
  };
  let precond = {
    id: undefined,
    system: undefined,
    sctId: undefined,
    display: undefined,
    composedOf: undefined,
  };

  let headVars = guidelineData.head.vars;
  let bindingsList = guidelineData.results.bindings;


  for (const index in bindingsList) {
    //results object
    const bindingObj = bindingsList[index];
    //one TR and CB object per CB id encountered
    //one object per causation belief
    const cbData = {
      id: undefined,
      author: undefined,
      contribution: undefined,
      derivedFrom: undefined,
      hasSource: undefined,
      wasAttributedTo: undefined,
      generatedAtTime: undefined,
      probability: undefined,
      evidence: undefined,
      careActionTypeRef: undefined,
      transition: {
        id: undefined,
        effect: undefined,
        property: {
          id: undefined,
          code: undefined,
          sctId: undefined,
          system: undefined,
          display: undefined,
        },
        situationTypes: [
          {
            id: undefined,
            type: "hasTransformableSituation",
            value: {
              stateOfProp: undefined,
              sctId: undefined,
              system: undefined,
              display: undefined,
              code: undefined,
            },
          },
          {
            id: undefined,
            type: "hasExpectedSituation",
            value: {
              stateOfProp: undefined,
              sctId: undefined,
              system: undefined,
              display: undefined,
              code: undefined,
            },
          },
        ],
      },
    };

    //format data by looping through head vars
    for (const pos in headVars) {
      //variable name
      let headVar = headVars[pos];

      //check there is a corresponding binding, if not, next head var
      if (!(headVar && bindingObj.hasOwnProperty(headVar))) continue;

      //otherwise, retrieve value
      let value = bindingObj[headVar].value;
      //temporary var
      let temp;

      //for each CB
      switch (headVar) {
        case "derivedFromCB":
          temp = value.split(",");
          cbData.derivedFrom = temp;
          break;
        case "hasSourcesCB":
          temp = value.split(",");
          cbData.hasSource = temp;
          break;
        case "contrib":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.contribution = temp.toLowerCase();
          break;
        case "actAdminCb":
          cbData.careActionTypeRef = value;
          break;
        case "cbUri":
          cbData.id = value;
          break;
        case "freq":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.probability = temp.toLowerCase();
          break;
        case "evidence":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.evidence = temp.toLowerCase();
          break;
        case "TrUri":
          cbData.transition.id = value;
          break;
        case "propLabel":
          cbData.transition.property.display = value;
          break;
        case "propSctId":
          cbData.transition.property.sctId = value;
          cbData.transition.property.system = sctUri;
          break;
        case "PropUri":
          cbData.transition.property.id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.property.code = temp;
          break;
        case "sitFromId":
          cbData.transition.situationTypes[0].id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.situationTypes[0].value.code = temp;
          break;
        case "sitToId":
          cbData.transition.situationTypes[1].id = value;
          //extract code
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.situationTypes[1].value.code = temp;
          break;
        case "sitFromLabel":
          cbData.transition.situationTypes[0].value.display = value;
          break;
        case "sitFromStateOf":
          cbData.transition.situationTypes[0].value.stateOfProp = value;
          break;
        case "sctPreSit":
          cbData.transition.situationTypes[0].value.sctId = value;
          cbData.transition.situationTypes[0].value.system = sctUri;
          break;
        case "sitToLabel":
          cbData.transition.situationTypes[1].value.display = value;
          break;
        case "sitToStateOf":
          cbData.transition.situationTypes[1].value.stateOfProp = value;
          break;
        case "sitFromSctId":
          cbData.transition.situationTypes[0].value.sctId = value;
          cbData.transition.situationTypes[0].value.system = sctUri;
          break;
        case "deriv":
          temp = value.split("/");
          temp = temp[temp.length - 1];
          cbData.transition.effect = temp.toLowerCase();
          break;
        case "sitToSctId":
          cbData.transition.situationTypes[1].value.sctId = value;
          cbData.transition.situationTypes[1].value.system = sctUri;
          break;
        default:
          logger.debug(
            "switch headVar param: " + headVar + " with value : " + value
          );
          break;
      }

      //take the first binding object and
      //create the outer content of recommend minus CBs
      if (index < 1) {
        switch (headVar) {
          //precondition
          case "precond":
            precond.id = value;
            break;
          case "sctPrecond":
            precond.sctId = value;
            break;
          case "precondLbl":
            precond.display = value;
            break;
          case "compoundSituation": //TODO
            // precond.composedOf = value;
            break;
          //recommendation
          case "partOf":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.partOf = temp;
            break;
          case "extractedFrom":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.extractedFrom = temp;
            break;
          case "text":
            recData.label = value;
            break;
          case "strength":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.suggestion =
              temp == "Should"
                ? "recommend"
                : temp == "Should_not"
                ? "nonRecommend"
                : temp.toLowerCase();
            break;
          case "derivedFrom":
            temp = value.split(",");
            recData.derivedFrom = temp;
            break;
          case "hasSources":
            temp = value.split(",");
            recData.hasSource = temp;
            break;
          case "attributedTo":
            recData.wasAttributedTo = value;
            break;
          case "generatedTime":
            recData.generatedAtTime = value;
            break;
          //care action
          case "actAdmin":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.careActionType.id = value;
            recData.careActionType.code = temp;
            break;
          case "adminLabel":
            recData.careActionType.label = value;
            break;
          case "actId":
            temp = value.split("/");
            temp = temp[temp.length - 1];
            recData.careActionType.drug.id = value;
            recData.careActionType.drug.code = temp;
            break;
          case "actLabel":
            recData.careActionType.drug.display = value;
            break;
          case "components":
            temp = value.split(",");
            recData.careActionType.hasComponents = temp;
            break;
          case "sctDrg":
            recData.careActionType.drug.sctId = value;
            recData.careActionType.drug.system = sctUri;
            break;
          case "of":
            //extract code
            temp = value.split("/");
            temp = temp[temp.length - 1];
            //check for therapy
            if (temp === "applicationOf") {
              recData.careActionType.requestType = 1;
              recData.type.sctId = "304541006";
              recData.type.system = sctUri;
              recData.type.display =
                "Recommendation to perform treatment (procedure)";
            }
            //check for vaccine
            if (temp === "inoculationOf") {
              recData.careActionType.requestType = 2;
              recData.type.system = sctUri;
              (recData.type.sctId = "830152006"),
                (recData.type.display =
                  "Recommendation regarding vaccination (procedure)");
            }
            break;
        }
        if (precond.id) recData.hasFilterSituation = precond;
      }

      //test values of each binding object
      //logger.debug(`head var is ${headVar} and value is ${value}`);
    } //endOf headVars
    //add one CB object to Rec
    recData.causationBeliefs.push(cbData);
  } //endOf bindingObj

  return recData;
}

function action_rec(req, res, insertOrDelete) {
  //data id for this rec
  const id = `data:Rec${req.body.cig_id}-${req.body.rec_id}`;
  let sources = "";
  const date = new Date().toJSON();
  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = `${id} tmr:isPartOf data:CIG-${req.body.cig_id} .`;

  if (req.body.derivedFrom) {
    sources = `  prov:wasDerivedFrom  `;

    req.body.derivedFrom.split(",").forEach(function (code) {
      sources += ` <` + code + `> ,`;
    });
    //this removes the last coma
    sources = sources.substring(0, sources.length - 1);
  }

  // Guideline format:
  const head =
    id +
    `_head { 
          ` +
    id +
    `_head
              a     nanopub:Nanopublication ;
              nanopub:hasAssertion        ` +
    id +
    ` ;
              nanopub:hasProvenance       ` +
    id +
    `_provenance ;
              nanopub:hasPublicationInfo  ` +
    id +
    `_publicationinfo .
    }`;

  const body =
    id +
    ` {
      ` +
    id +
    `  a   tmr:ClinicalRecommendation ;
              rdfs:label  '''` +
    req.body.label +
    `'''@en ;
              tmr:aboutExecutionOf  data:ActAdminister` +
    req.body.careAction_id +
    ` ;
              tmr:partOf            data:CIG-` +
    req.body.cig_id +
    ` ;
              tmr:strength          tmr:` +
    req.body.strength +
    ` .
    }`;

  const provenance =
    id +
    `_provenance {
      ` +
    id +
    `_provenance
      a  oa:Annotation ;
      oa:hasBody  ` +
    id +
    ` ;
      oa:hasTarget                  [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
      ` +
    id +
    `
      ` +
    sources +
    ` .
      }`;

  const publication =
    id +
    `_publicationinfo {
        ` +
    id +
    `_head
        prov:generatedAtTime          "` +
    date +
    `"^^xsd:dateTime ;
        prov:wasAttributedTo          data:` +
    req.body.author +
    `.
      }`;

  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    "GRAPH " +
      head +
      "\nGRAPH " +
      body +
      "\nGRAPH " +
      provenance +
      "\nGRAPH " +
      publication,
    insertOrDelete,
    function (err, status) {
      if (status === 200) {
        //add assertion id to default graph as part of CIG
        utils.sparqlUpdate(
          "CIG-" + req.body.cig_id,
          id2CIG,
          insertOrDelete,
          function (err2, status2) {
            res.status(status2).end();
          }
        );
      } else {
        //didnt work. send first status back
        logger.error(err);
        res.status(status);
      }
    }
  );
}

function insert_CB_in_rec(req, res, insertOrDelete) {
  const recId = `data:Rec` + req.body.cig_id + `-` + req.body.rec_id;

  const body =
    recId +
    ` {
      ` +
    recId +
    ` tmr:basedOn data:CB` +
    req.body.belief_id +
    ` .
      data:CB` +
    req.body.belief_id +
    ` tmr:contribution tmr:` +
    req.body.contribution +
    `.
    }`;

  const graph = `GRAPH ${body}`;
  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    graph,
    insertOrDelete,
    function (err, status) {
      if (err) {
        logger.debug(`error when updating recommendation with belief: ${err}`);
      }
      res.status(status).end();
    }
  );
}

function insert_precond_in_rec(req, res, insertOrDelete) {
  const recId = `data:Rec` + req.body.cig_id + `-` + req.body.rec_id;

  const body =
    recId +
    ` {
      ` +
    recId +
    ` tmr:hasFilterSituation data:Sit` +
    req.body.precond_id +
    ` .
    }`;

  const graph = `GRAPH ${body}`;
  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    graph,
    insertOrDelete,
    function (err, status) {
      if (err) {
        logger.debug(
          `error when updating recommendation with precondition: ${err}`
        );
      }
      res.status(status).end();
    }
  );
}

function action_gprec(req, res, insertOrDelete) {
  //data id for this rec
  const id = `data:GPRec` + req.body.cig_id + `-` + req.body.gpRec_id;

  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = id + ` tmr:isPartOf data:CIG-` + req.body.cig_id + ` .`;

  // Graph format:
  const head =
    id +
    `_head { 
          ` +
    id +
    `_head
              a     nanopub:Nanopublication ;
              nanopub:hasAssertion        ` +
    id +
    ` ;
              nanopub:hasProvenance       ` +
    id +
    `_provenance ;
              nanopub:hasPublicationInfo  ` +
    id +
    `_publicationinfo .
    }`;

  const body =
    id +
    ` {
      ` +
    id +
    `
              a   tmr:GoodPracticeRecommendation ;
              rdfs:label  '''${req.body.gpRec_label}'''@en ;
              tmr:aboutNotificationOf  data:ST` +
    req.body.statement_id +
    ` ;
              tmr:partOf            data:CIG-` +
    req.body.cig_id +
    ` ;
  
    }`;

  const provenance =
    id +
    `_provenance {
      ` +
    id +
    `
              prov:wasDerivedFrom  <` +
    (req.body.source ? req.body.source : "unknown") +
    `> .
  
      ` +
    id +
    `_provenance
              a             oa:Annotation ;
              oa:hasBody    ` +
    id +
    ` ;
              oa:hasTarget  [ oa:hasSource  <http://hdl.handle.net/10222/43703> ] .
    }`;

  const publication =
    id +
    `_publicationinfo {
        ` +
    id +
    `_head
              prov:generatedAtTime  "2020-03-01"^^xsd:dateTime ;
              prov:wasAttributedTo  data:` +
    req.body.author +
    ` .
    }`;
  const graph = `GRAPH ${head} \n GRAPH ${body} \n GRAPH ${provenance} \n GRAPH ${publication}`;

  utils.sparqlUpdate(
    "CIG-" + req.body.cig_id,
    graph,
    insertOrDelete,
    function (err, status) {
      if (err) {
        logger.debug(
          `error when updating good practice recommendation: ${err}`
        );
        res.status(status).end();
      } else {
        //add assertion id to default graph as part of CIG
        if (status === 200) {
          utils.sparqlUpdate(
            "CIG-" + req.body.cig_id,
            id2CIG,
            insertOrDelete,
            function (err2, status2) {
              if (err2) logger.debug(`action_gprec Error: ${err2}`);

              res.status(status2).end();
            }
          );
        }
      }
    }
  );
}

//filter out TMR types when unnecesary for the triggered router
function filter_TMR_rec_type(RecUris) {
  if (!Array.isArray(RecUris)) return RecUris;
  let result = RecUris.filter(
    (uri) =>
      !(
        uri === "http://anonymous.org/tmr/ClinicalRecommendation" ||
        uri === "http://anonymous.org/tmr/GoodPracticeRecommendation"
      )
  );
  return result;
}

/**
 * 
 * @param {array} head_vars 
 * @param {object} binding 
 */
function get_care_action(head_vars, binding) {
 logger.debug(head_vars)
 logger.debug(binding)

  let careAction = {};

  //format rdf by looping through head vars
  for (let pos in head_vars) {
    //variable name
    let headVar = head_vars[pos];
    logger.debug(`headVar is ${headVar}`)
    //check there is a corresponding binding, if not, next head var
    if (!binding.hasOwnProperty(headVar)) continue;

    //otherwise, retrieve value
    let value = binding[headVar].value;
    logger.debug(`binding value is ${value}`)
    //for each heading, add a field
    switch (headVar) {
      case "actId":
        careAction.id = value;
        break;
      case "adminLabel":
        careAction.display = value;
        break;
      case "actType":
        //extract code
        let type ="";
        logger.debug(value)
        if(value.startsWith('http://anonymous.org/vocab/')){
           type = value.slice(27);
        } else {
          //http://anonymous.org/tmr/vocab/DrugType
          type = value.slice(31);
        }
        careAction.code = type;
        careAction.requestType = 0; //for individual or category drugs that are not vaccines
        //check for therapy
        if (type.startsWith("NonDrugT")) {
          careAction.requestType = 1;
        } else {
          //check for vaccine
          if (type.startsWith("VaccineT")) {
            careAction.requestType = 2;
          } else {
            //check for care actions combination
            if(type.startsWith("CombT")) {
              careAction.requestType = 3;
            }
          }
        }
        break;
      case "actLabel":
        careAction.drugLabel = value;
        careAction.sct_trm = value;
        break;
      case "snomed":
        careAction.snomedCode = value;
        careAction.sct_id = value;
        break;
      case "sameAs":
        careAction.sameAs = value.split(", ");
        break;
      case "hasGroupingCriteria":
        careAction.hasGroupingCriteria = value.split(", ");
        break;
      case "subsumes":
        careAction.subsumes = value.split(", ");
        break;
      case "components":
        careAction.hasComponents = value.split(", ");
        break;
    }
  }//endOf loop

  if (!careAction.hasOwnProperty('snomedCode'))  careAction.sct_trm = undefined;

  return careAction
}

module.exports = {
  filter_TMR_rec_type,
  action_gprec,
  insert_precond_in_rec,
  insert_CB_in_rec,
  action_rec,
  get_rec_json_data,
  get_statement_data,
  get_rec_data,
  actionSubguideline,
  tmrDataUri,
  sctUri,
  setUri,
  get_care_action
};
