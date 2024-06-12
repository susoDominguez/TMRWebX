const express = require("express");
const router = express.Router();
//const axios = require("axios");
const bodyParser = require("body-parser");
//const Promise = require("bluebird");
const config = require("../lib/config");
const guidelines = require("../lib/prefixes");
const utils = require("../lib/utils");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const { handleError, ErrorHandler } = require("../lib/errorHandler");
const logger = require("../config/winston");
const {
  insert_precond_in_rec,
  insert_CB_in_rec,
  get_CB_uris_from_bindings,
  set_uri,
  get_rec_json_data,
  set_cig_id,
  get_precondition_object
} = require("../lib/router_functs/guideline_functs.js");
const { error } = require("console");

const dataUri = "http://anonymous.org/data";
const sctUri = `http://snomed.info/sct/`;

async function get_recs_uris(cig_id) {
  let { status, bindings, head_vars } =
    await utils.get_named_subject_in_named_graphs_from_object(
      cig_id,
      "vocab:ClinicalRecommendation"
    );

  return auxFuncts.get_rdf_atom_as_array(bindings);
}

async function get_gprecs_uris(cig_id) {
  let { status, bindings, head_vars } =
    await utils.get_named_subject_in_named_graphs_from_object(
      cig_id,
      "vocab:GoodPracticeRecommendation"
    );

  return auxFuncts.get_rdf_atom_as_array(bindings);
}

async function get_gprec_contents(idCig, recURI) {
  let gpRec = {};

  //add main contents of gpRec
  gpRec = await utils
    .getRecStmntData(idCig, recURI, "statements")
    .then(({ status, bindings, head_vars }) =>
      auxFuncts.get_gpRec_data(head_vars, bindings[0])
    );

  //then clinical statement(s)

  //if no clinical statemnts are found it is an error
  if (gpRec["clinicalStatements"].length === 0)
    throw new ErrorHandler(
      500,
      `clinicalStatements field is missing when constructing goodPractice recommendation in router gprec/all/get `
    );

  //otherwise, fetch the rdf and convert to JSON for all URIs found
  let results = await Promise.all(
    gpRec["clinicalStatements"].map((uri) =>
      utils.getStatementData("statements", null, uri)
    )
  );

  gpRec["clinicalStatements"] = results.map(({ status, head_vars, bindings }) =>
    auxFuncts.get_ST_data(head_vars, bindings[0])
  );

  return gpRec;
}

async function get_rec_contents(cigId, recURI) {
  let rec = null;

  //add main contents of rec
  let { status, bindings, head_vars } = await utils.getRecData(
    cigId,
    recURI,
    "beliefs",
    "transitions",
    "careActions"
  );

  //format rec knowledge to JSON
  let rec_json = get_rec_json_data(recURI, head_vars, bindings[0]);

  //Array for Promise.all
  let promise_array = new Array();

  //then causation beliefs
 
  //retrieve each CB and associated data
  let { uris, contribs } = await get_CB_uris_from_bindings(bindings);

  promise_array = uris.map( (uri) =>
    utils.getBeliefData("beliefs", uri, "transitions", "careActions")
  );

  //add careAction getter
  promise_array.push(
    utils.getCareActionData("careActions", undefined, rec_json.care_action)
  );

  //if filter situation, add it as last element
  let hasPrecondition = false;

  if (rec_json.hasOwnProperty("hasFilterSituation") && typeof rec_json['hasFilterSituation'] !== 'undefined') {

    hasPrecondition = true;

    promise_array.push(
      utils.getPreconditionData(
        "transitions",
        rec_json["hasFilterSituation"]["id"]
      )
    );
  }

  let promise_rslt, filter_situation_rslt, care_action_rslt;

  try {
    //get results
    promise_rslt = await Promise.all(promise_array).catch(err=> {logger.error(`Error when retrieving CBs,care_actions_predicates: ${JSON.stringify(err)}`); throw err;});

    //get precondition results, if any
    if (hasPrecondition) filter_situation_rslt = promise_rslt.pop();

    //logger.debug(JSON.stringify(`filter situation is ${JSON.stringify(filter_situation_rslt)}`));

    //get care action results
    care_action_rslt = promise_rslt.pop();

    //now promise_rslt is CBs only
  } catch (err) {
    logger.error(`Error in get guideline recommendation : ${err}`);
    throw err;
  }
 
  //add CBs 
  rec_json["causation_beliefs"] = promise_rslt.map(
    ({ status, head_vars, bindings }) => {
      let cb_obj = auxFuncts.get_CB_object(head_vars, bindings[0]);
      cb_obj['contribution'] = contribs.get(cb_obj.id);
      return cb_obj;
    }
  );
 
  //if precondition, add
  if (hasPrecondition) {
    rec_json["hasFilterSituation"] = get_precondition_object(filter_situation_rslt.head_vars, filter_situation_rslt.bindings[0]); 
  }
  
  //add care_Actions
  let ca_data = auxFuncts.get_care_action(
    care_action_rslt.head_vars,
    care_action_rslt.bindings[0]
  );

  let type = ca_data["administers"]["type"];

  rec_json.type = {
    system: sctUri,
    code:
      type === "nonDrugType"
        ? "304541006"
        : type === "vaccineType"
        ? "830152006"
        : "306807008",
    display:
      type === "nonDrugType"
        ? "Recommendation to perform treatment (procedure)"
        : type === "vaccineType"
        ? "Recommendation regarding vaccination (procedure)"
        : "Recommendation to start drug treatment (procedure)",
  };

  rec_json["care_action"] = ca_data;

  return rec_json;
}

function action_gprec(req) {
  //data id for this rec
  const id = `data:GPRec` + req.body.cig_id + `-` + req.body.gpRec_id;

  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = id + ` vocab:isPartOf data:CIG-` + req.body.cig_id + ` .`;

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
    `         a   vocab:GoodPracticeRecommendation ;
              rdfs:label  '''${req.body.gpRec_label}'''@en ;
              vocab:aboutNotificationOf  data:ST` +
    req.body.statement_id +
    ` ;
              vocab:partOf            data:CIG-` +
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
  return `  ${id2CIG}  \n GRAPH ${head} \n GRAPH ${body} \n GRAPH ${provenance} \n GRAPH ${publication}`;
}

function action_rec(req) {
  //data id for this rec
  const id = `data:Rec${req.body.cig_id}-${req.body.id}`;
  let sources = "";
  const date = new Date().toJSON();
  //this nanopublication is included in the main  guideline (to be added to default graph)
  const id2CIG = `${id} vocab:isPartOf data:CIG-${req.body.cig_id} .`;

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
    `  a   vocab:ClinicalRecommendation ;
              rdfs:label  '''` +
    req.body.label +
    `'''@en ;
              vocab:aboutExecutionOf  data:ActAdminister` +
    req.body.careAction_id +
    ` ;
              vocab:partOf            data:CIG-` +
    req.body.cig_id +
    ` ; 
    vocab:basedOn           data:CB${req.body.belief_id} ;
              vocab:strength       '''` +
    req.body.strength +
    `''' .
    data:CB${req.body.belief_id}  vocab:contribution  '''${req.body.contribution}''' .
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
      oa:hasTarget     [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
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
        prov:generatedAtTime          '''` +
    date +
    `'''^^xsd:dateTime ;
        prov:wasAttributedTo          data:` +
    req.body.author +
    ` .
      }`;

  return `INSERT DATA { ${id2CIG} } ; 
  INSERT DATA { GRAPH ${head} 
    GRAPH ${body} 
    GRAPH ${provenance} 
    GRAPH ${publication} 
  } `;
}

function actionSubguideline(req) {
  let subciguri = set_uri(
    req.body.subcig_id,
    `subCIG` + set_cig_id(req.body.cig_id, true),
    false,
    true
  );

  if (!req.body.description) {
    req.body.description = "subGuideline " + subciguri;
  }

  // SubGuideline declaration:
  const description =
    subciguri +
    ` rdf:type vocab:SubGuideline, owl:NamedIndividual ;
                           rdfs:label "` +
    req.body.description +
    `"@en ;
                           vocab:isSubGuidelineOf  ` +
    set_uri(set_cig_id(req.body.cig_id), null, false, true) +
    ` . \n`;

  //var to construct the assignment of recs to a subguideline. initial whitespace to be kept
  var recDeclaration = " ";

  if (req.body.recs_ids) {
    //nanopublication is part of this subGuideline. contains  pred and object of resource
    const isPartOf = ` vocab:isPartOf ${subciguri} . \n`;

    req.body.recs_ids.split(",").forEach(function (recId) {
      recDeclaration += ` ${set_uri(
        recId.trim(),
        "Rec" + set_cig_id(req.body.cig_id, true),
        false,
        true
      )} ${isPartOf} `;
    });
  }

  return description + recDeclaration;
}

/**
 * Create a persistent or in-memory CIG and return label of CIG
 */
router.post("/create", bodyParser.json(), async function (req, res) {
  //if not id given, use DATE
  let id = req.body.cig_id ? req.body.cig_id : new Date().toISOString();
  //add prefix if not given
  let cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

  const dbType = req.body.IsPersistent ? `tdb` : `mem`;

  let description = req.body.description ?? `Guideline ${cigId}`;

  const content = ` data:${cigId} rdf:type tmr:ClinicalGuideline, owl:NamedIndividual ;
                        rdfs:label '''${description}'''@en . `;

  let sprql_query = `INSERT DATA { ${content} } `;

  //logger.debug(sprql_query);
  return utils
    .sparqlDatasetUpdate(false, cigId, dbType)
    .then(({ data, status }) => {
      if (status < 400) {
        utils
          .sparqlUpdate(cigId, sprql_query)
          .then((status = "200", data) =>
            res.status(200).json({ cig_id: cigId })
          );
      } else {
        res.status(status).json(data);
      }
    })
    .catch(({ data = "", status }) => res.status(status).send(data));
});

/**
 * Delete a persistent or in-memory CIG
 */
router.post("/delete", function (req, res, next) {
  let id = req.body.cig_id;

  if (!id) return res.status(404).send("Missing CIG Id parameter.");
  //otherwise

  const cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

  return utils
    .sparqlDatasetUpdate(true, cigId)
    .then(({ data, status = "200" }) => res.status(status).end())
    .catch(({ data = "", status }) => res.status(status).send(data));
}); //checked!

router.post("/gprec/add", async function (req, res, next) {
  let content = action_gprec(req);

  let query = `INSERT DATA { ${content} }`;
  logger.debug(query);
  let { status, data } = await utils.sparqlUpdate(
    `CIG-${req.body.cig_id}`,
    query
  );

  res.status(status).json(data);
}); //checked!

router.post("/gprec/delete", async function (req, res, next) {
  let query = auxFuncts.sparql_drop_named_graphs(
    `CIG-${req.body.cig_id}`,
    `GPRec${req.body.cig_id}-${req.body.gpRec_id}`
  );
  let { status, data } = await utils.sparqlUpdate(
    `CIG-${req.body.cig_id}`,
    query
  );
  //clear any referencing  on the default graph
  query = `DELETE { ?s ?p ?o } WHERE { ?s ?p ?o . FILTER (?s = data:GPRec${req.body.cig_id}-${req.body.gpRec_id} || ?o = data:GPRec${req.body.cig_id}-${req.body.gpRec_id} )} `;
  let { status: st, data: dt } = await utils.sparqlUpdate(
    `CIG-${req.body.cig_id}`,
    query
  );
  res.status(st).send(dt);
});

router.post("/rec/add", async function (req, res, next) {
  let query = action_rec(req);
  logger.debug(query);
  let { status, data } = await utils.sparqlUpdate(
    `CIG-${req.body.cig_id}`,
    query
  );

  res.status(status).json(data);
});

router.post("/rec/delete", async function (req, res, next) {
  //drop graphs
  let query = auxFuncts.sparql_drop_named_graphs(
    `CIG-${req.body.cig_id}`,
    `Rec${req.body.cig_id}-${req.body.rec_id}`
  );
  let { status, data } = await utils.sparqlUpdate(
    `CIG-${req.body.cig_id}`,
    query
  );
  //clear any referencing  on the default graph
  query = `DELETE { ?s ?p ?o } WHERE { ?s ?p ?o . FILTER (?s = data:Rec${req.body.cig_id}-${req.body.rec_id} || ?o = data:Rec${req.body.cig_id}-${req.body.rec_id} )} `;
  let { status: st, data: dt } = await utils.sparqlUpdate(
    `CIG-${req.body.cig_id}`,
    query
  );
  res.status(st).send(dt);
});

///create subguideline by referencing assertion  resources which are same as assertion graph names in main guideline
router.post("/subguideline/add", async function (req, res, next) {
  let cig_id = set_cig_id(req.body.cig_id);

  let content = actionSubguideline(req);

  let query = ` INSERT DATA { ${content} } `;

  let { status, data } = await utils.sparqlUpdate(cig_id, query);

  res.status(status).send(data);
});

//delete subguideline along with its components
router.post("/subguideline/delete", async function (req, res, next) {
  let subciguri = `data:subCIG` + req.body.cig_id + `-` + req.body.subcig_id;
  //clear any referencing  on the default graph
  query = `DELETE { ?s ?p ?o } WHERE { ?s ?p ?o . FILTER (?s = ${subciguri} || ?o = ${subciguri} )} `;
  let { status: st, data: dt } = await utils.sparqlUpdate(
    `CIG-${req.body.cig_id}`,
    query
  );
  res.status(st).send(dt);
});

/**
 * get URIs of all Recommendations in a given CIG
 */
router.post("/recs/get", async function (req, res) {
  let { cig_id } = req.body;
  if (!cig_id)
    throw new ErrorHandler(
      400,
      "Router /recs/get: no cig_id parameter provided."
    );

  let results = await get_recs_uris(set_cig_id(cig_id));

  return res.status(200).json(results);
}); //checked

/**
 * get URIs of all Good Practice Recommendations (so, no care actions involved) in a given CIG
 */
router.post("/gprecs/get", async function (req, res, next) {
  let { cig_id } = req.body;
  if (!cig_id)
    throw new ErrorHandler(
      400,
      "Router /rec/get: no cig_id parameter provided."
    );

  let results = await get_gprecs_uris(set_cig_id(cig_id));

  return res.status(200).json(results);
}); //checked

//TODO: review
router.post("/belief/add", function (req, res, next) {
  insert_CB_in_rec(req, res, config.INSERT);
});

router.post("/precond/add", function (req, res, next) {
  insert_precond_in_rec(req, res, config.INSERT);
});

////////////////////////

router.post("/rec/all/get/", async function (req, res, next) {
  //checks
  if (!(req.body.uri || req.body.id)) {
    logger.error(
      `missing parameter for recommendation in endpoint /rec/all/get.`
    );
    return res.sendStatus(404);
  }

  if (!req.body.cig_id) {
    logger.error(
      `missing parameters in endpoint /rec/all/get. Rec URI is ${
        req.body.uri ? req.body.uri : req.body.id
      }.`
    );
    return res.sendStatus(404);
  }

  const cigId = req.body.cig_id.startsWith(`CIG-`)
    ? req.body.cig_id.trim()
    : `CIG-${req.body.cig_id.trim()}`;
  const cig_id = req.body.cig_id.startsWith(`CIG-`)
    ? req.body.cig_id.substring(4)
    : req.body.cig_id.trim();
  const recURI = req.body.uri
    ? req.body.uri.trim()
    : `${dataUri}/Rec${cig_id}-${req.body.id.trim()}`;

  //logger.debug(`rec uri is ${recURI} and cigId is ${cigId} and cig_id is ${cig_id}.`);

  try {
    let result = await get_rec_contents(cigId, recURI);

    return res.json(result);
  } catch (err) {
    logger.error(
      `error when retrieving clinical recommendations at get_rec_contents with cig ${cigId} and rec URI ${recURI}. Error is ${JSON.stringify(err)}`
    );
    return res.status(500).end();
  }
});

/**
 * get  knowledge from one gprecommendation
 */
router.post("/gprec/all/get/", async function (req, res, next) {
  var id = set_cig_id(req.body.cig_id, true);
  var idCig = set_cig_id(req.body.cig_id);

  const recURI = set_uri(
    req.body.uri ? req.body.uri : req.body.id,
    "GPRec" + id,
    true
  );

  try {
    let gpRec = await get_gprec_contents(idCig, recURI);

    return res.json(gpRec);
  } catch (err) {
    logger.error(
      `error when retrieving good practice recommendation with cig ${idCig} and rec URI ${recURI}: ${JSON.stringify(
        error
      )}`
    );
    return res.status(500);
  }
});

/**
 * add nanopub graphs from one existing CIG to another
 */
router.post("/add", async function (req, res, next) {
  //list of recommendations to be copied from a dataset
  let recList = new Array();
  let gprecList = new Array();

  let { cig_from, cig_to, subguidelines, recommendations } = req.body;

  //logger.debug(`subguidelines is : ${subguidelines}`);
  //logger.debug(`recommendations is : ${recommendations}`);

  //check both datasets are given
  if (!(cig_from && cig_to)) {
    logger.error(
      `Error: parameters cig_from and cig_to cannot be empty when adding data from one dataset to another. cig_from is ${
        cig_from || null
      }; cig_to is ${cig_to || null}.`
    );

    return res.status(406).json({
      status: "error",
      error: `parameters cig_from and cig_to cannot be empty when adding data from one dataset to another. cig_from is ${
        cig_from || null
      }; cig_to is ${cig_to || null}.`,
    });
  }

  const cigIdFrom = set_cig_id(cig_from);
  //get postfix Id of dataset (e.g., COPD in CIG-COPD)
  const cig_from_id = set_cig_id(cig_from, true);
  //get postfix Id of dataset (e.g., COPD in CIG-COPD)
  const cigIdTo = set_cig_id(cig_to);

  logger.debug(`cig_from is ${cigIdFrom} and cig_to is ${cigIdTo}`);

  //check URI and set, if required
  //cig_from = setUri(cig_from, "CIG", false, false);
  //cig_to = setUri(cig_to, "CIG", false, false);

  //are identifiers correctly construed?
  logger.debug(`identifiers for datasets are ${cig_from_id} with ${cigIdFrom}`);

  let filterSubgString = ``;

  //case where there are no subguidelines or recommendations identified
  //then add ALL Recs from one dataset to the other
  if (!(subguidelines || recommendations)) {
    try {
      recList = await get_recs_uris(cigIdFrom);
      gprecList = await get_gprecs_uris(cigIdFrom);

      if (!recList || recList.length == 0)
        throw new Error(
          `Unexpected output when retrieving ALL recommendations from dataset ${cigIdFrom}. Undefined or empy output`
        );
    } catch (err) {
      logger.error(
        `Error: Function sparqlGetSubjectAllNamedGraphs with dataset Id ${cigIdFrom}. The error is ${JSON.stringify(
          err
        )}`
      );

      return res.status(406).json({
        status: "error",
        error: `Unsuccessful dataset query when retrieving ALL recommendations Ids for CIG with Id ${cigIdFrom}`,
      });
    }
  } else {
    //act on subguidelines
    if (subguidelines) {
      //create list
      subguidelines.split(",").forEach(function (subId) {
        filterSubgString += `?sg = ${set_uri(
          subId.trim(),
          `subCIG` + cig_from_id,
          false,
          true
        )} || `;
      });

      //remove last operator and whitespace
      filterSubgString = filterSubgString.substring(
        0,
        filterSubgString.length - 4
      );

      //check construed URIs
      logger.debug(`filterSubgString is ${filterSubgString}`);

      try {
        //select nanopub URIs from subguidelines
        recList = await utils
          .sparqlGetNamedNanopubFromSubguidelines(cigIdFrom, filterSubgString)
          .then(({ status, head_vars, bindings }) =>
            auxFuncts.get_rdf_atom_as_array(bindings)
          );
        //no result
        if (!recList || (Array.isArray(recList) && recList.length == 0))
          throw new Error(
            `Unexpected output when fetching subguidelines : query result is either undefined or an empty array.`
          );
      } catch (err) {
        logger.error(
          `Error: Function sparqlGetNamedNanopubFromSubguidelinesAsync with dataset Id ${cigIdFrom} and filterSubgString = ${filterSubgString}. The error is ${JSON.stringify(
            err
          )}`
        );

        return res.status(406).json({
          status: "error",
          error: `Unsuccessful dataset query when fetching subguidelines for CIG with Id ${cigIdFrom}`,
        });
      }
    }

    //act on recommendations
    //add recommendations identified in the CDS request call
    if (recommendations) {
      recommendations.split(",").forEach((recId) => {
        recId = recId.trim();
        if (recId.includes("/GPRec")) {
          gprecList.push(recId);
        } else {
          recId = set_uri(recId, `Rec${cig_from_id}`, true);
          recList.push(set_uri(recId, `Rec${cig_from_id}`, true));
        }
      });
    }
    ///////////
  } //endOf else

  //remove potential repeated identifiers
  recList = Array.from(new Set(recList));
  gprecList = Array.from(new Set(gprecList));
  //concat
  recList = recList.concat(gprecList);

  //what is on the list of recommendations?
  logger.debug(
    `the list of recommendations identifiers is ${JSON.stringify(recList)} `
  );

  try {
    //for each assertion URI, add the rest of the related nano graphs
    const promises = recList.map((uri) => {
      let query = auxFuncts.addGraphsDataFromToCig(
        cigIdFrom,
        cigIdTo,
        uri + `_head`, //nanoHeadList,
        uri + ``, //assertionList,
        uri + `_provenance`, //nanoProvList,
        uri + `_publicationinfo` //nanoPubList,
      );

      return utils.sparqlUpdate(cigIdTo, query);
    });

    await Promise.all(promises).catch((err) => logger.error(err));

    return res.status(204).end();
  } catch (error) {
    logger.error(
      `Error when adding graphs from dataset ${cigIdFrom} to dataset ${cigIdTo}. The error is ${JSON.stringify(
        error
      )}`
    );

    return res.status(406).json({
      status: "error",
      error: error,
    });
  }
}); //checked

//TODO: check it is operating for multiple CBs and GPRecs
/**
 * get knowledge from all Recommendations in a given CIG
 */
router.post("/all/get", async function (req, res) {
  //checks

  if (!req.body.cig_id) {
    logger.error(
      `missing parameters in endpoint guideline/all/get. Rec URI is ${req.body.uri}.`
    );
    return res.status(404).end();
  }

  let results = [];
  let status = 200;
  let {cig_id} = req.body

  const cigId = set_cig_id(cig_id);

  try {
    //get rec URIs
    //get gpRec URIs
    let recList = await get_recs_uris(cigId);
    let gprecList = await get_gprecs_uris(cigId);
    //get recs data
    let rec_data_list = recList.map((recUri) =>
      get_rec_contents(cigId, recUri)
    );
    //get gprecs data
    let gprec_data_list = gprecList.map((gprecUri) =>
      get_gprec_contents(cigId, gprecUri)
    );
    //join
    results = rec_data_list.concat(gprec_data_list);
    results = await Promise.all(results);
  } catch (err) {
    logger.error(err);
    status = 500;
  }
  return res.status(status).json(results);
});

router.post("/careAction/get", async function (req, res, next) {
  var postData = "";

  if (req.body.rec_id) {
    postData = require("querystring").stringify({
      guideline_id: `CIG-` + req.body.cig_id,
      rec_id:
        "http://anonymous.org/tmr/data/Rec" +
        req.body.cig_id +
        "-" +
        req.body.rec_id,
    });
  } else {
    //this one will be more accurate when combining guidelines
    postData = require("querystring").stringify({
      guideline_id: `CIG-` + req.body.cig_id,
      rec_URI: req.body.rec_URI,
    });
  }

  let response = {};
  let st = 200;
  try {
    response = await utils.callPrologServerAsync("drug", postData, res);
  } catch (error) {
    logger.error(JSON.stringify(error));
    st = 500;
    response.error = `Prolog server has failed for dataset ${req.body.cig_id} and recommendation URI ${req.body.rec_id}`;
  } finally {
    res.status(st).json(response);
  }
});

module.exports = router;
