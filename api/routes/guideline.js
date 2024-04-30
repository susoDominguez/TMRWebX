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
  tmrDataUri,
  get_statement_data,
  get_rec_json_data,
  filter_TMR_rec_type,
  setUri,
} = require("../lib/router_functs/guideline_functs.js");
const { error } = require("console");

const dataUri = "http://anonymous.org/data";

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
              vocab:strength       '''` + req.body.strength +`''' .
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

  return  `INSERT DATA { ${id2CIG} } ; 
  INSERT DATA { GRAPH ${head} 
    GRAPH ${body} 
    GRAPH ${provenance} 
    GRAPH ${publication} 
  } `;
}

function actionSubguideline(req) {

  let subciguri =`data:subCIG` + req.body.cig_id + `-` + req.body.subcig_id ;

  if (!req.body.description) {
    req.body.description = "subGuideline " + subciguri;
  }

  // SubGuideline declaration:
  const description = subciguri + ` rdf:type vocab:SubGuideline, owl:NamedIndividual ;
                           rdfs:label "` + req.body.description + `"@en ;
                           vocab:isSubGuidelineOf  data:CIG-` + req.body.cig_id + ` .` ;

  //var to construct the assignment of recs to a subguideline. initial whitespace to be kept
  var recDeclaration = " ";

  if (req.body.recs_ids) {
    //nanopublication is part of this subGuideline. contains  pred and object of resource
    const isPartOf = ` vocab:isPartOf ${subciguri} . \n`;

    req.body.recs_ids.split(",").forEach(function (recId) {
      recDeclaration +=
        `data:Rec` + req.body.cig_id + `-` + recId.trim() + isPartOf ;
    });
  }

  return description + recDeclaration ;
}

/**
 * Create a persistent or in-memory CIG and return label of CIG
 */
router.post("/create", bodyParser.json(),async function (req, res) {
    //if not id given, use DATE
    let id = req.body.cig_id ? req.body.cig_id : new Date().toISOString();
    //add prefix if not given
    let cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

    const dbType = req.body.IsPersistent ? `tdb` : `mem`;


    let description = req.body.description ?? `Guideline ${cigId}`;

    const content = ` data:${cigId} rdf:type tmr:ClinicalGuideline, owl:NamedIndividual ;
                        rdfs:label '''${description}'''@en . ` ;

    let sprql_query = `INSERT DATA { ${content} } `;

    //logger.debug(sprql_query);
    return utils.sparqlDatasetUpdate(false, cigId,dbType).then(({data, status}) => {
      if(status < 400){
        utils.sparqlUpdate(cigId,sprql_query).then((status='200', data) => res.status(200).json({cig_id: cigId}));
      } else {
       res.status(status).json(data);
      }
    }).catch( ({data='',status}) => res.status(status).send(data));

});

/**
 * Delete a persistent or in-memory CIG
 */
router.post("/delete", function (req, res, next) {
  let id = req.body.cig_id;

  if (!id) return res.status(404).send('Missing CIG Id parameter.')
  //otherwise

  const cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

  return utils.sparqlDatasetUpdate(true, cigId).then(({data, status='200'}) => res.status(status).end()).catch( ({data='',status}) => res.status(status).send(data));

}); //checked!

router.post("/gprec/add", async function (req, res, next) {

  let  content = action_gprec(req);

  let query = `INSERT DATA { ${content} }`;
  logger.debug(query)
  let {status, data} = await utils.sparqlUpdate(`CIG-${req.body.cig_id}`, query);
    
      res.status(status).json(data);
}); //unchecked!

router.post("/gprec/delete", async function (req, res, next) {
  let query = auxFuncts.sparql_drop_named_graphs(`CIG-${req.body.cig_id}`, `GPRec${req.body.cig_id}-${req.body.gpRec_id}`);
  let {status, data} = await utils.sparqlUpdate(`CIG-${req.body.cig_id}`, query);
  //clear any referencing  on the default graph
  query = `DELETE { ?s ?p ?o } WHERE { ?s ?p ?o . FILTER (?s = data:GPRec${req.body.cig_id}-${req.body.gpRec_id} || ?o = data:GPRec${req.body.cig_id}-${req.body.gpRec_id} )} `
  let {status:st, data:dt} = await utils.sparqlUpdate(`CIG-${req.body.cig_id}`, query);
  res.status(st).send(dt);
}); 

router.post("/rec/add", async function (req, res, next) {
  let query = action_rec(req);
  logger.debug(query)
  let {status, data} = await utils.sparqlUpdate(`CIG-${req.body.cig_id}`, query);
    
      res.status(status).json(data);
});

router.post("/rec/delete", async function (req, res, next) {
  //drop graphs
  let query = auxFuncts.sparql_drop_named_graphs(`CIG-${req.body.cig_id}`, `Rec${req.body.cig_id}-${req.body.rec_id}`);
  let {status, data} = await utils.sparqlUpdate(`CIG-${req.body.cig_id}`, query);
  //clear any referencing  on the default graph
  query = `DELETE { ?s ?p ?o } WHERE { ?s ?p ?o . FILTER (?s = data:Rec${req.body.cig_id}-${req.body.rec_id} || ?o = data:Rec${req.body.cig_id}-${req.body.rec_id} )} `
  let {status:st, data:dt} = await utils.sparqlUpdate(`CIG-${req.body.cig_id}`, query);
  res.status(st).send(dt);
});

///create subguideline by referencing assertion  resources which are same as assertion graph names in main guideline
router.post("/subguideline/add", async function (req, res, next) {

  let content = actionSubguideline(req) ;

  let query = ` INSERT DATA { ${content} } `;

  let {status, data} = await utils.sparqlUpdate( "CIG-" + req.body.cig_id, query) ;

  res.status(status).send(data);

});

//delete subguideline along with its components
router.post("/subguideline/delete", async function (req, res, next) {
  let subciguri =`data:subCIG` + req.body.cig_id + `-` + req.body.subcig_id ;
   //clear any referencing  on the default graph
   query = `DELETE { ?s ?p ?o } WHERE { ?s ?p ?o . FILTER (?s = ${subciguri} || ?o = ${subciguri} )} `
   let {status:st, data:dt} = await utils.sparqlUpdate(`CIG-${req.body.cig_id}`, query);
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

  let {status, bindings, head_vars} = await utils.get_named_subject_in_named_graphs_from_object(`CIG-` + cig_id, "vocab:ClinicalRecommendation");
  
  let results = await auxFuncts.get_rdf_atom_as_array(bindings);
  
  return res.status(status).json(results);
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

  let {status, bindings, head_vars} = await utils.get_named_subject_in_named_graphs_from_object(`CIG-` + cig_id, "vocab:GoodPracticeRecommendation");
  let results = await auxFuncts.get_rdf_atom_as_array(bindings);
  
  return res.status(status).json(results);
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
      `missing parameters in endpoint /rec/all/get. Rec URI is ${req.body.uri}.`
    );
    return res.sendStatus(404);
  }

  const cigId = req.body.cig_id.startsWith(`CIG-`) ? req.body.cig_id.trim() : `CIG-${req.body.cig_id.trim()}`;
  const cig_id = req.body.cig_id.startsWith(`CIG-`) ? req.body.cig_id.substring(4) : req.body.cig_id.trim();
  const recURI = req.body.uri ? req.body.uri.trim() : `${dataUri}/Rec${cig_id}-${req.body.id.trim()}`;
  

  logger.debug(`rec uri is ${recURI} and cigId is ${cigId} and cig_id is ${cig_id}.`);

  try {
    
    let {status, bindings, head_vars} = await utils.getRecData(
      cigId,
      recURI,
      "beliefs",
      "transitions",
      "careActions"
    );

    logger.debug(`bindings: ${JSON.stringify(bindings)}`);

     //format knowledge to JSON
     let st_json = get_rec_json_data(recURI,head_vars, bindings);

     return res.status(status).json(st_json);

  } catch (err) {
    logger.error(
      `error when retrieving clinical recommendation at getRecData_multiple_CBs with cig ${cigId} and rec URI ${recURI}`
    );
    return res.status(500).end();
  }
});

/**
 * get  knowledge from one gprecommendation
 */
router.post("/gprec/all/get/", async function (req, res, next) {
  var id = req.body.cig_id.trim();
  var idCig;

  //separate lable id from dataset id
  if (id.startsWith(`CIG-`)) {
    idCig = id;
    //remove it
    id = id.substring(`CIG-`.length);
  } else {
    idCig = `CIG-` + id;
  }

  const recURI = req.body.uri
    ? req.body.uri.trim()
    : req.body.id
    ? `${dataUri}/GPRec${id}-${req.body.id.trim()}`
    : undefined;


  try {
    let {status, bindings, head_vars} = await utils.getRecStmntData(
      idCig,
      recURI,
      "statements",
      "transitions"
    );

    logger.debug(`bindings: ${bindings}`);
    
    //format knowledge to JSON
    let st_json = auxFuncts.get_statement_data(recURI, bindings);
    return res.status(status).json(st_json);
  } catch (err) {
    logger.error(
      `error when retrieving good practice recommendation at getRecStmntDataAsync with cig ${idCig} and rec URI ${recURI}`
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

  let { cig_from, cig_to, subguidelines, recommendations} = req.body; 

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

  const cigIdFrom = cig_from.startsWith(`CIG-`) ? cig_from.trim() : `CIG-${cig_from.trim()}`;
    //get postfix Id of dataset (e.g., COPD in CIG-COPD)
  const cig_from_id = cigIdFrom.substring("CIG-".length);
    //get postfix Id of dataset (e.g., COPD in CIG-COPD)
  const cigIdTo = req.body.cig_to.startsWith(`CIG-`) ? req.body.cig_to.trim() : `CIG-${req.body.cig_to.trim()}`;
  let cig_to_id = cigIdTo.substring("CIG-".length);

  logger.debug(`cig_from is ${cigIdFrom} and cig_to is ${cigIdTo}`);

  //check URI and set, if required
  //cig_from = setUri(cig_from, "CIG", false, false);
  //cig_to = setUri(cig_to, "CIG", false, false);

  //are identifiers correctly construed?
  logger.debug(
    `identifiers for datasets are ${cig_from_id} with ${cigIdFrom}, and also ${cigIdTo}`
  );

  let filterSubgString = ``;

  //case where there are no subguidelines or recommendations identified
  //then add ALL Recs from one dataset to the other
  if (!(subguidelines || recommendations)) {
    try {
     let {status, head_vars, bindings} = await utils.sparqlGetSubjectAllNamedGraphs(
        cigIdFrom,
        "vocab:ClinicalRecommendation"
      );
     
     
      let recList = await auxFuncts.get_rdf_atom_as_array(bindings);

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
        filterSubgString += `?sg = ${setUri(
          subId.trim(),
          "subCIG",
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
        recList = await utils.sparqlGetNamedNanopubFromSubguidelines(
          cig_from,
          filterSubgString
        ).then( ({status, head_vars, bindings}) =>  auxFuncts.get_rdf_atom_as_array(bindings));
        //no result
        if (!recList || (Array.isArray(recList) && recList.length == 0))
          throw new Error(
            `Unexpected output when fetching subguidelines : query result is either undefined or an empty array.`
          );
      } catch (err) {
        logger.error(
          `Error: Function sparqlGetNamedNanopubFromSubguidelinesAsync with dataset Id ${cig_from} and filterSubgString = ${filterSubgString}. The error is ${JSON.stringify(
            err
          )}`
        );

        return res.status(406).json({
          status: "error",
          error: `Unsuccessful dataset query when fetching subguidelines for CIG with Id ${cig_from}`,
        });
      }
    }

    //act on recommendations
    //add recommendations identified in the CDS request call
    if (recommendations) {
      recommendations
        .split(",")
        .forEach((recId) =>
          recList.push(setUri(recId.trim(), `Rec${cig_from_id}`, true))
        );
    }
    ///////////
  } //endOf else

  //remove potential repeated identifiers
  recList = Array.from(new Set(recList));

  //what is on the list of recommendations?
  logger.debug(
    `the list of recommendations identifiers is ${JSON.stringify(recList)}`
  );

  try {
    //for each assertion URI, add the rest of the related nano graphs
    const promises = recList.map( (uri) => {
      // logger.info(uri);
      let query = auxFuncts.addGraphsDataFromToCig(
        cig_from,
        cig_to,
        uri + `_head`, //nanoHeadList,
        uri + ``, //assertionList,
        uri + `_provenance`, //nanoProvList,
        uri + `_publicationinfo` //nanoPubList,
      );
      return utils.sparqlUpdate(cig_to,query);
    });

    await Promise.all(promises).catch((err) => logger.error(err));

    return res.status(204).end();
  } catch (error) {
    logger.error(
      `Error when adding graphs from dataset ${cig_from} to dataset ${cig_to}. The error is ${JSON.stringify(
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
router.post("/all/get", async function (req, res, next) {
  //label of CIG
  let cigId;

  //full URI
  if (!req.body.uri) {
    //just the id
    if (!req.body.id) {
      return res.status(406).json({
        status: "error",
        error: "parameter id is missing when calling endpoint /cig/get.",
      });
    } else {
      //cig Id
      cigId = req.body.id.trim();
    }
  } else {
    //cig Id from URI
    cigId = req.body.uri.trim().substring(tmrDataUri.length);
  }

  let uriList;

  try {
    //deliver list of Rec URIs
    uriList = await utils.sparqlGetSubjectAllNamedGraphsAsync(cigId, [
      "tmr:ClinicalRecommendation",
      "tmr:GoodPracticeRecommendation",
    ]);

    logger.debug(`uriList is ${uriList}`);
  } catch (error) {
    logger.error(
      `Error in utils.sparqlGetSubjectAllNamedGraphsAsync sent from endpoint /cig/get : ${JSON.stringify(
        error
      )}`
    );
    //return status
    return res.status(500).json({
      error:
        "Error when retrieving list of recommendation URIs from dataset with Id " +
        cigId,
    });
  }

  if (uriList && Array.isArray(uriList)) {
    //list to hold recs data
    let recDataList = new Array();
    //list to hold info on the type of rec stored in the list above
    let is_gp_rec_list = new Array();
    //data extracted from store service
    let dataset_data,
      dataset_json = new Array();

    //for each uri in list, fetch knowledge from dataset
    for (let index = 0; index < uriList.length; index++) {
      const uri_rec = uriList[index];

      //if it is a TMR rec
      if (uri_rec.startsWith(tmrDataUri + `Rec`)) {
        //add data to list
        recDataList.push(
          utils.getRecData_multiple_CBsAsync(
            cigId,
            uri_rec,
            "beliefs",
            "transitions",
            "careActions"
          )
        );
        //add info on type of rec
        is_gp_rec_list.push(false);
      } else {
        //if it is a TMR GP rec
        if (uri_rec.startsWith(tmrDataUri + `GPRec`)) {
          //add data to list
          recDataList.push(
            utils.getRecStmntDataAsync(
              cigId,
              uri_rec,
              "statements",
              "transitions",
              "careActions"
            )
          );
          //add info on type of rec
          is_gp_rec_list.push(true);
        }
      } //endOf else
    } //endOf loop

    //check Rec URI list has same size as list with knowledge from recs
    //otherwise, throw error
    if (recDataList.length !== uriList.length) {
      logger.error(
        `list of recommendations URIs does not have the same size has the list of recommendation knowledge in endpoint cig/get with Rec URI list: ${JSON.stringify(
          uriList
        )} .`
      );
      return res.status(500).json({
        error:
          "There has been an issue when recovering knowledge from TMR recommendations: the number of URIs and the number of distinct associated data extracted do not match in dataset " +
          cigId,
      });
    }

    //convert knowledge into JSON format
    try {
      // evaluate async list
      dataset_data = await Promise.all(recDataList);
    } catch (error) {
      // throw new ErrorHandler(500, `endpoint cig/get error when evaluating promises : ${JSON.stringify(error)}`);
      logger.error(
        `Error from endpoint cig/get when applying promise.all to recDataList: ${JSON.stringify(
          error
        )}`
      );
      return res.status(500).json({
        error: "Internal error when fetching knowledge from dataset " + cigId,
      });
    }

    //Attempt to format all data, if error is found, stop process and return error silently
    try {
      //for each element with TMR knowledge, identify type of rec and format to JSON
      for (let index = 0; index < dataset_data.length; index++) {
        //URI Rec
        const recUri = uriList[index];
        //TMR rec in rdf format associated to URI rec
        const knowledge_rec = dataset_data[index];
        //boolean identifyoing whether URI rec is good practice rec or not
        const is_gp_rec = is_gp_rec_list[index];

        //if it has data, convert to JSON
        if (
          knowledge_rec &&
          knowledge_rec.constructor === Object &&
          Object.entries(knowledge_rec).length != 0
        ) {
          //act accordingly to type when formatting
          if (is_gp_rec) {
            try {
              //format knowledge to JSON
              let st_json = get_statement_data(recUri, knowledge_rec);
              //add formatted knowledge
              dataset_json.push(st_json);
              //
              // logger.debug(st_json);
            } catch (err) {
              //silently log error?
              logger.error(
                `Error at getStatementData for dataset ${cigId} with recommendation URI ${recUri} and associated knowledge: ${JSON.stringify(
                  knowledge_rec
                )}`
              );
              logger.error(`Error getStatementData : ${JSON.stringify(error)}`);

              throw new ErrorHandler(500, {
                error:
                  "Error when formatting knowledge from dataset " +
                  cigId +
                  " with recommendation URI " +
                  recUri,
              });
            }
          } //endOf if

          if (!is_gp_rec) {
            try {
              //format knowledge to JSON
              let rec_json = get_rec_json_data(recUri, knowledge_rec);
              //add formatted knowledge
              dataset_json.push(rec_json);
              //
              //logger.debug(rec_json);
            } catch (err) {
              //silently log error?
              logger.error(
                `Error at get_rec_json_data for dataset ${cigId} with recommendation URI ${recUri} and associated knowledge: ${JSON.stringify(
                  knowledge_rec
                )}`
              );
              logger.error(
                `Error get_rec_json_data : ${JSON.stringify(error)}`
              );
              throw new ErrorHandler(500, {
                error:
                  "Error when formatting knowledge to JSON from dataset " +
                  cigId +
                  " with recommendation URI " +
                  recUri,
              });
            }
          } //endOf if !is_pg_rec
        } else {
          logger.error(
            `Error in cig/get endpoint. dataset ${cigId} has recommendation URI ${recUri} with knowledge data = ${knowledge_rec}.`
          );
        } //endOf if knowledge_rec && ...
      } //endOf for loop

      //send formatted dataset knowledge
      return res.status(200).json(dataset_json);
    } catch (error) {
      logger.error(error);
      return res.sendStatus(500);
    }
  } //endOf if UriList && ...

  logger.error(
    `Error endpoint cig/get. There is no list of Recommendations URIs`
  );
  return res.statusCode(500);
}); //checked!


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
