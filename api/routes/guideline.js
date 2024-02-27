const express = require("express");
const router = express.Router();
const request = require("request");
const bodyParser = require("body-parser");
const Promise = require("bluebird");
const config = require("../lib/config");
const guidelines = require("../lib/prefixes");
const utils = Promise.promisifyAll(require("../lib/utils"));
const { handleError, ErrorHandler } = require("../lib/errorHandler");
const logger = require("../config/winston");
const {
  action_gprec,
  insert_precond_in_rec,
  insert_CB_in_rec,
  action_rec,
  actionSubguideline,
  tmrDataUri,
  get_statement_data,
  get_rec_json_data,
  filter_TMR_rec_type,
  setUri,
} = require("../lib/router_functs/guideline_functs.js");
//const e = require("express");


/**
 * Create a persistent or in-memory CIG and return label of CIG
 */
router.post("/dataset/create", bodyParser.json(), function (req, res, next) {
  let id = req.body.cig_id ? req.body.cig_id : new Date().toISOString();

  cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

  const dbType = req.body.IsPersistent ? `tdb` : `mem`;

  request.post(
    {
      url:
        "http://" +
        config.JENA_HOST +
        ":" +
        config.JENA_PORT +
        "/$/datasets?dbType=" +
        dbType +
        "&dbName=" +
        cigId,
      headers: {
        Authorization:
          "Basic " +
          new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
      },
    },
    function (error, response, body) {
      //send error and end
      if (error) {
        res.status(404).send({ error: error });
        return;
      }

      if (!req.body.description) {
        req.body.description = `Guideline ` + cigId;
      }

      const description = `data:${cigId} rdf:type tmr:ClinicalGuideline, owl:NamedIndividual ;
                         rdfs:label '''${req.body.description}'''@en .`;

      utils.sparqlUpdateAsync(
        cigId,
        description,
        config.INSERT,
        function (err, status) {
          if (err)
            return next(
              new ErrorHandler(
                status,
                `sparql has returned an error when creating clinical guideline dataset.`
              )
            );
          //otherwise
          res.json({ cig_id: cigId });
        }
      );
    }
  );
}); //checked!

/**
 * Delete a persistent or in-memory CIG
 */
router.post("/dataset/delete", function (req, res, next) {
  let id = req.body.cig_id;

  if (!id)
    return next(
      new ErrorHandler(
        400,
        `guideline could not be deleted due to internal error.`
      )
    );
  //otherwise

  const cigId = id.startsWith(`CIG-`) ? id : `CIG-` + id;

  request.delete(
    {
      url:
        "http://" +
        config.JENA_HOST +
        ":" +
        config.JENA_PORT +
        "/$/datasets/" +
        cigId,
      headers: {
        Authorization:
          "Basic " +
          new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
      },
    },
    function (error, response, body) {
      if (error) {
        logger.error(`error when deleting cig ${cigId}. Response is ${JSON.stringify(response)}. Error is ${JSON.stringify(error)}`);
        res.status(400).send(error);
      } else {
        res.sendStatus(200);
      }
    }
  );
});//checked!

router.post("/gprec/add", function (req, res, next) {
  action_gprec(req, res, config.INSERT);
});//unchecked!

router.post("/gprec/delete", function (req, res, next) {
  var id = req.body.cig_id;
  var idCig;

  if (id.startsWith(`CIG-`)) {
    idCig = id;
    id = id.substring(`CIG-`.length - 1);
  } else {
    idCig = `CIG-` + id;
  }

  const gpRecUri = req.body.gprec_uri
    ? req.body.gprec_uri
    : "data:GPRec" + id + "-" + req.body.gprec_id;

  utils
    .sparqlDropGraphsAsync(idCig, gpRecUri)
    .then((st) => res.status(200).send(st))
    .catch((err) => {
      logger.error(err);
      return res.status(500);
    });
});//unchecked!

router.post("/rec/add", function (req, res, next) {
  action_rec(req, res, config.INSERT);
});

router.post("/belief/add", function (req, res, next) {
  insert_CB_in_rec(req, res, config.INSERT);
});

router.post("/precond/add", function (req, res, next) {
  insert_precond_in_rec(req, res, config.INSERT);
});

router.post("/rec/delete", function (req, res, next) {
  var id = req.body.cig_id;
  var idCig;

  if (id.startsWith(`CIG-`)) {
    idCig = id;
    id = id.substring(`CIG-`.length - 1);
  } else {
    idCig = `CIG-` + id;
  }

  const recUri = req.body.rec_uri
    ? req.body.rec_uri
    : "data:Rec" + id + "-" + req.body.rec_id;

  utils.sparqlDropGraphs(idCig, recUri, function (err, status) {
    res.sendStatus(err, status);
  });
});

///create subguideline by referencing assertion  resources which are same as assertion graph names in main guideline
router.post("/subguideline/add", function (req, res, next) {
  actionSubguideline(req, res, config.INSERT);
});

//delete subguideline along with its components
router.post("/subguideline/delete", function (req, res, next) {
  actionSubguideline(req, res, config.DELETE);
});


////////////////////////


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


router.post("/rec/all/get/", async function (req, res, next) {
  //checks
  if(!(req.body.uri || req.body.id)) {
    logger.error(`missing parameter for recommendation in endpoint /rec/all/get.`);
    return res.sendStatus(404);
  }

  if(! req.body.cig_id) {
    logger.error(`missing parameters in endpoint /rec/all/get. Rec URI is ${req.body.uri}.`);
    return res.sendStatus(404);
  };

  let idCig, cig;
    //separate lable id from dataset id
    if (req.body.cig_id.startsWith(`CIG-`)) {
      idCig = req.body.cig_id.trim();
      //remove it
      cig = req.body.cig_id.trim().substring(`CIG-`.length);
    } else {
      cig = req.body.cig_id;
      idCig = `CIG-` + req.body.cig_id;
    }

  const recURI = req.body.uri? req.body.uri.trim() : req.body.id.startsWith(`Rec`) ? `${tmrDataUri}/${req.body.id.trim()}` : `${tmrDataUri}/Rec${cig} -${req.body.id.trim()}`;

   logger.debug(`rec uri is ${recURI} and cig id is ${idCig}.`);

  let knowledge_rec;
  try {
    knowledge_rec = await utils.getRecData_multiple_CBsAsync(
      idCig,
      recURI,
      "beliefs",
      "transitions",
      "careActions"
    );
  } catch (err) {
    logger.error(
      `error when retrieving good practice recommendation at getRecData_multiple_CBs with cig ${idCig} and rec URI ${recURI}`
    );
    return res.status(500);
  }
  //format data
  let st_json = {};
  let status = 200;
  try {
    //format knowledge to JSON
    st_json = get_rec_json_data(recURI, knowledge_rec);
    //
    // logger.debug(st_json);
  } catch (err) {
    status = 500;

    logger.error(
      `Error at getStatementData for dataset ${cigId} with recommendation URI ${recURI} and associated knowledge: ${JSON.stringify(
        knowledge_rec
      )}`
    );
    logger.error(`Error getStatementData : ${JSON.stringify(err)}`);
  } finally {
    return res.status(status).json(st_json);
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
    ? `${tmrDataUri}GPRec${id}-${req.body.id.trim()}`
    : undefined;

  let knowledge_rec;
  try {
    knowledge_rec = await utils.getRecStmntDataAsync(
      idCig,
      recURI,
      "statements",
      "transitions",
      "careActions"
    );
  } catch (err) {
    logger.error(
      `error when retrieving good practice recommendation at getRecStmntDataAsync with cig ${idCig} and rec URI ${recURI}`
    );
    return res.status(500);
  }

  if (
    knowledge_rec &&
    knowledge_rec.constructor === Object &&
    Object.entries(knowledge_rec).length != 0
  ) {
    //format data
    let st_json = {};
    let status = 200;
    try {
      //format knowledge to JSON
      st_json = get_statement_data(recURI, knowledge_rec);
      //
      // logger.debug(st_json);
    } catch (err) {
      status = 500;
      logger.error(
        `Error at getStatementData for dataset ${cigId} with recommendation URI ${recURI} and associated knowledge: ${JSON.stringify(
          knowledge_rec
        )}`
      );
      logger.error(`Error getStatementData : ${JSON.stringify(err)}`);
    } finally {
      return res.status(status).json(st_json);
    }
  } else {
    logger.error(`no knowledge fetched from CIG ${cigId}  and GP recommendation URI ${recURI}.`)
    return res.sendStatus(500);
  }
});

/**
 * get URIs of all Recommendations in a given CIG
 */
router.post("/rec/get", function (req, res, next) {
  if (req.body.cig_id) {
    var cigId = req.body.cig_id;

    cigId = cigId.startsWith(`CIG-`) ? cigId : `CIG-` + cigId;
    try {
      utils.sparqlGetSubjectAllNamedGraphsAsync(
        cigId,
        "tmr:ClinicalRecommendation",
        function (err, RecUris) {
          err ? res.status(400).end() : res.status(200).json(RecUris);
        }
      );
    } catch (error) {
      logger.error(error);
    }
  } else {
    res.status(400).end();
  }
}); //checked

/**
 * get URIs of all Good Practice Recommendations (so, no care actions involved) in a given CIG
 */
router.post("/gprec/get", function (req, res, next) {
  if (req.body.cig_id) {
    var cigId = req.body.cig_id;

    cigId = cigId.startsWith(`CIG-`) ? cigId : `CIG-` + cigId;
    try {
      utils.sparqlGetSubjectAllNamedGraphs(
        cigId,
        "tmr:GoodPracticeRecommendation",
        function (err, RecUris) {
          err ? res.status(400).end() : res.send(filter_TMR_rec_type(RecUris));
        }
      );
    } catch (error) {
      logger.error(error);
    }
  } else {
    res.status(400).end();
  }
}); //checked

/**
 * add nanopub graphs from one existing CIG to another
 */
router.post("/dataset/add", async function (req, res, next) {
  //list of recommendations to be copied from a dataset
  let recList = new Array();


  let { cig_from } = req.body;
  let { cig_to } = req.body;

  //check both datasets are given
  if ( !(cig_from && cig_to) ) {

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


  //get postfix Id of dataset (e.g., COPD in CIG-COPD)
  let cig_from_id = cig_from.startsWith(tmrDataUri + "CIG-")
    ? cig_from.substring((tmrDataUri + "CIG-").length)
    : cig_from.startsWith("CIG-")
    ? cig_from.substring("CIG-".length)
    : cig_from;

  //check URI and set, if required
  cig_from = setUri(cig_from, "CIG", false, false);
  cig_to = setUri(cig_to, "CIG", false, false);

  //are identifiers correctly construed?
  logger.debug(`identifiers for datasets are ${cig_from} with ${cig_from_id}, and also ${cig_to}`);

  //subguideline and recommendations
  let { subguidelines } = req.body;
  let { recommendations } = req.body;

  let filterSubgString = ``;

  //case where there are no subguidelines or recommendations identified
  //then add ALL Recs from one dataset to the other
  if (!(subguidelines || recommendations)) {
    try {
      recList = await utils.sparqlGetSubjectAllNamedGraphsAsync(
        cig_from,
        "tmr:ClinicalRecommendation"
      );

      if (!recList || recList.length == 0)
        throw new Error(
          `Unexpected output when retrieving ALL recommendations from dataset ${cig_from}. Undefined or empy output`
        );
    } catch (err) {
      logger.error(
        `Error: Function sparqlGetSubjectAllNamedGraphs with dataset Id ${cig_from}. The error is ${JSON.stringify(
          err
        )}`
      );

      return res.status(406).json({
        status: "error",
        error: `Unsuccessful dataset query when retrieving ALL recommendations Ids for CIG with Id ${cig_from}`,
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
      logger.debug(`filterSubgString is ${filterSubgString}`)

      try {
        //select nanopub URIs from subguidelines
        recList = await utils.sparqlGetNamedNanopubFromSubguidelinesAsync(
          cig_from,
          filterSubgString
        );
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
      recommendations.split(",").forEach((recId) => recList.push(setUri(recId.trim(),`Rec${cig_from_id}`,true)));
    }
    ///////////
  } //endOf else

  //remove potential repeated identifiers
  recList = Array.from(new Set(recList)) ;

  //what is on the list of recommendations?
  logger.debug(`the list of recommendations identifiers is ${JSON.stringify(recList)}`);

  try {
    //for each assertion URI, add the rest of the related nano graphs
    const promises = recList.map((uri) => {
      // logger.info(uri);
      return utils.addGraphsDataFromToCigAsync(
        cig_from,
        cig_to,
        uri + `_head`, //nanoHeadList,
        uri, //assertionList,
        uri + `_provenance`, //nanoProvList,
        uri + `_publicationinfo` //nanoPubList,
      );
    });

    //
    await Promise.all(promises).catch(err => logger.error(err));

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
});//checked

//TODO: check it is operating for multiple CBs and GPRecs
/**
 * get knowledge from all Recommendations in a given CIG
 */
router.post("/dataset/all/get", async function (req, res, next) {
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

module.exports = router;
