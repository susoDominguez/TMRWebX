const n3 = require("n3");
const axios = require("axios").default;
const qs = require("qs");
const parser = new n3.Parser();
const { ErrorHandler } = require("./errorHandler.js");
const config = require("../lib/config");
const guidelines = require("./prefixes.js");
const logger = require("../config/winston");

let reasoner_config = {
  //default
  url: null, //to be added from args
  baseURL: `http://${config.PROLOG_HOST}:${config.PROLOG_PORT}`,
  timeout: 1000,
  auth: {
    username: `${config.FUSEKI_USER}`,
    password: `${config.FUSEKI_PASSWORD}`,
  },
  headers: {
    "Content-Type": "application/x-www-form-urlencoded",
    Authorization: `Basic ${new Buffer(
      config.FUSEKI_USER + ":" + config.FUSEKI_PASSWORD
    ).toString("base64")}`,
  },
};

const jena_baseUrl = `http://${config.JENA_HOST || "127.0.0.1"}:${
  config.JENA_PORT || "3030"
}`;
const basic_auth = {
  username: config.FUSEKI_USER,
  password: config.FUSEKI_PASSWORD,
};

const fuseki_headers = {
  // 'Content-Type' : 'application/sparql-update'
  "Content-Type": "application/x-www-form-urlencoded",
};

const fuseki_headers_json_accept = {
  // 'Content-Type' : 'application/sparql-update'
  "Content-Type": "application/x-www-form-urlencoded",
  Accept: "application/sparql-results+json",
};

function nList(list, n) {
  let pairedPredicateObject = [];

  for (let i = 0; i < list.length; i += n) {
    let nTuple = [];

    for (let j = i; j < i + n; j++) {
      nTuple.push(list[j]);
    }

    pairedPredicateObject.push(nTuple);
  }

  return pairedPredicateObject;
}

/**
 *
 * @param {string} dataset_id identifier of CIG
 * @param {string} query SPARQL query
 * @param {(Error, [])} callback callback returns empty array if err found
 */
async function sparqlQuery(dataset_id, query) {
  
  logger.info(`query: ${query}`);

  //add URL to axios config
  let url = `${jena_baseUrl}/${dataset_id}/query`;

  const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query;

  //sparql query
  const axios_instance = axios.create({
    timeout: 1000,
    auth: basic_auth,
  });

  try {
    const { status = 500, data = [] } = await axios_instance.post(
      url,
      qs.stringify({ query: prefixAndSparqlQuery }),
      fuseki_headers
    );
   // logger.debug(data);

    let response = { status: status, bindings: [], head_vars: [] };
    if (data.hasOwnProperty("head") && data.head.hasOwnProperty("vars"))
      response.head_vars = data.head.vars;
    if (
      data.hasOwnProperty("results") &&
      data.results.hasOwnProperty("bindings")
    )
      response.bindings = data.results.bindings;

    return response;
  } catch (error) {
    if (error.response) {
      // The request was made and the server responded with a status code
      // that falls out of the range of 2xx
      logger.debug(error.response.data);
      logger.debug(error.response.status);
      logger.debug(error.response.headers);
    } else if (error.request) {
      // The request was made but no response was received
      // `error.request` is an instance of XMLHttpRequest in the browser and an instance of
      // http.ClientRequest in node.js
      logger.debug(error.request);
    } else {
      // Something happened in setting up the request that triggered an Error
      logger.debug("Error", error.message ? error.message : error);
    }
    logger.debug(error.config);
    return {
      status: 500,
      bindings: [],
      head_vars: [],
    };
  }
}


/**
 *
 * @param {string} dataset_id identifier of CIG
 * @param {string} query SPARQL query
 * @param {(Error, JSON)} callback callback function
 */
async function sparqlJSONQuery(dataset_id, query) {
  // logger.debug(`SPRQL query is ${query}`);
  const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query;

  const url =
    "http://" +
    config.JENA_HOST +
    ":" +
    config.JENA_PORT +
    "/" +
    dataset_id +
    "/query";

  let options = {
    uri: url,
    method: "POST",
    headers: {
      Authorization:
        "Basic " +
        new Buffer(`${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`).toString(
          "base64"
        ),
      Accept: "application/sparql-results+json",
    },
    form: { query: prefixAndSparqlQuery },
  };
  axios.post(options, function (error, response, body) {
    if (error || (response && response.statusCode !== 200)) {
      logger.error(
        "SPARQL query failed: " +
          JSON.stringify(response) +
          ". Error: " +
          error +
          ". Body: " +
          body +
          ". Status: " +
          (response && response.statusCode
            ? response.statusCode
            : "No response.") +
          "."
      );
      callback(
        new ErrorHandler(
          response ? response.statusCode : 500,
          `Error: ${JSON.stringify(error)} . Body: ${JSON.stringify(body)}`
        ),
        null
      );
    } else {
      // logger.debug("body of sparql result" + JSON.stringify(body));
      callback(null, JSON.parse(body));
    }
  });
}

module.exports = {
  /**
   *
   * @param {boolean} isDel is CRUD DELETE operation? otherwise it is POST
   * @param {string} cigId identifier of CIG
   * @param {string | undefined} dbType type of database memory (permanent or temporary)
   * @returns
   */
  sparqlDatasetUpdate: async function (isDel, cigId, dbType) {
    //add URL to axios config
    let url = isDel
      ? "/$/datasets/" + cigId
      : "/$/datasets?dbType=" + dbType + "&dbName=" + cigId;

    let response = {
      status: 500,
      data: "",
    };

    try {
      let {
        data,
        status,
        statusText = "OK",
      } = await axios({
        method: isDel ? "delete" : "post",
        url: jena_baseUrl + url,
        auth: basic_auth,
        headers: fuseki_headers,
      });

      logger.debug(
        `axio response is ${JSON.stringify(data ? data : statusText)}`
      );

      //add values to response
      response.status = status;
      response.data = data ? data : statusText;
      return response;
    } catch (error) {
      // logger.error(error.toJSON());

      if (error.response) {
        // The request was made and the server responded with a status code
        // that falls out of the range of 2xx
        logger.debug(error.response.data);
        logger.debug(error.response.status);
        logger.debug(error.response.headers);

        //add values to response object
        response.data = error.response.data;
        response.status = error.response.status;
      } else if (error.request) {
        // The request was made but no response was received
        // `error.request` is an instance of XMLHttpRequest in the browser and an instance of
        // http.ClientRequest in node.js
        logger.debug(error.request);
        response.data = error.request;
      } else {
        // Something happened in setting up the request that triggered an Error
        logger.debug("Error", error.message);

        //add values to response object
        response.data = error.message;
      }

      logger.debug(error.config);
      return response;
    }
  },

  /**
   *
   * @param {string} dataset_id CIG identifier
   * @param {string} content SPARQL query content
   */
  sparqlUpdate: async function (dataset_id, content) {
    let response = {
      status: 500,
      data: "",
    };

    let prefixAndSparqlUpdate = {
      update: guidelines.PREFIXES + " \n " + content,
    };

    logger.debug(prefixAndSparqlUpdate);

    //add UR to axios config
    let url = `${jena_baseUrl}/${dataset_id}`;
    let params = prefixAndSparqlUpdate;
    let config = {
      auth: basic_auth,
      headers: fuseki_headers,
    };

    try {
      let {
        data,
        status,
        statusText = "OK",
      } = await axios.post(url, params, config);

      logger.debug(`data is ${JSON.stringify(data ? data : statusText)}`);

      response.status = status;
      response.data = data.data ? data.data : data.statusText;
      return response;
    } catch (error) {
      // logger.error(error.toJSON());

      if (error.response) {
        // The request was made and the server responded with a status code
        // that falls out of the range of 2xx
        logger.debug(error.response.data);
        logger.debug(error.response.status);
        logger.debug(error.response.headers);

        //add values to response object
        response.data = error.response.data;
        response.status = error.response.status;
      } else if (error.request) {
        // The request was made but no response was received
        // `error.request` is an instance of XMLHttpRequest in the browser and an instance of
        // http.ClientRequest in node.js
        logger.debug(error.request);
        response.data = error.request;
      } else {
        // Something happened in setting up the request that triggered an Error
        logger.debug("Error", error.message);

        //add values to response object
        response.data = error.message;
      }

      logger.debug(error.config);
      return response;
    }
  },

  /**
   * TODO: review
   * @param {string} path path to Prolog server function
   * @param {string} data RDF-based dataset for querying
   */
  callPrologServer: async function (path, data) {
    return reasoner_instance.post("/" + path, { data: data });

    /*
  function (error, response, body) {
        if (!error && response && response.statusCode < 400 && body) {
          callback(null, body);
        } else {
          let err =
            "Failed to call prolog server with path: " +
            URL +
            ". Data: " +
            data +
            ". Error: " +
            error
              ? error
              : "unknown" +
                ". Body: " +
                (body ? body : "None") +
                ". Status: " +
                (response && response.statusCode
                  ? response.statusCode
                  : "No response.") +
                ".";
          logger.error(err);
          callback(err, null);
        }
      }
    );*/
  },

  /**
   *
   * @param {String} dataset_id clinical guideline
   * @param {Array<String>} filterString nanopub named
   */
  sparqlGetNamedNanopubFromSubguidelines: async function (
    dataset_id,
    filterString
  ) {
    let query = `
    SELECT DISTINCT ?rec
    WHERE {
   ?rec vocab:isPartOf ?sg
   FILTER( ${filterString} ) } `;

    return sparqlQuery(dataset_id, query);
  },

  sparqlGetNamedGraphsFromObject: async function (dataset_id, instance) {
    let query =
      `
  SELECT ?g
  WHERE {
    GRAPH ?g { ?s a ` +
      instance +
      ` }
  }
  `;
    return sparqlQuery(dataset_id, query);
  },

  get_named_subject_in_named_graphs_from_object: async function (dataset_id, instance) {
    let query =
      `
  SELECT DISTINCT ?s
  WHERE {
    GRAPH ?g { ?s a ${instance} } .
       } `;
    return sparqlQuery(dataset_id, query);
  },

  sparqlGetPreds_Objcts: async function (dataset_id, subject) {
    let query = `
  SELECT DISTINCT ?p ?o
  WHERE {
   ?s ?p ?o .
   FILTER ( ?s = ${subject} ) .
  }
  `;

    return sparqlQuery(dataset_id, query);
  },

  sparqlGetResourcesFromNamedGraph: async function (dataset_id, graph) {
    let query =
      `
		SELECT ?s ?p ?o
		WHERE {
			GRAPH  ` +
      graph +
      `  { ?s ?p ?o }
		}
		`;

    return sparqlQuery(dataset_id, query)
      .then((data) => nList(data, 3))
      .catch((err) => {
        throw new ErrorHandler(500, JSON.stringify(err));
      });
  },

  sparqlGetSubjectDefaultGraph: async function (dataset_id, object_instance) {
    let query = `
		SELECT ?s
		WHERE { ?s ?p ${object_instance} } `;

    return sparqlQuery(dataset_id, query);
  },
  /**
   *
   * @param {String} dataset_id
   * @param {String} instance
   */
  sparqlGetSubjectAllNamedGraphs: async function (dataset_id, instance) {
    //instance could be more than one, like Recs and GpRecs together
    //convert to array
    instance = Array.isArray(instance) ? instance : new Array(instance);

    //filter op for SPARQL query. it will always have at least one filter op
    let filterString = ``;
    //add a filtering situation
    instance.forEach((element) => {
      filterString += `?p = ${element} ||`;
    });
    //remove last 2 chars and close parentheses.
    filterString = filterString.substring(0, filterString.length - 2) + ``;

    logger.info(`dataset_id: ` + dataset_id + ` and instance: ` + filterString);

    //query construction
    let query = `
      SELECT DISTINCT ?s
      WHERE {
        data:${dataset_id}  rdf:type  vocab:ClinicalGuideline , owl:NamedIndividual .
        ?s vocab:isPartOf  data:${dataset_id} . 
        GRAPH ?s {
          ?s rdf:type ?p . 
          FILTER(${filterString}).
        }
      }
      `;

    return sparqlQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} dataset_id
   * @param {string} TrUri
   */
  getTransitionData: async function (dataset_id, TrUri) {
    let query = ` SELECT DISTINCT  ?TrId ?sitFromId ?sitToId ?sitFromLabel ?sitToLabel ?deriv ?propLabel ?propUri ?sitFromIdSCT ?sitToIdSCT ?propUriSCT 
      WHERE {
      ?TrId a vocab:TransitionType ;
            vocab:derivative ?deriv ; 
            vocab:hasTransformableSituation ?sitFromId ;
            vocab:hasExpectedSituation ?sitToId .
      FILTER (?TrId = ${TrUri}) . 
      ?sitFromId a vocab:SituationType ;
                 rdfs:label ?sitFromLabel .
      ?sitToId a vocab:SituationType ;
          rdfs:label ?sitToLabel .
      OPTIONAL { ?TrId  vocab:affects  ?propUri .
         ?propUri a  vocab:TropeType ;
           rdfs:label ?propLabel  } .
      OPTIONAL { ?sitFromId vocab:snomedCode  ?sitFromIdSCT } .
      OPTIONAL { ?sitToId vocab:snomedCode  ?sitToIdSCT } .
      OPTIONAL { ?propUri vocab:snomedCode  ?propUriSCT } .
      }
		`;

    return sparqlQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} dataset_id
   * @param {string | undefined} id
   * @param {string | undefined} uri
   */
  getCareActionData: async function (dataset_id, id, uri) {
    let atom = id ? `data:${id}` : `<${uri}>`;

    let query = `SELECT DISTINCT  ?actId ?adminLabel ?actType ?actLabel ?snomed 
      (GROUP_CONCAT(DISTINCT ?subsumption;   SEPARATOR=", ") AS ?subsumes)
      (GROUP_CONCAT(DISTINCT ?criterion;    SEPARATOR=", ") AS ?hasGroupingCriteria)
      (GROUP_CONCAT(DISTINCT ?same; SEPARATOR=", ") AS ?sameAs)  
      (GROUP_CONCAT(DISTINCT ?component;   SEPARATOR=", ") AS ?components)
		WHERE {
			 ${atom} a owl:NamedIndividual ;
			  a ?adminT ;
				?Of ?actId ;
			  rdfs:label ?adminLabel .
     OPTIONAL { ${atom} vocab:subsumes ?subsumption . }  
     OPTIONAL { ${atom} vocab:hasComponent ?component . }  
			?actId a owl:NamedIndividual .
			?actId a ?actType .
			?actId rdfs:label ?actLabel .
			OPTIONAL { ?actId vocab:snomedCode  ?snomed . }
      OPTIONAL { ?actId vocab:hasGroupingCriteria  ?criterion . }
      OPTIONAL { ?actId owl:sameAs ?same . } 
			FILTER ( ?actType != owl:NamedIndividual &&
				 ( ?Of = vocab:administrationOf || ?Of = vocab:applicationOf || ?Of = vocab:combinedParticipationOf || ?Of = vocab:inoculationOf ) &&
				 ?adminT != owl:NamedIndividual ) .
		} GROUP BY ?actId ?adminLabel ?actType ?actLabel ?snomed
		`;
    //logger.debug(query);

    return sparqlQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} datasetId
   * @param {string} sta_Uri
   */
  getStatementData: async function (datasetId, sta_id) {
    if (!sta_id) throw new ErrorHandler(500, `statement URI is missing.`);
    let query = `
	    SELECT DISTINCT 
	    ?st_id ?statementTitle ?statementText ?organizationName ?jurisdiction
	    WHERE {
		    GRAPH ?st_id {
          ?st_id  a  vocab:ClinicalStatement ;
                 vocab:OrganizationName ?organizationName ;
                 vocab:OrganizationJurisdiction ?jurisdiction ;
                 vocab:hasStatementTitle ?statementTitle ;
                 vocab:hasStatementText ?statementText .
		        }
            FILTER ( ?st_id = data:ST${sta_id} ) .
	  } `;
    return sparqlQuery(datasetId, query);
  },

  /**
   *
   * @param {string} datasetId
   * @param {string} belief_id
   * @param {string} tr_ds_id
   * @param {string} care_act_ds_id
   * @param {(Error, JSON)} callback
   */
  getBeliefData: async function (
    datasetId,
    belief_id,
    tr_ds_id,
    care_act_ds_id
  ) {
    const TrUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      tr_ds_id +
      "/query>";

    const actUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      care_act_ds_id +
      "/query>";

    let query = `
    SELECT DISTINCT 
    ?freq ?strength ?TrUri
    ?propUri ?propLabel
    ?deriv ?sitFromId ?sitToId  ?sitFromLabel ?sitToLabel
    ?actAdmin ?adminLabel ?actType ?actLabel ?actId
    WHERE {
      GRAPH ${belief_id} {
        ${belief_id} a vocab:CausationBelief ; 
       vocab:frequency ?freq ;
       vocab:strength ?strength .
      ?actAdmin vocab:causes ?TrUri .
      }
      SERVICE ${actUrl}
      {
        ?actAdmin a owl:NamedIndividual , ?adminT ;
       	?of ?actId ;
        rdfs:label ?adminLabel .
        ?actId a owl:NamedIndividual , ?actType ;
        rdfs:label ?actLabel .
        FILTER ( ?adminT != owl:NamedIndividual && ?actType != owl:NamedIndividual &&
           (?of = vocab:administrationOf || ?of = vocab:applicationOf || ?of = vocab:inoculationOf || ?of = vocab:combinedParticipationOf )
           ) .
      }
        SERVICE ${TrUrl}
       { 
        ?TrUri a vocab:TransitionType ;
           vocab:hasTransformableSituation ?sitFromId ;
           vocab:hasExpectedSituation ?sitToId ;
             vocab:affects ?propUri ;
           vocab:derivative ?deriv .
        OPTIONAL { ?propUri  a  vocab:TropeType , owl:NamedIndividual ;
             rdfs:label ?propLabel } .		        
        ?sitFromId a vocab:SituationType , owl:NamedIndividual ;
          rdfs:label ?sitFromLabel .
        ?sitToId a vocab:SituationType , owl:NamedIndividual ;
          rdfs:label ?sitToLabel .
      }
    }
    `;
    return sparqlQuery(datasetId, query);
  },

  /**
   *
   * @param {string} cigId
   * @param {string} rec_uri
   * @param {string} beliefDsId
   * @param {string} TrDsId
   * @param {string} caDsId
   */
  getRecData_multiple_CBs: async function (
    cigId,
    rec_uri,
    cbDsId,
    trDsId,
    caDsId
  ) {


    const recAssertURI = `<${rec_uri}>`;
    const recProvURI = `<${rec_uri}_provenance>`;
    const recPubInfoURI = `<${rec_uri}_publicationinfo>`;
    const recHeadURI = `<${rec_uri}_head>`;


    const actUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      caDsId +
      "/query>";

    const cbUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      cbDsId +
      "/query>";

    const trUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      trDsId +
      "/query>";

    let query = ` SELECT DISTINCT ?cbUri ?cbProvUri ?contrib ?freq ?evidence ?actAdminCb ?text ?strength ?derived ?partOf
    ?actAdmin ?adminT ?actId ?adminLabel ?actType ?actLabel  ?sctDrg 
    ?TrUri  ?deriv ?sitFromId ?sitToId  ?sitFromLabel ?sitToLabel ?sitFromSctId ?sitToSctId ?sitFromStateOf ?sitToStateOf
    ?PropUri ?propSctId ?propLabel ?propSctId 
    ?generatedTime ?attributedTo
    WHERE { 
      GRAPH ${recHeadURI} {
        ${recHeadURI} a nanopub:Nanopublication ;
          nanopub:hasAssertion ${recAssertURI} ;
          nanopub:hasProvenance ${recProvURI} ;
          nanopub:hasPublicationInfo ${recPubInfoURI} .
       }
       GRAPH ${recProvURI} {
        ${recProvURI} a  oa:Annotation ; 
            oa:hasBody    ${recAssertURI} . 
        ${recAssertURI} prov:wasDerivedFrom ?derived .
      } 
      GRAPH ${recPubInfoURI} {
        ${recHeadURI} prov:generatedAtTime ?generatedTime ;
            prov:wasAttributedTo  ?attributedTo .
      }
      GRAPH ${recAssertURI} {
        ${recAssertURI} a  vocab:ClinicalRecommendation ;
               vocab:aboutExecutionOf ?actAdmin ;
               vocab:strength ?strength ;
               rdfs:label ?text ;
               vocab:basedOn ?cbUri ;
               vocab:partOf ?partOf .
            ?cbUri vocab:contribution ?contrib . 
        OPTIONAL { ${recAssertURI} vocab:extractedFrom ?extractedFrom . } 
      }
      SERVICE ${actUrl} {
        ?actAdmin a owl:NamedIndividual , ?adminT ;
       	?Of ?actId ;
       rdfs:label ?adminLabel .
        ?actId a owl:NamedIndividual ;
       a ?actType ;
        rdfs:label ?actLabel .
        OPTIONAL { ?actId vocab:snomedCode  ?sctDrg . }
        OPTIONAL { ?actId vocab:hasComponent  ?hasComponent . 
          ?hasComponent  a owl:NamedIndividual ;
                        a ?compAdminT ;
                        ?compOf ?compActId ;
                       rdfs:label ?compAdminLabel .
          ?compActId a owl:NamedIndividual ;
                     a ?compActType ;
                     rdfs:label ?compActLabel . }
        FILTER (?actType != owl:NamedIndividual && ?adminT != owl:NamedIndividual && ( ?Of = vocab:administrationOf || ?Of = vocab:applicationOf || ?Of = vocab:inoculationOf || ?Of =vocab:combinedParticipationOf)) .
      }
      SERVICE ${cbUrl} {
        GRAPH  ?cbUri {
          ?cbUri a  vocab:CausationBelief ; 
                 vocab:frequency ?freq ;
                 vocab:strength ?evidence .
          ?actAdminCb vocab:causes ?TrUri .
        }
        GRAPH ?cbProvUri {
          ?cbProvUri  a  oa:Annotation ; 
              oa:hasBody    ?cbUri . 
              ?cbUri prov:wasDerivedFrom ?derivedCB .
        } 
      } 
      SERVICE ${trUrl} { 
        ?TrUri a vocab:TransitionType ;
              vocab:affects ?PropUri ;
              vocab:derivative ?deriv ;
              vocab:hasTransformableSituation ?sitFromId ;
              vocab:hasExpectedSituation ?sitToId .
        ?PropUri  a  vocab:TropeType ;
                    rdfs:label ?propLabel .
        ?sitFromId a vocab:SituationType ;
                        rdfs:label ?sitFromLabel .
        ?sitToId a vocab:SituationType ;
                     rdfs:label ?sitToLabel .
        OPTIONAL { ?sitFromId vocab:snomedCode ?sitFromSctId .   
          ?PropUri  vocab:snomedCode ?propSctId	.	
          ?sitToId vocab:snomedCode ?sitToSctId . }
        OPTIONAL { ?sitFromId vocab:stateOf ?sitFromStateOf . 
            ?sitToId vocab:stateOf ?sitToStateOf . }
      }
    }
       `;

  logger.info(`query: ${query}`);

    return sparqlQuery(cigId, query);
  },

  /**
   *
   * @param {string} cigId
   * @param {string} recAssertUri
   * @param {string} StatmntsId
   * @param {string} TrDsId
   * @param {string} actDsId
   */
  getRecStmntData: async function (
    cigId,
    recAssertUri,
    StatmntsId,
    TrDsId
  ) {
    const recHeadURI = `<` + recAssertUri + `_head>`;
    const recAssertURI = `<` + recAssertUri + `>`;
    const recProvURI = `<` + recAssertUri + `_provenance>`;
    const recPubInfoURI = `<` + recAssertUri + `_publicationinfo>`;

    const stmntUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      StatmntsId +
      "/query>";
    const TrUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      TrDsId +
      "/query>";

    let query = `
      SELECT DISTINCT 
        ?stUri ?label ?sourceCB ?stTxt ?partOf ?extractedFrom ?stTtl 
        ?sctPrecond ?precondLbl ?generatedTime ?attributedTo ?provAttributedTo
      (GROUP_CONCAT(DISTINCT ?derived;   SEPARATOR=",") AS ?derivedFrom)
      (GROUP_CONCAT(DISTINCT ?hasSource;   SEPARATOR=",") AS ?hasSources)
      (GROUP_CONCAT(DISTINCT ?derivedSt;   SEPARATOR=",") AS ?derivedFromSt)
      (GROUP_CONCAT(DISTINCT ?stOn;   SEPARATOR=",") AS ?orgNmsSt)
      (GROUP_CONCAT(DISTINCT ?stOj;   SEPARATOR=",") AS ?orgJursSt)
      (GROUP_CONCAT(DISTINCT ?provhasSource;   SEPARATOR=",") AS ?provHasSources)
      WHERE { 
        GRAPH ${recHeadURI} {
          ${recHeadURI} a nanopub:Nanopublication ;
              nanopub:hasAssertion ${recAssertURI} ;
              nanopub:hasProvenance ${recProvURI} ;
              nanopub:hasPublicationInfo ${recPubInfoURI} .
        }
        GRAPH ${recProvURI} {
          ${recProvURI} rdf:type oa:Annotation ;
                        oa:hasBody ${recAssertURI} .
          ${recAssertURI} prov:wasDerivedFrom ?derived .
         OPTIONAL {  ${recProvURI}      oa:hasTarget  [  oa:hasSource ?provHasSource ] . }
        }
        GRAPH ${recPubInfoURI} {
          ${recHeadURI} prov:generatedAtTime ?generatedTime ;
                      prov:wasAttributedTo  ?attributedTo .
        }
        GRAPH ${recAssertURI} {
           ${recAssertURI} a  vocab:GoodPracticeRecommendation ;
                         rdfs:label ?label .
           {   ${recAssertURI}   vocab:aboutNotificationOf ?stUri . 
              SERVICE ${stmntUrl} {
                GRAPH  ?stUri {
                  ?stUri a  vocab:ClinicalStatement ;
                       vocab:statementText ?stTxt ;
                        vocab:statementTitle ?stTtl ;
                        vocab:organizationJurisdiction ?stOj ;
                        vocab:organizationName ?stOn .   
                }
              GRAPH ?stUriProv {
               ?stUriProv oa:hasBody ?stUri ;
                         rdf:type oa:Annotation .  
               OPTIONAL { ?stUriProv  oa:hasTarget  [  oa:hasSource ?hasSourceSt ] .} 
               OPTIONAL { ?stUri prov:wasDerivedFrom ?derivedSt . }
              }
            }
           OPTIONAL { ${recAssertURI} vocab:extractedFrom ?extractedFrom . } 
           OPTIONAL { ${recAssertURI} vocab:partOf ?partOf  . } 
           OPTIONAL { ${recAssertURI} vocab:hasFilterSituation ?precond .
             SERVICE ${TrUrl} {  
               ?precond  rdf:type  owl:NamedIndividual  ;
                        rdfs:label ?precondLbl  .
               OPTIONAL { ?precond  vocab:snomedCode ?sctPrecond . }
             }
           }
         } 
        }
      } GROUP BY ?stUri ?label  ?sourceCB ?stTxt ?partOf ?extractedFrom ?stTtl 
            ?sctPrecond ?precondLbl ?generatedTime ?attributedTo ?provAttributedTo
       `;

    return sparqlQuery(cigId, query);
  },

  /**
   *
   * @param {string} cigId
   * @param {string} recAssertUri full recommendtion URI
   * @param {string} beliefDsId
   * @param {string} TrDsId
   * @param {string} actDsId
   */
  getRecData: async function (
    cigId,
    recAssertUri,
    beliefDsId,
    TrDsId,
    actDsId
  ) {
    const recAssertURI = `<` + recAssertUri + `>`;
    const recProvURI = `<` + recAssertUri + `_provenance>`;

    const cbUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      beliefDsId +
      "/query>";
    const TrUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      TrDsId +
      "/query>";
    const actUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      actDsId +
      "/query>";

    let query = ` SELECT DISTINCT ?text ?actAdmin ?cbUri ?strength ?contrib ?partOf
						?freq ?evidence ?TrUri ?PropUri ?deriv ?sitFromId ?sitToId ?propTxt ?sitFromLabel ?sitToLabel
						?adminT ?actId ?adminLabel ?actType ?actLabel  ?sitFromSctId ?sitToSctId
	    WHERE { 
		   GRAPH ${recAssertURI} {
        ${recAssertURI} a  vocab:ClinicalRecommendation ; 
                       rdfs:label ?text ;
                       vocab:aboutExecutionOf ?actAdmin ;
                       vocab:basedOn ?cbUri ;
                       vocab:strength ?strength .
			?cbUri vocab:contribution ?contrib .
			OPTIONAL { ${recAssertURI} vocab:extractedFrom ?extractedFrom . } 
			OPTIONAL { ${recAssertURI} vocab:partOf ?partOf . } 
			} 
			SERVICE ${cbUrl} {
				GRAPH  ?cbUri {
					?cbUri a  vocab:CausationBelief . 
					?cbUri vocab:frequency ?freq .
					?cbUri vocab:strength ?evidence .
				  ?actAdmin vocab:causes ?TrUri .
				}
			} 
			SERVICE ${TrUrl} { 
				?TrUri a vocab:TransitionType .
				?TrUri vocab:affects ?PropUri .
				?TrUri vocab:derivative ?deriv .
				?TrUri vocab:hasTransformableSituation ?sitFromId .
				?TrUri vocab:hasExpectedSituation ?sitToId .
				?PropUri  a  vocab:TropeType .
				?PropUri rdfs:label ?propTxt .
				?sitFromId a vocab:SituationType .
				?sitToId a vocab:SituationType .
				?sitFromId rdfs:label ?sitFromLabel .
				?sitToId rdfs:label ?sitToLabel .
        ?sitFromId vocab:snomedCode ?sitFromSctId .
				?sitToId vocab:snomedCode ?sitToSctId .
			} 
			SERVICE ${actUrl} {
				?actAdmin a owl:NamedIndividual ;
				 a ?adminT ;
					?Of ?actId ;
				 rdfs:label ?adminLabel .
				?actId a owl:NamedIndividual ;
				 a ?actType ;
				rdfs:label ?actLabel .
				FILTER (?actType != owl:NamedIndividual && ( ?Of = vocab:administrationOf || ?Of = vocab:applicationOf  || ?Of = vocab:inoculationOf  || ?Of = vocab:combinedParticipationOf) && ?adminT != owl:NamedIndividual ) .
			} 
 	   }  `;

    return sparqlQuery(cigId, query);
  },

};
