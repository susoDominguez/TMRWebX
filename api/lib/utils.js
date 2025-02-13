const n3 = require("n3");
const axios = require("axios").default;
const qs = require("qs");
const parser = new n3.Parser();
const { StatusCodes, ReasonPhrases } = require("http-status-codes");
const { ErrorHandler } = require("./errorHandler.js");
const config = require("../lib/config");
const guidelines = require("./prefixes.js");
const logger = require("../config/winston");

let reasoner_config = {
  // Default settings
  url: null, // to be added from args
  baseURL: `http://${config.PROLOG_HOST}:${config.PROLOG_PORT}`,
  timeout: 1000,
  auth: {
    username: `${config.FUSEKI_USER || "admin"}`,
    password: `${config.FUSEKI_PASSWORD || "road2h"}`,
  },
  headers: {
    "Content-Type": "application/x-www-form-urlencoded",
    Authorization: `Basic ${Buffer.from(
      `${config.FUSEKI_USER || "admin"}:${config.FUSEKI_PASSWORD || "road2h"}`
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
  //logger.info(`query: ${query}`);

  //add URL to axios config
  let url = `${jena_baseUrl}/${dataset_id}/query`;

  const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query;

  //sparql query
  const axios_instance = axios.create({
    timeout: 1000,
    auth: basic_auth,
  });

  try {
    const { status = StatusCodes.BAD_GATEWAY, data = [] } =
      await axios_instance.post(
        url,
        qs.stringify({ query: prefixAndSparqlQuery }),
        fuseki_headers
      );

    logger.debug(`data is ${JSON.stringify(data)}`);

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
 * Executes a SPARQL query against a Jena Fuseki dataset and returns the raw JSON results.
 *
 * @param {string} dataset_id - Identifier of the Fuseki dataset.
 * @param {string} query - SPARQL query string.
 * @returns {Promise<JSON>} - Raw SPARQL query results as JSON.
 * @throws {Error} - If the SPARQL query fails.
 */
async function sparqlJSONQuery(dataset_id, query) {
  const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query;

  const url = `http://${config.JENA_HOST}:${config.JENA_PORT}/${dataset_id}/query`;

  const options = {
    headers: {
      Authorization: `Basic ${Buffer.from(
        `${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`
      ).toString("base64")}`,
      Accept: "application/sparql-results+json",
    },
  };

  try {
    const response = await axios.post(
      url,
      { query: prefixAndSparqlQuery },
      options
    );

    if (response.status !== 200) {
      throw new Error(
        `SPARQL query failed with status code ${response.status}: ${response.statusText}`
      );
    }

    return response.data; // Return raw SPARQL results
  } catch (error) {
    logger.error(
      `SPARQL query failed: ${error.message}. Query: ${query}. Dataset: ${dataset_id}`
    );
    throw new Error(
      `SPARQL query error: ${error.message}. Check logs for details.`
    );
  }
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
   * Calls the Prolog server with the specified path and data.
   *
   * @param {string} path - Path to the Prolog server function.
   * @param {string} data - RDF-based dataset for querying.
   * @returns {Promise<JSON>} - Response from the Prolog server as JSON.
   * @throws {Error} - If the request fails.
   */
  callPrologServer: async function (path, data) {
    try {
      // Ensure the path does not accidentally contain leading/trailing slashes
      const sanitizedPath = path.replace(/^\/+|\/+$/g, "");

      // Create an Axios instance using the reasoner configuration
      const reasonerInstance = axios.create(reasoner_config);

      // Make a POST request to the Prolog server
      const response = await reasonerInstance.post(`/${sanitizedPath}`, {
        data,
      });

      // Check for non-200 HTTP status codes
      if (response.status !== 200) {
        throw new Error(
          `Prolog server returned status ${response.status}: ${response.statusText}`
        );
      }

      // Return the response data
      return response.data;
    } catch (error) {
      // Log the error for debugging
      logger.error(
        `Prolog server call failed. Path: ${path}. Error: ${error.message}`
      );

      // Throw a user-friendly error
      throw new Error(
        `Failed to call Prolog server at path '${path}'. Check logs for details.`
      );
    }
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

  get_named_subject_in_named_graphs_from_object: async function (
    dataset_id,
    instance
  ) {
    let query = `
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
      OPTIONAL { ?sitFromId vocab:sctid  ?sitFromIdSCT } .
      OPTIONAL { ?sitToId vocab:sctid  ?sitToIdSCT } .
      OPTIONAL { ?propUri vocab:sctid  ?propUriSCT } .
      }
		`;

    return sparqlJSONQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} dataset_id
   * @param {string} TrUri
   */
  getPreconditionData: async function (dataset_id, TrUri) {
    let query = ` SELECT  ?pred_id ?lbl
      WHERE {
      ?pred_id a ?type .
      ?pred_id rdfs:label ?lbl .
      FILTER (?pred_id = <${TrUri}> && (?type = vocab:PredicateType || ?type = vocab:PreconditionType ) ) . }
		`;
    return sparqlJSONQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} dataset_id
   * @param {string | undefined} id
   * @param {string | undefined} uri
   */
  getCareActionData: async function (dataset_id, id, uri) {
    let atom = id ? `data:ActAdminister${id}` : `<${uri}>`;

    const query = `
      PREFIX vocab: <http://example.com/vocab#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    
      SELECT DISTINCT  ?actId ?adminT ?adminLabel ?drugType ?drugLabel ?sctid ?drugTid ?sctid_label
      (GROUP_CONCAT(DISTINCT ?subsumption;   SEPARATOR=", ") AS ?subsumes)
      (GROUP_CONCAT(DISTINCT ?criterion;    SEPARATOR=", ") AS ?hasGroupingCriteria)
      (GROUP_CONCAT(DISTINCT ?same; SEPARATOR=", ") AS ?sameAs)  
      (GROUP_CONCAT(DISTINCT ?component;   SEPARATOR=", ") AS ?components)
		WHERE {
      ?actId a owl:NamedIndividual , ?adminT ;
				       ?Of ?drugTid ;
			         rdfs:label ?adminLabel .
     OPTIONAL { ?actId vocab:subsumes ?subsumption . }  
     OPTIONAL { ?actId vocab:hasComponent ?component . }  
			   ?drugTid a owl:NamedIndividual , ?drugType  ;
			         rdfs:label ?drugLabel .
			OPTIONAL { ?drugTid vocab:sctid  ?sctid . }
      OPTIONAL { ?sctid rdfs:label ?sctid_label . }
      OPTIONAL { ?drugTid vocab:hasGroupingCriteria  ?criterion . }
      OPTIONAL { ?drugTid owl:sameAs ?same . } 
			FILTER ( ?actId = ${atom} && ?drugType != owl:NamedIndividual && ?adminT != owl:NamedIndividual && ( ?Of = vocab:administrationOf || ?Of = vocab:applicationOf || ?Of = vocab:combinedAdministrationOf || ?Of = vocab:vaccinationWith ) ) .
		} GROUP BY ?actId ?adminLabel ?drugType ?drugLabel ?sctid ?drugTid ?adminT
		`;
    //logger.debug(query);

    return sparqlJSONQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} datasetId
   * @param {string} sta_Uri
   */
  getStatementData: async function (
    datasetId = "statements",
    sta_id = null,
    sta_uri = null
  ) {
    if (!sta_id && !sta_uri)
      throw new ErrorHandler(500, `statement URI is missing.`);

    const st_assert = sta_uri ? `<${sta_uri}>` : `data:ST${sta_id}`;
    const st_prov = sta_uri
      ? `<${sta_uri}_provenance>`
      : `data:ST${sta_id}_provenance`;

    let query = `
	    SELECT DISTINCT 
	    ?st_id ?statementTitle ?statementText ?organizationName ?jurisdiction ?derivedFromSt
	    WHERE {
		    GRAPH ${st_assert} {
          ?st_id  a  vocab:ClinicalStatement ;
                 vocab:OrganizationName ?organizationName ;
                 vocab:OrganizationJurisdiction ?jurisdiction ;
                 vocab:hasStatementTitle ?statementTitle ;
                 vocab:hasStatementText ?statementText .
		        }
        GRAPH ${st_prov} {
          ?id_prov  prov:wasDerivedFrom ?derivedFromSt . 
        }
	  } `;
    return sparqlJSONQuery(datasetId, query);
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

    //format belief_id
    if (belief_id.startsWith("http")) belief_id = `<${belief_id}>`;

    let query = `
    SELECT DISTINCT 
    ?cbUri ?freq ?strength ?TrUri
    ?propUri ?propLabel ?propUriSCT
    ?deriv ?sitFromId ?sitToId  ?sitFromLabel ?sitToLabel ?sitFromIdSCT ?sitToIdSCT
    ?actAdmin ?adminLabel ?actType ?actLabel ?actId
    WHERE {
      GRAPH ${belief_id} {
        ?cbUri a vocab:CausationBelief ; 
       vocab:frequency ?freq ;
       vocab:strength ?strength .
      ?actAdmin vocab:causes ?TrUri .
      } FILTER ( ?cbUri = ${belief_id}) .
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
        OPTIONAL { ?propUri vocab:sctid ?propUriSCT } .	        
        ?sitFromId a vocab:SituationType , owl:NamedIndividual ;
          rdfs:label ?sitFromLabel .
        OPTIONAL { ?sitFromId vocab:sctid ?sitFromIdSCT } .
        ?sitToId a vocab:SituationType , owl:NamedIndividual ;
          rdfs:label ?sitToLabel .
        OPTIONAL { ?sitToId vocab:sctid ?sitToIdSCT } .
      }
    }
    `;
    return sparqlJSONQuery(datasetId, query);
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
        OPTIONAL { ?actId vocab:sctid  ?sctDrg . }
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
        OPTIONAL { ?sitFromId vocab:sctid ?sitFromSctId .   
          ?PropUri  vocab:sctid ?propSctId	.	
          ?sitToId vocab:sctid ?sitToSctId . }
        OPTIONAL { ?sitFromId vocab:stateOf ?sitFromStateOf . 
            ?sitToId vocab:stateOf ?sitToStateOf . }
      }
    }
       `;

    logger.info(`query: ${query}`);

    return sparqlJSONQuery(cigId, query);
  },

  /**
   *
   * @param {string} cigId
   * @param {string} recAssertUri
   * @param {string} StatmntsId
   * @param {string} TrDsId
   * @param {string} actDsId
   */
  getRecStmntData: async function (cigId, recAssertUri, StatmntsId) {
    const recHeadURI = `<` + recAssertUri + `_head>`;
    const graphUri = `<` + recAssertUri + `>`;
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

    let query = `
      SELECT DISTINCT ?gpRecId ?label ?partOf ?extractedFrom ?wasDerivedFrom
      (GROUP_CONCAT(DISTINCT ?stUri;   SEPARATOR=",") AS ?stUris)
      WHERE { 
         GRAPH ${graphUri} {
            ?gpRecId a vocab:GoodPracticeRecommendation ;
                         rdfs:label ?label ;
                       vocab:partOf ?partOf  ;
                       vocab:aboutNotificationOf ?stUri .  
           OPTIONAL { ?gpRecId vocab:extractedFrom ?extractedFrom . } 
         } 
         GRAPH ${recProvURI} {
          OPTIONAL { ?gpRecId prov:wasDerivedFrom ?wasDerivedFrom . }   
       }
      } GROUP BY ?gpRecId ?label ?partOf ?extractedFrom ?wasDerivedFrom
       `;

    return sparqlJSONQuery(cigId, query);
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
    const recPubURI = `<` + recAssertUri + `_publicationinfo>`;

    let query = ` SELECT DISTINCT ?recId ?text ?actAdmin ?cbUri ?strength ?contrib ?partOf ?extractedFrom ?attributedTo ?generatedAtTime ?pred
        (GROUP_CONCAT( DISTINCT ?wasDerivedFrom;   SEPARATOR=",") AS ?derivedFrom)
	    WHERE { 
		   GRAPH ${recAssertURI} {
        ?recId a  vocab:ClinicalRecommendation ; 
                       rdfs:label ?text ;
                       vocab:aboutExecutionOf ?actAdmin ;
                       vocab:basedOn ?cbUri ;
                       vocab:strength ?strength .
			?cbUri vocab:contribution ?contrib .
			OPTIONAL { ?recId vocab:extractedFrom ?extractedFrom . } 
			OPTIONAL { ?recId vocab:partOf ?partOf . } 
      OPTIONAL { ?recId vocab:hasFilterSituation ?pred . }
			} 
      GRAPH ${recProvURI} {
        OPTIONAL { ?recId prov:wasDerivedFrom ?wasDerivedFrom . }
      }
 	   }  GROUP BY ?recId ?text ?actAdmin ?cbUri ?strength ?contrib ?partOf ?extractedFrom ?attributedTo ?generatedAtTime ?pred `;

    return sparqlJSONQuery(cigId, query);
  },
};
