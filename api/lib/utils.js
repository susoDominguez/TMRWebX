const n3 = require("n3");
const axios = require("axios");
const jsonata = require("jsonata");
const qs = require("qs");
const parser = new n3.Parser();
//const xmlReader = require("xml-reader");
//const xmlQuery = require("xml-query");
const { ErrorHandler } = require("./errorHandler.js");
const config = require("../lib/config");
const guidelines = require("./prefixes.js");
const logger = require("../config/winston");
const { urlencoded } = require("body-parser");
//const { options } = require("../routes/careAction.js");
//const { post } = require("../routes/careAction.js");
//const { response } = require("express");
const prefix = `http://anonymous.org/tmr/data`;

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
const jena_baseUrl = `http://${config.JENA_HOST}:${config.JENA_PORT}`;
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
  //add URL to axios config
  let url = `${jena_baseUrl}/${dataset_id}/query`;

  const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query;

  //sparql query
  const axios_instance = axios.create({
    timeout: 1000,
    auth: {
      username: config.FUSEKI_USER,
      password: config.FUSEKI_PASSWORD,
    },
  });

  try {
    const { status = 500, data = [] } = await axios_instance.post(
      url,
      qs.stringify({ query: prefixAndSparqlQuery }),
      fuseki_headers
    );

    let response = { status: status, data: undefined, head_vars: undefined };

    //if one head var then one list of items otherwise lists of paired rdfs s.t. each list is 1-2-1 with head vars
    let bindings_list;
    if (Array.isArray(data.head.vars)) {
      if (data.head.vars.length == 1) {
        //return in one go
        const expr_singleton = jsonata("results.bindings.**.value");
        response.data = await expr_singleton.evaluate(data);
      } else {
        response.head_vars = data.head.vars;
        bindings_list = new Array(response.head_vars.length);
        for (let index = 0; index < response.head_vars.length; index++) {
          const expression = jsonata(`results.bindings[${index}].**.value`);
          const result = await expression.evaluate(data);
          bindings_list.push(result);
        }
        response.data = bindings_list;
      }
    }

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
      status: error.response.status ? error.response.status : 500,
      data: `Error: ${error.response.data}`,
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
   */
  sparqlDatasetUpdate : async function (isDel, cigId, dbType) {
    //add URL to axios config
    let url = isDel ? '/$/datasets/'+cigId : '/$/datasets?dbType='+dbType+'&dbName='+cigId ;
    
    try {
        let ax = await axios({
        method: isDel? 'delete' : 'post',
        url: jena_baseUrl+url,
        //baseUrl: jena_baseUrl,
        timeout: 1000,
        auth: {
          username: config.FUSEKI_USER,
          password: config.FUSEKI_PASSWORD,
        }
      });
      logger(`axio response is ${JSON.stringify(ax)}`)
      return ax;
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
      logger.debug("Error", error.message);
    }
    logger.debug(error.config);
    return {
      status: error.response.status,
      data: `Error: ${error.response.data}`,
    };
  }},

  /**
   *
   * @param {string} dataset_id CIG identifier
   * @param {string} content SPARQL query content
   */
  sparqlUpdate: async function (dataset_id, content) {
    let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + content;

    //add UR to axios config
    let url = `${jena_baseUrl}/${dataset_id}/update`;

    const axios_instance = axios.create({
      timeout: 1000,
      auth: {
        username: config.FUSEKI_USER,
        password: config.FUSEKI_PASSWORD,
      },
    });

    try {
      let data = await axios_instance.post(
        url,
        qs.stringify({ update: prefixAndSparqlUpdate }),
        fuseki_headers
      );
      return {
        status: data.status,
        data: data.data ? data.data : data.statusText,
      };
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
        logger.debug("Error", error.message);
      }
      logger.debug(error.config);
      return {
        status: error.response.status,
        data: `Error: ${error.response.data}`,
      };
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
   ?rec tmr:isPartOf ?sg
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

  sparqlGetPreds_Objcts: async function (dataset_id, subject) {
    let query =
      `
  SELECT ?p ?o
  WHERE {
   ` +
      subject +
      ` ?p ?o .
  }
  `;

    return sparqlQuery(dataset_id, query)
      .then((data) => nList(data, 2))
      .catch((err) => {
        throw new ErrorHandler(500, JSON.stringify(err));
      });
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

  sparqlGetSubjectDefaultGraph: async function (dataset_id, instance) {
    let query = `
		SELECT ?s
		WHERE { ?s ?a ${instance} } `;

    return  sparqlQuery(dataset_id, query);
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
        data:${dataset_id}  rdf:type  tmr:ClinicalGuideline , owl:NamedIndividual .
        ?s tmr:isPartOf  data:${dataset_id} . 
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
    let query =
      `SELECT DISTINCT  ?sitFromId ?sitToId ?sitFromLabel ?sitToLabel ?propTxt ?propUri ?deriv
		WHERE {
			` +
      TrUri +
      ` a tmr:TransitionType .
			` +
      TrUri +
      ` tmr:affects ?propUri .
			` +
      TrUri +
      ` tmr:derivative ?deriv.
			` +
      TrUri +
      ` tmr:hasTransformableSituation ?sitFromId .
			` +
      TrUri +
      ` tmr:hasExpectedSituation ?sitToId .
			?PropUri  a  tmr:TropeType .
			?PropUri rdfs:label ?propTxt .
			?sitFromId a tmr:SituationType .
			?sitToId a tmr:SituationType .
			?sitFromId rdfs:label ?sitFromLabel .
			?sitToId rdfs:label ?sitToLabel .
		}
		`;

    return sparqlJSONQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} dataset_id
   * @param {string} uri
   * @param {(Error, JSON)} callback
   */
  getCareActionData: async function (dataset_id, uri) {

    let query = `SELECT DISTINCT  ?actId ?adminLabel ?actType ?actLabel ?snomed 
      (GROUP_CONCAT(DISTINCT ?subsumption;   SEPARATOR=", ") AS ?subsumes)
      (GROUP_CONCAT(DISTINCT ?criterion;    SEPARATOR=", ") AS ?hasGroupingCriteria)
      (GROUP_CONCAT(DISTINCT ?same; SEPARATOR=", ") AS ?sameAs)  
		WHERE {
			<${uri}> a owl:NamedIndividual ;
			  a ?adminT ;
				?Of ?actId ;
			  rdfs:label ?adminLabel.
     OPTIONAL { <${uri}> tmr:subsumes ?subsumption . }  
			?actId a owl:NamedIndividual.
			?actId a ?actType.
			?actId rdfs:label ?actLabel.
			?actId tmr:snomedCode  ?snomed.
      OPTIONAL { ?actId tmr:hasGroupingCriteria  ?criterion . }
      OPTIONAL { ?actId owl:sameAs ?same . } 
			FILTER ( ?actType != owl:NamedIndividual &&
				 ( ?Of = tmr:administrationOf || ?Of = tmr:applicationOf || ?Of = tmr:inoculationOf ) &&
				 ?adminT != owl:NamedIndividual ) .
		} GROUP BY ?actId ?adminLabel ?actType ?actLabel ?snomed
		`;

    return sparqlQuery(dataset_id, query);
  },

  /**
   *
   * @param {string} datasetId
   * @param {string} sta_Uri
   */
  getStatementData: async function (datasetId, sta_uri) {
    if (!sta_uri) throw new ErrorHandler(500, `statement URI is missing.`);
    let query = `
	    SELECT DISTINCT 
	    ?statementTitle ?statementText ?organizationName ?jurisdiction
	    WHERE {
		    GRAPH ${sta_uri} {
           ${sta_uri}  a  tmr:ClinicalStatement ;
                 tmr:organizationName ?organizationName ;
                 tmr:organizationJurisdiction ?jurisdiction ;
                 tmr:statementTitle ?statementTitle ;
                 tmr:statementText ?statementText .
		        }
	  } `;
    return sparqlJSONQuery(datasetId, query);
  },

  /**
   *
   * @param {string} datasetId
   * @param {string} belief_Uri
   * @param {string} TrId
   * @param {string} actId
   * @param {(Error, JSON)} callback
   */
  getBeliefData: async function (datasetId, belief_Uri, TrId, actId) {
    const TrUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      TrId +
      "/query>";
    const actUrl =
      "<http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      actId +
      "/query>";

    let query =
      `
    SELECT DISTINCT 
    ?freq ?strength ?TrUri
    ?propUri ?deriv ?sitFromId ?sitToId ?propTxt ?sitFromLabel ?sitToLabel
    ?actAdmin ?adminLabel ?actType ?actLabel 
    WHERE {
      GRAPH  ` +
      belief_Uri +
      ` {
       ` +
      belief_Uri +
      ` a  tmr:CausationBelief . 
       ` +
      belief_Uri +
      ` tmr:frequency ?freq .
       ` +
      belief_Uri +
      ` tmr:strength ?strength .
        ?actAdmin tmr:causes ?TrUri .
       }
      SERVICE ` +
      actUrl +
      ` {
        ?actAdmin a owl:NamedIndividual .
        ?actAdmin a ?adminT .
        ?actAdmin	?of ?actId .
        ?actAdmin rdfs:label ?adminLabel .
        ?actId a owl:NamedIndividual .
        ?actId a ?actType .
        ?actId rdfs:label ?actLabel .
        FILTER ( ?adminT != owl:NamedIndividual && ?actType != owl:NamedIndividual &&
           (?of = tmr:administrationOf || ?of = tmr:applicationOf || ?of = tmr:inoculationOf ) ) .
      }
        SERVICE ` +
      TrUrl +
      ` { 
        ?TrUri a tmr:TransitionType ;
            a owl:NamedIndividual ;
           tmr:hasTransformableSituation ?sitFromId ;
           tmr:hasExpectedSituation ?sitToId ;
             tmr:affects ?propUri ;
           tmr:derivative ?deriv .
        ?propUri  a  tmr:TropeType ;
          a owl:NamedIndividual ;
             rdfs:label ?propTxt .			        
        ?sitFromId a tmr:SituationType ;
          a owl:NamedIndividual ;
          rdfs:label ?sitFromLabel .
        ?sitToId a tmr:SituationType ;
          a owl:NamedIndividual ;
          rdfs:label ?sitToLabel .
      }
    }
    `;
    return sparqlJSONQuery(datasetId, query);
  },

  /**
   *
   * @param {string} cigId
   * @param {string} recAssertUri
   * @param {string} beliefDsId
   * @param {string} TrDsId
   * @param {string} caDsId
   */
  getRecData_multiple_CBs: async function (
    cigId,
    recAssertUri,
    cbDsId,
    trDsId,
    caDsId
  ) {
    const recAssertURI = `<${recAssertUri}>`;
    const recProvURI = `<${recAssertUri}_provenance>`;
    const recPubInfoURI = `<${recAssertUri}_publicationinfo>`;
    const recHeadURI = `<${recAssertUri}_head>`;

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
          ${recAssertURI} a  tmr:ClinicalRecommendation ;
               tmr:aboutExecutionOf ?actAdmin ;
               tmr:strength ?strength ;
               rdfs:label ?text ;
               tmr:basedOn ?cbUri .
            ?cbUri tmr:contribution ?contrib .
            ${recAssertURI} tmr:partOf ?partOf .  
        OPTIONAL { ${recAssertURI} tmr:extractedFrom ?extractedFrom . } 
      }
      SERVICE ${actUrl} {
        ?actAdmin a owl:NamedIndividual .
        ?actAdmin a ?adminT .
        ?actAdmin	?Of ?actId .
        ?actAdmin rdfs:label ?adminLabel .
        ?actId a owl:NamedIndividual .
        ?actId a ?actType .
        ?actId rdfs:label ?actLabel .
        OPTIONAL { ?actId tmr:snomedCode  ?sctDrg . }
        OPTIONAL { ?actId tmr:hasComponent  ?hasComponent . 
          ?hasComponent  a owl:NamedIndividual ;
                        a ?compAdminT ;
                        ?compOf ?compActId ;
                       rdfs:label ?compAdminLabel .
          ?compActId a owl:NamedIndividual ;
                     a ?compActType ;
                     rdfs:label ?compActLabel . }
        FILTER (?actType != owl:NamedIndividual && ?adminT != owl:NamedIndividual && ( ?Of = tmr:administrationOf || ?Of = tmr:applicationOf)) .
      }
      SERVICE ${cbUrl} {
        GRAPH  ?cbUri {
          ?cbUri a  tmr:CausationBelief ; 
                 tmr:frequency ?freq ;
                 tmr:strength ?evidence .
          ?actAdminCb tmr:causes ?TrUri .
        }
        GRAPH ?cbProvUri {
          ?cbProvUri  a  oa:Annotation ; 
              oa:hasBody    ?cbUri . 
              ?cbUri prov:wasDerivedFrom ?derivedCB .
        } 
      } 
      SERVICE ${trUrl} { 
        ?TrUri a tmr:TransitionType ;
              tmr:affects ?PropUri ;
              tmr:derivative ?deriv ;
              tmr:hasTransformableSituation ?sitFromId ;
              tmr:hasExpectedSituation ?sitToId .
        ?PropUri  a  tmr:TropeType ;
                    rdfs:label ?propLabel .
        ?sitFromId a tmr:SituationType ;
                        rdfs:label ?sitFromLabel .
        ?sitToId a tmr:SituationType ;
                     rdfs:label ?sitToLabel .
        OPTIONAL { ?sitFromId tmr:snomedCode ?sitFromSctId .   
          ?PropUri  tmr:snomedCode ?propSctId	.	
          ?sitToId tmr:snomedCode ?sitToSctId . }
        OPTIONAL { ?sitFromId tmr:stateOf ?sitFromStateOf . 
            ?sitToId tmr:stateOf ?sitToStateOf . }
      }
    }
       `;

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
  getRecStmntData: async function (
    cigId,
    recAssertUri,
    StatmntsId,
    TrDsId,
    actDsId
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
           ${recAssertURI} a  tmr:GoodPracticeRecommendation ;
                         rdfs:label ?label .
           {   ${recAssertURI}   tmr:aboutNotificationOf ?stUri . 
              SERVICE ${stmntUrl} {
                GRAPH  ?stUri {
                  ?stUri a  tmr:ClinicalStatement ;
                       tmr:statementText ?stTxt ;
                        tmr:statementTitle ?stTtl ;
                        tmr:organizationJurisdiction ?stOj ;
                        tmr:organizationName ?stOn .   
                }
              GRAPH ?stUriProv {
               ?stUriProv oa:hasBody ?stUri ;
                         rdf:type oa:Annotation .  
               OPTIONAL { ?stUriProv  oa:hasTarget  [  oa:hasSource ?hasSourceSt ] .} 
               OPTIONAL { ?stUri prov:wasDerivedFrom ?derivedSt . }
              }
            }
           OPTIONAL { ${recAssertURI} tmr:extractedFrom ?extractedFrom . } 
           OPTIONAL { ${recAssertURI} tmr:partOf ?partOf  . } 
           OPTIONAL { ${recAssertURI} tmr:hasFilterSituation ?precond .
             SERVICE ${TrUrl} {  
               ?precond  rdf:type  owl:NamedIndividual  ;
                        rdfs:label ?precondLbl  .
               OPTIONAL { ?precond  tmr:snomedCode ?sctPrecond . }
             }
           }
         } 
        }
      } GROUP BY ?stUri ?label  ?sourceCB ?stTxt ?partOf ?extractedFrom ?stTtl 
            ?sctPrecond ?precondLbl ?generatedTime ?attributedTo ?provAttributedTo
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

    let query = ` SELECT DISTINCT ?text ?actAdmin ?cbUri ?strength ?contrib ?sourceOfRec ?partOf
						?freq ?evidence ?TrUri ?PropUri ?deriv ?sitFromId ?sitToId ?propTxt ?sitFromLabel ?sitToLabel
						?adminT ?actId ?adminLabel ?actType ?actLabel  ?sitFromSctId ?sitToSctId
	    WHERE { 
		   GRAPH ${recAssertURI} {
        ${recAssertURI} a  tmr:ClinicalRecommendation ; 
                       rdfs:label ?text ;
                       tmr:aboutExecutionOf ?actAdmin ;
                       tmr:basedOn ?cbUri ;
                       tmr:strength ?strength .
			?cbUri tmr:contribution ?contrib .
			OPTIONAL { ${recAssertURI} tmr:extractedFrom ?extractedFrom . } 
			OPTIONAL { ${recAssertURI} tmr:partOf ?partOf . } 
			}
			GRAPH ${recProvURI} {
        ${recProvURI} a  oa:Annotation ; 
                      oa:hasBody    ${recAssertURI} . 
        OPTIONAL { ${recAssertURI} tmr:partOf ?partOf . }
      ${recAssertURI} prov:wasDerivedFrom ?sourceOfRec .
			} 
			SERVICE ${cbUrl} {
				GRAPH  ?cbUri {
					?cbUri a  tmr:CausationBelief . 
					?cbUri tmr:frequency ?freq .
					?cbUri tmr:strength ?evidence .
				  ?actAdmin tmr:causes ?TrUri .
				}
			} 
			SERVICE ${TrUrl} { 
				?TrUri a tmr:TransitionType .
				?TrUri tmr:affects ?PropUri .
				?TrUri tmr:derivative ?deriv .
				?TrUri tmr:hasTransformableSituation ?sitFromId .
				?TrUri tmr:hasExpectedSituation ?sitToId .
				?PropUri  a  tmr:TropeType .
				?PropUri rdfs:label ?propTxt .
				?sitFromId a tmr:SituationType .
				?sitToId a tmr:SituationType .
				?sitFromId rdfs:label ?sitFromLabel .
				?sitToId rdfs:label ?sitToLabel .
        ?sitFromId tmr:snomedCode ?sitFromSctId .
				?sitToId tmr:snomedCode ?sitToSctId .
			} 
			SERVICE ${actUrl} {
				?actAdmin a owl:NamedIndividual .
				?actAdmin a ?adminT .
				?actAdmin	?Of ?actId .
				?actAdmin rdfs:label ?adminLabel .
				?actId a owl:NamedIndividual .
				?actId a ?actType .
				?actId rdfs:label ?actLabel .
				FILTER (?actType != owl:NamedIndividual && ( ?Of = tmr:administrationOf || ?Of = tmr:applicationOf) && ?adminT != owl:NamedIndividual ) .
			} 
 	   }`;

    return sparqlJSONQuery(cigId, query);
  },
};
