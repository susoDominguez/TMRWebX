const request = require("request");
const n3 = require("n3");
const parser = new n3.Parser();
const xmlReader = require("xml-reader");
const xmlQuery = require("xml-query");
const { ErrorHandler } = require("./errorHandler.js");
const config = require("../lib/config");
const guidelines = require("./prefixes.js");
const logger = require("../config/winston");
const prefix = `http://anonymous.org/tmr/data`;
class Sparql_Util {
  /**
   *
   * @param {string} path path to Prolog server function
   * @param {string} data RDF-based dataset for querying
   * @param {any} callback
   */
  static callPrologServer(path, data, callback) {

    //path to swi-prolog server
    const URL =
      "http://" + config.PROLOG_HOST + ":" + config.PROLOG_PORT + "/" + path;

      logger.info(`call prolog server has path ${URL} and data ${data}`);

    request.post(
      URL,
      {
        headers: {
          Authorization:
            "Basic " +
            new Buffer(
              `${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`
            ).toString("base64"),
          "Content-Type": "application/x-www-form-urlencoded",
        },
        body: data,
      },

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
    );
  }

  /**
   *
   * @param {string} dataset_id CIG identifier
   * @param {string} content SPARQL query content
   * @param {string} insertOrDelete insert | delete
   * @param {(Error, number)} callback callback where number has status (200 | 400)
   */
  static sparqlUpdate(dataset_id, content, insertOrDelete, callback) {
    let sparqlUpdate =
      ` ` +
      insertOrDelete +
      ` DATA {
							` +
      content +
      `}`;

    let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate;

    const URL =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      dataset_id +
      "/update";

    //logger.info(`Jena URL = ${URL}`);
    //logger.info("Jena headers = "+config.FUSEKI_USER+":"+config.FUSEKI_PASSWORD);

    request.post(
      URL,
      {
        headers: {
          Authorization:
            "Basic " +
            new Buffer(
              `${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`
            ).toString("base64"),
        },
        body: prefixAndSparqlUpdate,
      },

      function (error, response, body) {
        if (!error && response && response.statusCode < 400) {
          callback(null, 200);
        } else {
          let err =
            "SPARQL update failed at: " +
            URL +
            " Query: " +
            prefixAndSparqlUpdate +
            ". " +
            (error ? error : "None") +
            ". Body: " +
            (body ? body : "None") +
            ". Status: " +
            (response && response.statusCode ? response.statusCode : "401") +
            ".";

          logger.error(err);
          //throw new ErrorHandler((response && response.statusCode) ? response.statusCode : "401",
          //err);
          callback(
            error,
            response && response.statusCode ? response.statusCode : "401"
          );
        }
      }
    );
  }

  /**
   *
   * @param {string} dataset_id identifier of Jenna dataset
   * @param {string} rec_uri URI of recommendation assertion to be deleted
   * @param {(Error, number) => number} callback callback function to deliver response (err, status)
   */
  static sparqlDropGraphs(dataset_id, graph_uri, callback) {
    let sparqlUpdate =
      ` DROP SILENT GRAPH ` +
      graph_uri +
      `_head ;
		 DROP SILENT GRAPH ` +
      graph_uri +
      ` ;
		 DROP SILENT GRAPH ` +
      graph_uri +
      `_provenance ; 
		 DROP SILENT GRAPH ` +
      graph_uri +
      `_publicationinfo ;
		 DELETE  SILENT { ` +
      graph_uri +
      ` tmr:isPartOf ?subguideline } WHERE 
		 { ` +
      graph_uri +
      ` tmr:isPartOf ?subguideline } `;

    let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate;
    const URL =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      dataset_id +
      "/update";

    request.post(
      URL,
      {
        headers: {
          Authorization:
            "Basic " +
            new Buffer(
              `${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`
            ).toString("base64"),
        },
        body: prefixAndSparqlUpdate,
      },

      function (error, response, body) {
        if (!error && response && response.statusCode < 400) {
          callback(null, 200);
        } else {
          logger.error(
            "SPARQL update failed at: " +
              URL +
              "  Query: " +
              prefixAndSparqlUpdate +
              ". Error: " +
              (error ? error : "None") +
              ". Body: " +
              (body ? body : "None") +
              ". Status: " +
              (response && response.statusCode
                ? response.statusCode
                : "No response.") +
              "."
          );
          callback(error, 400);
        }
      }
    );
  }

  /**
   *
   * @param {string} dataset_id identifier of CIG
   * @param {string} query SPARQL query
   * @param {(Error, [])} callback callback returns empty array if err found
   */
  static sparqlQuery(cigId, query, callback) {
    let dataset_id = cigId;
    //if dataset_id is an uri, remove prefixed part
    if (dataset_id.startsWith(prefix))
      dataset_id = dataset_id.substring(prefix.length + 1); //+1 dueTo char '/'
    logger.debug("response is " + dataset_id);

    const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query;
    const url =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      dataset_id +
      "/query";

    request.post(
      encodeURI(url),
      {
        headers: {
          Authorization:
            "Basic " +
            new Buffer(
              `${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`
            ).toString("base64"),
          //"Accept": 'application/sparql-results+json'
        },

        form: { query: prefixAndSparqlQuery },
      },
      function (error, response, body) {
        if (response && response.statusCode < 400) {
          let data = [];

          let queryContainer = xmlQuery(xmlReader.parseSync(body));

          queryContainer.find("binding").each(function (binding) {
            data.push(binding.children[0].children[0].value);
          });

          callback(null, data);
        } else {
          logger.error(
            "SPARQL query failed: " +
              ". " +
              error +
              ". Body: " +
              body +
              ". Status: " +
              (response && response.statusCode
                ? response.statusCode
                : "No response.") +
              "."
          );

          callback(error ? error : body, null);
        }
      }
    );
  }

  /**
   *
   * @param {string} dataset_id identifier of CIG
   * @param {string} query SPARQL query
   * @param {(Error, JSON)} callback callback function
   */
  static sparqlJSONQuery(dataset_id, query, callback) {
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
          new Buffer(
            `${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`
          ).toString("base64"),
        Accept: "application/sparql-results+json",
      },
      form: { query: prefixAndSparqlQuery },
    };
    request.post(options, function (error, response, body) {
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

  /**
   *
   * @param {string} cigFrom original CIG
   * @param {string} cigTo destination CIG
   * @param {string} nanoHead
   * @param {string} nanoAssert
   * @param {string} nanoProv
   * @param {string} nanoPubInfo
   * @param {(Error, number) => number} callback callback function returning status
   */
  static addGraphsDataFromToCig(
    cigFrom,
    cigTo,
    nanoHead,
    nanoAssert,
    nanoProv,
    nanoPub,
    callback
  ) {
    let insertGraphsData = ``;
    let graphs;
    let assertGraphs = ``;
    let provGraphs = ``;
    let headGraphs = ``;
    let nanopubGraphs = ``;
    let graphDescrDel = ``;
    let graphDescrIns = ``;

    let deleteTriples;

    const cigFromUrl =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      cigFrom +
      "/query";

    assertGraphs = `\nGRAPH <` + nanoAssert + `> { ?a  ?b ?c } `;
    provGraphs = `\nGRAPH <` + nanoProv + `> { ?d  ?e ?f } `;
    nanopubGraphs = `\nGRAPH <` + nanoPub + `> { ?g ?h ?i } `;
    headGraphs = `\nGRAPH <` + nanoHead + `> { ?j  ?k ?l } `;

    graphDescrDel +=
      `\nGRAPH <` +
      nanoAssert +
      `> { <` +
      nanoAssert +
      `> tmr:partOf data:` +
      cigFrom +
      ` } `;

    graphDescrIns +=
      `\nGRAPH <` +
      nanoAssert +
      `> { <` +
      nanoAssert +
      `> tmr:partOf data:` +
      cigTo +
      ` } `;

    insertGraphsData =
      `\nINSERT {` +
      headGraphs +
      assertGraphs +
      graphDescrIns +
      nanopubGraphs +
      provGraphs +
      `} \nWHERE { SERVICE <` +
      cigFromUrl +
      `> { ` +
      headGraphs +
      assertGraphs +
      nanopubGraphs +
      provGraphs +
      ` } } ; `;

    deleteTriples = `\nDELETE WHERE { ` + graphDescrDel + ` } ; `;

    //////UPDATE GRAPH STORE//////

    let sparqlUpdate = insertGraphsData + deleteTriples;

    let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate;

    const URL =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      cigTo +
      "/update";

    try {
      request.post(
        URL,
        {
          headers: {
            Authorization:
              "Basic " +
              new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
          },
          body: prefixAndSparqlUpdate,
        },
        function (error, response, body) {
          if (!error && response && response.statusCode < 400) {
            callback(null, response.statusCode);
          } else {
            logger.error(
              "SPARQL update failed at: " +
                URL +
                " Query: " +
                prefixAndSparqlUpdate +
                ". Error: " +
                (error ? error : "None") +
                ". Body: " +
                (body ? body : "None") +
                ". Status: " +
                (response && response.statusCode
                  ? response.statusCode
                  : "No response.") +
                "."
            );

            callback(error, null);
          }
        }
      );
    } catch (err) {
      callback(error, null);
    }
  }

  /**
   *
   * @param {string} cigId
   * @param {string} recAssertUri full recommendtion URI
   * @param {string} beliefDsId
   * @param {string} TrDsId
   * @param {string} actDsId
   * @param {(Error, JSON)} callback
   */
  static getRecData(
    cigId,
    recAssertUri,
    beliefDsId,
    TrDsId,
    actDsId,
    callback
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

    this.sparqlJSONQuery(cigId, query, callback);
  }

  /**
   *
   * @param {string} cigId
   * @param {string} recAssertUri
   * @param {string} StatmntsId
   * @param {string} TrDsId
   * @param {string} actDsId
   * @param {(Error, JSON)} callback
   */
  static getRecStmntData(
    cigId,
    recAssertUri,
    StatmntsId,
    TrDsId,
    actDsId,
    callback
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

    this.sparqlJSONQuery(cigId, query, callback);
  }

  /**
   *
   * @param {string} cigId
   * @param {string} recAssertUri
   * @param {string} beliefDsId
   * @param {string} TrDsId
   * @param {string} caDsId
   * @param {(Error, JSON)} callback
   */
  static getRecData_multiple_CBs(
    cigId,
    recAssertUri,
    cbDsId,
    trDsId,
    caDsId,
    callback
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

    this.sparqlJSONQuery(cigId, query, callback);
  }

  /**
   *
   * @param {string} datasetId
   * @param {string} belief_Uri
   * @param {string} TrId
   * @param {string} actId
   * @param {(Error, JSON)} callback
   */
  static getBeliefData(datasetId, belief_Uri, TrId, actId, callback) {
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
    this.sparqlJSONQuery(datasetId, query, callback);
  }

  /**
   *
   * @param {string} datasetId
   * @param {string} sta_Uri
   * @param {(Error, JSON)} callback
   */
  static getStatementData(datasetId, sta_uri, callback) {
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
    this.sparqlJSONQuery(datasetId, query, callback);
  }

  /**
   *
   * @param {string} dataset_id
   * @param {string} uri
   * @param {(Error, JSON)} callback
   */
  static getCareActionData(dataset_id, uri, callback) {
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

    this.sparqlJSONQuery(dataset_id, query, callback);
  }

  /**
   *
   * @param {string} dataset_id
   * @param {string} TrUri
   * @param {(Error, JSON)} callback
   */
  static getTransitionData(dataset_id, TrUri, callback) {
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

    this.sparqlJSONQuery(dataset_id, query, callback);
  }

  /**
   *
   * @param {String} dataset_id
   * @param {String} instance
   * @param {(Error, JSON)} callback
   */
  static sparqlGetSubjectAllNamedGraphs(dataset_id, instance, callback) {
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

    //logger.info(`dataset_id: ` + dataset_id + ` and instance: ` + filterString);

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

    this.sparqlQuery(dataset_id, query, callback);
  }

  static sparqlGetSubjectDefaultGraph(dataset_id, instance, callback) {
    let query =
      `
		SELECT ?s
		WHERE {
		   ?s a ` +
      instance +
      ` 
		}
		`;

    this.sparqlQuery(dataset_id, query, callback);
  }

  static nList(list, n) {
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

  static sparqlGetResourcesFromNamedGraph(dataset_id, graph, callback) {
    let query =
      `
		SELECT ?s ?p ?o
		WHERE {
			GRAPH  ` +
      graph +
      `  { ?s ?p ?o }
		}
		`;

    this.sparqlQuery(dataset_id, query, function (err, data) {
      //list could be empty if err
      callback(err, Sparql_Util.nList(data, 3));
    });
  }

  static sparqlGetPreds_Objcts(dataset_id, subject, callback) {
    let query =
      `
		SELECT ?p ?o
		WHERE {
		 ` +
      subject +
      ` ?p ?o .
		}
		`;

    this.sparqlQuery(dataset_id, query, function (err, data) {
      callback(err, Sparql_Util.nList(data, 2));
    });
  }

  static sparqlGetNamedGraphsFromObject(dataset_id, instance, callback) {
    let query =
      `
		SELECT ?g
		WHERE {
		  GRAPH ?g { ?s a ` +
      instance +
      ` }
		}
		`;

    this.sparqlQuery(dataset_id, query, callback);
  }

  /**
   *
   * @param {String} dataset_id clinical guideline
   * @param {Array<String>} filterString nanopub named
   * @param {} callback callback function
   */
  static sparqlGetNamedNanopubFromSubguidelines(
    dataset_id,
    filterString,
    callback
  ) {
    let query =
      `
		SELECT DISTINCT ?rec
		WHERE {
			?rec tmr:isPartOf ?sg
			FILTER( ${filterString} ) } `;

    this.sparqlQuery(dataset_id, query, callback);
  }
}

module.exports = Sparql_Util;
