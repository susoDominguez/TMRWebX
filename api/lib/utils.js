const request = require('request');
const n3 = require('n3');
const parser = new n3.Parser();
const xmlReader = require('xml-reader');
const xmlQuery = require('xml-query');
const { ErrorHandler } = require('./errorHandler.js');
const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const logger = require('../config/winston');

class Sparql_Util {

	/**
	 * 
	 * @param {string} dataset_id CIG identifier
	 * @param {string} content SPARQL query content
	 * @param {string} insertOrDelete insert | delete
	 * @param {(Error, number)} callback callback where number has status (200 | 400)
	 */
	static sparqlUpdate(dataset_id, content, insertOrDelete, callback) {

		let sparqlUpdate = ` ` + insertOrDelete + ` DATA {
	` + content + `}`;

		let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate
		const URL = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/update";

		request.post(

			URL, {
			headers: {
				"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
			},
			body: prefixAndSparqlUpdate
		},

			function (error, response, body) {

				if (!error && response && response.statusCode < 400) {

					callback(null, 200);

				} else {

					console.log("SPARQL update failed at: " + URL + " Query: " + prefixAndSparqlUpdate + ". Error: " + (error ? error : "None") + ". Body: " + (body ? body : "None") + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(err, 400);

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

		let sparqlUpdate = ` DROP SILENT GRAPH ` + graph_uri + `_head ;
		 DROP SILENT GRAPH ` + graph_uri + ` ;
		 DROP SILENT GRAPH ` + graph_uri + `_provenance ; 
		 DROP SILENT GRAPH ` + graph_uri + `_publicationinfo ;
		 DELETE  SILENT { `+ graph_uri + ` vocab:isPartOf ?subguideline } WHERE 
		 { `+ graph_uri + ` vocab:isPartOf ?subguideline } `

		let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate
		const URL = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/update";

		request.post(

			URL, {
			headers: {
				"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
			},
			body: prefixAndSparqlUpdate
		},

			function (error, response, body) {

				if (!error && response && response.statusCode < 400) {

					callback(null, 200);

				} else {

					console.log("SPARQL update failed at: " + URL + "  Query: " + prefixAndSparqlUpdate + ". Error: " + (error ? error : "None") + ". Body: " + (body ? body : "None") + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
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
	static sparqlQuery(dataset_id, query, callback) {

		const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query
		const url = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/query";

		request.post(url, {

			headers: {
				"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
				//"Accept": 'application/sparql-results+json'				
			},

			form: { query: prefixAndSparqlQuery }
		},
			function (error, response, body) {

				if (response && response.statusCode < 400) {

					let data = [];

					let queryContainer = xmlQuery(xmlReader.parseSync(body));

					queryContainer.find('binding').each(function (binding) {

						data.push(binding.children[0].children[0].value);

					});

					callback(null, data);

				} else {

					console.log("SPARQL query failed: "  + ". Error: " + error + ". Body: " + body +
					 ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(body, null);

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

		const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query
		const url = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/query";

		let options = {
			uri: url,
			method: "POST",
			headers: {
				"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
				"Accept": 'application/sparql-results+json'
			},
			form: { query: prefixAndSparqlQuery }
		};
		request.post(options,
			function (error, response, body) {
			
				if( error || (response && response.statusCode !== 200))
				{
					logger.error("SPARQL query failed: " + response + ". Error: " + error + ". Body: " + body + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(new ErrorHandler(response.statusCode, "Error: " + error + ". Body: " + body), null);
				} else {
					callback(null, JSON.parse(body));
				}
			}
		);
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
	static addGraphsDataFromToCig(cigFrom, cigTo, nanoHead, nanoAssert, nanoProv, nanoPub, callback) {

		let insertGraphsData = ``;
		let graphs ;
		let preGraphs = ``;
		let provGraphs = ``;
		let postGraphs = ``;
		let nanopubGraphs = ``;
		let graphDescrDel = ``;
		let graphDescrIns = ``;

		let deleteTriples ;
		
		

		const cigFromUrl = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + cigFrom + "/query";


		for (let index in nanoHead) {
			
			preGraphs += 
				 `\nGRAPH <` + nanoAssert[index] + `> { ?a`+index+` ?b`+index+` ?c`+index+` } `;
			postGraphs += 
				 `\nGRAPH <` + nanoProv[index] + `> { <` + nanoAssert[index] + `> prov:wasDerivedFrom ?d`+index+` } ` ;

			graphDescrDel += `\nGRAPH <` + nanoAssert[index] + `> { <`+ nanoAssert[index] +`> vocab:partOf data:`+ cigFrom +` } `;
			
			graphDescrIns += `\nGRAPH <` + nanoAssert[index] + `> { <`+ nanoAssert[index] +`> vocab:partOf data:`+ cigTo +` } `;

		}

		insertGraphsData = `\nINSERT {` + preGraphs + graphDescrIns + postGraphs + `} \nWHERE { SERVICE <` + cigFromUrl + `> { ` +  preGraphs  + postGraphs + ` } } ; `;

		deleteTriples = `\nDELETE WHERE { ` + graphDescrDel + ` } ; `;

		//////UPDATE GRAPH STORE//////

		let sparqlUpdate =  insertGraphsData + deleteTriples ;
		

		let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate
	
		const URL = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + cigTo + "/update";

		try{

		
		request.post(
			URL, {
			headers: {
				"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64")
			},
			body: prefixAndSparqlUpdate
		},
			function (error, response, body) {
				
				if (!error && response && response.statusCode < 400) {

					callback(null, response.statusCode);

				} else {

					logger.error("SPARQL update failed at: " + URL + " Query: " + prefixAndSparqlUpdate + ". Error: " + (error ? error : "None") + ". Body: " + (body ? body : "None") + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");

					callback(error, null);
				}

			}

		);
		} catch(err){
			callback(error, null);
		}

	}

	/**
	 * 
	 * @param {string} cigId 
	 * @param {string} recAssertUri 
	 * @param {string} beliefDsId 
	 * @param {string} TrDsId 
	 * @param {string} actDsId 
	 * @param {(Error, JSON)} callback 
	 */
	static getRecData(cigId, recAssertUri, beliefDsId, TrDsId, actDsId, callback) {

		const recAssertURI = `<` + recAssertUri + `>`;
		const recProvURI = `<` + recAssertUri + `_provenance>`;

		const cbUrl = "<http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + beliefDsId + "/query>";
		const TrUrl = "<http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + TrDsId + "/query>";
		const actUrl = "<http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + actDsId + "/query>";

		let query = 
		`
		SELECT DISTINCT ?text ?actAdmin ?cbUri ?strength ?contrib ?sourceOfRec ?partOf
						?freq ?evidence ?TrUri ?PropUri ?deriv ?sitFromId ?sitToId ?propTxt ?sitFromLabel ?sitToLabel
						?adminT ?actId ?adminLabel ?actType ?actLabel  
	    WHERE { 

		   GRAPH   `+ recAssertURI + `  {
			`+ recAssertURI + ` a  vocab:ClinicalRecommendation . 
			`+ recAssertURI + ` rdfs:label ?text .
			`+ recAssertURI + ` vocab:aboutExecutionOf ?actAdmin .
			`+ recAssertURI + ` vocab:basedOn ?cbUri .
			`+ recAssertURI + ` vocab:strength ?strength .
			?cbUri vocab:contribution ?contrib .
			OPTIONAL { `+ recAssertURI + ` vocab:extractedFrom ?extractedFrom . } .
			OPTIONAL { `+ recAssertURI + ` vocab:partOf ?partOf . } .
			}

			GRAPH   `+ recProvURI + `  {
				`+ recProvURI + ` a  <http://www.w3.org/ns/oa#Annotation> . 
				`+ recProvURI + `  <http://www.w3.org/ns/oa#hasBody> ` + recAssertURI + ` .
				`+ recAssertURI + ` prov:wasDerivedFrom ?sourceOfRec .
				}

			SERVICE `+ cbUrl + ` {
				GRAPH  ?cbUri {
					?cbUri a  vocab:CausationBelief . 
					?cbUri vocab:frequency ?freq .
					?cbUri vocab:strength ?evidence .
				  ?actAdmin vocab:causes ?TrUri .
				}
			}

			SERVICE `+ TrUrl + ` { 
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
			}

			SERVICE `+ actUrl + ` {
				?actAdmin a owl:NamedIndividual .
				?actAdmin a ?adminT .
				?actAdmin	?Of ?actId .
				?actAdmin rdfs:label ?adminLabel .
				?actId a owl:NamedIndividual .
				?actId a ?actType .
				?actId rdfs:label ?actLabel .
				FILTER (?actType != owl:NamedIndividual &&
					 (?Of = vocab:administrationOf || ?Of = vocab:applicationOf) &&
					 ?adminT != owl:NamedIndividual) .
			}
	   }
	   `; 
	   
		this.sparqlJSONQuery( cigId, query, callback );

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

		const TrUrl = "<http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + TrId + "/query>";
		const actUrl = "<http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + actId + "/query>";

		let query = `
	SELECT DISTINCT 
	?freq ?strength ?TrUri
	?propUri ?deriv ?sitFromId ?sitToId ?propTxt ?sitFromLabel ?sitToLabel
	?actId ?adminLabel ?actType ?actLabel 
	WHERE {
		GRAPH  `+ belief_Uri + ` {
		 `+ belief_Uri + ` a  vocab:CausationBelief . 
		 `+ belief_Uri + ` vocab:frequency ?freq .
		 `+ belief_Uri + ` vocab:strength ?strength .
		  ?actAdmin vocab:causes ?TrUri .
		 }
		SERVICE `+ TrUrl + ` { 
			?TrUri a vocab:TransitionType ;
					a owl:NamedIndividual ;
				 vocab:hasTransformableSituation ?sitFromId ;
				 vocab:hasExpectedSituation ?sitToId ;
			     vocab:affects ?propUri ;
				 vocab:derivative ?deriv .
			?propUri  a  vocab:TropeType ;
				a owl:NamedIndividual ;
			 		rdfs:label ?propTxt .			        
			?sitFromId a vocab:SituationType ;
				a owl:NamedIndividual ;
				rdfs:label ?sitFromLabel .
			?sitToId a vocab:SituationType ;
				a owl:NamedIndividual ;
				rdfs:label ?sitToLabel .
			
		}
		SERVICE `+ actUrl + ` {
			?actAdmin a owl:NamedIndividual .
			?actAdmin a ?adminT .
			?actAdmin	?Of ?actId .
			?actAdmin rdfs:label ?adminLabel .
			?actId a owl:NamedIndividual .
			?actId a ?actType .
			?actId rdfs:label ?actLabel .
			FILTER (?actType != owl:NamedIndividual &&
				 (?Of = vocab:administrationOf || ?Of = vocab:applicationOf) &&
				 ?adminT != owl:NamedIndividual) .
		}
	}
	`;
		this.sparqlJSONQuery( datasetId, query, callback);

	}

	/**
	 * 
	 * @param {string} dataset_id 
	 * @param {string} uri 
	 * @param {(Error, JSON)} callback 
	 */
	static getCareActionData(dataset_id, uri, callback) {

		let query = `SELECT DISTINCT  ?actId ?adminLabel ?actType ?actLabel ?snomed
		WHERE {
			<`+ uri + `> a owl:NamedIndividual.
			<`+ uri + `> a ?adminT.
			<`+ uri + `>	?Of ?actId.
			<`+ uri + `> rdfs:label ?adminLabel.
			?actId a owl:NamedIndividual.
			?actId a ?actType.
			?actId rdfs:label ?actLabel.
			?actId vocab:snomedCode  ?snomed.
			FILTER (?actType != owl:NamedIndividual &&
				 (?Of = vocab:administrationOf || ?Of = vocab:applicationOf) &&
				 ?adminT != owl:NamedIndividual).
		}
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

		let query = `SELECT DISTINCT  ?sitFromId ?sitToId ?sitFromLabel ?sitToLabel ?propTxt ?propUri ?deriv
		WHERE {
			`+ TrUri + ` a vocab:TransitionType .
			`+ TrUri + ` vocab:affects ?propUri .
			`+ TrUri + ` vocab:derivative ?deriv.
			`+ TrUri + ` vocab:hasTransformableSituation ?sitFromId .
			`+ TrUri + ` vocab:hasExpectedSituation ?sitToId .
			?PropUri  a  vocab:TropeType .
			?PropUri rdfs:label ?propTxt .
			?sitFromId a vocab:SituationType .
			?sitToId a vocab:SituationType .
			?sitFromId rdfs:label ?sitFromLabel .
			?sitToId rdfs:label ?sitToLabel .
		}
		`;

		this.sparqlJSONQuery(dataset_id, query, callback );
	}

	/**
	 * 
	 * @param {String} dataset_id 
	 * @param {String} instance 
	 * @param {(Error, JSON)} callback 
	 */
	static sparqlGetSubjectAllNamedGraphs(dataset_id, instance, callback) {

		//logger.info(`dataset_id: ` + dataset_id + ` and instance: ` + instance);

		let query = `
		SELECT ?s
		WHERE {
		  GRAPH ?g { ?s a ` + instance + ` }
		}
		`;

		this.sparqlQuery(dataset_id, query, callback);

	}

	static sparqlGetSubjectDefaultGraph(dataset_id, instance, callback) {

		let query = `
		SELECT ?s
		WHERE {
		   ?s a ` + instance + ` 
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	static  nList(list, n) {

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
		let query = `
		SELECT ?s ?p ?o
		WHERE {
			GRAPH  `+ graph + `  { ?s ?p ?o }
		}
		`;

		this.sparqlQuery(dataset_id, query, function (err, data) {

			//list could be empty if err
			callback(err, Sparql_Util.nList(data, 3));

		});

	}

	static sparqlGetPreds_Objcts(dataset_id, subject, callback) {

		let query = `
		SELECT ?p ?o
		WHERE {
		 `+ subject + ` ?p ?o .
		}
		`;

		this.sparqlQuery(dataset_id, query, function (err, data) {

			callback(err, Sparql_Util.nList(data, 2));

		});

	}

	static sparqlGetNamedGraphsFromObject(dataset_id, instance, callback) {

		let query = `
		SELECT ?g
		WHERE {
		  GRAPH ?g { ?s a ` + instance + ` }
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	/**
	 * 
	 * @param {clinical guideline} dataset_id 
	 * @param {nanopub named} filterString 
	 * @param {callback function} callback 
	 */
	static sparqlGetNamedNanopubFromSubguidelines(dataset_id, filterString, callback) {

		let query = `
		SELECT DISTINCT ?g
		WHERE {
			?g vocab:isPartOf ?sg
			`+ filterString + `
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	//path: interactions,drugeffects etc.., data=parameters for querying
	static callPrologServer(path, data, callback) {

		//path to swi-prolog server
		const URL = "http://" + config.PROLOG_HOST + ":" + config.PROLOG_PORT + "/" + path;

		request.post(

			URL, {
			headers: {
				"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
				"Content-Type": "application/x-www-form-urlencoded"
			},
			body: data
		},

			function (error, response, body) {
				
				if (!error && response && response.statusCode < 400 && body) {
			
					callback(null, body);

				} else {

					logger.error("Failed to call prolog server with path: " + path + ". Data: " + data + ". Error: " + error + ". Body: " + (body ? body : "None") + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(error, null);

				}

			}

		);

	}

}

module.exports = Sparql_Util;
