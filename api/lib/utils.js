const request = require('request');
const n3 = require('n3');
const parser = new n3.Parser();
const xmlReader = require('xml-reader');
const xmlQuery = require('xml-query');

const config = require('../lib/config');
const guidelines = require('../lib/guidelines');
const logger = require('../config/winston');

class Util {

	/**
	 * 
	 * @param {CIG identifier} dataset_id 
	 * @param {SPARQL query content} content 
	 * @param {insert | delete} insertOrDelete 
	 * @param {callback} callback 
	 */
	static sparqlUpdate(dataset_id, content, insertOrDelete, callback) {

		var sparqlUpdate = ` ` + insertOrDelete + ` DATA {
	` + content + `}`;

		var prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate
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

					callback(200);

				} else {

					console.log("SPARQL update failed at: " + URL + " Query: " + prefixAndSparqlUpdate + ". Error: " + (error ? error : "None") + ". Body: " + (body ? body : "None") + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(400);

				}

			}

		);

	}

	/**
	 * 
	 * @param {identifier of Jenna dataset} dataset_id 
	 * @param {URI of recommendation assertion to be deleted} rec_uri 
	 * @param {status of response/error} callback 
	 */
	static sparqlDropGraphs(dataset_id, graph_uri, callback) {

		var sparqlUpdate = ` DROP silent GRAPH data:` + graph_uri + `_head;
		 DROP silent GRAPH data:` + graph_uri + `;
		 DROP silent GRAPH data:` + graph_uri + `_provenance; 
		 DROP silent GRAPH data:` + graph_uri + `_publicationinfo;
		 DELETE  { data:`+ graph_uri + ` vocab:isPartOf ?subguideline } WHERE { data:` + graph_uri + ` vocab:isPartOf ?subguideline }`;

		var prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate
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

					callback(200);

				} else {

					console.log("SPARQL update failed at: " + URL + "  Query: " + prefixAndSparqlUpdate + ". Error: " + (error ? error : "None") + ". Body: " + (body ? body : "None") + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(400);

				}

			}

		);

	}

	/**
	 * 
	 * @param {identifier of CIG} dataset_id 
	 * @param {SPARQL query} query 
	 * @param {callback} callback 
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
				console.log("body:\n" + body)
				if (!error && response && response.statusCode == 200) {

					var data = [];

					var queryContainer = xmlQuery(xmlReader.parseSync(body));

					queryContainer.find('binding').each(function (binding) {

						data.push(binding.children[0].children[0].value);

					});

					callback(data);

				} else {

					console.log("SPARQL query failed: " + query + ". Error: " + error + ". Body: " + body + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(null);

				}

			}/*
		request.get(
		

			"http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/query?sparql=" +guidelines.PREFIXES + "\n" + query,

			function (error, response, body) {

				if ( !error && response && response.statusCode == 200 ) {

					var data = [];

					var queryContainer = xmlQuery(xmlReader.parseSync(body));

					queryContainer.find('binding').each(function(binding) {

						data.push(binding.children[0].children[0].value);

					});

					callback(data);

				} else {

					console.log("SPARQL query failed: " + query + ". Error: " + error + ". Body: " + body + ". Status: " + ( ( response && response.statusCode ) ? response.statusCode : "No response." ) + ".");
					callback(null);

				}

			}*/

		);

	}

	/**
	 * 
	 * @param {identifier of CIG} dataset_id 
	 * @param {SPARQL query} query 
	 * @param {callback} callback 
	 */
	static sparqlJSONQuery(dataset_id, query, callback) {

		const prefixAndSparqlQuery = guidelines.PREFIXES + "\n" + query
		const url = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + dataset_id + "/query";

		request.post(url, {

			headers: {
				"Authorization": "Basic " + new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
				"Accept": 'application/sparql-results+json'
			},

			form: { query: prefixAndSparqlQuery }
		},
			function (error, response, body) {
				console.log(body)
				if (!error && response && response.statusCode == 200) {

					callback(JSON.parse(body));

				} else {
					console.log("SPARQL query failed: " + query + ". Error: " + error + ". Body: " + body + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback({});
				}
			}
		);
	}

	static getRecData(cigId, recAssertUri, beliefDsId, TrDsId, actDsId, callback) {

		const cbUrl = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + beliefDsId + "/query";
		const TrUrl = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + TrDsId + "/query";
		const actUrl = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + actDsId + "/query";

		var query = `
	    SELECT DISTINCT ?text ?motive ?strength ?contrib ?cbUri
	   			?freq ?evidence ?TrUri
				?propUri ?deriv ?sitFromId ?sitToId ?propTxt ?sitFromLabel ?sitToLabel
				?actId ?adminLabel ?actType ?actLabel ?snomed 
	    WHERE {
		   GRAPH   <`+ recAssertUri +`>  {
			<`+ recAssertUri + `> a  vocab:ClinicalRecommendation . 
			<`+ recAssertUri + `> rdfs:label ?text .
			<`+ recAssertUri + `> vocab:aboutExecutionOf ?actAdmin .
			<`+ recAssertUri + `> vocab:basedOn ?cbUri .
			<`+ recAssertUri + `> vocab:motivation ?motive .
			<`+ recAssertUri + `> vocab:strength ?strength .
			?cbUri vocab:contribution ?contrib .
			}
			SERVICE <`+ cbUrl + `> {
				GRAPH  ?cbUri {
					?cbUri a  vocab:CausationBelief . 
					?cbUri vocab:frequency ?freq .
					?cbUri vocab:strength ?evidence .
				  ?actAdmin vocab:causes ?TrUri .
				}
			}
				SERVICE <`+ TrUrl + `> { 
					?TrUri a vocab:TransitionType .
					?TrUri vocab:affects ?propUri .
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
				SERVICE <`+ actUrl + `> {
					?actAdmin a owl:NamedIndividual .
					?actAdmin a ?adminT .
					?actAdmin	?Of ?actId .
					?actAdmin rdfs:label ?adminLabel .
					?actId a owl:NamedIndividual .
					?actId a ?actType .
					?actId rdfs:label ?actLabel .
					?actId vocab:snomedCode  ?snomed .
					FILTER (?actType != owl:NamedIndividual &&
						 (?Of = vocab:administrationOf || ?Of = vocab:applicationOf) &&
						 ?adminT != owl:NamedIndividual) .
				}
	   }
	   `;

		this.sparqlJSONQuery(cigId, query, function (data) {

			callback(data);

		});

	}

	static getBeliefData(datasetId, beliefUri, TrId, actId, callback) {

		const TrUrl = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + TrId + "/query";
		const actUrl = "http://" + config.JENA_HOST + ":" + config.JENA_PORT + "/" + actId + "/query";

		var query = `
	SELECT DISTINCT 
	?freq ?strength ?TrUri
	?propUri ?deriv ?sitFromId ?sitToId ?propTxt ?sitFromLabel ?sitToLabel
	?actId ?adminLabel ?actType ?actLabel ?snomed 
	WHERE {
		GRAPH  <`+ beliefUri + `>  {
		 <`+ beliefUri + `> a  vocab:CausationBelief . 
		 <`+ beliefUri + `> vocab:frequency ?freq .
		 <`+ beliefUri + `> vocab:strength ?strength .
		  ?actAdmin vocab:causes ?TrUri .
		 }
		SERVICE <`+ TrUrl + `> { 
			?TrUri a vocab:TransitionType .
			?TrUri vocab:affects ?propUri .
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
		SERVICE <`+ actUrl + `> {
			?actAdmin a owl:NamedIndividual .
			?actAdmin a ?adminT .
			?actAdmin	?Of ?actId .
			?actAdmin rdfs:label ?adminLabel .
			?actId a owl:NamedIndividual .
			?actId a ?actType .
			?actId rdfs:label ?actLabel .
			?actId vocab:snomedCode  ?snomed .
			FILTER (?actType != owl:NamedIndividual &&
				 (?Of = vocab:administrationOf || ?Of = vocab:applicationOf) &&
				 ?adminT != owl:NamedIndividual) .
		}
	}
	`;

		this.sparqlJSONQuery(datasetId, query, function (data) {

			callback(data);

		});

	}

	static getCareActionData(dataset_id, uri, callback) {

		var query = `SELECT DISTINCT  ?actId ?adminLabel ?actType ?actLabel ?snomed
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

		this.sparqlJSONQuery(dataset_id, query, callback)
	}

	static getTransitionData(dataset_id, TrUri, callback) {

		var query = `SELECT DISTINCT  ?sitFromId ?sitToId ?sitFromLabel ?sitToLabel ?propTxt ?propUri ?deriv
		WHERE {
			<`+ TrUri + `> a vocab:TransitionType .
			<`+ TrUri + `> vocab:affects ?propUri .
			<`+ TrUri + `> vocab:derivative ?deriv.
			<`+ TrUri + `> vocab:hasTransformableSituation ?sitFromId .
			<`+ TrUri + `> vocab:hasExpectedSituation ?sitToId .
			?PropUri  a  vocab:TropeType .
			?PropUri rdfs:label ?propTxt .
			?sitFromId a vocab:SituationType .
			?sitToId a vocab:SituationType .
			?sitFromId rdfs:label ?sitFromLabel .
			?sitToId rdfs:label ?sitToLabel .
		}
		`;

		this.sparqlJSONQuery(dataset_id, query, callback)
	}

	static sparqlGetSubjectAllNamedGraphs(dataset_id, instance, callback) {

		var query = `
		SELECT ?s
		WHERE {
		  GRAPH ?g { ?s a ` + instance + ` }
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	static sparqlGetSubjectDefaultGraph(dataset_id, instance, callback) {

		var query = `
		SELECT ?s
		WHERE {
		   ?s a ` + instance + ` 
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	static nList(list, n) {

		var pairedPredicateObject = [];

		for (var i = 0; i < list.length; i += n) {

			var nTuple = [];

			for (var j = i; j < i + n; j++) {

				nTuple.push(list[j]);

			}

			pairedPredicateObject.push(nTuple);

		}

		return pairedPredicateObject;

	}

	static sparqlGetResourcesFromNamedGraph(dataset_id, graph, callback) {
		var query = `
		SELECT ?s ?p ?o
		WHERE {
			GRAPH  `+ graph + `  { ?s ?p ?o }
		}
		`;

		this.sparqlQuery(dataset_id, query, function (data) {

			callback(Util.nList(data, 3));

		});

	}

	static sparqlGetPreds_Objcts(dataset_id, subject, callback) {

		var query = `
		SELECT ?p ?o
		WHERE {
		  GRAPH ?g { `+ subject + ` ?p ?o }
		}
		`;

		this.sparqlQuery(dataset_id, query, function (data) {

			callback(Util.nList(data, 2));

		});

	}

	static sparqlGetNamedGraphsFromObject(dataset_id, instance, callback) {

		var query = `
		SELECT ?g
		WHERE {
		  GRAPH ?g { ?s a ` + instance + ` }
		}
		`;

		this.sparqlQuery(dataset_id, query, callback)

	}

	//path: interactions,drugeffects etc.., data=parameters for querying
	static callPrologServer(path, data, res, callback) {

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

					callback(body);

				} else {

					logger.error("Failed to call prolog server with path: " + path + ". Data: " + data + ". Error: " + error + ". Body: " + (body ? body : "None") + ". Status: " + ((response && response.statusCode) ? response.statusCode : "No response.") + ".");
					callback(null);

				}

			}

		);

	}

}

module.exports = Util;
