const express = require("express");
const router = express.Router();
const logger = require("../config/winston");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const utils = require("../lib/utils");
const { ErrorHandler } = require("../lib/errorHandler");

const PREFIX = "http://anonymous.org/data/";

/**
 * Constructs the SPARQL query for inserting a causation belief.
 * @param {Object} req - The request object containing belief details.
 * @returns {string} - The SPARQL query.
 */
function constructInsertQuery(req) {
  const id = `${PREFIX}CB${req.body.id}`;
  const date = new Date().toISOString();

  const derivedFrom =
    req.body.derivedFrom
      ?.split(",")
      .map((source) => `<${source.trim()}>`)
      .join(", ") || `<${PREFIX}Not_given>`;

  const head = `GRAPH <${id}_head> {
    <${id}> a nanopub:Nanopublication ;
              nanopub:hasAssertion <${id}> ;
              nanopub:hasProvenance <${id}_provenance> ;
              nanopub:hasPublicationInfo <${id}_publicationinfo> .
  }`;

  const assertion = `GRAPH <${id}> {
    <${PREFIX}ActAdminister${req.body.care_action_id}> vocab:causes <${PREFIX}Tr${req.body.transition_id}> .
    <${id}> a vocab:CausationBelief ;
            vocab:strength """${req.body.strength}""" ;
            vocab:frequency """${req.body.frequency}""" .
  }`;

  const provenance = `GRAPH <${id}_provenance> {
    <${id}_provenance> a oa:Annotation ;
                      oa:hasBody <${id}> ;
                      prov:wasDerivedFrom ${derivedFrom} .
  }`;

  const publication = `GRAPH <${id}_publicationinfo> {
    <${id}_head> prov:generatedAtTime "${date}"^^xsd:dateTime ;
                  prov:wasAttributedTo <${PREFIX}${req.body.author}> .
  }`;

  return `INSERT DATA { ${head} ${assertion} ${provenance} ${publication} }`;
}

/**
 * Endpoint to add a causation belief.
 */
router.post("/add", async (req, res) => {
  try {
    const sparqlQuery = constructInsertQuery(req);
    logger.debug(sparqlQuery);

    const { status, data } = await utils.sparqlUpdate("beliefs", sparqlQuery);
    return res.status(status).json(data);
  } catch (error) {
    logger.error(`Error adding causation belief: ${error}`);
    return res.status(500).json({ error: "Failed to add causation belief." });
  }
});

/**
 * Endpoint to delete a causation belief.
 */
router.post("/delete", async (req, res) => {
  try {
    const query = auxFuncts.sparql_drop_named_graphs(
      "beliefs",
      `CB${req.body.id}`
    );
    const { status, data } = await utils.sparqlUpdate("beliefs", query);
    return res.status(status).send(data);
  } catch (error) {
    logger.error(`Error deleting causation belief: ${error}`);
    return res
      .status(500)
      .json({ error: "Failed to delete causation belief." });
  }
});

/**
 * Endpoint to retrieve a specific causation belief.
 */
router.post("/all/get", async (req, res) => {
  try {
    const id =
      req.body.uri ||
      (req.body.id.includes("CB")
        ? `${PREFIX}${req.body.id}`
        : `${PREFIX}CB${req.body.id}`);

    const { status, head_vars, bindings } = await utils.getBeliefData(
      "beliefs",
      id,
      "transitions",
      "careActions"
    );

    if (status < 400 && bindings.length > 0) {
      const data = auxFuncts.get_CB_object(head_vars, bindings[0]);
      logger.debug(data);
      return res.status(status).json(data);
    }

    return res.status(status).json({});
  } catch (error) {
    logger.error(`Error retrieving causation belief: ${error}`);
    return res
      .status(500)
      .json({ error: "Failed to retrieve causation belief." });
  }
});

module.exports = router;
