const express = require("express");
const router = express.Router();
const config = require("../lib/config");
const utils = require("../lib/utils");
const auxFunct = require("../lib/router_functs/guideline_functs");
const { ErrorHandler } = require("../lib/errorHandler");

// Utility function for building SPARQL queries
async function postTransition(transitionData, insertOrDelete, type = "") {
  const deleteStr = `data:${type + transitionData} ?p ?o .`;
  const content = insertOrDelete === config.INSERT ? transitionData : deleteStr;

  let sparqlQuery = `${insertOrDelete} ${
    insertOrDelete === config.INSERT ? "DATA" : ""
  } { ${content} }`;

  if (insertOrDelete === config.DELETE) {
    sparqlQuery += ` WHERE { ${deleteStr} }`;
  }

  return utils.sparqlUpdate("transitions", sparqlQuery);
}

// Handlers for creating and deleting properties, situations, and transitions
router.post("/property/add", async (req, res) => {
  const definition = buildPropertyDefinition(req);
  const { data, status } = await postTransition(definition, config.INSERT);
  res.status(status).send(data);
});

router.post("/property/delete", async (req, res) => {
  const { data, status } = await postTransition(
    req.body.id,
    config.DELETE,
    "Prop"
  );
  res.status(status).send(data);
});

router.post("/situation/compound/add", async (req, res) => {
  const definition = buildCompoundSituationDefinition(req);
  const { data, status } = await postTransition(definition, config.INSERT);
  res.status(status).send(data);
});

router.post("/situation/add", async (req, res) => {
  const definition = buildSituationDefinition(req);
  const { data, status } = await postTransition(definition, config.INSERT);
  res.status(status).send(data);
});

router.post("/situation/delete", async (req, res) => {
  const { data, status } = await postTransition(
    req.body.id,
    config.DELETE,
    "Sit"
  );
  res.status(status).send(data);
});

router.post("/situation/compound/delete", async (req, res) => {
  const { data, status } = await postTransition(
    req.body.id,
    config.DELETE,
    "Sit"
  );
  res.status(status).send(data);
});

router.post("/add", async (req, res) => {
  const definition = buildTransitionDefinition(req);
  const { data, status } = await postTransition(definition, config.INSERT);
  res.status(status).send(data);
});

router.post("/delete", async (req, res) => {
  const { data, status } = await postTransition(
    req.body.id,
    config.DELETE,
    "Tr"
  );
  res.status(status).send(data);
});

router.post("/all/get/", async (req, res) => {
  const id = req.body.id ? `data:Tr${req.body.id}` : `<${req.body.uri}>`;
  try {
    const { status, head_vars, bindings } = await utils.getTransitionData(
      "transitions",
      id
    );

    if (status < 400 && bindings.length > 0) {
      const data = auxFunct.get_transition_object(head_vars, bindings[0]);
      return res.status(status).json(data);
    } else {
      return res.status(status).json({});
    }
  } catch (error) {
    return res.status(500).json({ error: error.message });
  }
});

// Definition builders
function buildTransitionDefinition(req) {
  return `data:Tr${req.body.id} rdf:type vocab:TransitionType ;
          vocab:hasTransformableSituation data:Sit${req.body.pre_situation_id} ;
          vocab:hasExpectedSituation data:Sit${req.body.post_situation_id} ;
          vocab:derivative "${req.body.derivative}" ;
          vocab:affects data:Prop${req.body.affected_property_id} .`;
}

function buildSituationDefinition(req) {
  let situationDef = `data:Sit${req.body.id} rdf:type vocab:SituationType, owl:NamedIndividual ;
                      rdfs:label "${req.body.label}"@en ;
                      vocab:stateOf "${req.body.stateOfproperty}"`;

  situationDef += appendCodes(req, [
    "umlsCode",
    "atcCode",
    "icd10Code",
    "sctid",
  ]);
  situationDef += appendCodeLabels(req, [
    "umlsCode_label",
    "atcCode_label",
    "icd10Code_label",
    "sctid_label",
  ]);
  return `${situationDef} .`;
}

function buildCompoundSituationDefinition(req) {
  if (!req.body.situation_id_list) {
    throw new ErrorHandler(500, "List of situations missing");
  }

  let situationDef = `data:Sit${req.body.id} rdf:type vocab:CompoundSituationType, owl:NamedIndividual ;
                      rdfs:label "${req.body.label}"@en ;
                      vocab:${req.body.connective} `;

  const situationList = req.body.situation_id_list
    .split(",")
    .map((id) => `data:Sit${id.trim()}`)
    .join(", ");

  situationDef += `${situationList}`;
  situationDef += appendCodes(req, ["icd10Code", "sctid"]);
  situationDef += appendCodes(req, ["icd10Code_label", "sctid_label"]);

  return `${situationDef} .`;
}

function buildPropertyDefinition(req) {
  let propertyDef = `data:Prop${req.body.id} rdf:type vocab:TropeType, owl:NamedIndividual ;
                     rdfs:label "${req.body.label}"@en`;

  propertyDef += appendCodes(req, ["icd10Code", "sctid"]);
  propertyDef += appendCodes(req, ["icd10Code_label", "sctid_label"]);
  return `${propertyDef} .`;
}

// Helper function for appending codes to definitions
// Helper function for appending a single code to definitions
function appendCodes(req, codeFields) {
  let codes = "";
  codeFields.forEach((field) => {
    if (req.body[field]) {
      codes += `vocab:${field} "${req.body[field].trim()}"^^xsd:string ;\n`;
    }
  });
  return codes.replace(/;\n$/, ""); // Remove the trailing semicolon and newline
}

// Helper function for appending a single code to definitions
function appendCodeLabels(req, codeFields) {
  let codes = "";
  codeFields.forEach((field) => {
    if (req.body[field]) {
      codes += `vocab:${field} "${req.body[field].trim()}"^^xsd:string ;\n`;
    }
  });
  return codes.replace(/;\n$/, ""); // Remove the trailing semicolon and newline
}

module.exports = router;
