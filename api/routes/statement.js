const express = require("express");
const router = express.Router();
const auxFuncts = require("../lib/router_functs/guideline_functs");
const config = require("../lib/config");
const { ErrorHandler } = require("../lib/errorHandler");
const utils = require("../lib/utils");
const logger = require("../config/winston");

function action(req) {
  //data id for this statement
  const id = `data:ST` + req.body.id;
  const date = new Date().toJSON();
  let sources = "";

  req.body.derivedFrom ??= 'http://anonymous.org/tmr/data/Not_given';
  req.body.derivedFrom.split(",").forEach(function (code) {
    sources += ` <${code.trim()}> ,`;
  });
  //this removes the last coma
  sources = sources.substring(0, sources.length - 1);

  // Belief format:
  const head = ` GRAPH ${id}_head {
    ${id} a nanopub:Nanopublication ;
              nanopub:hasAssertion ${id} ;
              nanopub:hasProvenance ${id}_provenance ;
              nanopub:hasPublicationInfo  ${id}_publicationinfo . 
      } `;

  const assertion = ` GRAPH ${id} {
      ${id} a vocab:ClinicalStatement ;
       vocab:OrganizationName "${req.body.organization_name}" ;
          vocab:OrganizationJurisdiction "${req.body.geographical_jurisdiction}" ;
          vocab:hasStatementTitle   "${req.body.statement_title}" ;
          vocab:hasStatementText '''${req.body.statement_text}''' . } `;

  const provenance = ` GRAPH ${id}_provenance {
    ${id}_provenance a  oa:Annotation ;
           oa:hasBody ${id} ;
           prov:wasDerivedFrom ${sources} . } `;


  const publication = ` GRAPH ${id}_publicationinfo {
    ${id}_head prov:generatedAtTime "${date}"^^xsd:dateTime ;
          prov:wasAttributedTo  data:${req.body.author} .
    } `;

    return ` INSERT DATA { ${head}  ${assertion} ${provenance} ${publication} } ` ;
}


router.post("/add", async function (req, res) {
  let sparql_str = action(req);
  logger.debug(sparql_str)
  let {status, data} = await utils.sparqlUpdate("statements", sparql_str);
    
      res.status(status).json(data);
});

router.post("/delete", async function (req, res) {
  let query = auxFuncts.sparql_drop_named_graphs("statements", `ST${req.body.id}`);
  let {status, data} = await utils.sparqlUpdate("statements", query);
  res.status(status).send(data);
});

router.post("/all/get/", async function (req, res, next) {
  //statement_URI
  let {status, head_vars, bindings} = await utils.getStatementData(
    "statements",
    req.body.id);

    let result = auxFuncts.get_ST_data(head_vars,bindings[0]);
    res.status(status).json(result ??= {});
  });



module.exports = router;
