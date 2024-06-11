const express = require("express");
const router = express.Router();
const logger = require("../config/winston");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const { ErrorHandler } = require("../lib/errorHandler");
const utils = require("../lib/utils");
const prefix = "http://anonymous.org/data/";

function action(req) {
  //data id for this belief
  const id = `data:CB` + req.body.id;
  const date = new Date().toJSON();
  let sources = "";

  req.body.derivedFrom = req.body.derivedFrom ? req.body.derivedFrom : 'http://anonymous.org/tmr/data/Not_given';
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
      data:ActAdminister${req.body.care_action_id} vocab:causes data:Tr${req.body.transition_id} .
                      ${id} a vocab:CausationBelief ;
          vocab:strength '''${req.body.strength}''' ;
          vocab:frequency   '''${req.body.frequency}''' . 
  } `;

  const provenance = ` GRAPH ${id}_provenance {
    ${id}_provenance a  oa:Annotation ;
           oa:hasBody ${id} ;
           prov:wasDerivedFrom ${sources} . } `;

  // oa:hasTarget [ oa:hasSource <http://hdl.handle.net/10222/43703> ] . `

  const publication = ` GRAPH ${id}_publicationinfo {
    ${id}_head prov:generatedAtTime "${date}"^^xsd:dateTime ;
          prov:wasAttributedTo  data:${req.body.author} .
    } `;

    return ` INSERT DATA { ${head}  ${assertion} ${provenance} ${publication} } ` ;
}

router.post("/add", async function (req, res) {
  let sparql_str = action(req);
  logger.debug(sparql_str)
  let {status, data} = await utils.sparqlUpdate("beliefs", sparql_str);
    
      res.status(status).json(data);
});

router.post("/delete", async function (req, res) {
  let query = auxFuncts.sparql_drop_named_graphs("beliefs", `CB${req.body.id}`);
  let {status, data} = await utils.sparqlUpdate("beliefs", query);
  res.status(status).send(data);
});

router.post("/all/get/", async function (req, res, next) {
  const id =  req.body.uri ? req.body.uri : req.body.id.includes('CB') ? "data:"+req.body.id : "data:CB"+req.body.id;

  try{
  let {status,head_vars,bindings} = await utils.getBeliefData("beliefs",id,"transitions","careActions");

  logger.debug(bindings);

  if(status < 400 && bindings.length > 0) {
    let data = auxFuncts.get_CB_object(head_vars, bindings[0]);
    logger.debug(data);
    return res.status(status).json(data);
  } else {
    return res.status(status).json({});
  } } catch(err){
    return res.status(500).json(JSON.stringify(err));
  }
    
});

module.exports = router;
