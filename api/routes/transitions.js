const express = require('express');
const router = express.Router();
const auxFunct = require('../lib/router_functs/guideline_functs');
const config = require('../lib/config');
const logger = require('../config/winston')
const utils = require('../lib/utils');

router.post('/get', async function(req, res, next) {

  let {status =500, bindings, head_vars } = await utils.sparqlGetSubjectDefaultGraph("transitions", "vocab:TransitionType");
  if(status<400){
    let result = await auxFunct.get_rdf_atom_as_array(bindings);
    return res.status(status).send(result);
  } else {
    return res.status(status).json([]);
  }

});

router.post('/situations/get', async function(req, res, next) {

  let {status =500, bindings, head_vars } = await utils.sparqlGetSubjectDefaultGraph("transitions", "vocab:SituationType");
  if(status<400){
    let result = await auxFunct.get_rdf_atom_as_array(bindings);
    return res.status(status).send(result);
  } else {
    return res.status(status).json([]);
  }

});

router.post('/properties/get', async function(req, res, next) {

  let {status =500, bindings, head_vars } = await utils.sparqlGetSubjectDefaultGraph("transitions", "vocab:TropeType");
  if(status<400){
    let result = await auxFunct.get_rdf_atom_as_array(bindings);
    return res.status(status).send(result);
  } else {
    return res.status(status).json([]);
  }

});

module.exports = router;
