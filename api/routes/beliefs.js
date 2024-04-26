const express = require('express');
const router = express.Router();
const auxFuncts = require('../lib/router_functs/guideline_functs')
const utils = require('../lib/utils');

router.post('/get', async function(req, res, next) {

  let {status =500, bindings, head_vars } = await utils.get_named_subject_in_named_graphs_from_object("beliefs", "vocab:CausationBelief");
  
  if(status<400){
    let result = await auxFuncts.get_rdf_atom_as_array(bindings);
    return res.status(status).send(result);
  } else {
    return res.status(status).json([]);
  }

});

module.exports = router;
