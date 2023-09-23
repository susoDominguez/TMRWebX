const express = require('express');
const router = express.Router();

const utils = require('../lib/utils');

router.post('/get', function(req, res) {

  utils.sparqlGetNamedGraphsFromObject("statements", "tmr:ClinicalStatement", function(err, uris) {

    res.send(uris);

  });

});

module.exports = router;
