const express = require('express');
const router = express.Router();

const utils = require('../lib/utils');

router.post('/get', function(req, res) {

  utils.sparqlGraphInstanceOf("beliefs", "<http://anonymous.org/vocab/CausationBelief>", function(uris) {

    res.send(uris);

  });

});

module.exports = router;
