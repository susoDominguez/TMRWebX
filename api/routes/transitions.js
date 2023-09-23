const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
//const guidelines = require('../lib/prefixes');
const utils = require('../lib/utils');

router.post('/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("transitions", "tmr:TransitionType", function(err, uris) {

    res.send(uris);

  });

});

router.post('/situations/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("transitions", "tmr:SituationType", function(err, uris) {

    res.send(uris);

  });

});

router.post('/properties/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("transitions", "tmr:TropeType", function(err, uris) {

    res.send(uris);

  });

});

module.exports = router;
