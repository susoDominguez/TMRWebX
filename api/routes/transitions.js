const express = require('express');
const router = express.Router();
const request = require('request');

const config = require('../lib/config');
const guidelines = require('../lib/namespace_PREFIX');
const utils = require('../lib/utils');

router.post('/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("transitions", "vocab:TransitionType", function(err, uris) {

    res.send(uris);

  });

});

router.post('/situations/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("transitions", "vocab:SituationType", function(err, uris) {

    res.send(uris);

  });

});

router.post('/properties/get', function(req, res, next) {

  utils.sparqlGetSubjectDefaultGraph("transitions", "vocab:TropeType", function(err, uris) {

    res.send(uris);

  });

});

module.exports = router;
