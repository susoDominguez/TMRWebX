const express = require("express");
const router = express.Router();
const Promise = require("bluebird");
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar");
const { reportError } = require("../lib/parser/parserError");
const utils = Promise.promisifyAll(require("../lib/utils"));
const logger = require("../config/winston");
const { ErrorHandler } = require("../lib/errorHandler");
const { error } = require("console");
const { json } = require("body-parser");
//const e = require("express");
//const { throws } = require("assert");

router.post("/interactions", function (req, res, next) {
  if (!req.body.cig_id) {
    res.status(406).send({ error: "cig_id param missing" });
    return;
  }

  let cigId = req.body.cig_id.startsWith(`CIG-`)
    ? req.body.cig_id
    : `CIG-` + req.body.cig_id;

  let postData = require("querystring").stringify({
    //Jena dataset name
    guideline_id: cigId,
  });

  logger.info(
    "Determining interactions with data: " + JSON.stringify(postData)
  );

  utils
    .callPrologServerAsync("interactions", postData)
    .then((data) => {
      logger.info("data sent to grammar parser is: " + data);

      //use grammar to parse response into a JSON object
      const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar), {
        keepHistory: true,
      });

      let result;

      try {
        parser.feed(data);

        result = parser.results[0];
        //convert type of first recommendation to secondary when type of interaction is repairable
        for (let val of result) {
          if (val.type === "repairable") {
            val.interactionNorms[0].type = "secondary";
          }
        }
      } catch (e) {
        reportError(e, parser);
        throw ErrorHandler(400, "result of parsing failed");
      }

      if (result) {
        return res.status(200).json(result);
      } else {
        throw ErrorHandler(400, "result of parsing failed");
      }
    })
    .catch((err) => {
      res.status(400).json({
        status: "error",
        error: err,
      });
    });
});

module.exports = router;
