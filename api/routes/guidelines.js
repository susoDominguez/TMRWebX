const express = require("express");
const router = express.Router();
const Promise = require("bluebird");
const nearley = require("nearley");
const grammar = require("../lib/parser/grammar");
const { reportError } = require("../lib/parser/parserError");
const utils = Promise.promisifyAll(require("../lib/utils"));
const logger = require("../config/winston");
const { ErrorHandler } = require("../lib/errorHandler");
const auxFuncts = require("../lib/router_functs/guideline_functs");
const querystring = require("querystring");

router.post("/interactions", async (req, res) => {
  try {
    if (!req.body.cig_id) {
      return res.status(406).json({ error: "cig_id param missing" });
    }

    const cigId = req.body.cig_id.startsWith("CIG-")
      ? req.body.cig_id
      : `CIG-${req.body.cig_id}`;

    const postData = querystring.stringify({ guideline_id: cigId });
    logger.info(
      "Determining interactions with data: " + JSON.stringify(postData)
    );

    const data = await utils.callPrologServer("interactions", postData);
    logger.info("Data sent to grammar parser is: " + data);

    const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar), {
      keepHistory: true,
    });

    try {
      parser.feed(data);
      const result = parser.results[0];

      if (result) {
        // Convert type of first recommendation to secondary when type of interaction is repairable
        result.forEach((val) => {
          if (val.type === "repairable") {
            val.interactionNorms[0].type = "secondary";
          }
        });

        return res.status(200).json(result);
      } else {
        throw new ErrorHandler(400, "Result of parsing failed");
      }
    } catch (parseError) {
      reportError(parseError, parser);
      throw new ErrorHandler(400, "Result of parsing failed");
    }
  } catch (error) {
    logger.error(`Error in /interactions: ${error.message}`);
    res.status(error.statusCode || 500).json({
      status: "error",
      error: error.message,
    });
  }
});

module.exports = router;
