const express = require("express");
const router = express.Router();

const config = require("../lib/config");
const { ErrorHandler } = require("../lib/errorHandler");
const utils = require("../lib/utils");
const logger = require("../config/winston");

function action(req, res, insertOrDelete) {
  //data id for this statement
  const id = `data:ST` + req.body.statement_id;

  // statement format:
  const head =
    id +
    `_head {
       ` +
    id +
    `_head
              a            nanopub:Nanopublication ;
              nanopub:hasAssertion          ` +
    id +
    ` ;
              nanopub:hasProvenance         ` +
    id +
    `_provenance ;
              nanopub:hasPublicationInfo    ` +
    id +
    `_publicationinfo .
  }`;

  const body =
    id +
    ` {
     ` +
    id +
    `   a   tmr:ClinicalStatement ;
                tmr:OrganizationName  "` +
    req.body.organization_name +
    `" ;
                tmr:OrganizationJurisdiction    "` +
    req.body.geographical_jurisdiction +
    `" ;  
                tmr:hasStatementTitle    '` +
    req.body.statement_title +
    `' ; 
                tmr:hasStatementText  '''` +
    req.body.statement_text +
    `''' .
  }`;

  const provenance =
    id +
    `_provenance {
          ` +
    id +
    `_provenance
          a                             oa:Annotation ;
          oa:hasBody                    ` +
    id +
    ` ;
          oa:hasTarget                  [ oa:hasSource <http://hdl.handle.net/10222/43703> ] .
          ` +
    id +
    `
          prov:wasDerivedFrom           <http://hdl.handle.net/10222/43703> .
  }`;

  const publication =
    id +
    `_publicationinfo {
          ` +
    id +
    `_head
          prov:generatedAtTime          "2023-01-01T13:14:15"^^xsd:dateTime ;
          prov:wasAttributedTo          data:` +
    req.body.creator +
    `.
  }`;

  utils.sparqlUpdate(
    "statements",
    "GRAPH " +
      head +
      "\nGRAPH " +
      body +
      "\nGRAPH " +
      provenance +
      "\nGRAPH " +
      publication,
    insertOrDelete,
    function (err, status) {
      if (err) {
        throw new ErrorHandler(
          404,
          "statements dataset could not be updated: Error " + err
        );
    } else {
            res.status(status).end();
        }
      
    }
  );
}

router.post("/add", function (req, res) {
  action(req, res, config.INSERT);
});

router.post("/delete", function (req, res) {
  //action(req, res, config.DELETE);
  const statement_URI = req.body.statement_id
    ? req.body.statement_id
    : "data:ST" + req.body._id;
  const dataset_id = "statements";

  utils.sparqlDropGraphs(dataset_id, statement_URI, function (err, status) {
    res.status(status).end();
  });
});

router.post("/all/get/", function (req, res, next) {
  //statement_URI
  utils.getStatementData(
    "statements",
    req.body.statement_URI
      ? "<" + req.body.statement_URI + ">"
      : "data:ST" + req.body.statement_URI,
    function (err, statementData) {
      if (
        err ||
        !statementData ||
        statementData.constructor !== Object ||
        Object.entries(statementData).length === 0
      ) {
        next(err);
        return;
      }
      //otherwise
      try {
        var stData = {
          id: req.body.statement_URI,
          author: "JDA",
        };

        var vars = statementData.head.vars;
        var bindings = statementData.results.bindings;

        //format data by looping through results
        for (let pos in bindings) {
          var bind = bindings[pos];

          for (var varPos in vars) {
            var value = bind[vars[varPos]].value;

            //for each heading, add a field
            switch (vars[varPos]) {
              case "hasStatementText":
                stData.statement = value;
                break;
              case "organizationName":
                stData.organizationName = value;
                break;
              case "OrganizationJurisdiction":
                stData.jurisdiction = value;
                break;
              case "statementTitle":
                stData.hasStatementTitle = value;
                break;
            }
          }
        }

        res.send(stData);
      } catch (error) {
        next(new ErrorHandler(500, err));
      }
    }
  );
});

module.exports = router;
