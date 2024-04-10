
function (error, response, body) {
  if (!error && response && response.statusCode < 400) {
    callback(null, 200);
  } else {
    let err =
      "SPARQL update failed at: " +
      URL +
      " Query: " +
      prefixAndSparqlUpdate +
      ". " +
      (error ? error : "None") +
      ". Body: " +
      (body ? body : "None") +
      ". Status: " +
      (response && response.statusCode ? response.statusCode : "401") +
      ".";

    logger.error(err);
    //throw new ErrorHandler((response && response.statusCode) ? response.statusCode : "401",
    //err);
    callback(
      error,
      response && response.statusCode ? response.statusCode : "401"
    );
  }
}
  /**
   *
   * @param {string} dataset_id identifier of Jenna dataset
   * @param {string} rec_uri URI of recommendation assertion to be deleted
   * @param {(Error, number) => number} callback callback function to deliver response (err, status)
   */
   sparqlDropGraphs(dataset_id, graph_uri, callback) {
    let sparqlUpdate =
      ` DROP SILENT GRAPH ` +
      graph_uri +
      `_head ;
		 DROP SILENT GRAPH ` +
      graph_uri +
      ` ;
		 DROP SILENT GRAPH ` +
      graph_uri +
      `_provenance ; 
		 DROP SILENT GRAPH ` +
      graph_uri +
      `_publicationinfo ;
		 DELETE  SILENT { ` +
      graph_uri +
      ` tmr:isPartOf ?subguideline } WHERE 
		 { ` +
      graph_uri +
      ` tmr:isPartOf ?subguideline } `;

    let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate;
    const URL =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      dataset_id +
      "/update";

    axios.post(
      URL,
      {
        headers: {
          Authorization:
            "Basic " +
            new Buffer(
              `${config.FUSEKI_USER}:${config.FUSEKI_PASSWORD}`
            ).toString("base64"),
        },
        body: prefixAndSparqlUpdate,
      },

      function (error, response, body) {
        if (!error && response && response.statusCode < 400) {
          callback(null, 200);
        } else {
          logger.error(
            "SPARQL update failed at: " +
              URL +
              "  Query: " +
              prefixAndSparqlUpdate +
              ". Error: " +
              (error ? error : "None") +
              ". Body: " +
              (body ? body : "None") +
              ". Status: " +
              (response && response.statusCode
                ? response.statusCode
                : "No response.") +
              "."
          );
          callback(error, 400);
        }
      }
    );
  }





  /**
   *
   * @param {string} cigFrom original CIG
   * @param {string} cigTo destination CIG
   * @param {string} nanoHead
   * @param {string} nanoAssert
   * @param {string} nanoProv
   * @param {string} nanoPubInfo
   * @param {(Error, number) => number} callback callback function returning status
   */
   addGraphsDataFromToCig(
    cigFrom,
    cigTo,
    nanoHead,
    nanoAssert,
    nanoProv,
    nanoPub,
    callback
  ) {
    let insertGraphsData = ``;
    let graphs;
    let assertGraphs = ``;
    let provGraphs = ``;
    let headGraphs = ``;
    let nanopubGraphs = ``;
    let graphDescrDel = ``;
    let graphDescrIns = ``;

    let deleteTriples;

    const cigFromUrl =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      cigFrom +
      "/query";

    assertGraphs = `\nGRAPH <` + nanoAssert + `> { ?a  ?b ?c } `;
    provGraphs = `\nGRAPH <` + nanoProv + `> { ?d  ?e ?f } `;
    nanopubGraphs = `\nGRAPH <` + nanoPub + `> { ?g ?h ?i } `;
    headGraphs = `\nGRAPH <` + nanoHead + `> { ?j  ?k ?l } `;

    graphDescrDel +=
      `\nGRAPH <` +
      nanoAssert +
      `> { <` +
      nanoAssert +
      `> tmr:partOf data:` +
      cigFrom +
      ` } `;

    graphDescrIns +=
      `\nGRAPH <` +
      nanoAssert +
      `> { <` +
      nanoAssert +
      `> tmr:partOf data:` +
      cigTo +
      ` } `;

    insertGraphsData =
      `\nINSERT {` +
      headGraphs +
      assertGraphs +
      graphDescrIns +
      nanopubGraphs +
      provGraphs +
      `} \nWHERE { SERVICE <` +
      cigFromUrl +
      `> { ` +
      headGraphs +
      assertGraphs +
      nanopubGraphs +
      provGraphs +
      ` } } ; `;

    deleteTriples = `\nDELETE WHERE { ` + graphDescrDel + ` } ; `;

    //////UPDATE GRAPH STORE//////

    let sparqlUpdate = insertGraphsData + deleteTriples;

    let prefixAndSparqlUpdate = guidelines.PREFIXES + "\n" + sparqlUpdate;

    const URL =
      "http://" +
      config.JENA_HOST +
      ":" +
      config.JENA_PORT +
      "/" +
      cigTo +
      "/update";

    try {
      axios.post(
        URL,
        {
          headers: {
            Authorization:
              "Basic " +
              new Buffer("admin:" + config.FUSEKI_PASSWORD).toString("base64"),
          },
          body: prefixAndSparqlUpdate,
        },
        function (error, response, body) {
          if (!error && response && response.statusCode < 400) {
            callback(null, response.statusCode);
          } else {
            logger.error(
              "SPARQL update failed at: " +
                URL +
                " Query: " +
                prefixAndSparqlUpdate +
                ". Error: " +
                (error ? error : "None") +
                ". Body: " +
                (body ? body : "None") +
                ". Status: " +
                (response && response.statusCode
                  ? response.statusCode
                  : "No response.") +
                "."
            );

            callback(error, null);
          }
        }
      );
    } catch (err) {
      callback(error, null);
    }
  }










 

  



 

