// Import required modules
const express = require("express");
const router = express.Router();
const auxFuncts = require("../lib/router_functs/guideline_functs");
const utils = require("../lib/utils");

/**
 * Endpoint to retrieve causation beliefs from named graphs.
 */
router.post("/get", async (req, res) => {
  try {
    // Fetch causation beliefs from the data store
    const {
      status = 500,
      bindings,
      head_vars,
    } = await utils.get_named_subject_in_named_graphs_from_object(
      "beliefs",
      "vocab:CausationBelief"
    );

    if (status < 400) {
      // Process the bindings into a readable format
      const result = await auxFuncts.get_rdf_atom_as_array(bindings);
      return res.status(status).json(result);
    } else {
      // Return an empty array on failure
      return res.status(status).json([]);
    }
  } catch (error) {
    // Log and return a 500 status with error details in case of an exception
    console.error(`Error in /get: ${error.message}`, error);
    return res.status(500).json({ error: "An unexpected error occurred." });
  }
});

module.exports = router;
