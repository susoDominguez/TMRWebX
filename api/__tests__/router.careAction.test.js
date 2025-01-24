const request = require("supertest");
const express = require("express");
const router = require("../routes/careAction"); // Update with the correct path
const utils = require("../lib/utils");
const config = require("../lib/config");
const logger = require("../config/winston");

jest.mock("../lib/utils");
jest.mock("../config/winston");

const app = express();
app.use(express.json());
app.use(router);

describe("Guideline Routes Test Suite", () => {
  describe("POST /drug/individual/add", () => {
    it("should add a drug individual care action", async () => {
      utils.sparqlUpdate.mockResolvedValue({ status: 200, data: "Success" });

      const response = await request(app)
        .post("/drug/individual/add")
        .send({ id: "123", label: "DrugLabel" });

      expect(response.status).toBe(200);
      expect(response.body).toBe("Success");
    });

    it("should handle errors gracefully", async () => {
      utils.sparqlUpdate.mockRejectedValue(new Error("SPARQL error"));

      const response = await request(app)
        .post("/drug/individual/add")
        .send({ id: "123", label: "DrugLabel" });

      expect(response.status).toBe(500);
      expect(response.body).toEqual({ error: "Failed to add individual care action" });
    });
  });

  describe("POST /nondrug/individual/add", () => {
    it("should add a non-drug individual care action", async () => {
      utils.sparqlUpdate.mockResolvedValue({ status: 200, data: "Success" });

      const response = await request(app)
        .post("/nondrug/individual/add")
        .send({ id: "456", label: "NonDrugLabel" });

      expect(response.status).toBe(200);
      expect(response.body).toBe("Success");
    });

    it("should handle errors gracefully", async () => {
      utils.sparqlUpdate.mockRejectedValue(new Error("SPARQL error"));

      const response = await request(app)
        .post("/nondrug/individual/add")
        .send({ id: "456", label: "NonDrugLabel" });

      expect(response.status).toBe(500);
      expect(response.body).toEqual({ error: "Failed to add non-drug related care action" });
    });
  });

  describe("POST /drug/individual/delete", () => {
    it("should delete a drug individual care action", async () => {
      utils.sparqlUpdate.mockResolvedValue({ status: 200, data: "Success" });

      const response = await request(app)
        .post("/drug/individual/delete")
        .send({ id: "123" });

      expect(response.status).toBe(200);
      expect(response.body).toBe("Success");
    });

    it("should handle errors gracefully", async () => {
      utils.sparqlUpdate.mockRejectedValue(new Error("SPARQL error"));

      const response = await request(app)
        .post("/drug/individual/delete")
        .send({ id: "123" });

      expect(response.status).toBe(500);
      expect(response.body).toEqual({ error: "Failed to delete individual care action" });
    });
  });

  describe("POST /all/get", () => {
    it("should retrieve all care actions", async () => {
      utils.getCareActionData.mockResolvedValue([{ id: "123", label: "CareAction" }]);
      const mockParsedData = { careActions: [{ id: "123", label: "CareAction" }] };

      jest.spyOn(require("../lib/router_functs/guideline_functs"), "get_care_action_data").mockReturnValue(mockParsedData);

      const response = await request(app).post("/all/get").send({ id: "123", uri: "http://example.com" });

      expect(response.status).toBe(200);
      expect(response.body).toEqual(mockParsedData);
    });

    it("should handle errors gracefully", async () => {
      utils.getCareActionData.mockRejectedValue(new Error("SPARQL error"));

      const response = await request(app).post("/all/get").send({ id: "123", uri: "http://example.com" });

      expect(response.status).toBe(500);
      expect(response.body).toEqual({ error: "Failed to retrieve care action" });
    });
  });
});
