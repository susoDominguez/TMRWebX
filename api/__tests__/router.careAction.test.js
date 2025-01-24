// tests/router.test.js

const request = require("supertest");
const express = require("express");
const router = require("../routes/careAction"); // Adjust this to your router path

const app = express();
app.use(express.json());
app.use(router);

// Optional: Set a global timeout for all tests
jest.setTimeout(10000); // Set timeout to 10 seconds

describe("API Tests for care action types creation", () => {

  it("should add a drug individually", async () => {
    const response = await request(app)
      .post("/drug/individual/add")
      .send({
        id: "drug1",
        label: "Aspirin",
        action_label: "Administer Aspirin",
        "subsumed_ids": "Aspirin_subsumed"
        icd10Codes: "A01,B02",
        snomedCodes: "12345,67890"
      });

    expect(response.status).toBe(200);
    expect(response.body).toHaveProperty("data");
  }, 10000); // Increase timeout for this specific test

  it("should add a vaccine individually", async () => {
    const response = await request(app)
      .post("/vaccine/individual/add")
      .send({
        id: "vaccine1",
        label: "COVID-19 Vaccine",
        action_label: "Administer COVID-19 Vaccine"
      });

    expect(response.status).toBe(200);
    expect(response.body).toHaveProperty("data");
  }, 10000); // Increase timeout for this specific test

  it("should delete a drug individually", async () => {
    const response = await request(app)
      .post("/drug/individual/delete")
      .send({
        id: "drug1"
      });

    expect(response.status).toBe(200);
    expect(response.body).toHaveProperty("data");
  }, 10000); // Increase timeout for this specific test

  it("should delete a vaccine individually", async () => {
    const response = await request(app)
      .post("/vaccine/individual/delete")
      .send({
        id: "vaccine1"
      });

    expect(response.status).toBe(200);
    expect(response.body).toHaveProperty("data");
  }, 10000); // Increase timeout for this specific test

  it("should return 406 for missing parameters in /all/get", async () => {
    const response = await request(app)
      .post("/all/get/")
      .send({}); // No parameters sent

    expect(response.status).toBe(406);
  }, 10000); // Increase timeout for this specific test

  // Optionally, you can add cleanup steps after all tests
  afterAll(async () => {
    // If you're using a database, close the connection
    // await db.close();
    // Optionally close the app if needed
  });
  
  // Add more tests for other routes and scenarios as needed
});
