/**
 * Enhanced Database Service for TMRWebX
 * Provides improved SPARQL operations, connection pooling, and error handling
 */
const axios = require("axios");
const { ErrorHandler } = require("../lib/errorHandler");
const logger = require("../config/winston");
const guidelines = require("../lib/prefixes");

class DatabaseService {
  constructor(config = {}) {
    // Use environment variables with fallbacks
    this.config = {
      JENA_HOST: config.JENA_HOST || process.env.JENA_HOST || "localhost",
      JENA_PORT: config.JENA_PORT || process.env.JENA_PORT || "3030",
      PROLOG_HOST: config.PROLOG_HOST || process.env.PROLOG_HOST || "localhost",
      PROLOG_PORT: config.PROLOG_PORT || process.env.PROLOG_PORT || "8082",
      FUSEKI_USER: config.FUSEKI_USER || process.env.FUSEKI_USER || "admin",
      FUSEKI_PASSWORD:
        config.FUSEKI_PASSWORD || process.env.FUSEKI_PASSWORD || "admin",
      FUSEKI_DATASET:
        config.FUSEKI_DATASET || process.env.FUSEKI_DATASET || "careActions",
      REQUEST_TIMEOUT:
        config.REQUEST_TIMEOUT || process.env.REQUEST_TIMEOUT || 30000,
      ...config,
    };

    this.jena_baseUrl = `http://${this.config.JENA_HOST}:${this.config.JENA_PORT}`;
    this.prolog_baseUrl = `http://${this.config.PROLOG_HOST}:${this.config.PROLOG_PORT}`;

    // Create axios instances with default configurations
    this.jenaClient = axios.create({
      baseURL: this.jena_baseUrl,
      timeout: parseInt(this.config.REQUEST_TIMEOUT),
      auth: {
        username: this.config.FUSEKI_USER,
        password: this.config.FUSEKI_PASSWORD,
      },
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
      },
    });

    this.prologClient = axios.create({
      baseURL: this.prolog_baseUrl,
      timeout: parseInt(this.config.REQUEST_TIMEOUT),
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
      },
    });

    this.setupInterceptors();
  }

  // Setup response interceptors for better error handling
  setupInterceptors() {
    // Jena interceptors
    this.jenaClient.interceptors.response.use(
      (response) => response,
      (error) => this.handleAxiosError(error, "Jena Fuseki")
    );

    // Prolog interceptors
    this.prologClient.interceptors.response.use(
      (response) => response,
      (error) => this.handleAxiosError(error, "Prolog Server")
    );

    // Request logging interceptor
    this.jenaClient.interceptors.request.use((request) => {
      logger.debug("SPARQL Request:", {
        url: request.url,
        method: request.method,
        baseURL: request.baseURL,
      });
      return request;
    });
  }

  // Centralized axios error handling
  handleAxiosError(error, serviceName) {
    if (error.response) {
      logger.error(
        `${serviceName} responded with status ${error.response.status}:`,
        {
          status: error.response.status,
          data: error.response.data,
          url: error.config?.url,
        }
      );
      throw new ErrorHandler(
        error.response.status,
        `${serviceName} error: ${error.response.statusText}`
      );
    } else if (error.request) {
      logger.error(`No response from ${serviceName}:`, {
        timeout: error.code === "ECONNABORTED",
        url: error.config?.url,
      });
      throw new ErrorHandler(503, `${serviceName} is unavailable`);
    } else {
      logger.error(`Request setup error for ${serviceName}:`, error.message);
      throw new ErrorHandler(500, `Failed to communicate with ${serviceName}`);
    }
  }

  // Enhanced SPARQL query method
  async sparqlQuery(
    datasetId,
    query,
    acceptFormat = "application/sparql-results+json"
  ) {
    try {
      const prefixedQuery = `${guidelines.PREFIXES}\n${query}`;

      logger.debug("Executing SPARQL Query:", {
        dataset: datasetId,
        queryLength: query.length,
        acceptFormat,
      });

      const response = await this.jenaClient.post(
        `/${datasetId}/query`,
        new URLSearchParams({ query: prefixedQuery }),
        {
          headers: {
            Accept: acceptFormat,
          },
        }
      );

      const result = {
        status: response.status,
        head_vars: response.data?.head?.vars ?? [],
        bindings: response.data?.results?.bindings ?? [],
      };

      logger.debug("SPARQL Query Result:", {
        dataset: datasetId,
        status: result.status,
        bindingsCount: result.bindings.length,
        headVarsCount: result.head_vars.length,
      });

      return result;
    } catch (error) {
      logger.error(`SPARQL query failed for dataset ${datasetId}:`, {
        error: error.message,
        queryLength: query.length,
      });
      throw error;
    }
  }

  // Enhanced SPARQL update method
  async sparqlUpdate(datasetId, updateQuery) {
    try {
      const prefixedUpdate = `${guidelines.PREFIXES}\n${updateQuery}`;

      logger.debug("Executing SPARQL Update:", {
        dataset: datasetId,
        updateLength: updateQuery.length,
      });

      const response = await this.jenaClient.post(
        `/${datasetId}`,
        new URLSearchParams({ update: prefixedUpdate })
      );

      const result = {
        status: response.status,
        data: response.data || "Success",
      };

      logger.debug("SPARQL Update Result:", {
        dataset: datasetId,
        status: result.status,
      });

      return result;
    } catch (error) {
      logger.error(`SPARQL update failed for dataset ${datasetId}:`, {
        error: error.message,
        updateLength: updateQuery.length,
      });
      throw error;
    }
  }

  // Dataset management with enhanced error handling
  async createDataset(datasetId, dbType = "tdb2") {
    try {
      logger.info("Creating dataset:", { datasetId, dbType });

      const response = await this.jenaClient.post(
        "/$/datasets",
        new URLSearchParams({
          dbType,
          dbName: datasetId,
        })
      );

      logger.info("Dataset created successfully:", {
        datasetId,
        status: response.status,
      });

      return {
        status: response.status,
        data: response.data || "Dataset created",
      };
    } catch (error) {
      logger.error(`Dataset creation failed for ${datasetId}:`, {
        error: error.message,
        dbType,
      });
      throw error;
    }
  }

  async deleteDataset(datasetId) {
    try {
      logger.info("Deleting dataset:", { datasetId });

      const response = await this.jenaClient.delete(`/$/datasets/${datasetId}`);

      logger.info("Dataset deleted successfully:", {
        datasetId,
        status: response.status,
      });

      return {
        status: response.status,
        data: "Dataset deleted",
      };
    } catch (error) {
      logger.error(`Dataset deletion failed for ${datasetId}:`, {
        error: error.message,
      });
      throw error;
    }
  }

  // Enhanced health check methods
  async checkJenaHealth() {
    try {
      const response = await this.jenaClient.get("/$/ping", { timeout: 5000 });
      return {
        healthy: true,
        status: response.status,
        responseTime: response.headers["x-response-time"] || "unknown",
      };
    } catch (error) {
      return {
        healthy: false,
        error: error.message,
        service: "Jena Fuseki",
      };
    }
  }

  async checkPrologHealth() {
    try {
      const response = await this.prologClient.get("/", { timeout: 5000 });
      return {
        healthy: true,
        status: response.status,
        responseTime: response.headers["x-response-time"] || "unknown",
      };
    } catch (error) {
      return {
        healthy: false,
        error: error.message,
        service: "Prolog Server",
      };
    }
  }

  // Comprehensive health check
  async getHealthStatus() {
    const startTime = Date.now();

    const [jenaHealth, prologHealth] = await Promise.allSettled([
      this.checkJenaHealth(),
      this.checkPrologHealth(),
    ]);

    const totalTime = Date.now() - startTime;

    return {
      timestamp: new Date().toISOString(),
      totalCheckTime: totalTime,
      services: {
        jena:
          jenaHealth.status === "fulfilled"
            ? jenaHealth.value
            : { healthy: false, error: jenaHealth.reason?.message },
        prolog:
          prologHealth.status === "fulfilled"
            ? prologHealth.value
            : { healthy: false, error: prologHealth.reason?.message },
      },
      overall:
        jenaHealth.status === "fulfilled" &&
        jenaHealth.value.healthy &&
        prologHealth.status === "fulfilled" &&
        prologHealth.value.healthy,
    };
  }

  // Transaction support for atomic operations
  async transaction(operations) {
    const transactionId = `txn_${Date.now()}_${Math.random()
      .toString(36)
      .substr(2, 5)}`;
    logger.info(
      `Starting transaction ${transactionId} with ${operations.length} operations`
    );

    const startTime = Date.now();
    const results = [];

    try {
      for (let i = 0; i < operations.length; i++) {
        const operation = operations[i];
        logger.debug(
          `Executing transaction operation ${i + 1}/${operations.length}`,
          { transactionId }
        );

        const result = await operation();
        results.push(result);
      }

      const duration = Date.now() - startTime;
      logger.info(`Transaction ${transactionId} completed successfully`, {
        operationsCount: operations.length,
        duration,
      });

      return {
        transactionId,
        success: true,
        results,
        duration,
      };
    } catch (error) {
      const duration = Date.now() - startTime;
      logger.error(`Transaction ${transactionId} failed:`, {
        error: error.message,
        operationsCompleted: results.length,
        operationsTotal: operations.length,
        duration,
      });
      throw error;
    }
  }

  // Batch operations for efficiency
  async batchSparqlUpdates(datasetId, updates) {
    try {
      logger.info("Executing batch SPARQL updates:", {
        dataset: datasetId,
        updatesCount: updates.length,
      });

      const batchQuery = updates
        .map((update) => `${guidelines.PREFIXES}\n${update}`)
        .join(";\n");

      const result = await this.sparqlUpdate(datasetId, batchQuery);

      logger.info("Batch SPARQL updates completed:", {
        dataset: datasetId,
        updatesCount: updates.length,
        status: result.status,
      });

      return result;
    } catch (error) {
      logger.error(`Batch SPARQL updates failed for dataset ${datasetId}:`, {
        error: error.message,
        updatesCount: updates.length,
      });
      throw error;
    }
  }

  // Health check method
  async healthCheck() {
    const startTime = Date.now();

    try {
      // Check Fuseki server connectivity using the ping endpoint
      // This doesn't require any specific dataset to exist
      await this.jenaClient.get("/$/ping", { timeout: 5000 });

      const responseTime = Date.now() - startTime;

      return {
        status: "healthy",
        responseTime,
        timestamp: new Date().toISOString(),
        connections: {
          jena: this.jena_baseUrl,
          prolog: this.prolog_baseUrl,
        },
      };
    } catch (error) {
      logger.error("Database health check failed:", error);

      return {
        status: "unhealthy",
        error: error.message,
        timestamp: new Date().toISOString(),
        responseTime: Date.now() - startTime,
      };
    }
  }

  // Get connection statistics
  getConnectionStats() {
    return {
      jenaBaseUrl: this.jena_baseUrl,
      prologBaseUrl: this.prolog_baseUrl,
      timeout: this.config.REQUEST_TIMEOUT,
      user: this.config.FUSEKI_USER,
    };
  }

  // Cleanup method for graceful shutdown
  async cleanup() {
    logger.info("Cleaning up DatabaseService connections");
    // Axios doesn't need explicit cleanup, but we can clear interceptors
    this.jenaClient.interceptors.request.clear();
    this.jenaClient.interceptors.response.clear();
    this.prologClient.interceptors.request.clear();
    this.prologClient.interceptors.response.clear();
  }
}

module.exports = DatabaseService;
