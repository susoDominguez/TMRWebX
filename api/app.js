const createError = require("http-errors");
const express = require("express");
const path = require("path");
const cookieParser = require("cookie-parser");
const morgan = require("morgan");
const helmet = require("helmet");
const logger = require("./config/winston");
const { handleError } = require("./lib/errorHandler");

// Import enhanced middleware and services
const {
  errorHandler,
  notFoundHandler,
  requestIdMiddleware,
  requestLogger,
  healthCheckErrorHandler,
} = require("./middleware/errorMiddleware");
const DatabaseService = require("./services/DatabaseService");

// Import configuration and monitoring
const appConfig = require("./config/appConfig");
const appMonitoring = require("./config/monitoring");

require("dotenv").config(); // Load environment variables

// Import Routers
const routers = [
  { path: "/guideline", router: require("./routes/guideline") },
  { path: "/careAction", router: require("./routes/careAction") },
  { path: "/belief", router: require("./routes/belief") },
  { path: "/statement", router: require("./routes/statement") },
  { path: "/transition", router: require("./routes/transition") },
  { path: "/guidelines", router: require("./routes/guidelines") },
  { path: "/careActions", router: require("./routes/careActions") },
  { path: "/beliefs", router: require("./routes/beliefs") },
  { path: "/statements", router: require("./routes/statements") },
  { path: "/transitions", router: require("./routes/transitions") },
  { path: "/admin", router: require("./routes/admin") }, // Admin routes
];

const app = express();

/**
 * Initialize Database Service with configuration
 */
const dbConfig = appConfig.getDatabaseConfig();
const dbService = new DatabaseService({
  baseUrl: dbConfig.fuseki.url,
  dataset: dbConfig.fuseki.dataset,
  timeout: dbConfig.fuseki.timeout,
  retryAttempts: dbConfig.fuseki.retryAttempts,
  poolSize: dbConfig.fuseki.poolSize,
});

// Make services available to routes
app.locals.db = dbService;
app.locals.config = appConfig;
app.locals.monitoring = appMonitoring;

/**
 * View Engine Setup
 */
app.set("views", path.join(__dirname, "views"));
app.set("view engine", "pug");

/**
 * Middleware Setup
 */
// Enhanced middleware for request tracking and error handling
app.use(requestIdMiddleware); // Add unique request IDs
app.use(appMonitoring.requestTrackingMiddleware()); // Add metrics tracking
app.use(requestLogger); // Enhanced request logging

app.use(morgan("combined", { stream: logger.stream }));
app.use(express.json());
app.use(express.urlencoded({ extended: true }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, "public")));

// Configure helmet with settings from config
const securityConfig = appConfig.getSecurityConfig();
app.use(
  helmet({
    contentSecurityPolicy: securityConfig.helmet.contentSecurityPolicy,
    crossOriginEmbedderPolicy: securityConfig.helmet.crossOriginEmbedderPolicy,
  })
);

// Route Setup: Dynamically load routes
routers.forEach(({ path, router }) => {
  app.use(path, router);
});

// Default Route
app.get("/", (req, res) => res.status(200).send("Welcome to TMR Web API"));

// Enhanced health check with database service status
app.get("/health", async (req, res) => {
  try {
    const dbStatus = await dbService.healthCheck();
    const monitoringHealth = appMonitoring.getHealthStatus();

    const overallStatus =
      dbStatus.status === "healthy" && monitoringHealth.status === "healthy"
        ? "healthy"
        : "degraded";

    res.status(overallStatus === "healthy" ? 200 : 503).json({
      status: overallStatus,
      database: dbStatus,
      monitoring: monitoringHealth,
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  } catch (error) {
    res.status(503).json({
      status: "unhealthy",
      database: { status: "error", error: error.message },
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  }
});

// Database service health endpoint
app.get("/health/database", async (req, res) => {
  try {
    const status = await dbService.healthCheck();
    res.status(status.status === "healthy" ? 200 : 503).json(status);
  } catch (error) {
    res.status(503).json({
      status: "error",
      message: error.message,
      timestamp: new Date().toISOString(),
    });
  }
});

// Monitoring metrics endpoint
app.get("/metrics", async (req, res) => {
  if (!appConfig.getMonitoringConfig().metricsEnabled) {
    return res.status(404).json({
      status: "error",
      message: "Metrics endpoint disabled",
      requestId: req.requestId,
    });
  }

  const metrics = appMonitoring.getMetrics();
  res.json({
    status: "success",
    data: metrics,
    timestamp: new Date().toISOString(),
    requestId: req.requestId,
  });
});
/**
 * Enhanced Error Handling
 */
// Special handling for health check errors
app.use(healthCheckErrorHandler);

// 404 handler with enhanced logging
app.use(notFoundHandler);

// Main error handler with comprehensive error handling
app.use(errorHandler);

/**
 * Process-level error handling
 */
// Initialize health checks
appMonitoring.addHealthCheck(
  "database",
  async () => {
    return await dbService.healthCheck();
  },
  30000
);

appMonitoring.addHealthCheck(
  "memory",
  async () => {
    const memUsage = process.memoryUsage();
    const heapUsedMB = Math.round(memUsage.heapUsed / 1024 / 1024);
    return {
      status: heapUsedMB > 512 ? "warning" : "healthy",
      heapUsed: heapUsedMB,
      threshold: 512,
    };
  },
  60000
);

// Record database operations in monitoring
const originalSparqlQuery = dbService.sparqlQuery.bind(dbService);
dbService.sparqlQuery = async function (...args) {
  const start = Date.now();
  try {
    const result = await originalSparqlQuery(...args);
    appMonitoring.recordDatabaseOperation(
      "sparql_query",
      Date.now() - start,
      true
    );
    return result;
  } catch (error) {
    appMonitoring.recordDatabaseOperation(
      "sparql_query",
      Date.now() - start,
      false
    );
    throw error;
  }
};

// Catch unhandled promise rejections
process.on("unhandledRejection", (reason, promise) => {
  logger.error(`Unhandled Rejection at: ${promise}, reason: ${reason}`);
  appMonitoring.addAlert(
    "unhandled_rejection",
    `Unhandled rejection: ${reason}`,
    "critical"
  );
  process.exit(1); // Gracefully exit
});

// Catch uncaught exceptions
process.on("uncaughtException", (error) => {
  logger.error(`Uncaught Exception: ${error.message}`, { stack: error.stack });
  appMonitoring.addAlert(
    "uncaught_exception",
    `Uncaught exception: ${error.message}`,
    "critical"
  );
  process.exit(1); // Gracefully exit
});

// Graceful shutdown
process.on("SIGTERM", async () => {
  logger.info("SIGTERM received, shutting down gracefully");
  await dbService.cleanup();
  process.exit(0);
});

process.on("SIGINT", async () => {
  logger.info("SIGINT received, shutting down gracefully");
  await dbService.cleanup();
  process.exit(0);
});

module.exports = app;
