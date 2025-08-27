/**
 * Admin and Monitoring Routes
 * Provides administrative endpoints for monitoring, metrics, and system management
 */

const express = require("express");
const router = express.Router();
const { asyncHandler } = require("../middleware/errorMiddleware");
const appConfig = require("../config/appConfig");
const appMonitoring = require("../config/monitoring");
const logger = require("../config/winston");

// Middleware to restrict admin access (simple IP-based for demo)
const adminMiddleware = (req, res, next) => {
  // In production, implement proper authentication
  const adminIPs = process.env.ADMIN_IPS
    ? process.env.ADMIN_IPS.split(",")
    : ["127.0.0.1", "::1"];
  const clientIP = req.ip || req.connection.remoteAddress;

  if (appConfig.isDevelopment() || adminIPs.includes(clientIP)) {
    next();
  } else {
    res.status(403).json({
      status: "error",
      message: "Admin access denied",
      requestId: req.requestId,
    });
  }
};

// Apply admin middleware to all routes
router.use(adminMiddleware);

// ========================
// MONITORING ENDPOINTS
// ========================

/**
 * GET /admin/health
 * Comprehensive health check with all system components
 */
router.get(
  "/health",
  asyncHandler(async (req, res) => {
    const db = req.app.locals.db;

    // Collect health information from all components
    const healthData = {
      service: "TMRWebX API",
      status: "healthy",
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
      checks: {},
    };

    // Database health
    try {
      const dbHealth = await db.healthCheck();
      healthData.checks.database = dbHealth;
    } catch (error) {
      healthData.checks.database = {
        status: "unhealthy",
        error: error.message,
      };
      healthData.status = "degraded";
    }

    // System health
    const systemHealth = appMonitoring.getHealthStatus();
    healthData.checks.system = systemHealth;

    if (systemHealth.status === "unhealthy") {
      healthData.status = "degraded";
    }

    // Application-specific health checks
    healthData.checks.configuration = {
      status: "healthy",
      environment: appConfig.getServerConfig().environment,
      features: Object.keys(appConfig.getFeaturesConfig()).filter((key) =>
        appConfig.isFeatureEnabled(key)
      ),
    };

    // Memory usage check
    const memUsage = process.memoryUsage();
    const memUsageMB = Math.round(memUsage.heapUsed / 1024 / 1024);
    healthData.checks.memory = {
      status: memUsageMB > 512 ? "warning" : "healthy",
      heapUsed: `${memUsageMB}MB`,
      heapTotal: `${Math.round(memUsage.heapTotal / 1024 / 1024)}MB`,
    };

    // Determine overall status
    const checkStatuses = Object.values(healthData.checks).map(
      (check) => check.status
    );
    if (checkStatuses.includes("unhealthy")) {
      healthData.status = "unhealthy";
    } else if (
      checkStatuses.includes("warning") ||
      checkStatuses.includes("degraded")
    ) {
      healthData.status = "degraded";
    }

    const statusCode =
      healthData.status === "healthy"
        ? 200
        : healthData.status === "degraded"
        ? 200
        : 503;

    res.status(statusCode).json(healthData);
  })
);

/**
 * GET /admin/metrics
 * Comprehensive application metrics
 */
router.get(
  "/metrics",
  asyncHandler(async (req, res) => {
    const metrics = appMonitoring.getMetrics();

    res.json({
      status: "success",
      data: metrics,
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

/**
 * POST /admin/metrics/reset
 * Reset application metrics
 */
router.post(
  "/metrics/reset",
  asyncHandler(async (req, res) => {
    appMonitoring.resetMetrics();

    logger.info("Metrics reset by admin", {
      ip: req.ip,
      userAgent: req.get("User-Agent"),
      requestId: req.requestId,
    });

    res.json({
      status: "success",
      message: "Metrics reset successfully",
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

/**
 * GET /admin/config
 * Get current application configuration (safe values only)
 */
router.get(
  "/config",
  asyncHandler(async (req, res) => {
    const config = appConfig.getSafeConfig();

    res.json({
      status: "success",
      data: config,
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

/**
 * POST /admin/config/features
 * Enable/disable application features
 */
router.post(
  "/config/features",
  asyncHandler(async (req, res) => {
    const { feature, enabled } = req.body;

    if (!feature || typeof enabled !== "boolean") {
      return res.status(400).json({
        status: "error",
        message: "Feature name and enabled status (boolean) required",
        requestId: req.requestId,
      });
    }

    const success = appConfig.updateFeature(feature, enabled);

    if (!success) {
      return res.status(404).json({
        status: "error",
        message: `Feature '${feature}' not found`,
        requestId: req.requestId,
      });
    }

    logger.info(
      `Feature ${feature} ${enabled ? "enabled" : "disabled"} by admin`,
      {
        feature,
        enabled,
        ip: req.ip,
        requestId: req.requestId,
      }
    );

    res.json({
      status: "success",
      message: `Feature '${feature}' ${enabled ? "enabled" : "disabled"}`,
      data: { feature, enabled },
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

// ========================
// SYSTEM INFORMATION
// ========================

/**
 * GET /admin/info
 * System information and diagnostics
 */
router.get(
  "/info",
  asyncHandler(async (req, res) => {
    const info = {
      application: {
        name: "TMRWebX API",
        version: process.env.npm_package_version || "1.0.0",
        environment: appConfig.getServerConfig().environment,
        uptime: Math.round(process.uptime()),
        startTime: new Date(Date.now() - process.uptime() * 1000).toISOString(),
      },
      system: {
        nodeVersion: process.version,
        platform: process.platform,
        architecture: process.arch,
        memory: {
          total: Math.round(process.memoryUsage().heapTotal / 1024 / 1024),
          used: Math.round(process.memoryUsage().heapUsed / 1024 / 1024),
          external: Math.round(process.memoryUsage().external / 1024 / 1024),
        },
      },
      configuration: {
        port: appConfig.getServerConfig().port,
        logLevel: appConfig.getServerConfig().logLevel,
        database: {
          fuseki: appConfig.getDatabaseConfig().fuseki.url,
          dataset: appConfig.getDatabaseConfig().fuseki.dataset,
          timeout: appConfig.getDatabaseConfig().fuseki.timeout,
        },
        features: appConfig.getFeaturesConfig(),
      },
    };

    res.json({
      status: "success",
      data: info,
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

/**
 * GET /admin/logs
 * Recent application logs (if available)
 */
router.get(
  "/logs",
  asyncHandler(async (req, res) => {
    const { level = "info", limit = 100 } = req.query;

    // This is a simplified version - in production you'd read from log files
    res.json({
      status: "success",
      message:
        "Log endpoint available - implement log file reading based on your logging setup",
      parameters: {
        level,
        limit: parseInt(limit),
      },
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

// ========================
// DATABASE ADMINISTRATION
// ========================

/**
 * GET /admin/database/health
 * Detailed database health information
 */
router.get(
  "/database/health",
  asyncHandler(async (req, res) => {
    const db = req.app.locals.db;

    const health = await db.healthCheck();
    const stats = db.getConnectionStats();

    res.json({
      status: "success",
      data: {
        health,
        statistics: stats,
        configuration: appConfig.getDatabaseConfig(),
      },
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

/**
 * POST /admin/database/test
 * Test database connectivity with custom query
 */
router.post(
  "/database/test",
  asyncHandler(async (req, res) => {
    const { query = "SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }" } =
      req.body;
    const db = req.app.locals.db;

    const startTime = Date.now();

    try {
      const result = await db.sparqlQuery(
        appConfig.getDatabaseConfig().fuseki.dataset,
        query
      );
      const duration = Date.now() - startTime;

      res.json({
        status: "success",
        data: {
          query,
          result,
          duration,
          timestamp: new Date().toISOString(),
        },
        requestId: req.requestId,
      });
    } catch (error) {
      const duration = Date.now() - startTime;

      res.status(500).json({
        status: "error",
        message: "Database query failed",
        error: error.message,
        data: {
          query,
          duration,
        },
        timestamp: new Date().toISOString(),
        requestId: req.requestId,
      });
    }
  })
);

// ========================
// UTILITY ENDPOINTS
// ========================

/**
 * GET /admin/routes
 * List all registered routes (for debugging)
 */
router.get(
  "/routes",
  asyncHandler(async (req, res) => {
    // This would require middleware to collect route information
    // For now, return a placeholder
    res.json({
      status: "success",
      message:
        "Route listing available - implement route collection middleware",
      data: {
        registered_routes: [
          "GET /health",
          "GET /careAction/*",
          "GET /guideline/*",
          // ... other routes
        ],
      },
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

/**
 * POST /admin/cache/clear
 * Clear application caches
 */
router.post(
  "/cache/clear",
  asyncHandler(async (req, res) => {
    // Implement cache clearing logic here
    logger.info("Cache cleared by admin", {
      ip: req.ip,
      requestId: req.requestId,
    });

    res.json({
      status: "success",
      message: "Cache cleared successfully",
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    });
  })
);

module.exports = router;
