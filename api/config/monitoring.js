/**
 * Application Monitoring and Metrics System
 * Provides performance tracking, health monitoring, and operational metrics
 */

const logger = require("./winston");
const appConfig = require("./appConfig");

class AppMonitoring {
  constructor() {
    this.metrics = {
      requests: {
        total: 0,
        success: 0,
        errors: 0,
        byEndpoint: new Map(),
        byMethod: new Map(),
        byStatusCode: new Map(),
      },
      performance: {
        averageResponseTime: 0,
        responseTimeHistory: [],
        slowestRequests: [],
        fastestRequests: [],
      },
      database: {
        connections: 0,
        queries: 0,
        errors: 0,
        averageQueryTime: 0,
        healthStatus: "unknown",
      },
      system: {
        uptime: Date.now(),
        memoryUsage: {},
        cpuUsage: {},
      },
    };

    this.healthChecks = new Map();
    this.alerts = [];

    if (appConfig.getMonitoringConfig().metricsEnabled) {
      this.startMetricsCollection();
    }
  }

  // Middleware to track request metrics
  requestTrackingMiddleware() {
    return (req, res, next) => {
      const startTime = Date.now();
      req.startTime = startTime;

      // Override res.end to capture response metrics
      const originalEnd = res.end;
      res.end = (...args) => {
        const duration = Date.now() - startTime;
        this.recordRequest(req, res, duration);
        originalEnd.apply(res, args);
      };

      next();
    };
  }

  // Record request metrics
  recordRequest(req, res, duration) {
    const endpoint = `${req.method} ${req.route?.path || req.url}`;
    const statusCode = res.statusCode;
    const isError = statusCode >= 400;

    // Update counters
    this.metrics.requests.total++;
    if (isError) {
      this.metrics.requests.errors++;
    } else {
      this.metrics.requests.success++;
    }

    // Track by endpoint
    const endpointStats = this.metrics.requests.byEndpoint.get(endpoint) || {
      count: 0,
      errors: 0,
      totalTime: 0,
      averageTime: 0,
    };
    endpointStats.count++;
    endpointStats.totalTime += duration;
    endpointStats.averageTime = endpointStats.totalTime / endpointStats.count;
    if (isError) endpointStats.errors++;
    this.metrics.requests.byEndpoint.set(endpoint, endpointStats);

    // Track by method
    const methodStats = this.metrics.requests.byMethod.get(req.method) || {
      count: 0,
      errors: 0,
    };
    methodStats.count++;
    if (isError) methodStats.errors++;
    this.metrics.requests.byMethod.set(req.method, methodStats);

    // Track by status code
    const statusStats = this.metrics.requests.byStatusCode.get(statusCode) || 0;
    this.metrics.requests.byStatusCode.set(statusCode, statusStats + 1);

    // Update performance metrics
    this.updatePerformanceMetrics(endpoint, duration, req, res);

    // Log if enabled
    if (appConfig.getMonitoringConfig().requestLogging) {
      logger.info("Request processed", {
        endpoint,
        method: req.method,
        statusCode,
        duration,
        ip: req.ip,
        userAgent: req.get("User-Agent"),
        requestId: req.requestId,
      });
    }
  }

  // Update performance metrics
  updatePerformanceMetrics(endpoint, duration, req, res) {
    // Update response time history (keep last 100)
    this.metrics.performance.responseTimeHistory.push(duration);
    if (this.metrics.performance.responseTimeHistory.length > 100) {
      this.metrics.performance.responseTimeHistory.shift();
    }

    // Calculate average response time
    const total = this.metrics.performance.responseTimeHistory.reduce(
      (a, b) => a + b,
      0
    );
    this.metrics.performance.averageResponseTime =
      total / this.metrics.performance.responseTimeHistory.length;

    // Track slowest requests (top 10)
    const requestInfo = {
      endpoint,
      method: req.method,
      duration,
      timestamp: new Date().toISOString(),
      requestId: req.requestId,
    };

    this.metrics.performance.slowestRequests.push(requestInfo);
    this.metrics.performance.slowestRequests.sort(
      (a, b) => b.duration - a.duration
    );
    this.metrics.performance.slowestRequests =
      this.metrics.performance.slowestRequests.slice(0, 10);

    // Track fastest requests (top 10)
    this.metrics.performance.fastestRequests.push(requestInfo);
    this.metrics.performance.fastestRequests.sort(
      (a, b) => a.duration - b.duration
    );
    this.metrics.performance.fastestRequests =
      this.metrics.performance.fastestRequests.slice(0, 10);

    // Alert on slow requests
    if (duration > 5000) {
      // 5 seconds
      this.addAlert(
        "slow_request",
        `Slow request detected: ${endpoint} took ${duration}ms`,
        "warning"
      );
    }
  }

  // Record database operation
  recordDatabaseOperation(operation, duration, success = true) {
    this.metrics.database.queries++;
    if (!success) {
      this.metrics.database.errors++;
    }

    // Update average query time
    const totalTime =
      this.metrics.database.averageQueryTime *
        (this.metrics.database.queries - 1) +
      duration;
    this.metrics.database.averageQueryTime =
      totalTime / this.metrics.database.queries;

    logger.debug("Database operation recorded", {
      operation,
      duration,
      success,
      totalQueries: this.metrics.database.queries,
    });
  }

  // Update database health status
  updateDatabaseHealth(status) {
    this.metrics.database.healthStatus = status;
    if (status === "unhealthy") {
      this.addAlert(
        "database_health",
        "Database health check failed",
        "critical"
      );
    }
  }

  // Add health check
  addHealthCheck(name, checkFunction, interval = 30000) {
    const healthCheck = {
      name,
      checkFunction,
      interval,
      lastCheck: null,
      lastResult: null,
      status: "unknown",
    };

    this.healthChecks.set(name, healthCheck);

    // Start periodic health check
    setInterval(async () => {
      try {
        const result = await checkFunction();
        healthCheck.lastCheck = new Date().toISOString();
        healthCheck.lastResult = result;
        healthCheck.status = result.status || "healthy";
      } catch (error) {
        healthCheck.lastCheck = new Date().toISOString();
        healthCheck.lastResult = { status: "unhealthy", error: error.message };
        healthCheck.status = "unhealthy";

        this.addAlert(
          "health_check_failed",
          `Health check ${name} failed: ${error.message}`,
          "warning"
        );
      }
    }, interval);
  }

  // Add alert
  addAlert(type, message, severity = "info") {
    const alert = {
      id: `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
      type,
      message,
      severity,
      timestamp: new Date().toISOString(),
      acknowledged: false,
    };

    this.alerts.unshift(alert);

    // Keep only last 100 alerts
    this.alerts = this.alerts.slice(0, 100);

    logger.warn(`Alert: ${severity.toUpperCase()} - ${message}`, { alert });
  }

  // Start system metrics collection
  startMetricsCollection() {
    setInterval(() => {
      this.collectSystemMetrics();
    }, 60000); // Every minute

    logger.info("Metrics collection started");
  }

  // Collect system metrics
  collectSystemMetrics() {
    // Memory usage
    const memUsage = process.memoryUsage();
    this.metrics.system.memoryUsage = {
      rss: Math.round(memUsage.rss / 1024 / 1024), // MB
      heapTotal: Math.round(memUsage.heapTotal / 1024 / 1024), // MB
      heapUsed: Math.round(memUsage.heapUsed / 1024 / 1024), // MB
      external: Math.round(memUsage.external / 1024 / 1024), // MB
      arrayBuffers: Math.round(memUsage.arrayBuffers / 1024 / 1024), // MB
    };

    // CPU usage (simplified)
    const usage = process.cpuUsage();
    this.metrics.system.cpuUsage = {
      user: usage.user,
      system: usage.system,
    };

    // Alert on high memory usage
    if (this.metrics.system.memoryUsage.heapUsed > 512) {
      // 512MB
      this.addAlert(
        "high_memory",
        `High memory usage: ${this.metrics.system.memoryUsage.heapUsed}MB`,
        "warning"
      );
    }
  }

  // Get comprehensive metrics report
  getMetrics() {
    const uptime = Date.now() - this.metrics.system.uptime;

    return {
      summary: {
        uptime: Math.round(uptime / 1000), // seconds
        totalRequests: this.metrics.requests.total,
        successRate:
          this.metrics.requests.total > 0
            ? Math.round(
                (this.metrics.requests.success / this.metrics.requests.total) *
                  100
              )
            : 0,
        averageResponseTime: Math.round(
          this.metrics.performance.averageResponseTime
        ),
        databaseHealth: this.metrics.database.healthStatus,
      },
      requests: {
        total: this.metrics.requests.total,
        success: this.metrics.requests.success,
        errors: this.metrics.requests.errors,
        byEndpoint: Object.fromEntries(this.metrics.requests.byEndpoint),
        byMethod: Object.fromEntries(this.metrics.requests.byMethod),
        byStatusCode: Object.fromEntries(this.metrics.requests.byStatusCode),
      },
      performance: {
        averageResponseTime: Math.round(
          this.metrics.performance.averageResponseTime
        ),
        slowestRequests: this.metrics.performance.slowestRequests.slice(0, 5),
        fastestRequests: this.metrics.performance.fastestRequests.slice(0, 5),
      },
      database: {
        queries: this.metrics.database.queries,
        errors: this.metrics.database.errors,
        averageQueryTime: Math.round(this.metrics.database.averageQueryTime),
        healthStatus: this.metrics.database.healthStatus,
      },
      system: {
        uptime: Math.round(uptime / 1000),
        memory: this.metrics.system.memoryUsage,
        cpu: this.metrics.system.cpuUsage,
      },
      healthChecks: Object.fromEntries(
        Array.from(this.healthChecks.entries()).map(([name, check]) => [
          name,
          {
            status: check.status,
            lastCheck: check.lastCheck,
            lastResult: check.lastResult,
          },
        ])
      ),
      alerts: this.alerts.slice(0, 10), // Latest 10 alerts
    };
  }

  // Get health status
  getHealthStatus() {
    const healthChecks = Array.from(this.healthChecks.values());
    const unhealthyChecks = healthChecks.filter(
      (check) => check.status === "unhealthy"
    );

    return {
      status: unhealthyChecks.length === 0 ? "healthy" : "unhealthy",
      checks: healthChecks.length,
      unhealthy: unhealthyChecks.length,
      details: Object.fromEntries(
        healthChecks.map((check) => [
          check.name,
          {
            status: check.status,
            lastCheck: check.lastCheck,
          },
        ])
      ),
    };
  }

  // Reset metrics
  resetMetrics() {
    this.metrics.requests = {
      total: 0,
      success: 0,
      errors: 0,
      byEndpoint: new Map(),
      byMethod: new Map(),
      byStatusCode: new Map(),
    };

    this.metrics.performance.responseTimeHistory = [];
    this.metrics.performance.slowestRequests = [];
    this.metrics.performance.fastestRequests = [];

    logger.info("Metrics reset");
  }
}

// Create singleton instance
const appMonitoring = new AppMonitoring();

module.exports = appMonitoring;
