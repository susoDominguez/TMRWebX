/**
 * Enhanced Error Handling Middleware for TMRWebX
 * Provides centralized error handling, logging, and consistent response formats
 */
const logger = require("../config/winston");
const { ErrorHandler } = require("../lib/errorHandler");

/**
 * Standardized error response format
 */
const sendErrorResponse = (
  res,
  statusCode,
  message,
  details = null,
  requestId = null
) => {
  const errorResponse = {
    status: "error",
    statusCode,
    message,
    timestamp: new Date().toISOString(),
  };

  if (requestId) {
    errorResponse.requestId = requestId;
  }

  if (details && process.env.NODE_ENV === "development") {
    errorResponse.details = details;
  }

  res.status(statusCode).json(errorResponse);
};

/**
 * Async wrapper to catch errors in async route handlers
 * This prevents the need to wrap every async route in try-catch
 */
const asyncHandler = (fn) => (req, res, next) => {
  const requestId =
    req.requestId || `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  req.requestId = requestId;

  Promise.resolve(fn(req, res, next)).catch(next);
};

/**
 * Enhanced validation error handler
 */
const handleValidationError = (err, res, requestId) => {
  if (err.name === "ValidationError") {
    const errors = Object.values(err.errors).map((e) => e.message);
    sendErrorResponse(res, 400, "Validation Error", errors, requestId);
    return true;
  }

  // Express-validator errors
  if (err.array && typeof err.array === "function") {
    const errors = err.array();
    sendErrorResponse(res, 400, "Validation failed", errors, requestId);
    return true;
  }

  return false;
};

/**
 * Database and network error handler
 */
const handleDatabaseError = (err, res, requestId) => {
  // Connection errors
  if (err.code === "ECONNREFUSED") {
    sendErrorResponse(res, 503, "Database connection failed", null, requestId);
    return true;
  }

  if (err.code === "ETIMEDOUT" || err.code === "ECONNABORTED") {
    sendErrorResponse(res, 504, "Database request timeout", null, requestId);
    return true;
  }

  // SPARQL-specific errors
  if (err.message && err.message.includes("SPARQL")) {
    sendErrorResponse(res, 400, "SPARQL query error", err.message, requestId);
    return true;
  }

  // Jena Fuseki errors
  if (err.message && err.message.includes("Jena Fuseki")) {
    sendErrorResponse(
      res,
      503,
      "Triple store service unavailable",
      null,
      requestId
    );
    return true;
  }

  return false;
};

/**
 * Rate limiting error handler
 */
const handleRateLimitError = (err, res, requestId) => {
  if (err.statusCode === 429 || err.status === 429) {
    sendErrorResponse(
      res,
      429,
      "Too many requests, please try again later",
      null,
      requestId
    );
    return true;
  }
  return false;
};

/**
 * Authentication and authorization error handler
 */
const handleAuthError = (err, res, requestId) => {
  if (err.statusCode === 401 || err.status === 401) {
    sendErrorResponse(res, 401, "Authentication required", null, requestId);
    return true;
  }

  if (err.statusCode === 403 || err.status === 403) {
    sendErrorResponse(res, 403, "Access forbidden", null, requestId);
    return true;
  }

  return false;
};

/**
 * Main error handling middleware
 * This should be the last middleware in your app
 */
const errorHandler = (err, req, res, next) => {
  const requestId =
    req.requestId || `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

  // Log the error with context
  logger.error(
    `Error ${err.statusCode || err.status || 500} - ${err.message}`,
    {
      requestId,
      error: {
        name: err.name,
        message: err.message,
        stack: err.stack,
        statusCode: err.statusCode || err.status,
      },
      request: {
        url: req.originalUrl,
        method: req.method,
        ip: req.ip,
        userAgent: req.get("User-Agent"),
        headers:
          process.env.NODE_ENV === "development" ? req.headers : undefined,
      },
      body: process.env.NODE_ENV === "development" ? req.body : undefined,
      params: req.params,
      query: req.query,
    }
  );

  // Handle custom ErrorHandler instances
  if (err instanceof ErrorHandler) {
    return sendErrorResponse(res, err.statusCode, err.message, null, requestId);
  }

  // Handle specific error types
  if (handleValidationError(err, res, requestId)) return;
  if (handleDatabaseError(err, res, requestId)) return;
  if (handleRateLimitError(err, res, requestId)) return;
  if (handleAuthError(err, res, requestId)) return;

  // Handle HTTP errors
  const statusCode = err.statusCode || err.status || 500;
  const message = err.message || "Internal Server Error";

  // Don't expose internal errors in production
  const publicMessage =
    statusCode >= 500 && process.env.NODE_ENV === "production"
      ? "Internal Server Error"
      : message;

  sendErrorResponse(res, statusCode, publicMessage, err.stack, requestId);
};

/**
 * 404 Not Found handler
 * This should be used before the main error handler
 */
const notFoundHandler = (req, res, next) => {
  const requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  req.requestId = requestId;

  logger.warn(`404 - Endpoint not found: ${req.originalUrl}`, {
    requestId,
    method: req.method,
    url: req.originalUrl,
    ip: req.ip,
    userAgent: req.get("User-Agent"),
  });

  sendErrorResponse(
    res,
    404,
    `Endpoint not found: ${req.method} ${req.originalUrl}`,
    null,
    requestId
  );
};

/**
 * Request ID middleware
 * Adds a unique request ID to every request for tracking
 */
const requestIdMiddleware = (req, res, next) => {
  req.requestId = `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  res.setHeader("X-Request-ID", req.requestId);
  next();
};

/**
 * Error logging middleware
 * Logs successful requests for monitoring
 */
const requestLogger = (req, res, next) => {
  const startTime = Date.now();

  // Override res.end to log response
  const originalEnd = res.end;
  res.end = function (...args) {
    const duration = Date.now() - startTime;

    // Only log if response was successful
    if (res.statusCode < 400) {
      logger.info(`${req.method} ${req.originalUrl} - ${res.statusCode}`, {
        requestId: req.requestId,
        method: req.method,
        url: req.originalUrl,
        statusCode: res.statusCode,
        duration,
        ip: req.ip,
        userAgent: req.get("User-Agent"),
      });
    }

    originalEnd.apply(this, args);
  };

  next();
};

/**
 * Health check error handler
 * Special handling for health check endpoints
 */
const healthCheckErrorHandler = (err, req, res, next) => {
  if (req.originalUrl.includes("/health")) {
    const requestId =
      req.requestId ||
      `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

    logger.error("Health check failed", {
      requestId,
      error: err.message,
      service: req.originalUrl,
    });

    return res.status(503).json({
      status: "unhealthy",
      error: err.message,
      timestamp: new Date().toISOString(),
      requestId,
    });
  }

  next(err);
};

module.exports = {
  asyncHandler,
  errorHandler,
  notFoundHandler,
  requestIdMiddleware,
  requestLogger,
  healthCheckErrorHandler,
  sendErrorResponse,
};
