/**
 * Application Configuration Management
 * Centralized configuration with validation and environment-specific settings
 */

const logger = require("./winston");

class AppConfig {
  constructor() {
    this.loadConfiguration();
    this.validateConfiguration();
  }

  loadConfiguration() {
    // Server Configuration
    this.server = {
      port: parseInt(process.env.PORT) || 8888,
      host: process.env.HOST || "0.0.0.0",
      environment: process.env.NODE_ENV || "development",
      logLevel: process.env.LOG_LEVEL || "info",
    };

    // Database Configuration
    this.database = {
      fuseki: {
        url: process.env.FUSEKI_URL || "http://localhost:3030",
        dataset: process.env.FUSEKI_DATASET || "tmr",
        timeout: parseInt(process.env.DB_TIMEOUT) || 10000,
        retryAttempts: parseInt(process.env.DB_RETRY_ATTEMPTS) || 3,
        poolSize: parseInt(process.env.DB_POOL_SIZE) || 10,
      },
      prolog: {
        url: process.env.PROLOG_URL || "http://localhost:5000",
        timeout: parseInt(process.env.PROLOG_TIMEOUT) || 5000,
      },
    };

    // Security Configuration
    this.security = {
      rateLimiting: {
        windowMs: parseInt(process.env.RATE_LIMIT_WINDOW) || 15 * 60 * 1000, // 15 minutes
        maxRequests: parseInt(process.env.RATE_LIMIT_MAX) || 100,
        skipSuccessfulRequests: process.env.RATE_LIMIT_SKIP_SUCCESS === "true",
      },
      cors: {
        origin: process.env.CORS_ORIGIN || "*",
        credentials: process.env.CORS_CREDENTIALS === "true",
      },
      helmet: {
        contentSecurityPolicy: process.env.CSP_ENABLED !== "false",
        crossOriginEmbedderPolicy: process.env.COEP_ENABLED !== "false",
      },
    };

    // Monitoring Configuration
    this.monitoring = {
      healthCheckInterval: parseInt(process.env.HEALTH_CHECK_INTERVAL) || 30000, // 30 seconds
      metricsEnabled: process.env.METRICS_ENABLED !== "false",
      requestLogging: process.env.REQUEST_LOGGING !== "false",
      performanceTracking: process.env.PERFORMANCE_TRACKING !== "false",
    };

    // Cache Configuration
    this.cache = {
      enabled: process.env.CACHE_ENABLED !== "false",
      ttl: parseInt(process.env.CACHE_TTL) || 300, // 5 minutes
      maxSize: parseInt(process.env.CACHE_MAX_SIZE) || 1000,
      compression: process.env.CACHE_COMPRESSION !== "false",
    };

    // Features Configuration
    this.features = {
      swaggerDocs: process.env.SWAGGER_ENABLED !== "false",
      apiVersioning: process.env.API_VERSIONING === "true",
      requestValidation: process.env.REQUEST_VALIDATION !== "false",
      responseCompression: process.env.RESPONSE_COMPRESSION !== "false",
    };
  }

  validateConfiguration() {
    const errors = [];

    // Validate server configuration
    if (this.server.port < 1 || this.server.port > 65535) {
      errors.push("Invalid port number");
    }

    if (
      !["development", "production", "test"].includes(this.server.environment)
    ) {
      errors.push("Invalid NODE_ENV value");
    }

    // Validate database configuration
    if (!this.database.fuseki.url.startsWith("http")) {
      errors.push("Invalid Fuseki URL");
    }

    if (this.database.fuseki.timeout < 1000) {
      errors.push("Database timeout too low (minimum 1000ms)");
    }

    if (
      this.database.fuseki.poolSize < 1 ||
      this.database.fuseki.poolSize > 50
    ) {
      errors.push("Invalid pool size (must be between 1 and 50)");
    }

    // Validate rate limiting
    if (this.security.rateLimiting.windowMs < 60000) {
      errors.push("Rate limit window too small (minimum 1 minute)");
    }

    if (this.security.rateLimiting.maxRequests < 10) {
      errors.push("Rate limit max requests too low (minimum 10)");
    }

    if (errors.length > 0) {
      logger.error("Configuration validation failed:", errors);
      throw new Error(`Configuration validation failed: ${errors.join(", ")}`);
    }

    logger.info("Configuration validated successfully", {
      environment: this.server.environment,
      port: this.server.port,
      database: this.database.fuseki.url,
      features: Object.keys(this.features).filter((key) => this.features[key]),
    });
  }

  // Get configuration for specific component
  getServerConfig() {
    return { ...this.server };
  }

  getDatabaseConfig() {
    return { ...this.database };
  }

  getSecurityConfig() {
    return { ...this.security };
  }

  getMonitoringConfig() {
    return { ...this.monitoring };
  }

  getCacheConfig() {
    return { ...this.cache };
  }

  getFeaturesConfig() {
    return { ...this.features };
  }

  // Check if feature is enabled
  isFeatureEnabled(featureName) {
    return this.features[featureName] === true;
  }

  // Check if we're in development mode
  isDevelopment() {
    return this.server.environment === "development";
  }

  // Check if we're in production mode
  isProduction() {
    return this.server.environment === "production";
  }

  // Get all configuration as a safe object (without sensitive data)
  getSafeConfig() {
    const safeConfig = {
      server: {
        port: this.server.port,
        environment: this.server.environment,
        logLevel: this.server.logLevel,
      },
      database: {
        fuseki: {
          url: this.database.fuseki.url,
          dataset: this.database.fuseki.dataset,
          timeout: this.database.fuseki.timeout,
          poolSize: this.database.fuseki.poolSize,
        },
      },
      security: {
        rateLimiting: this.security.rateLimiting,
        cors: {
          origin: this.security.cors.origin === "*" ? "*" : "[CONFIGURED]",
        },
      },
      features: this.features,
      monitoring: this.monitoring,
      cache: this.cache,
    };

    return safeConfig;
  }

  // Update configuration at runtime (for non-critical settings)
  updateFeature(featureName, value) {
    if (this.features.hasOwnProperty(featureName)) {
      this.features[featureName] = value;
      logger.info(`Feature ${featureName} ${value ? "enabled" : "disabled"}`);
      return true;
    }
    return false;
  }

  // Get environment-specific configuration
  getEnvironmentConfig() {
    const envConfig = {
      development: {
        logging: {
          level: "debug",
          requests: true,
          errors: true,
          performance: true,
        },
        security: {
          strict: false,
          cors: { origin: "*" },
        },
        features: {
          swaggerDocs: true,
          debugging: true,
        },
      },
      production: {
        logging: {
          level: "info",
          requests: true,
          errors: true,
          performance: false,
        },
        security: {
          strict: true,
          cors: { origin: process.env.CORS_ORIGIN },
        },
        features: {
          swaggerDocs: false,
          debugging: false,
        },
      },
      test: {
        logging: {
          level: "error",
          requests: false,
          errors: true,
          performance: false,
        },
        security: {
          strict: false,
          cors: { origin: "*" },
        },
        features: {
          swaggerDocs: false,
          debugging: true,
        },
      },
    };

    return envConfig[this.server.environment] || envConfig.development;
  }
}

// Create singleton instance
const appConfig = new AppConfig();

module.exports = appConfig;
