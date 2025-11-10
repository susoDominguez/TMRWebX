const logger = require("../config/winston");

function buildContext(req, extra = {}) {
  const context = {
    requestId: req?.requestId,
    ip: req?.ip,
    method: req?.method,
    route: req?.originalUrl,
  };

  if (req && typeof req.get === "function") {
    const userAgent = req.get("User-Agent");
    if (userAgent) {
      context.userAgent = userAgent;
    }
  }

  return { ...context, ...extra };
}

function logStart(req, message, extra = {}) {
  const startTime = Date.now();
  logger.info(message, {
    ...buildContext(req, { stage: "start" }),
    ...extra,
  });
  return startTime;
}

function logSuccess(req, message, startTime, extra = {}) {
  logger.info(message, {
    ...buildContext(req, {
      stage: "complete",
      durationMs: Date.now() - startTime,
    }),
    ...extra,
  });
}

function logWarn(req, message, startTime, extra = {}) {
  logger.warn(message, {
    ...buildContext(req, {
      stage: "warning",
      durationMs: startTime ? Date.now() - startTime : undefined,
    }),
    ...extra,
  });
}

function logError(req, message, startTime, error, extra = {}) {
  logger.error(message, {
    ...buildContext(req, {
      stage: "error",
      durationMs: startTime ? Date.now() - startTime : undefined,
      error: error?.message,
      stack: error?.stack,
    }),
    ...extra,
  });
}

module.exports = {
  buildContext,
  logStart,
  logSuccess,
  logWarn,
  logError,
};
