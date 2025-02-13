const createError = require("http-errors");
const express = require("express");
const path = require("path");
const cookieParser = require("cookie-parser");
const morgan = require("morgan");
const helmet = require("helmet");
const logger = require("./config/winston");
const { handleError } = require("./lib/errorHandler");
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
];

const app = express();

/**
 * View Engine Setup
 */
app.set("views", path.join(__dirname, "views"));
app.set("view engine", "pug");

/**
 * Middleware Setup
 */
app.use(morgan("combined", { stream: logger.stream }));
app.use(express.json());
app.use(express.urlencoded({ extended: true }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, "public")));
app.use(helmet()); // Secure HTTP headers
// app.use(limiter); // Rate limiter (Uncomment if you plan to use)

// Route Setup: Dynamically load routes
routers.forEach(({ path, router }) => {
  app.use(path, router);
});

// Default Route
app.get("/", (req, res) => res.status(200).send("Welcome to TMR Web API"));

/**
 * Global error handling middleware for custom errors
 */
app.use((err, req, res, next) => {
  // If the error is an instance of ErrorHandler, use handleError
  if (err instanceof ErrorHandler) {
    return handleError(err, res);
  }
  next(err); // Pass to the next error handler
});

// Catch unhandled promise rejections
process.on("unhandledRejection", (reason, promise) => {
  logger.error(`Unhandled Rejection at: ${promise}, reason: ${reason}`);
});

// Catch 404 Errors
app.use((req, res, next) => {
  next(createError(404, "Endpoint not found"));
});

// General Error Handler
app.use((err, req, res, next) => {
  const status = err.status || 500;
  const message = err.message || "Internal Server Error";

  logger.error(
    `${status} - ${message} - ${req.originalUrl} - ${req.method} - ${req.ip}`
  );

  res.locals.message = message;
  res.locals.error = req.app.get("env") === "development" ? err : {};

  res.status(status).json({
    status: "error",
    message,
  });
});

module.exports = app;
