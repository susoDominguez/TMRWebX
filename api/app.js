const createError = require("http-errors");
const express = require("express");
const path = require("path");
const cookieParser = require("cookie-parser");
const morgan = require("morgan");
const logger = require("./config/winston");
const { handleError } = require("./lib/errorHandler.js");
require("dotenv").config(); // Load environment variables

// Import Routers
const guidelineRouter = require("./routes/guideline");
const careActionRouter = require("./routes/careAction");
const beliefRouter = require("./routes/belief");
const statementRouter = require("./routes/statement");
const transitionRouter = require("./routes/transition");
const guidelinesRouter = require("./routes/guidelines");
const careActionsRouter = require("./routes/careActions");
const beliefsRouter = require("./routes/beliefs");
const statementsRouter = require("./routes/statements");
const transitionsRouter = require("./routes/transitions");

const app = express();
const router = express.Router();

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

/**
 * Route Setup
 */
router.get("/", (req, res) => res.status(200).send("Welcome to TMR Web API"));
router.use("/guideline", guidelineRouter);
router.use("/careAction", careActionRouter);
router.use("/belief", beliefRouter);
router.use("/statement", statementRouter);
router.use("/transition", transitionRouter);
router.use("/guidelines", guidelinesRouter);
router.use("/careActions", careActionsRouter);
router.use("/beliefs", beliefsRouter);
router.use("/statements", statementsRouter);
router.use("/transitions", transitionsRouter);

app.use("/tmrweb", router);

/**
 * Global Error Handling
 */
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
