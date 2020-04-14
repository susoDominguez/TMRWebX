const createError = require('http-errors');
const express = require('express');
const path = require('path');
const cookieParser = require('cookie-parser');
const morgan = require('morgan');
const logger = require('./config/winston');
//const bodyParser = require('body-parser')

// Environment constiables
require('dotenv').config()

const guidelineRouter = require('./routes/guideline');
const careActionRouter = require('./routes/careAction');
const beliefRouter = require('./routes/belief');
const transitionRouter = require('./routes/transition');
const guidelinesRouter = require('./routes/guidelines');
const careActionsRouter = require('./routes/careActions');
const beliefsRouter = require('./routes/beliefs');
const transitionsRouter = require('./routes/transitions');

const app = express();
const router = express.Router();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

app.use(morgan('combined', { stream: logger.stream }));
app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

router.get('/', function(req, res, next) {

  res.end();

});

router.use('/guideline', guidelineRouter);
router.use('/careAction', careActionRouter);
router.use('/belief', beliefRouter);
router.use('/transition', transitionRouter);
router.use('/guidelines', guidelinesRouter);
router.use('/careActions', careActionsRouter);
router.use('/beliefs', beliefsRouter);
router.use('/transitions', transitionsRouter);

app.use('/tmrweb', router);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  next(createError(404));
});

// error handler
app.use(function(err, req, res, next) {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  logger.error(`${err.status || 500} - ${err.message} - ${req.originalUrl} - ${req.method} - ${req.ip}`);
  
  // render the error page
  res.status(err.status || 500);
  res.render('error');
});

module.exports = app; //DEBUG=drug-interaction-middleware:* npm run devstart
