# Integration Guide: Enhanced Database Service and Error Middleware

This guide explains how to integrate the newly created `DatabaseService` and `errorMiddleware` with your existing enhanced routes.

## Overview

The integration provides:

1. **Enhanced Database Service** (`/api/services/DatabaseService.js`)

   - Connection pooling and management
   - Health monitoring and circuit breaker
   - Batch operations and transactions
   - Comprehensive error handling
   - Performance metrics

2. **Centralized Error Middleware** (`/api/middleware/errorMiddleware.js`)

   - Consistent error responses
   - Request tracking with unique IDs
   - Enhanced logging
   - Specialized error handlers for different scenarios

3. **Updated App Configuration** (`/api/app.js`)
   - Integrated service initialization
   - Enhanced health checks
   - Graceful shutdown handling

## Using DatabaseService in Your Routes

### Basic Usage

```javascript
// In any route file (e.g., /api/routes/guideline.js)
const { asyncHandler } = require("../middleware/errorMiddleware");

// Access the database service
router.get(
  "/example",
  asyncHandler(async (req, res) => {
    const db = req.app.locals.db; // Database service instance

    // Simple query
    const query = `
    SELECT ?subject ?predicate ?object WHERE {
      ?subject ?predicate ?object
    } LIMIT 10
  `;

    const results = await db.query(query);
    res.json(results);
  })
);
```

### Advanced Operations

```javascript
// Batch operations
router.post(
  "/bulk-create",
  asyncHandler(async (req, res) => {
    const db = req.app.locals.db;
    const { guidelines } = req.body;

    const operations = guidelines.map((guideline) => ({
      query: `
      INSERT DATA {
        <${guideline.uri}> a tmr:ClinicalGuideline ;
                           rdfs:label "${guideline.label}" ;
                           tmr:hasRecommendation "${guideline.recommendation}" .
      }
    `,
      type: "update",
    }));

    const results = await db.batchOperation(operations);
    res.json({ created: results.length, results });
  })
);

// Using transactions
router.put(
  "/complex-update/:id",
  asyncHandler(async (req, res) => {
    const db = req.app.locals.db;
    const { id } = req.params;
    const { updates } = req.body;

    const transaction = await db.beginTransaction();

    try {
      // Multiple related updates
      for (const update of updates) {
        await transaction.update(update.query);
      }

      await transaction.commit();
      res.json({ message: "Update completed successfully" });
    } catch (error) {
      await transaction.rollback();
      throw error; // Will be handled by errorMiddleware
    }
  })
);
```

## Error Handling Integration

### Using asyncHandler

Wrap all your async route handlers with `asyncHandler` to automatically catch and forward errors:

```javascript
const { asyncHandler } = require("../middleware/errorMiddleware");

// Before (manual error handling)
router.get("/old-way", async (req, res) => {
  try {
    const results = await someAsyncOperation();
    res.json(results);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// After (automatic error handling)
router.get(
  "/new-way",
  asyncHandler(async (req, res) => {
    const results = await someAsyncOperation();
    res.json(results); // Errors automatically caught and handled
  })
);
```

### Custom Error Responses

```javascript
const { sendErrorResponse } = require("../middleware/errorMiddleware");

router.get(
  "/custom-error",
  asyncHandler(async (req, res) => {
    const { id } = req.params;

    if (!id) {
      return sendErrorResponse(
        res,
        400,
        "ID parameter is required",
        null,
        req.requestId
      );
    }

    const result = await db.query(`SELECT * WHERE { <${id}> ?p ?o }`);

    if (result.results.bindings.length === 0) {
      return sendErrorResponse(
        res,
        404,
        "Resource not found",
        null,
        req.requestId
      );
    }

    res.json(result);
  })
);
```

## Health Monitoring

### Database Health Checks

The DatabaseService provides health monitoring:

```javascript
// Check database health
const health = await db.healthCheck();
console.log(health);
// Output:
// {
//   status: 'healthy',
//   responseTime: 45,
//   timestamp: '2024-01-15T10:30:00.000Z',
//   connections: { active: 3, total: 10 }
// }
```

### API Health Endpoints

- `GET /health` - Overall application health including database
- `GET /health/database` - Specific database health status

## Configuration

### Environment Variables

Add these to your `.env` file:

```env
# Database Configuration
FUSEKI_URL=http://localhost:3030
FUSEKI_DATASET=tmr
DB_TIMEOUT=10000
DB_RETRY_ATTEMPTS=3
DB_POOL_SIZE=10

# Error Handling
NODE_ENV=development
LOG_LEVEL=info
```

### App Configuration

The `app.js` file now includes:

1. **Database Service Initialization**: Automatic setup with connection pooling
2. **Request Tracking**: Every request gets a unique ID for tracing
3. **Enhanced Health Checks**: Database status included in health endpoints
4. **Graceful Shutdown**: Proper cleanup of connections on app termination

## Migration Guide

### Updating Existing Routes

1. **Add asyncHandler**: Wrap async route handlers
2. **Use app.locals.db**: Replace direct SPARQL calls with DatabaseService
3. **Remove manual error handling**: Let errorMiddleware handle errors
4. **Add request logging**: Use the built-in request tracking

### Example Migration

**Before:**

```javascript
router.get("/guideline/:id", async (req, res) => {
  try {
    const query = `SELECT * WHERE { <${req.params.id}> ?p ?o }`;
    const response = await axios.post(
      `${config.fuseki.url}/query`,
      querystring.stringify({ query }),
      {
        headers: { "Content-Type": "application/x-www-form-urlencoded" },
      }
    );
    res.json(response.data);
  } catch (error) {
    logger.error("Query failed:", error);
    res.status(500).json({ error: "Internal server error" });
  }
});
```

**After:**

```javascript
router.get(
  "/guideline/:id",
  asyncHandler(async (req, res) => {
    const db = req.app.locals.db;
    const query = `SELECT * WHERE { <${req.params.id}> ?p ?o }`;
    const results = await db.query(query);
    res.json(results);
  })
);
```

## Benefits

1. **Better Error Handling**: Consistent error responses with proper logging
2. **Connection Management**: Efficient database connection pooling
3. **Monitoring**: Built-in health checks and performance metrics
4. **Reliability**: Circuit breaker pattern for database failures
5. **Debugging**: Request tracing with unique IDs
6. **Scalability**: Connection pooling and batch operations

## Next Steps

1. **Update Routes**: Gradually migrate your enhanced routes to use the new service
2. **Add Monitoring**: Set up alerts based on health check endpoints
3. **Performance Tuning**: Adjust pool sizes and timeouts based on usage
4. **Testing**: Test error scenarios and failover behavior

The integration maintains full compatibility with your existing enhanced routes while providing additional capabilities for better reliability and monitoring.
