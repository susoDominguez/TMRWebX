# Backend Integration Testing Guide

## Overview
This directory contains tests for verifying the integration between the Node.js API and the Prolog backend service.

## Test Files

### 1. `backend-integration.test.js`
Full Jest test suite for automated testing. Includes:
- Service health checks (Backend, Fuseki, API)
- Backend endpoint validation
- Prolog predicate verification
- TMR interaction detection logic
- RDF dataset loading verification

### 2. `manual-backend-test.js`
Manual test script that can be run directly without a test framework. Provides:
- Color-coded console output
- Detailed error diagnostics
- Step-by-step verification
- Helpful fix suggestions

## Prerequisites

1. **Services Running**: Start all Docker services
   ```bash
   docker-compose up -d
   ```

2. **Check Service Status**:
   ```bash
   docker ps
   ```
   
   You should see:
   - `reasoner_service` (Prolog backend on port 1234)
   - `interaction_service` (Node.js API on port 8888)
   - `store_service` (Fuseki on port 3030)

## Running Tests

### Option 1: Manual Test Script (Recommended for First-Time Testing)
```bash
cd api
node tests/manual-backend-test.js
```

This will:
- Check service connectivity
- Verify backend endpoints
- Test Prolog predicates
- Validate RDF loading
- Test interaction detection

### Option 2: Jest Test Suite
First, install Jest if not already installed:
```bash
cd api
npm install --save-dev jest
```

Then run tests:
```bash
npm test
```

Or run with verbose output:
```bash
npm test -- --verbose
```

## What the Tests Verify

### 1. Service Health
- ✓ Backend Prolog service is reachable
- ✓ Fuseki RDF store is reachable
- ✓ API service is reachable

### 2. Backend Endpoints
- ✓ `/interactions` endpoint (text response)
- ✓ `/v1/interactions` endpoint (JSON response)
- ✓ Proper error handling

### 3. Prolog Predicates
- ✓ `guideline_recommendations/2` is defined
- ✓ `interaction/5` is defined
- ✓ `inferInternalInteractions/0` from interaction_rules is loaded
- ✓ Core TMR modules (core_rules, norms_rules, interaction_rules) are consulted

### 4. RDF Dataset Loading
- ✓ Backend connects to Fuseki
- ✓ TMR base ontologies (tmr_concepts.trig, tmr4i.trig) are loaded
- ✓ Vocabulary namespaces (vocab:, vocab4i:) are recognized

### 5. Interaction Detection
- ✓ TMR logic detects interactions
- ✓ Returns properly structured JSON
- ✓ Includes interaction fields: interaction, label, elements, external
- ✓ Distinguishes internal vs external interactions

## Expected Test Output

### Success (All Green):
```
=== Service Health Checks ===
✓ Backend is reachable (status: 200)
✓ Fuseki is reachable (status: 200)

=== Backend Endpoint Tests ===
✓ Endpoint responds (status: 200)
✓ Response is JSON format
✓ Backend returned success status
✓ Interactions array present (0 interactions)

=== Prolog Predicate Verification ===
✓ All required predicates are loaded

=== RDF Dataset Loading Tests ===
✓ TMR ontologies loaded successfully

=== Interaction Detection Logic Tests ===
✓ Backend completed interaction detection workflow
```

### Common Errors and Fixes

#### Error: "Backend service is not running"
**Fix**: Start Docker services
```bash
docker-compose up -d
```

#### Error: "Missing predicate: guideline_recommendations/2"
**Fix**: The predicate should be defined in `backend/server.pl`. Check that the file contains:
```prolog
guideline_recommendations(GuidelineUri, Recommendations) :-
    findall(
        Norm,
        (
            rdfs_individual_of(GuidelineUri, vocab:'ClinicalGuideline'),
            rdf(Norm, vocab:partOf, GuidelineUri),
            rdfs_individual_of(Norm, vocab:'ClinicalRecommendation')
        ),
        Recommendations
    ).
```

#### Error: "Missing predicate: interaction/5"
**Fix**: The predicate should be defined in `backend/server.pl`. Check that the file contains:
```prolog
interaction(Recommendations, Interaction, Label, Elements, External) :-
    interaction(Interaction, Label, Elements),
    member(Element, Elements),
    member(Element, Recommendations),
    (   forall(member(E, Elements), member(E, Recommendations))
    ->  External = false
    ;   External = true
    ).
```

#### Error: "Missing module: interaction_rules"
**Fix**: Check that `backend/server.pl` includes:
```prolog
:- consult(current_logic/core_rules).
:- consult(current_logic/norms_rules).
:- consult(current_logic/interaction_rules).
```

#### Error: "TMR ontologies not loaded"
**Fix**: Check that TMR files exist:
```bash
ls -la backend/current_logic/tmr_concepts.trig
ls -la backend/current_logic/tmr4i.trig
```

## Integration with CI/CD

To integrate these tests into a CI/CD pipeline:

1. Update `package.json` with test script (see below)
2. Ensure Docker services start before tests
3. Set appropriate timeouts for container startup
4. Use environment variables for service URLs

## Troubleshooting

### Tests Timeout
Increase timeout in test configuration:
```javascript
const TEST_TIMEOUT = 60000; // 60 seconds
```

### Connection Refused Errors
1. Check Docker services are running: `docker ps`
2. Check service logs: `docker logs reasoner_service`
3. Verify network connectivity: `docker network inspect cig_handler`

### Backend Returns Empty Interactions
This is normal if no test data exists in Fuseki. The tests verify:
- Backend responds correctly (✓)
- JSON structure is valid (✓)
- No predicate errors occur (✓)

To test with actual data:
1. Create a guideline via API
2. Add recommendations with conflicts
3. Query for interactions

## Additional Resources

- **Backend server code**: `backend/server.pl`
- **Interaction rules**: `backend/current_logic/interaction_rules.pl`
- **TMR norms**: `backend/current_logic/norms_rules.pl`
- **API utils**: `api/lib/utils.js`
- **Docker config**: `docker-compose.yml`
