#!/usr/bin/env node

/**
 * Manual Backend Integration Test Script
 * 
 * Run this script to manually test the backend integration
 * Usage: node manual-backend-test.js
 */

const axios = require('axios');
const config = require('../lib/config');

// Configuration
const BACKEND_BASE_URL = `http://${config.PROLOG_HOST || 'localhost'}:${config.PROLOG_PORT || 1234}`;
const FUSEKI_BASE_URL = `http://${config.JENA_HOST || 'localhost'}:${config.JENA_PORT || 3030}`;

// Colors for console output
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m'
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

function logTest(testName) {
  console.log(`\n${colors.cyan}Testing: ${testName}${colors.reset}`);
}

function logSuccess(message) {
  log(`✓ ${message}`, 'green');
}

function logError(message) {
  log(`✗ ${message}`, 'red');
}

function logWarning(message) {
  log(`⚠ ${message}`, 'yellow');
}

async function testServiceHealth() {
  log('\n=== Service Health Checks ===', 'blue');
  
  // Test Backend
  logTest('Backend service connectivity');
  try {
    const response = await axios.get(`${BACKEND_BASE_URL}/`, { timeout: 5000 });
    logSuccess(`Backend is reachable (status: ${response.status})`);
  } catch (error) {
    if (error.code === 'ECONNREFUSED') {
      logError('Backend service is not running');
      logWarning('Start services with: docker-compose up -d');
      return false;
    }
    logError(`Backend error: ${error.message}`);
  }
  
  // Test Fuseki
  logTest('Fuseki service connectivity');
  try {
    const response = await axios.get(`${FUSEKI_BASE_URL}/$/ping`, { timeout: 5000 });
    logSuccess(`Fuseki is reachable (status: ${response.status})`);
  } catch (error) {
    if (error.code === 'ECONNREFUSED') {
      logError('Fuseki service is not running');
      logWarning('Start services with: docker-compose up -d');
      return false;
    }
    logError(`Fuseki error: ${error.message}`);
  }
  
  return true;
}

async function testBackendEndpoints() {
  log('\n=== Backend Endpoint Tests ===', 'blue');
  
  // Test text endpoint
  logTest('Backend /interactions (text response)');
  try {
    const response = await axios.post(
      `${BACKEND_BASE_URL}/interactions`,
      'guideline_id=test-guideline',
      {
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        timeout: 10000,
        validateStatus: () => true
      }
    );
    
    logSuccess(`Endpoint responds (status: ${response.status})`);
    
    if (response.status >= 400) {
      const errorText = typeof response.data === 'string' ? response.data : JSON.stringify(response.data);
      
      // Check for missing predicate errors
      if (errorText.includes('Unknown procedure') && errorText.includes('guideline_recommendations')) {
        logError('Missing predicate: guideline_recommendations/2');
        log('Fix: Add guideline_recommendations/2 to backend/server.pl', 'yellow');
      } else if (errorText.includes('Unknown procedure') && errorText.includes('interaction/5')) {
        logError('Missing predicate: interaction/5');
        log('Fix: Add interaction/5 to backend/server.pl', 'yellow');
      } else if (errorText.includes('Unknown procedure') && errorText.includes('inferInternalInteractions')) {
        logError('Missing module: interaction_rules');
        log('Fix: Add :- consult(current_logic/interaction_rules) to server.pl', 'yellow');
      } else {
        logWarning(`Error response: ${errorText.substring(0, 200)}...`);
      }
    }
  } catch (error) {
    logError(`Request failed: ${error.message}`);
  }
  
  // Test JSON endpoint
  logTest('Backend /v1/interactions (JSON response)');
  try {
    const response = await axios.post(
      `${BACKEND_BASE_URL}/v1/interactions`,
      { guideline_id: 'test-guideline' },
      {
        headers: { 'Content-Type': 'application/json' },
        timeout: 10000,
        validateStatus: () => true
      }
    );
    
    logSuccess(`Endpoint responds (status: ${response.status})`);
    
    if (response.headers['content-type']?.includes('application/json')) {
      logSuccess('Response is JSON format');
      
      if (response.data.status) {
        log(`  Status: ${response.data.status}`, 'cyan');
        
        if (response.data.status === 'success') {
          logSuccess('Backend returned success status');
          
          if (Array.isArray(response.data.interactions)) {
            logSuccess(`Interactions array present (${response.data.interactions.length} interactions)`);
            
            if (response.data.interactions.length > 0) {
              const sample = response.data.interactions[0];
              log('  Sample interaction structure:', 'cyan');
              log(`    - interaction: ${sample.interaction ? '✓' : '✗'}`, 'cyan');
              log(`    - label: ${sample.label ? '✓' : '✗'}`, 'cyan');
              log(`    - elements: ${Array.isArray(sample.elements) ? '✓' : '✗'}`, 'cyan');
              log(`    - external: ${typeof sample.external === 'boolean' ? '✓' : '✗'}`, 'cyan');
            }
          }
        } else if (response.data.status === 'error') {
          logWarning(`Backend returned error: ${response.data.message}`);
          
          // Check for specific errors
          if (response.data.message.includes('Unknown procedure: guideline_recommendations/2')) {
            logError('CRITICAL: guideline_recommendations/2 predicate is missing');
          } else if (response.data.message.includes('Unknown procedure: interaction/5')) {
            logError('CRITICAL: interaction/5 predicate is missing');
          } else if (response.data.message.includes('Unknown procedure: inferInternalInteractions/0')) {
            logError('CRITICAL: interaction_rules module not loaded');
          }
        }
      }
    } else {
      logWarning(`Response is not JSON (Content-Type: ${response.headers['content-type']})`);
    }
  } catch (error) {
    logError(`Request failed: ${error.message}`);
  }
}

async function testPrologPredicates() {
  log('\n=== Prolog Predicate Verification ===', 'blue');
  
  const testCases = [
    {
      name: 'guideline_recommendations/2',
      errorPattern: 'Unknown procedure: guideline_recommendations/2',
      fix: 'Add guideline_recommendations/2 definition to backend/server.pl'
    },
    {
      name: 'interaction/5',
      errorPattern: 'Unknown procedure: interaction/5',
      fix: 'Add interaction/5 definition to backend/server.pl'
    },
    {
      name: 'inferInternalInteractions/0',
      errorPattern: 'Unknown procedure: inferInternalInteractions/0',
      fix: 'Add :- consult(current_logic/interaction_rules) to backend/server.pl'
    },
    {
      name: 'interaction/3 (from interaction_rules)',
      errorPattern: 'Unknown procedure: interaction/3',
      fix: 'Ensure interaction_rules.pl is properly loaded'
    }
  ];
  
  logTest('Checking for missing predicates');
  
  try {
    const response = await axios.post(
      `${BACKEND_BASE_URL}/v1/interactions`,
      { guideline_id: 'test-predicate-check' },
      {
        headers: { 'Content-Type': 'application/json' },
        timeout: 10000,
        validateStatus: () => true
      }
    );
    
    if (response.data.status === 'error') {
      const errorMessage = response.data.message;
      let foundIssues = false;
      
      testCases.forEach(testCase => {
        if (errorMessage.includes(testCase.errorPattern)) {
          logError(`Missing: ${testCase.name}`);
          log(`  Fix: ${testCase.fix}`, 'yellow');
          foundIssues = true;
        }
      });
      
      if (!foundIssues) {
        // Error exists but not related to missing predicates
        logSuccess('All required predicates are loaded');
        logWarning(`Other error occurred: ${errorMessage.substring(0, 100)}...`);
      }
    } else {
      logSuccess('All required predicates are loaded');
    }
  } catch (error) {
    logError(`Predicate check failed: ${error.message}`);
  }
}

async function testRDFLoading() {
  log('\n=== RDF Dataset Loading Tests ===', 'blue');
  
  logTest('TMR ontology loading');
  try {
    const response = await axios.post(
      `${BACKEND_BASE_URL}/v1/interactions`,
      { guideline_id: 'test-ontology' },
      {
        headers: { 'Content-Type': 'application/json' },
        timeout: 15000,
        validateStatus: () => true
      }
    );
    
    if (response.data.status === 'error') {
      const errorMessage = response.data.message;
      
      if (errorMessage.includes('Unknown namespace') || errorMessage.includes('vocab:') || errorMessage.includes('vocab4i:')) {
        logError('TMR ontologies not loaded properly');
        logWarning('Check that tmr_concepts.trig and tmr4i.trig are accessible');
      } else if (errorMessage.includes('Connection refused') || errorMessage.includes('ECONNREFUSED')) {
        logError('Cannot connect to Fuseki for RDF loading');
        logWarning('Ensure Fuseki service is running');
      } else {
        logSuccess('TMR ontologies appear to be loaded (no namespace errors)');
      }
    } else {
      logSuccess('TMR ontologies loaded successfully');
    }
  } catch (error) {
    logError(`RDF loading test failed: ${error.message}`);
  }
}

async function testInteractionDetection() {
  log('\n=== Interaction Detection Logic Tests ===', 'blue');
  
  logTest('Interaction detection workflow');
  try {
    const response = await axios.post(
      `${BACKEND_BASE_URL}/v1/interactions`,
      { guideline_id: 'test-interaction-detection' },
      {
        headers: { 'Content-Type': 'application/json' },
        timeout: 15000,
        validateStatus: () => true
      }
    );
    
    if (response.data.status === 'success') {
      logSuccess('Backend completed interaction detection workflow');
      
      const interactions = response.data.interactions;
      log(`  Found ${interactions.length} interaction(s)`, 'cyan');
      
      // Analyze interaction types
      const interactionTypes = new Set();
      interactions.forEach(i => interactionTypes.add(i.label));
      
      if (interactionTypes.size > 0) {
        log('  Interaction types detected:', 'cyan');
        interactionTypes.forEach(type => log(`    - ${type}`, 'cyan'));
      }
      
      // Check for internal vs external
      const internal = interactions.filter(i => i.external === false).length;
      const external = interactions.filter(i => i.external === true).length;
      log(`  Internal interactions: ${internal}`, 'cyan');
      log(`  External interactions: ${external}`, 'cyan');
      
    } else {
      logWarning('Interaction detection returned error status');
      logWarning(`Message: ${response.data.message}`);
    }
  } catch (error) {
    logError(`Interaction detection test failed: ${error.message}`);
  }
}

async function runAllTests() {
  log('\n' + '='.repeat(60), 'blue');
  log('Backend Integration Manual Test Suite', 'blue');
  log('='.repeat(60), 'blue');
  
  log('\nConfiguration:', 'cyan');
  log(`  Backend URL: ${BACKEND_BASE_URL}`, 'cyan');
  log(`  Fuseki URL: ${FUSEKI_BASE_URL}`, 'cyan');
  
  const servicesRunning = await testServiceHealth();
  
  if (!servicesRunning) {
    log('\n⚠ Services not running. Please start Docker services first.', 'yellow');
    log('Command: docker-compose up -d\n', 'yellow');
    return;
  }
  
  await testBackendEndpoints();
  await testPrologPredicates();
  await testRDFLoading();
  await testInteractionDetection();
  
  log('\n' + '='.repeat(60), 'blue');
  log('Test Suite Complete', 'blue');
  log('='.repeat(60) + '\n', 'blue');
}

// Run tests
runAllTests().catch(error => {
  logError(`\nUnexpected error: ${error.message}`);
  console.error(error);
  process.exit(1);
});
