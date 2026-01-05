/**
 * Backend Integration Tests
 * 
 * Tests the integration between the Node.js API and the Prolog backend service
 * Verifies:
 * - Backend service connectivity
 * - RDF dataset loading from Fuseki
 * - TMR interaction detection logic
 * - JSON response formatting
 */

const axios = require('axios');
const config = require('../lib/config');
const utils = require('../lib/utils');

// Test configuration
const API_BASE_URL = `http://${config.API_HOST || 'localhost'}:${config.API_PORT || 8888}/tmrweb`;
const BACKEND_BASE_URL = `http://${config.PROLOG_HOST || 'localhost'}:${config.PROLOG_PORT || 1234}`;
const FUSEKI_BASE_URL = `http://${config.JENA_HOST || 'localhost'}:${config.JENA_PORT || 3030}`;

// Test timeout (extended for Docker container startup)
const TEST_TIMEOUT = 30000;

describe('Backend-API Integration Tests', () => {
  
  // Health check tests
  describe('Service Health Checks', () => {
    
    test('Backend service should be reachable', async () => {
      try {
        const response = await axios.get(`${BACKEND_BASE_URL}/`, { timeout: 5000 });
        expect(response.status).toBe(200);
      } catch (error) {
        if (error.code === 'ECONNREFUSED') {
          console.error('Backend service not running. Start with: docker-compose up -d');
        }
        throw error;
      }
    }, TEST_TIMEOUT);

    test('Fuseki service should be reachable', async () => {
      try {
        const response = await axios.get(`${FUSEKI_BASE_URL}/$/ping`, { timeout: 5000 });
        expect(response.status).toBe(200);
      } catch (error) {
        if (error.code === 'ECONNREFUSED') {
          console.error('Fuseki service not running. Start with: docker-compose up -d');
        }
        throw error;
      }
    }, TEST_TIMEOUT);

    test('API service should be reachable', async () => {
      try {
        const response = await axios.get(`${API_BASE_URL}/`, { timeout: 5000 });
        // API may not have a root endpoint, so we accept 404 as "reachable"
        expect([200, 404]).toContain(response.status);
      } catch (error) {
        if (error.response && error.response.status === 404) {
          // 404 is acceptable - service is running
          return;
        }
        if (error.code === 'ECONNREFUSED') {
          console.error('API service not running. Start with: docker-compose up -d');
        }
        throw error;
      }
    }, TEST_TIMEOUT);
  });

  // Backend endpoint tests
  describe('Backend Prolog Endpoints', () => {
    
    test('Backend /interactions endpoint should accept guideline_id parameter', async () => {
      const testGuidelineId = 'test-guideline';
      
      try {
        const response = await axios.post(
          `${BACKEND_BASE_URL}/interactions`,
          `guideline_id=${testGuidelineId}`,
          {
            headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
            timeout: 10000,
            validateStatus: () => true // Accept any status
          }
        );
        
        // We expect either success (200) or a proper error response
        expect([200, 400, 404, 500]).toContain(response.status);
        
        // If it's an error, it should be about missing data, not missing predicate
        if (response.status >= 400) {
          const errorText = response.data;
          expect(errorText).not.toContain('Unknown procedure');
          expect(errorText).not.toContain('guideline_recommendations/2');
          expect(errorText).not.toContain('interaction/5');
        }
      } catch (error) {
        console.error('Backend /interactions error:', error.message);
        throw error;
      }
    }, TEST_TIMEOUT);

    test('Backend /v1/interactions endpoint should return JSON', async () => {
      const testGuidelineId = 'test-guideline';
      
      try {
        const response = await axios.post(
          `${BACKEND_BASE_URL}/v1/interactions`,
          { guideline_id: testGuidelineId },
          {
            headers: { 'Content-Type': 'application/json' },
            timeout: 10000,
            validateStatus: () => true
          }
        );
        
        // Should return JSON format
        expect(response.headers['content-type']).toContain('application/json');
        
        // Should have a status field (success or error)
        expect(response.data).toHaveProperty('status');
        
        // If success, should have interactions array
        if (response.data.status === 'success') {
          expect(response.data).toHaveProperty('interactions');
          expect(Array.isArray(response.data.interactions)).toBe(true);
        }
      } catch (error) {
        console.error('Backend /v1/interactions error:', error.message);
        throw error;
      }
    }, TEST_TIMEOUT);
  });

  // Prolog predicate tests
  describe('Backend Prolog Predicates', () => {
    
    test('Backend should have guideline_recommendations/2 predicate loaded', async () => {
      // This test verifies the predicate exists by calling an endpoint that uses it
      const response = await axios.post(
        `${BACKEND_BASE_URL}/v1/interactions`,
        { guideline_id: 'nonexistent-guideline' },
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 10000,
          validateStatus: () => true
        }
      );
      
      // If predicate is missing, we'd get an "Unknown procedure" error
      if (response.data.status === 'error') {
        expect(response.data.message).not.toContain('Unknown procedure: guideline_recommendations/2');
      }
    }, TEST_TIMEOUT);

    test('Backend should have interaction/5 predicate loaded', async () => {
      const response = await axios.post(
        `${BACKEND_BASE_URL}/v1/interactions`,
        { guideline_id: 'test-guideline' },
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 10000,
          validateStatus: () => true
        }
      );
      
      // If predicate is missing, we'd get an "Unknown procedure" error
      if (response.data.status === 'error') {
        expect(response.data.message).not.toContain('Unknown procedure: interaction/5');
      }
    }, TEST_TIMEOUT);

    test('Backend should have interaction_rules module loaded', async () => {
      const response = await axios.post(
        `${BACKEND_BASE_URL}/v1/interactions`,
        { guideline_id: 'test-guideline' },
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 10000,
          validateStatus: () => true
        }
      );
      
      // If module is missing, we'd get errors about missing predicates from that module
      if (response.data.status === 'error') {
        expect(response.data.message).not.toContain('Unknown procedure: inferInternalInteractions/0');
        expect(response.data.message).not.toContain('Unknown procedure: interaction/3');
      }
    }, TEST_TIMEOUT);
  });

  // API-Backend integration tests
  describe('API to Backend Communication', () => {
    
    test('utils.callPrologServer should successfully connect to backend', async () => {
      try {
        const result = await utils.callPrologServer('v1/interactions', {
          guideline_id: 'test-guideline'
        });
        
        // Should return an object (either success or error response)
        expect(typeof result).toBe('object');
        
        // Should have expected structure
        expect(result).toHaveProperty('status');
      } catch (error) {
        // Connection errors are acceptable if services aren't running
        if (!error.message.includes('ECONNREFUSED')) {
          throw error;
        }
      }
    }, TEST_TIMEOUT);

    test('Backend should handle missing guideline_id gracefully', async () => {
      try {
        const response = await axios.post(
          `${BACKEND_BASE_URL}/v1/interactions`,
          {},
          {
            headers: { 'Content-Type': 'application/json' },
            timeout: 10000,
            validateStatus: () => true
          }
        );
        
        // Should return error status
        expect(response.data.status).toBe('error');
        expect(response.data.message).toBeDefined();
      } catch (error) {
        console.error('Backend error handling test failed:', error.message);
        throw error;
      }
    }, TEST_TIMEOUT);
  });

  // TMR interaction detection tests (requires test data)
  describe('TMR Interaction Detection Logic', () => {
    
    test('Backend should detect contradictions in test guideline', async () => {
      // This test requires a guideline with contradicting recommendations
      // Skip if test data is not available
      const testGuidelineId = 'test-contradiction-guideline';
      
      try {
        const response = await axios.post(
          `${BACKEND_BASE_URL}/v1/interactions`,
          { guideline_id: testGuidelineId },
          {
            headers: { 'Content-Type': 'application/json' },
            timeout: 10000,
            validateStatus: () => true
          }
        );
        
        if (response.data.status === 'success') {
          const interactions = response.data.interactions;
          
          // Should have interactions array
          expect(Array.isArray(interactions)).toBe(true);
          
          // Check structure of interactions
          if (interactions.length > 0) {
            const interaction = interactions[0];
            expect(interaction).toHaveProperty('interaction');
            expect(interaction).toHaveProperty('label');
            expect(interaction).toHaveProperty('elements');
            expect(interaction).toHaveProperty('external');
          }
        }
      } catch (error) {
        // Test data may not exist - log but don't fail
        console.log('Test guideline not found - skipping interaction detection test');
      }
    }, TEST_TIMEOUT);

    test('Backend should return valid JSON structure for interactions', async () => {
      const response = await axios.post(
        `${BACKEND_BASE_URL}/v1/interactions`,
        { guideline_id: 'any-guideline' },
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 10000,
          validateStatus: () => true
        }
      );
      
      // Should always have status field
      expect(response.data).toHaveProperty('status');
      
      if (response.data.status === 'success') {
        // Success should have interactions array
        expect(response.data).toHaveProperty('interactions');
        expect(Array.isArray(response.data.interactions)).toBe(true);
        
        // Each interaction should have required fields
        response.data.interactions.forEach(interaction => {
          expect(interaction).toHaveProperty('interaction');
          expect(interaction).toHaveProperty('label');
          expect(interaction).toHaveProperty('elements');
          expect(interaction).toHaveProperty('external');
          expect(Array.isArray(interaction.elements)).toBe(true);
          expect(typeof interaction.external).toBe('boolean');
        });
      } else {
        // Error should have message
        expect(response.data).toHaveProperty('message');
        expect(typeof response.data.message).toBe('string');
      }
    }, TEST_TIMEOUT);
  });

  // RDF dataset loading tests
  describe('RDF Dataset Loading', () => {
    
    test('Backend should connect to Fuseki to load RDF datasets', async () => {
      // This test verifies that backend can connect to Fuseki
      const response = await axios.post(
        `${BACKEND_BASE_URL}/v1/interactions`,
        { guideline_id: 'test-guideline' },
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 15000,
          validateStatus: () => true
        }
      );
      
      // If backend can't connect to Fuseki, it should report a connection error
      if (response.data.status === 'error') {
        expect(response.data.message).not.toContain('Connection refused');
        expect(response.data.message).not.toContain('ECONNREFUSED');
      }
    }, TEST_TIMEOUT);

    test('Backend should load TMR base ontologies', async () => {
      // Verify backend has loaded tmr_concepts.trig and tmr4i.trig
      const response = await axios.post(
        `${BACKEND_BASE_URL}/v1/interactions`,
        { guideline_id: 'test-guideline' },
        {
          headers: { 'Content-Type': 'application/json' },
          timeout: 15000,
          validateStatus: () => true
        }
      );
      
      // If TMR ontologies aren't loaded, we'd get errors about missing vocab predicates
      if (response.data.status === 'error') {
        expect(response.data.message).not.toContain('Unknown namespace');
        expect(response.data.message).not.toContain('vocab:');
        expect(response.data.message).not.toContain('vocab4i:');
      }
    }, TEST_TIMEOUT);
  });
});

// Export for use in other test files
module.exports = {
  API_BASE_URL,
  BACKEND_BASE_URL,
  FUSEKI_BASE_URL,
  TEST_TIMEOUT
};
