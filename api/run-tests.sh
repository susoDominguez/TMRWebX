#!/bin/bash

# Backend Integration Test Runner
# This script runs the backend integration tests

set -e

echo "========================================"
echo "Backend Integration Test Runner"
echo "========================================"
echo ""

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running"
    echo "Please start Docker and try again"
    exit 1
fi

echo "✓ Docker is running"
echo ""

# Check if services are running
echo "Checking Docker services..."
if ! docker ps | grep -q "reasoner_service"; then
    echo "⚠️  Backend service (reasoner_service) is not running"
    echo ""
    read -p "Would you like to start all services? (y/n) " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "Starting Docker services..."
        cd "$(dirname "$0")/.." && docker-compose up -d
        echo ""
        echo "Waiting for services to be ready (30 seconds)..."
        sleep 30
    else
        echo "Please start services manually with: docker-compose up -d"
        exit 1
    fi
else
    echo "✓ Backend service is running"
fi

if ! docker ps | grep -q "store_service"; then
    echo "⚠️  Fuseki service (store_service) is not running"
else
    echo "✓ Fuseki service is running"
fi

if ! docker ps | grep -q "interaction_service"; then
    echo "⚠️  API service (interaction_service) is not running"
else
    echo "✓ API service is running"
fi

echo ""
echo "========================================"
echo "Running Backend Integration Tests"
echo "========================================"
echo ""

# Change to api directory
cd "$(dirname "$0")"

# Run the manual test script
node tests/manual-backend-test.js

echo ""
echo "========================================"
echo "Test run complete!"
echo "========================================"
echo ""
echo "To run Jest tests, use: npm test"
echo "To run with verbose output: npm run test:verbose"
echo ""
