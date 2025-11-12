#!/bin/bash

# TMRWebX Development Environment Startup Script
echo "🚀 Starting TMRWebX Development Environment with Tilt..."

# Check if Tilt is installed
if ! command -v tilt &> /dev/null; then
    echo "❌ Tilt is not installed. Please install it first:"
    echo "   brew install tilt-dev/tap/tilt"
    exit 1
fi

# Check if Docker is running
if ! docker info &> /dev/null; then
    echo "❌ Docker is not running. Please start Docker first."
    exit 1
fi

# Start the development environment
echo "📊 Starting Tilt development environment..."
echo "   This will:"
echo "   - Build and start all services (API, Database, Reasoner)"
echo "   - Enable live code reloading for the API"
echo "   - Set up debugging on port 9229"
echo "   - Initialize Fuseki datasets"
echo ""
echo "🌐 Services will be available at:"
echo "   - API: http://localhost:8888"
echo "   - Fuseki Database: http://localhost:3030"
echo "   - Prolog Reasoner: http://localhost:1234"
echo "   - Debug Port: localhost:9229"
echo ""
echo "Press Ctrl+C to stop all services"
echo ""

# Run Tilt
tilt up

# Clean up on exit
echo "🧹 Cleaning up..."
tilt down
