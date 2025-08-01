#!/bin/bash

# Start Prometheus and Grafana monitoring stack
echo "Starting monitoring stack..."

# Create necessary directories if they don't exist
mkdir -p grafana/provisioning/datasources
mkdir -p grafana/provisioning/dashboards

# Start the monitoring services
docker-compose -f docker/docker-compose.monitoring.yml up -d

echo "Monitoring stack started!"
echo ""
echo "Access points:"
echo "- Prometheus: http://localhost:9090"
echo "- Grafana: http://localhost:3000 (admin/admin)"
echo "- SpacecastLiveview Metrics: http://localhost:9568/metrics"
echo ""
echo "To stop the monitoring stack:"
echo "docker-compose -f docker/docker-compose.monitoring.yml down" 