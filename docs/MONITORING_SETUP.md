# Monitoring Setup Guide

This guide explains how to set up and use the monitoring system for Spacecast.

## Overview

The monitoring system consists of:

- **PromEx**: Elixir library for Prometheus metrics
- **Prometheus**: Time-series database for metrics collection
- **Grafana**: Visualization and dashboard platform
- **HTTPoison**: HTTP client for sending metrics to external systems

## Quick Start

### 1. Start the Monitoring Stack

```bash
./monitoring/start_monitoring.sh
```

This will start:

- Prometheus on <http://localhost:9090>
- Grafana on <http://localhost:3000> (admin/admin)

### 2. Start Your Application

```bash
mix phx.server
```

Your application will now expose metrics at <http://localhost:9568/metrics>

### 3. Access Dashboards

- **Grafana**: <http://localhost:3000> (admin/admin)
- **Prometheus**: <http://localhost:9090>
- **Application Metrics**: <http://localhost:9568/metrics>

## Configuration

### Application Configuration

The monitoring is configured in `config/config.exs`:

```elixir
# PromEx Configuration
config :spacecast, Spacecast.PromEx,
  manual_metrics_start_delay: :no_delay,
  grafana: [
    host: "http://localhost:3000",
    username: "admin",
    password: "admin",
    upload_dashboards_on_start: true
  ],
  metrics_server: [
    port: 9568,
    path: "/metrics",
    protocol: :http
  ]

# External Monitoring Configuration
config :spacecast,
  enable_external_monitoring: true,
  metrics_http_endpoint: "http://localhost:9090/api/v1/write",
  external_monitoring_config: %{
    prometheus_enabled: true,
    http_endpoint: "http://localhost:9090/api/v1/write"
  }
```

### Prometheus Configuration

Prometheus is configured via `monitoring/prometheus.yml` to scrape metrics from your application.

## Metrics Available

The system tracks the following metrics:

### Event Processing Metrics

- `event_processing_total` - Total events processed
- `event_processing_duration_seconds` - Processing duration
- `event_slow_processing_total` - Slow processing events
- `event_processing_errors_total` - Processing errors

### Queue Metrics

- `event_queue_size` - Current queue size
- `event_queue_threshold` - Queue threshold
- `event_high_queue_total` - High queue events

### Backpressure Metrics

- `event_backpressure_status` - Backpressure status (0=normal, 1=warning, 2=critical)
- `event_backpressure_value` - Backpressure value
- `event_backpressure_total` - Backpressure events

### General Metrics

- `event_rate_per_second` - Event processing rate
- `event_total` - Total events

## HTTP Metrics Integration

The system can send metrics to external HTTP endpoints using HTTPoison:

```elixir
# Example payload sent to HTTP endpoint
%{
  event_type: :event_processing,
  timestamp: "2024-01-01T12:00:00Z",
  data: %{
    duration: 150,
    event_type: "user_created",
    handler: "UserHandler"
  }
}
```

## Custom Dashboards

A custom Grafana dashboard is automatically provisioned with:

- Event processing rate
- 95th percentile processing duration
- Error rate
- Queue size monitoring
- Backpressure status
- Event rate per second

## Troubleshooting

### Check if Services are Running

```bash
# Check Docker containers
docker ps

# Check application metrics endpoint
curl http://localhost:9568/metrics

# Check Prometheus targets
curl http://localhost:9090/api/v1/targets
```

### Common Issues

1. **Prometheus can't scrape metrics**: Ensure your application is running and the metrics endpoint is accessible
2. **Grafana can't connect to Prometheus**: Check that Prometheus is running and the datasource configuration is correct
3. **No metrics appearing**: Verify that telemetry events are being emitted and the handlers are registered

### Logs

```bash
# View Prometheus logs
docker logs prometheus

# View Grafana logs
docker logs grafana

# View application logs
tail -f log/dev.log
```

## Stopping the Monitoring Stack

```bash
docker-compose -f docker/docker-compose.monitoring.yml down
```

## Production Considerations

For production deployment:

1. **Security**: Configure authentication for Grafana and Prometheus
2. **Persistence**: Use persistent volumes for Prometheus and Grafana data
3. **Scaling**: Consider using Prometheus federation for multiple instances
4. **Alerting**: Set up alerting rules in Prometheus and Grafana
5. **Backup**: Regular backups of Prometheus and Grafana data

## Development

### Adding New Metrics

1. Add the metric to the telemetry handler in `lib/spacecast/events/core/event_monitor/telemetry.ex`
2. Update the PromEx configuration if needed
3. Add the metric to the Grafana dashboard

### Custom HTTP Endpoints

To send metrics to custom HTTP endpoints, update the configuration:

```elixir
config :spacecast,
  metrics_http_endpoint: "https://your-endpoint.com/metrics"
```

The system will automatically send JSON payloads to this endpoint for all telemetry events.
