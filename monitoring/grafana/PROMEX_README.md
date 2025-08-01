# PromEx Integration for Event Monitoring

This project now includes PromEx for Prometheus metrics collection and monitoring.

## What's Included

1. **PromEx Configuration** (`lib/spacecast/prom_ex.ex`)
   - Built-in plugins for Phoenix, Ecto, Oban, and more
   - Custom dashboard for event monitoring
   - Metrics server on port 9568

2. **Event Monitoring Telemetry** (`lib/spacecast/events/core/event_monitor/telemetry.ex`)
   - Event processing metrics
   - Slow processing detection
   - Error tracking
   - Queue size monitoring
   - Backpressure monitoring

3. **Custom Dashboard** (`priv/prometheus/dashboards/event_monitoring.json`)
   - Event processing rate
   - 95th percentile processing duration
   - Error rate
   - Queue size
   - Backpressure status
   - Event rate per second

## Setup

1. **Dependencies**: PromEx is already added to `mix.exs`
2. **Configuration**: PromEx is configured in `config/config.exs`
3. **Application**: PromEx is started in the supervision tree

## Usage

### Starting the Application

```bash
mix phx.server
```

### Accessing Metrics

- **Prometheus Metrics**: http://localhost:9568/metrics
- **Grafana Dashboard**: http://localhost:3000 (if Grafana is running)

### Current Implementation

The telemetry handlers currently log metrics to the console. To enable actual Prometheus metrics:

1. Set up a Prometheus server to scrape metrics from `http://localhost:9568/metrics`
2. Set up Grafana to visualize the metrics
3. Update the telemetry handlers to use actual Prometheus functions

### Metrics Available

- `event_processing_total` - Total events processed
- `event_processing_duration_seconds` - Processing duration
- `event_slow_processing_total` - Slow processing events
- `event_processing_errors_total` - Processing errors
- `event_queue_size` - Current queue size
- `event_backpressure_status` - Backpressure status
- `event_rate_per_second` - Event processing rate

## Next Steps

1. Set up Prometheus server
2. Configure Grafana with the custom dashboard
3. Replace logging with actual Prometheus metric calls
4. Set up alerting rules

## Configuration

The PromEx configuration can be customized in `config/config.exs`:

```elixir
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
``` 