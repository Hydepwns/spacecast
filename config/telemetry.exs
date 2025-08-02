import Config

# Telemetry configuration for Spacecast
config :spacecast, :telemetry,
  # Enable telemetry collection
  enabled: true,

  # Metrics collection interval (in milliseconds)
  collection_interval: 10_000,

  # Storage configuration
  storage: %{
    # ETS table names for metrics storage
    validation_errors: :validation_errors,
    validation_successes: :validation_successes,
    validation_stats: :validation_stats,
    validation_view_distribution: :validation_view_distribution,

    # Performance metrics storage
    performance_metrics: :performance_metrics,
    resource_metrics: :resource_metrics,
    event_metrics: :event_metrics
  },

  # Alerting thresholds
  alerts: %{
    # Validation error rate threshold (errors per minute)
    validation_error_rate_threshold: 10.0,

    # Performance thresholds (in milliseconds)
    slow_query_threshold: 1000,
    slow_request_threshold: 5000,

    # Resource usage thresholds
    # 80% of available memory
    memory_usage_threshold: 0.8,
    # 90% CPU usage
    cpu_usage_threshold: 0.9,
    # 90% disk usage
    disk_usage_threshold: 0.9
  },

  # Reporting configuration
  reporting: %{
    # Enable console reporting in development
    console_enabled: config_env() == :dev,

    # Enable metrics export for external monitoring
    export_enabled: config_env() == :prod,

    # Export format (prometheus, statsd, etc.)
    export_format: :prometheus,

    # Export endpoint (if using HTTP export)
    export_endpoint: System.get_env("METRICS_EXPORT_ENDPOINT"),

    # Export interval (in seconds)
    export_interval: 60
  }

# Configure telemetry handlers
config :telemetry,
  # Attach handlers for Phoenix events
  phoenix_events: [
    # Endpoint events
    [:phoenix, :endpoint, :start],
    [:phoenix, :endpoint, :stop],

    # Router events
    [:phoenix, :router_dispatch, :start],
    [:phoenix, :router_dispatch, :stop],
    [:phoenix, :router_dispatch, :exception],

    # LiveView events
    [:phoenix, :live_view, :mount, :start],
    [:phoenix, :live_view, :mount, :stop],
    [:phoenix, :live_view, :mount, :exception],
    [:phoenix, :live_view, :handle_event, :start],
    [:phoenix, :live_view, :handle_event, :stop],
    [:phoenix, :live_view, :handle_event, :exception],
    [:phoenix, :live_view, :handle_params, :start],
    [:phoenix, :live_view, :handle_params, :stop],
    [:phoenix, :live_view, :handle_params, :exception],

    # Socket events
    [:phoenix, :socket_connected],
    [:phoenix, :socket_disconnected],
    [:phoenix, :socket_joined],
    [:phoenix, :socket_left]
  ],

  # Attach handlers for Ecto events
  ecto_events: [
    [:spacecast, :repo, :query, :start],
    [:spacecast, :repo, :query, :stop],
    [:spacecast, :repo, :query, :exception]
  ],

  # Attach handlers for custom events
  custom_events: [
    # Socket validation events
    [:spacecast, :socket, :validation, :error],
    [:spacecast, :socket, :validation, :success],
    [:spacecast, :socket, :validation, :metrics],

    # Event system events
    [:spacecast, :events, :process, :start],
    [:spacecast, :events, :process, :stop],
    [:spacecast, :events, :process, :exception],

    # Resource events
    [:spacecast, :resources, :operation, :start],
    [:spacecast, :resources, :operation, :stop],
    [:spacecast, :resources, :operation, :exception],

    # Performance events
    [:spacecast, :performance, :measurement],
    [:spacecast, :performance, :alert]
  ]

# Configure telemetry metrics
config :telemetry_metrics,
  # Enable default metrics collection
  enabled: true,

  # Metrics configuration
  metrics: [
    # Phoenix metrics
    %{
      name: "phoenix.endpoint.stop.duration",
      type: :summary,
      unit: {:native, :millisecond},
      description: "Phoenix endpoint request duration"
    },
    %{
      name: "phoenix.router_dispatch.stop.duration",
      type: :summary,
      unit: {:native, :millisecond},
      tags: [:route],
      description: "Phoenix router dispatch duration"
    },

    # Database metrics
    %{
      name: "spacecast.repo.query.total_time",
      type: :summary,
      unit: {:native, :millisecond},
      description: "Database query total time"
    },
    %{
      name: "spacecast.repo.query.query_time",
      type: :summary,
      unit: {:native, :millisecond},
      description: "Database query execution time"
    },

    # VM metrics
    %{
      name: "vm.memory.total",
      type: :summary,
      unit: {:byte, :kilobyte},
      description: "Total VM memory usage"
    },
    %{
      name: "vm.total_run_queue_lengths.total",
      type: :summary,
      description: "Total run queue length"
    },

    # Custom metrics
    %{
      name: "spacecast.socket.validation.error.count",
      type: :counter,
      tags: [:view_module, :error_type],
      description: "Socket validation error count"
    },
    %{
      name: "spacecast.socket.validation.success.count",
      type: :counter,
      tags: [:view_module],
      description: "Socket validation success count"
    },
    %{
      name: "spacecast.events.process.duration",
      type: :summary,
      unit: {:native, :millisecond},
      tags: [:event_type, :handler],
      description: "Event processing duration"
    }
  ]

# Configure telemetry reporters
config :telemetry_metrics,
  reporters: [
    # Console reporter for development
    {Telemetry.Metrics.ConsoleReporter, metrics: Spacecast.Telemetry.metrics(), enabled: config_env() == :dev},

    # Prometheus reporter for production
    {Telemetry.Metrics.PrometheusReporter,
     metrics: Spacecast.Telemetry.metrics(),
     enabled: config_env() == :prod,
     port: String.to_integer(System.get_env("PROMETHEUS_PORT") || "9568")}
  ]
