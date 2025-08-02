defmodule Spacecast.Events.Core.EventMonitor.Telemetry do
  @moduledoc """
  Handles telemetry events for the EventMonitor system.

  This module is responsible for:
  - Registering telemetry handlers
  - Processing telemetry events
  - Sending alerts based on telemetry data
  - Integrating with external monitoring systems
  """

  require Logger

  alias Spacecast.Events.Core.NotificationSystem

  @doc """
  Registers telemetry handlers for event monitoring.
  """
  @spec register_handlers() :: :ok | {:error, any()}
  def register_handlers do
    config = %{
      enable_external_monitoring: Application.get_env(:spacecast, :enable_external_monitoring, false),
      external_monitoring_config: Application.get_env(:spacecast, :external_monitoring_config, %{}),
      alert_thresholds: %{
        slow_processing_ms: Application.get_env(:spacecast, :slow_processing_threshold_ms, 1000),
        high_queue_size: Application.get_env(:spacecast, :high_queue_threshold, 500),
        critical_error_rate: Application.get_env(:spacecast, :critical_error_rate_threshold, 0.1)
      },
      enable_detailed_logging: Application.get_env(:spacecast, :enable_detailed_telemetry_logging, false)
    }

    case :telemetry.attach_many(
           "spacecast-liveview-events-monitor",
           [
             [:spacecast, :events, :process],
             [:spacecast, :events, :queue_size],
             [:spacecast, :events, :backpressure],
             [:spacecast, :events, :metrics]
           ],
           &handle_telemetry_event/4,
           config
         ) do
      :ok ->
        Logger.info("EventMonitor: Telemetry handlers registered successfully")
        :ok

      {:error, reason} ->
        Logger.error("EventMonitor: Failed to register telemetry handlers: #{inspect(reason)}")
        {:error, reason}
    end
  rescue
    e ->
      Logger.error("EventMonitor: Exception while registering telemetry handlers: #{inspect(e)}")
      {:error, e}
  end

  # Handle telemetry events for monitoring and alerting
  defp handle_telemetry_event([:spacecast, :events, :process], measurements, metadata, config) do
    try do
      duration_ms = Map.get(measurements, :duration, 0)
      event_type = Map.get(metadata, :event_type, "unknown")
      handler = Map.get(metadata, :handler, "unknown")
      status = Map.get(metadata, :status, :success)

      # Check for slow processing
      slow_threshold = config.alert_thresholds.slow_processing_ms

      if duration_ms > slow_threshold do
        Logger.warning(
          "Slow event processing: #{event_type} by #{handler} took #{duration_ms}ms (>#{slow_threshold}ms)"
        )

        # Send alert for slow processing
        send_slow_processing_alert(event_type, handler, duration_ms, slow_threshold, config)
      end

      # Check for processing errors
      if status == :error do
        Logger.error("Event processing error: #{event_type} by #{handler} failed")

        # Send alert for processing errors
        send_processing_error_alert(event_type, handler, metadata, config)
      end

      # Detailed logging if enabled
      if config.enable_detailed_logging do
        Logger.debug("Event processing: #{event_type} by #{handler} in #{duration_ms}ms (status: #{status})")
      end

      # Send to external monitoring if enabled
      if config.enable_external_monitoring do
        send_to_external_monitoring(:process, measurements, metadata, config)
      end
    rescue
      e ->
        Logger.error("Error in process telemetry handler: #{inspect(e)}")
    end
  end

  defp handle_telemetry_event([:spacecast, :events, :queue_size], measurements, metadata, config) do
    try do
      queue_size = Map.get(measurements, :size, 0)
      handler = Map.get(metadata, :handler, "unknown")

      # Check for high queue sizes
      high_threshold = config.alert_thresholds.high_queue_size

      if queue_size > high_threshold do
        Logger.warning("High queue size: #{handler} has #{queue_size} items (>#{high_threshold})")

        # Send alert for high queue size
        send_high_queue_alert(handler, queue_size, high_threshold, config)
      end

      # Detailed logging if enabled
      if config.enable_detailed_logging do
        Logger.debug("Queue size for #{handler}: #{queue_size}")
      end

      # Send to external monitoring if enabled
      if config.enable_external_monitoring do
        send_to_external_monitoring(:queue_size, measurements, metadata, config)
      end
    rescue
      e ->
        Logger.error("Error in queue_size telemetry handler: #{inspect(e)}")
    end
  end

  defp handle_telemetry_event([:spacecast, :events, :backpressure], measurements, metadata, config) do
    try do
      status = Map.get(metadata, :status, :normal)
      value = Map.get(measurements, :value, 0)

      # Log backpressure status changes
      case status do
        :critical ->
          Logger.error("CRITICAL backpressure detected in event system (value: #{value})")
          send_critical_backpressure_alert(metadata, config)

        :warning ->
          Logger.warning("WARNING backpressure detected in event system (value: #{value})")
          send_warning_backpressure_alert(metadata, config)

        :normal ->
          Logger.info("Backpressure status: #{status} (value: #{value})")
      end

      # Send to external monitoring if enabled
      if config.enable_external_monitoring do
        send_to_external_monitoring(:backpressure, measurements, metadata, config)
      end
    rescue
      e ->
        Logger.error("Error in backpressure telemetry handler: #{inspect(e)}")
    end
  end

  defp handle_telemetry_event([:spacecast, :events, :metrics], measurements, metadata, config) do
    try do
      event_count = Map.get(measurements, :event_count, 0)
      events_per_second = Map.get(measurements, :events_per_second, 0.0)

      # Check for unusual event rates
      if events_per_second > 1000 do
        Logger.warning("High event rate: #{events_per_second} events/sec")
      end

      # Detailed logging if enabled
      if config.enable_detailed_logging do
        Logger.debug("Event metrics: #{event_count} events, #{events_per_second} events/sec")
      end

      # Send to external monitoring if enabled
      if config.enable_external_monitoring do
        send_to_external_monitoring(:metrics, measurements, metadata, config)
      end
    rescue
      e ->
        Logger.error("Error in metrics telemetry handler: #{inspect(e)}")
    end
  end

  # Catch-all handler for unknown telemetry events
  defp handle_telemetry_event(event_name, measurements, metadata, config) do
    Logger.warning("Unknown telemetry event: #{inspect(event_name)}")

    # Still try to send to external monitoring if enabled
    if config.enable_external_monitoring do
      try do
        send_to_external_monitoring(:unknown, measurements, metadata, config)
      rescue
        e -> Logger.error("Error sending unknown event to external monitoring: #{inspect(e)}")
      end
    end
  end

  # Alert functions
  defp send_slow_processing_alert(event_type, handler, duration_ms, threshold, config) do
    alert = %{
      type: :slow_processing,
      level: :warning,
      message: "Event processing is slow",
      details: %{
        event_type: event_type,
        handler: handler,
        duration_ms: duration_ms,
        threshold_ms: threshold
      },
      timestamp: DateTime.utc_now()
    }

    send_alert(alert, config)
  end

  defp send_processing_error_alert(event_type, handler, metadata, config) do
    alert = %{
      type: :processing_error,
      level: :error,
      message: "Event processing failed",
      details: %{
        event_type: event_type,
        handler: handler,
        error_details: Map.get(metadata, :details, %{})
      },
      timestamp: DateTime.utc_now()
    }

    send_alert(alert, config)
  end

  defp send_high_queue_alert(handler, queue_size, threshold, config) do
    alert = %{
      type: :high_queue_size,
      level: :warning,
      message: "Event queue size is high",
      details: %{
        handler: handler,
        queue_size: queue_size,
        threshold: threshold
      },
      timestamp: DateTime.utc_now()
    }

    send_alert(alert, config)
  end

  defp send_critical_backpressure_alert(metadata, config) do
    alert = %{
      type: :critical_backpressure,
      level: :critical,
      message: "Critical backpressure in event system",
      details: metadata,
      timestamp: DateTime.utc_now()
    }

    send_alert(alert, config)
  end

  defp send_warning_backpressure_alert(metadata, config) do
    alert = %{
      type: :warning_backpressure,
      level: :warning,
      message: "Warning backpressure in event system",
      details: metadata,
      timestamp: DateTime.utc_now()
    }

    send_alert(alert, config)
  end

  # Send alert through notification system
  defp send_alert(alert, _config) do
    try do
      # Use the NotificationSystem if available
      if Code.ensure_loaded(NotificationSystem) do
        NotificationSystem.send_alert(alert,
          channels: [:in_app, :log],
          recipients: :admins_only
        )
      else
        # Fallback to logging
        Logger.warning("Alert: #{alert.message} - #{inspect(alert.details)}")
      end
    rescue
      e -> Logger.error("Failed to send alert: #{inspect(e)}")
    end
  end

  # Send data to external monitoring systems
  defp send_to_external_monitoring(event_type, measurements, metadata, config) do
    try do
      external_config = config.external_monitoring_config

      # Example: Send to Prometheus metrics
      if Map.get(external_config, :prometheus_enabled, false) do
        send_to_prometheus(event_type, measurements, metadata, external_config)
      end

      # Example: Send to DataDog
      if Map.get(external_config, :datadog_enabled, false) do
        send_to_datadog(event_type, measurements, metadata, external_config)
      end

      # Example: Send to custom HTTP endpoint
      if Map.get(external_config, :http_endpoint) do
        send_to_http_endpoint(event_type, measurements, metadata, external_config)
      end
    rescue
      e -> Logger.error("Error sending to external monitoring: #{inspect(e)}")
    end
  end

  # PromEx implementations for external monitoring systems
  defp send_to_prometheus(event_type, measurements, metadata, _config) do
    try do
      case event_type do
        :event_processing ->
          handle_event_processing_metrics(measurements, metadata)

        :slow_processing ->
          handle_slow_processing_metrics(measurements, metadata)

        :processing_error ->
          handle_processing_error_metrics(measurements, metadata)

        :queue_size ->
          handle_queue_size_metrics(measurements, metadata)

        :backpressure ->
          handle_backpressure_metrics(measurements, metadata)

        :metrics ->
          handle_general_metrics(measurements, metadata)

        _ ->
          # Handle unknown event types
          :ok
      end
    rescue
      e ->
        Logger.error("Error sending to Prometheus: #{inspect(e)}")
        :ok
    end
  end

  defp handle_event_processing_metrics(measurements, metadata) do
    duration = Map.get(measurements, :duration, 0)
    event_type = Map.get(metadata, :event_type, "unknown")
    handler = Map.get(metadata, :handler, "unknown")

    # Send metrics to external monitoring via HTTP
    send_metrics_via_http(:event_processing, %{
      duration: duration,
      event_type: event_type,
      handler: handler
    })

    :ok
  end

  defp handle_slow_processing_metrics(measurements, metadata) do
    duration = Map.get(measurements, :duration, 0)
    event_type = Map.get(metadata, :event_type, "unknown")
    handler = Map.get(metadata, :handler, "unknown")
    threshold = Map.get(measurements, :threshold, 0)

    # Send slow processing metrics to external monitoring
    send_metrics_via_http(:slow_processing, %{
      duration: duration,
      event_type: event_type,
      handler: handler,
      threshold: threshold
    })

    :ok
  end

  defp handle_processing_error_metrics(_measurements, metadata) do
    event_type = Map.get(metadata, :event_type, "unknown")
    handler = Map.get(metadata, :handler, "unknown")
    error_type = Map.get(metadata, :error_type, "unknown")

    # Send error metrics to external monitoring
    send_metrics_via_http(:processing_error, %{
      event_type: event_type,
      handler: handler,
      error_type: error_type
    })

    :ok
  end

  defp handle_queue_size_metrics(measurements, metadata) do
    queue_size = Map.get(measurements, :queue_size, 0)
    handler = Map.get(metadata, :handler, "unknown")
    threshold = Map.get(measurements, :threshold, 0)

    # Send queue size metrics to external monitoring
    send_metrics_via_http(:queue_size, %{
      handler: handler,
      queue_size: queue_size,
      threshold: threshold
    })

    :ok
  end

  defp handle_backpressure_metrics(measurements, _metadata) do
    status = Map.get(measurements, :status, "unknown")
    value = Map.get(measurements, :value, 0)

    # Send backpressure metrics to external monitoring
    send_metrics_via_http(:backpressure, %{
      status: status,
      value: value
    })

    :ok
  end

  defp handle_general_metrics(measurements, _metadata) do
    event_count = Map.get(measurements, :event_count, 0)
    events_per_second = Map.get(measurements, :events_per_second, 0.0)

    # Send general metrics to external monitoring
    send_metrics_via_http(:metrics, %{
      event_count: event_count,
      events_per_second: events_per_second
    })

    :ok
  end

  defp send_to_datadog(_event_type, _measurements, _metadata, _config) do
    # Implementation would depend on the specific DataDog library being used
    :ok
  end

  defp send_to_http_endpoint(_event_type, _measurements, _metadata, _config) do
    # Implementation would use HTTPoison or similar to send metrics
    :ok
  end

  # Send metrics via HTTP to external monitoring systems
  defp send_metrics_via_http(event_type, metrics_data) do
    try do
      # Get HTTP endpoint configuration
      endpoint = Application.get_env(:spacecast, :metrics_http_endpoint)

      if endpoint do
        payload = %{
          event_type: event_type,
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
          data: metrics_data
        }

        HTTPoison.post(endpoint, Jason.encode!(payload), [{"Content-Type", "application/json"}])
      else
        # Fallback to logging if no endpoint configured
        Logger.debug("Metrics: #{event_type} - #{inspect(metrics_data)}")
      end
    rescue
      e ->
        Logger.error("Failed to send metrics via HTTP: #{inspect(e)}")
    end
  end
end
