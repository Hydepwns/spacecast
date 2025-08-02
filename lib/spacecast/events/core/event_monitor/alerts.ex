defmodule Spacecast.Events.Core.EventMonitor.Alerts do
  @moduledoc """
  Manages alerting for event processing issues.

  This module is responsible for:
  - Setting up alerting configurations
  - Checking metrics and triggering alerts
  - Managing alert thresholds and intervals
  - Sending notifications through various channels
  """

  require Logger

  alias Spacecast.Events.Core.NotificationSystem

  # Default alert check interval (1 minute)
  @default_alert_interval 60_000

  @doc """
  Sets up alerting configuration.

  ## Parameters
  * `notification_function` - Function to call when alert is triggered (for backward compatibility)
  * `opts` - Options for alerting:
    * `:interval_ms` - Alert check interval in milliseconds
    * `:lookback_seconds` - How far back to look for metrics
    * `:threshold_overrides` - Map of thresholds to override
    * `:notification_channels` - List of channels to notify (defaults to [:in_app, :log])
    * `:recipients` - Who should receive notifications (defaults to :admins_only)

  ## Returns
  * Alert configuration map
  """
  @spec setup_alerting((map() -> any()) | nil, keyword()) :: map()
  def setup_alerting(notification_function \\ nil, opts \\ [])
      when is_list(opts)
      when is_nil(notification_function) or is_function(notification_function, 1) do
    %{
      enabled: true,
      notification_fn: notification_function,
      notification_channels: Keyword.get(opts, :notification_channels, [:in_app, :log]),
      recipients: Keyword.get(opts, :recipients, :admins_only),
      interval_ms: Keyword.get(opts, :interval_ms, @default_alert_interval),
      lookback_seconds: Keyword.get(opts, :lookback_seconds, 300),
      last_alert_time: nil,
      thresholds: Keyword.get(opts, :threshold_overrides, %{})
    }
  end

  @doc """
  Clears alerting configuration.

  ## Returns
  * Disabled alert configuration map
  """
  @spec clear_alerting() :: map()
  def clear_alerting do
    %{
      enabled: false,
      notification_fn: nil,
      notification_channels: [],
      recipients: :none,
      interval_ms: @default_alert_interval,
      lookback_seconds: 300,
      last_alert_time: nil,
      thresholds: %{}
    }
  end

  @doc """
  Checks metrics and sends alerts if needed.

  ## Parameters
  * `metrics` - Current metrics data
  * `alert_config` - Alert configuration
  * `event_store` - Event store module

  ## Returns
  * Updated alert configuration
  """
  @spec check_metrics_and_alert(map(), map(), module()) :: map()
  def check_metrics_and_alert(metrics, alert_config, _event_store) do
    if alert_config.enabled and metrics.backpressure.status != :normal do
      maybe_send_backpressure_alert(metrics, alert_config)
    else
      alert_config
    end
  end

  defp maybe_send_backpressure_alert(metrics, alert_config) do
    now = DateTime.utc_now()

    if should_send_alert?(alert_config.last_alert_time, now) do
      alert = create_backpressure_alert(metrics.backpressure.status, metrics)
      send_alert(alert, alert_config)
      %{alert_config | last_alert_time: now}
    else
      alert_config
    end
  end

  defp should_send_alert?(nil, _now), do: true

  defp should_send_alert?(last_alert_time, now) do
    DateTime.diff(now, last_alert_time, :second) >= 300
  end

  @doc """
  Sends an alert through the notification system.

  ## Parameters
  * `alert` - Alert data
  * `alert_config` - Alert configuration

  ## Returns
  * `:ok` - Alert sent successfully
  * `{:error, reason}` - Failed to send alert
  """
  @spec send_alert(map(), map()) :: :ok | {:error, any()}
  def send_alert(alert, alert_config) do
    try do
      # Use the NotificationSystem if available
      case Code.ensure_loaded(NotificationSystem) do
        {:module, NotificationSystem} ->
          NotificationSystem.send_alert(alert,
            channels: alert_config.notification_channels,
            recipients: alert_config.recipients
          )

        _ ->
          # Fallback to logging
          Logger.warning("Alert: #{alert.message} - #{inspect(alert.details)}")
      end

      # For backward compatibility, still call the notification function if provided
      if alert_config.notification_fn != nil do
        alert_config.notification_fn.(alert)
      end

      :ok
    rescue
      e ->
        Logger.error("Failed to send alert: #{inspect(e)}")
        {:error, e}
    end
  end

  @doc """
  Creates a slow processing alert.

  ## Parameters
  * `event_type` - Type of event that was slow
  * `handler` - Handler that processed the event
  * `duration_ms` - Processing duration in milliseconds
  * `threshold_ms` - Threshold that was exceeded

  ## Returns
  * Alert data map
  """
  @spec create_slow_processing_alert(any(), any(), integer(), integer()) :: map()
  def create_slow_processing_alert(event_type, handler, duration_ms, threshold_ms) do
    %{
      type: :slow_processing,
      level: :warning,
      message: "Event processing is slow",
      details: %{
        event_type: event_type,
        handler: handler,
        duration_ms: duration_ms,
        threshold_ms: threshold_ms
      },
      timestamp: DateTime.utc_now()
    }
  end

  @doc """
  Creates a processing error alert.

  ## Parameters
  * `event_type` - Type of event that failed
  * `handler` - Handler that processed the event
  * `error_details` - Details about the error

  ## Returns
  * Alert data map
  """
  @spec create_processing_error_alert(any(), any(), map()) :: map()
  def create_processing_error_alert(event_type, handler, error_details) do
    %{
      type: :processing_error,
      level: :error,
      message: "Event processing failed",
      details: %{
        event_type: event_type,
        handler: handler,
        error_details: error_details
      },
      timestamp: DateTime.utc_now()
    }
  end

  @doc """
  Creates a high queue size alert.

  ## Parameters
  * `handler` - Handler with high queue
  * `queue_size` - Current queue size
  * `threshold` - Threshold that was exceeded

  ## Returns
  * Alert data map
  """
  @spec create_high_queue_alert(any(), integer(), integer()) :: map()
  def create_high_queue_alert(handler, queue_size, threshold) do
    %{
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
  end

  @doc """
  Creates a backpressure alert.

  ## Parameters
  * `status` - Backpressure status (:warning or :critical)
  * `details` - Backpressure details

  ## Returns
  * Alert data map
  """
  @spec create_backpressure_alert(atom(), map()) :: map()
  def create_backpressure_alert(status, details) do
    level = if status == :critical, do: :critical, else: :warning

    message =
      if status == :critical, do: "Critical backpressure in event system", else: "Warning backpressure in event system"

    %{
      type: :backpressure,
      level: level,
      message: message,
      details: details,
      timestamp: DateTime.utc_now()
    }
  end

  @doc """
  Schedules the next alert check.

  ## Parameters
  * `interval_ms` - Interval in milliseconds

  ## Returns
  * `:ok` - Timer scheduled
  """
  @spec schedule_alert_check(integer()) :: :ok
  def schedule_alert_check(interval_ms) do
    Process.send_after(self(), :check_and_alert, interval_ms)
    :ok
  end

  @doc """
  Validates alert configuration.

  ## Parameters
  * `config` - Alert configuration to validate

  ## Returns
  * `{:ok, config}` - Valid configuration
  * `{:error, reason}` - Invalid configuration
  """
  @spec validate_config(map()) :: {:ok, map()} | {:error, String.t()}
  def validate_config(config) do
    cond do
      not is_map(config) ->
        {:error, "Alert config must be a map"}

      not is_boolean(config.enabled) ->
        {:error, "Alert config enabled must be a boolean"}

      not is_integer(config.interval_ms) or config.interval_ms <= 0 ->
        {:error, "Alert config interval_ms must be a positive integer"}

      not is_integer(config.lookback_seconds) or config.lookback_seconds <= 0 ->
        {:error, "Alert config lookback_seconds must be a positive integer"}

      not is_list(config.notification_channels) ->
        {:error, "Alert config notification_channels must be a list"}

      true ->
        {:ok, config}
    end
  end
end
